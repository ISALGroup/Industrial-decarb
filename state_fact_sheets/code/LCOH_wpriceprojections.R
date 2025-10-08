# Projected Electricity Price Work # 
## Investigating the difference in outcomes when using projected electricity prices 

#### SET-UP ####
# Load Libraries
library(readxl)
library(readr)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(glue)

# Set state :) 
state <- "MN"

# Get census region
census_region <- 
  case_when(state %in% c('IL', 'MI') ~ 'eastnorthcentral', 
            state %in% c('MN') ~ 'westnorthcentral'
)

# Tech scenario input 
tech_input_df <- 
  read_excel(glue("LCOH modelling/output/longform_{state}.xlsx")) %>%
  mutate(facility_name = tolower(facility_name), 
         baseline_co2e_emissions = elec_ghg_emissions + noelec_ghg_emissions) %>% 
  select(-1, -facility_id, -opex) #removing for now til i get complete results from antoine

# TEMPORARY: natgas capex calculation
natgas_best <- 
  tech_input_df %>%
  filter(tech_scenario == 'Baseline') %>%
  mutate(tech_scenario = 'BaselineBest', 
         # best case natural gas 
         capex = (1.81 * (heat_mmbtu/.9) * 293.071) / 8000)

tech_input_df <- 
  tech_input_df %>%
  mutate(
    tech_scenario = if_else(tech_scenario == 'Baseline', 'BaselineWorst', tech_scenario),
    # worst case natural gas
    capex = if_else(tech_scenario == 'BaselineWorst', (18.11 * (heat_mmbtu/.75) * 293.071) / 8000, capex)
  ) %>%
  bind_rows(natgas_best)


# # Pull in lat and long from rlps file
# facility_lat_long <- 
#   read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>% 
#   select(facility_id, latitude, longitude) %>% 
#   distinct(facility_id, .keep_all = TRUE)
# 
# # Pull in facility info, this file doesn't have lat and long... (maybe add in the future)
# facility_info <- 
#   read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>% 
#   clean_names() %>% 
#   rename(naics_code = primary_naics) %>% 
#   rename(naics_description = naics_title) %>% 
#   mutate(facility_name = tolower(facility_name)) %>% 
#   select(facility_name, facility_id, naics_code, naics_description, county_fips, subregion) %>% 
#   inner_join(facility_lat_long, by = "facility_id") %>% 
#   mutate(
#     sector = case_when(
#       str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food & Beverage",
#       str_starts(naics_code, "325") ~ "Chemicals",
#       str_starts(naics_code, "322") ~ "Pulp & Paper",
#       TRUE ~ "Other Manufacturing"
#     )
#   )

fuel_prices <- 
  read_csv(glue("data/Energy projection data/AEO_price_projection_{census_region}_edit.csv")) |>
  filter(fuel_type %in% c('Natural Gas', 'Electricity') & projection_scenario == "Reference case") |>
  select(fuel_type, units, as.character(2024:2050)) |>
  pivot_longer(
    cols = any_of(as.character(2024:2050)),
    names_to = "year",
    values_to = "price"
  ) |>
  mutate(
    price = case_when(fuel_type == 'Electricity' ~ price / 293.07107, # Convert from $/mmbtu
                      TRUE ~ price), 
    units = case_when(fuel_type == 'Electricity' ~ "2024 $/kWh", 
                      TRUE ~ units)
  )

# --- Merge Inputs ---
tech_combined_df <- 
  tech_input_df %>%
  left_join(facility_info, by = "facility_name") 

#### LCOH Function #### 
lcoh_func <- function(
    ## parameters
  r, 
  elec_price_vector,
  ng_price_vector,
  t, # for now, assume same lifetime across equipment, but we can change this by technology later 
  # nat gas boiler assumptions
  #t_ngboiler,
  ngboiler_om_best, 
  ngboiler_om_worst, 
  # e-boiler assumptions
  #t_eboiler, 
  eboiler_om_best, 
  eboiler_om_worst, 
  # hp assumptions
  #t_hthp, 
  hthp_om_best, 
  hthp_om_worst, 
  
  ## tech scenario + calculations 
  tech_scenario,
  capex,
  heat_mmbtu,
  change_in_electricity_demand_kwh,
  
  ## policy scenarios
  capex_subsidy, 
  elec_discount){
  # time discounting formula 
  
  # Vector of per-year discount factors 
  discount_factors <- (1 + r)^-(seq_len(t))
  
  ## Inputting different parameters for different tech scenarios 
  case_when(
    str_detect(tech_scenario, "BaselineWorst") ~ {
      # Annual electricity + O&M costs 
      annual_ng_costs <- (heat_mmbtu/.75) * ng_price_vector 
      annual_om_costs <- ngboiler_om_worst * capex 
      
      # Apply discount factors to get present value of lifetime electricity + O&M costs
      PV_ng <- sum(annual_ng_costs * discount_factors)
      PV_om   <- sum(annual_om_costs * discount_factors)
      
      # LCOH formula
      numerator   <- capex + PV_om + PV_ng
      denominator <- sum(heat_mmbtu * discount_factors)
      lcoh <- numerator / denominator
      
      # Return value 
      lcoh
    }, 
    str_detect(tech_scenario, "BaselineBest") ~ {
      annual_ng_costs <- (heat_mmbtu/.9) * ng_price_vector       # energy costs
      annual_om_costs <- ngboiler_om_best * capex    # o&m costs
      
      # Apply discount factors to get present value of lifetime electricity + O&M costs
      PV_ng <- sum(annual_ng_costs * discount_factors)
      PV_om   <- sum(annual_om_costs * discount_factors)
      
      # LCOH formula
      numerator   <- capex + PV_om + PV_ng
      denominator <- sum(heat_mmbtu * discount_factors)
      lcoh <- numerator / denominator
      
      # Return value 
      lcoh
    }, 
    str_detect(tech_scenario, "Scenario1Best|Scenario3Best") ~ {
      # Incorporate capex subsidy 
      capex_adj <- capex * (1 - capex_subsidy)

      # Annual electricity + O&M costs 
      annual_elec_costs <- change_in_electricity_demand_kwh * (elec_price_vector * (1-elec_discount)) # this is a vector of costs over time
      annual_om_costs <- eboiler_om_best * capex

      # Apply discount factors to get present value of lifetime electricity + O&M costs
      PV_elec <- sum(annual_elec_costs * discount_factors)
      PV_om   <- sum(annual_om_costs * discount_factors)
      
      # LCOH formula
      numerator   <- capex_adj + PV_om + PV_elec
      denominator <- sum(heat_mmbtu * discount_factors)
      lcoh <- numerator / denominator
      
      # Return value 
      lcoh
    }, 
    str_detect(tech_scenario, "Scenario1Worst|Scenario3Worst") ~ {
      # Incorporate capex subsidy 
      capex_adj <- capex * (1 - capex_subsidy)
      
      # Annual electricity + O&M costs 
      annual_elec_costs <- change_in_electricity_demand_kwh * (elec_price_vector * (1-elec_discount)) # this is a vector of costs over time
      annual_om_costs <- eboiler_om_worst * capex
      
      # Apply discount factors to get present value of lifetime electricity + O&M costs
      PV_elec <- sum(annual_elec_costs * discount_factors)
      PV_om   <- sum(annual_om_costs * discount_factors)
      
      # LCOH formula
      numerator   <- capex_adj + PV_om + PV_elec
      denominator <- sum(heat_mmbtu * discount_factors)
      lcoh <- numerator / denominator
      
      # Return value 
      lcoh
    }, 
    str_detect(tech_scenario, "Scenario2Best|Scenario4Best") ~ {
      # Incorporate capex subsidy 
      capex_adj <- capex * (1 - capex_subsidy)
      
      # Annual electricity + O&M costs 
      annual_elec_costs <- change_in_electricity_demand_kwh * (elec_price_vector * (1-elec_discount)) # this is a vector of costs over time
      annual_om_costs <- hthp_om_best * capex
      
      # Apply discount factors to get present value of lifetime electricity + O&M costs
      PV_elec <- sum(annual_elec_costs * discount_factors)
      PV_om   <- sum(annual_om_costs * discount_factors)
      
      # LCOH formula
      numerator   <- capex_adj + PV_om + PV_elec
      denominator <- sum(heat_mmbtu * discount_factors)
      lcoh <- numerator / denominator
      
      # Return value 
      lcoh
    }, 
    str_detect(tech_scenario, "Scenario2Worst|Scenario4Worst") ~ {
      # Incorporate capex subsidy 
      capex_adj <- capex * (1 - capex_subsidy)
      
      # Annual electricity + O&M costs 
      annual_elec_costs <- change_in_electricity_demand_kwh * (elec_price_vector * (1-elec_discount)) # this is a vector of costs over time
      annual_om_costs <- hthp_om_worst * capex
      
      # Apply discount factors to get present value of lifetime electricity + O&M costs
      PV_elec <- sum(annual_elec_costs * discount_factors)
      PV_om   <- sum(annual_om_costs * discount_factors)
      
      # LCOH formula
      numerator   <- capex_adj + PV_om + PV_elec
      denominator <- sum(heat_mmbtu * discount_factors)
      lcoh <- numerator / denominator
      
      # Return value 
      lcoh
    }
  )
}

#### LCOH Calculation #### 
# Import parameters 
param <- 
  read_excel('state_fact_sheets/data/parameters.xlsx') %>%
  filter(scenario == state) |>
  select(-c(elec_price, ng_price)) |>
  mutate(t = 27) # just temporary since this is how many yrs of projections we have 

elec_price_vector <- 
  fuel_prices |>
  filter(fuel_type == "Electricity") |>
  pull(price)

ng_price_vector <- 
  fuel_prices |>
  filter(fuel_type == "Natural Gas") |>
  pull(price)

# --- Create Policy Grid ---
policy_grid <- expand.grid(
  capex_subsidy = seq(0.0, 1.0, by = 0.1),
  elec_discount = seq(0.0, 1.0, by = 0.25)
)

# --- Apply Policy Grid to Tech Scenarios ---
policy_applied_df <- 
  tidyr::crossing(tech_combined_df, policy_grid) %>%
  rowwise %>%
  mutate(
    lcoh = lcoh_func(
      param$r, 
      elec_price_vector,
      ng_price_vector,
      param$t, 
      param$ngboiler_om_best, 
      param$ngboiler_om_worst, 
      param$eboiler_om_best, 
      param$eboiler_om_worst, 
      param$hthp_om_best, 
      param$hthp_om_worst, 
      ## tech scenario + calculations 
      tech_scenario,
      capex,
      heat_mmbtu,
      change_in_electricity_demand_kwh,
      ## policy scenarios
      capex_subsidy, 
      elec_discount 
    ), 
    
    policy_label = paste0("Capex: -", capex_subsidy * 100, "%, Elec: -", elec_discount * 100, "%"), 
    policy_label = if_else(
      policy_label == 'Capex: -0%, Elec: -0%', 'No Policy', policy_label
    )
  )

#### EXPORT DATA ####

writexl::write_xlsx(policy_applied_df, glue("state_fact_sheets/data/modified/state-data/{state}/facility_lcoh_proj.price_{state}_{format(Sys.Date(), '%Y%m%d')}.xlsx")) 

