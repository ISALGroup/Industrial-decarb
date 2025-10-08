# State-Level Emissions & LCOH Calculation #
## September 15, 2025
## Takes state-level longform output, calculates emissions & LCOH outcomes 
## Feeds state_memo_figures 

# Version notes: 
## 1.7: incorporating copollutant workflow 
## 1.6: updates with emissions function, grid intensity change (no grid mix), capex revision 

#### SET STATE ####
# Set state :) 
st <- "IL"

#### SET-UP ####
# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(tidylog)
library(stringr)
library(janitor)
library(glue)

# Tech scenario input 
tech_input_df <- 
  read_excel(glue("LCOH modelling/output/copollutant_longform_{st}.xlsx")) %>%
  #read_excel('LCOH modelling/output/copollutant_test.xlsx') %>%
  mutate(facility_name = tolower(facility_name), 
         base_emissions_co2e = elec_ghg_emissions + noelec_ghg_emissions) %>% 
  rename(
    base_emissions_nox = `base_emissions_Nitrogen Oxides`,
    base_emissions_pm25 = `base_emissions_PM2.5 Primary (Filt + Cond)`, 
    base_emissions_so2 = `base_emissions_Sulfur Dioxide`
  ) %>%
  select(-1, -opex, -fuel_reduction) 

# TEMPORARY: natgas capex calculation
natgas_best <- 
  tech_input_df %>%
  filter(tech_scenario == 'Baseline') %>%
  mutate(tech_scenario = 'BaselineBest', 
         # best case natural gas 
         capex = 4733.79*(heat_mmbtu/49132.8)^0.8325)

tech_input_df <- 
  tech_input_df %>%
  mutate(
    tech_scenario = if_else(tech_scenario == 'Baseline', 'BaselineWorst', tech_scenario),
    # worst case natural gas
    capex = if_else(tech_scenario == 'BaselineWorst', 25761.09*(heat_mmbtu/40944)^0.8325, capex)
  ) %>%
  bind_rows(natgas_best)


# Pull in lat and long from rlps file
facility_lat_long <-
  read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>%
  select(facility_id, latitude, longitude) %>%
  distinct(facility_id, .keep_all = TRUE)

# Pull in facility info, this file doesn't have lat and long... (maybe add in the future)
facility_info <-
  read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>%
  clean_names() %>%
  rename(naics_code = primary_naics) %>%
  rename(naics_description = naics_title) %>%
  select(facility_id, naics_code, naics_description, county_fips, subregion) %>%
  inner_join(facility_lat_long, by = "facility_id") %>%
  mutate(
    sector = case_when(
      str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food & Beverage",
      str_starts(naics_code, "325") ~ "Chemicals",
      str_starts(naics_code, "322") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    )
  )

# eGRID data
egrid_df <-
  read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 2) %>%
  clean_names() %>%
  mutate(co2e_kg_kwh = co2e * 0.000453592) %>%
  mutate(ch4_kg_kwh = ch4 * 0.000453592) %>%
  mutate(n2o_kg_kwh = n2o * 0.000453592) %>%
  mutate(nox_kg_kwh = annual_n_ox * 0.000453592) %>%
  mutate(so2_kg_kwh = so2 * 0.000453592) %>%
  mutate(pm25_kg_kwh = pm_2_5 * 0.000453592) %>%
  rename(subregion = e_grid_subregion_acronym) %>%
  select(subregion, co2e_kg_kwh, nox_kg_kwh, so2_kg_kwh, pm25_kg_kwh)

# --- Merge Inputs ---
tech_combined_df <- 
  tech_input_df %>%
  left_join(facility_info, by = "facility_id") %>%
  left_join(egrid_df, by = "subregion")  %>%
  # DROPPING IF ONLY ONE FACILITY IN A SECTOR
  group_by(naics_code) %>%  
  filter(n_distinct(facility_id) > 1) %>%      
  ungroup()

#### EMISSIONS FUNCTION #### 
emissions_func <- 
  function(
    # From longform data
    facility_emissions, # facility emissions
    grid_emissions_kg_kwh, # kg of emissions per kwh
    change_in_electricity_demand_kwh, # how much electricity demand changes under electrification
    
    # Parameters
    grid_clean_pct_scenario # What % cleaner is the grid? should take a value from 0-1
  ) {
    
    # Scale the grid emissions (kg per kwh) to be more clean (or the same, depending on what the user selects). 
    scaled_grid.e_factor <- grid_emissions_kg_kwh * (1-grid_clean_pct_scenario)
    

    # emissions = the increase in emissions from the grid from electricity demand + non-electrifiable emissions at the facility 
    total_emissions_t = (change_in_electricity_demand_kwh * scaled_grid.e_factor) / 1000 + facility_emissions
    
    total_emissions_t
  }

#### EMISSIONS CALCULATION #### 

# --- Create Grid Intensity Levels (from 50% to 100% cleaner) ---
grid_scenarios <- tibble(grid_clean_pct_scenario = c(0, seq(0.5, 1.0, by = 0.1)))

# --- Expand to All Grid Scenarios ---
facility_emissions_long_df <- 
  tidyr::crossing(tech_combined_df, grid_scenarios) %>%
  # Moving to a facility-tech.scenario-grid.scenario-pollutant level dataset
  pivot_longer(
    cols = ends_with("_kg_kwh"),
    names_to = "pollutant_type",
    names_pattern = "^(.*)_kg_kwh$",  
    values_to = "grid_emissions_kg_kwh"
  ) %>%
  mutate(
    # make one emissions variable, calibrated to copollutant & scenario
    facility_emissions = case_when(
      pollutant_type == 'co2e' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_co2e, 
      pollutant_type == 'nox' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_nox, 
      pollutant_type == 'so2' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_so2, 
      pollutant_type == 'pm25' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_pm25,
      
      pollutant_type == 'co2e' & str_detect(tech_scenario, 'Scenario') ~ noelec_ghg_emissions, 
      pollutant_type == 'nox' & str_detect(tech_scenario, 'Scenario') ~ nox_emissions, 
      pollutant_type == 'so2' & str_detect(tech_scenario, 'Scenario')  ~ so2_emissions, 
      pollutant_type == 'pm25' & str_detect(tech_scenario, 'Scenario') ~ pm25_emissions
    ), 
    
    total_emissions = 
      emissions_func(
        facility_emissions, # facility emissions
        grid_emissions_kg_kwh, # kg of emissions per kwh
        change_in_electricity_demand_kwh, # how much electricity demand changes under electrification
        grid_clean_pct_scenario
      ),

    clean_grid_scenario_label = if_else(
      grid_clean_pct_scenario == 0,
      "Current Grid Mix",
      paste0(round(grid_clean_pct_scenario * 100), "% Cleaner Grid")
    )
  ) %>%
  select(-capex, -heat_mmbtu, -contains('base'), -elec_ghg_emissions, -noelec_ghg_emissions, 
         -nox_emissions, -so2_emissions, -pm25_emissions)

state_emissions_summary <-
  facility_emissions_long_df %>%
  group_by(sector, naics_code, naics_description, tech_scenario, clean_grid_scenario_label, pollutant_type) %>%
  summarise(
    facility_emissions = sum(facility_emissions, na.rm = TRUE),
    total_emissions = sum(total_emissions, na.rm = TRUE),
    .groups = "drop"
  )

# county_emissions_summary <- 
#   facility_emissions_long_df %>% 
#   group_by(county_fips, sector, naics_code, naics_description, tech_scenario, clean_grid_scenario_label) %>% 
#   summarise(
#     elec_ghg_emissions        = sum(elec_ghg_emissions, na.rm = TRUE),
#     noelec_ghg_emissions      = sum(noelec_ghg_emissions, na.rm = TRUE),
#     base_emissions_co2e   = sum(base_emissions_co2e, na.rm = TRUE),
#     emissions_total_t_co2e   = sum(emissions_total_t_co2e, na.rm = TRUE),
#     emissions_change_kg_nox = sum(emissions_change_kg_nox, na.rm = TRUE),
#     emissions_change_kg_so2 = sum(emissions_change_kg_so2, na.rm = TRUE),
#     emissions_change_kg_pm25 = sum(emissions_change_kg_pm25, na.rm = TRUE),
#     .groups = "drop"
#   ) 
# 

#### LCOH FUNCTION #### 
lcoh_func <- function(
    ## parameters
  r, 
  elec_price,
  ng_price,
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
  discount_sum <- sum((1 + r)^-(1:t))
  
  ## Inputting different parameters for different tech scenarios 
  case_when(
    str_detect(tech_scenario, "BaselineWorst") ~ {
      opex_ng <- (heat_mmbtu/.75) * ng_price      # energy costs
      opex_om <- ngboiler_om_worst * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "BaselineBest") ~ {
      opex_ng <- (heat_mmbtu/.9) * ng_price       # energy costs
      opex_om <- ngboiler_om_best * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario1Best|Scenario3Best") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- eboiler_om_worst * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario1Worst|Scenario3Worst") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- eboiler_om_best * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario2Best|Scenario4Best") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- hthp_om_worst * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario2Worst|Scenario4Worst") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- hthp_om_best * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }
  )
}

#### LCOH CALCULATION ####
# Import parameters 
param <- 
  read_csv('state_fact_sheets/data/parameters.csv') %>%
  filter(state == st)

# --- Create Policy Grid ---
policy_grid <- expand.grid(
  capex_subsidy = seq(0.0, 1.0, by = 0.1),
  elec_discount = seq(0.0, 1.0, by = 0.25)
)

# --- Apply Policy Grid to Tech Scenarios ---
policy_applied_df <- 
  tidyr::crossing(tech_combined_df, policy_grid) %>%
  mutate(
    lcoh = lcoh_func(
      param$r, 
      param$elec_price,
      param$ng_price,
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


#### DATA EXPORT ####
writexl::write_xlsx(policy_applied_df, glue("state_fact_sheets/data/modified/state-data/{st}/{st}_lcoh_{format(Sys.Date(), '%Y%m%d')}.xlsx")) 

writexl::write_xlsx(state_emissions_summary, glue("state_fact_sheets/data/modified/state-data/{st}/{st}_emissions_{format(Sys.Date(), '%Y%m%d')}.xlsx")) 