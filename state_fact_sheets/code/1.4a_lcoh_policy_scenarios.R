# August 1. 2025
# EMT
# Version 1.2
# Preparing policy modeling results for webtool dev + lcoh figure generation

# 8/18: updated LCOH calculations, refined pipeline 
# 8/15: split emissions & lcoh analysis 
# 8/13: added fats/soybeans, changed policy labels, changed emissions_total_mt_co2e to emissions_total_t_co2e
# 8/4 update: added in lat and long, added a current grid mix scenario 

#### SET-UP ####
# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(glue)

# Set state :) 
state <- "IL"

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
  mutate(facility_name = tolower(facility_name)) %>% 
  select(facility_name, facility_id, naics_code, naics_description, county_fips, subregion) %>% 
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

# Resource mix 
grid_mix_df <- 
  read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 3) %>%
  clean_names() %>%
  mutate(
    current_fossil_share = coal + oil + gas + other_fossil,
    current_clean_share = hydro + biomass + wind + solar + nuclear #removed geothermal for now due to E, close to 0
  ) %>%
  select(subregion = e_grid_subregion_acronym, current_fossil_share, current_clean_share)

# --- Merge Inputs ---
tech_combined_df <- 
  tech_input_df %>%
  left_join(facility_info, by = "facility_name") %>%
  left_join(egrid_df, by = "subregion") 

#### EMISSIONS CALCULATION #### 

# --- Create Grid Intensity Levels (from 50% to 100% clean) ---
grid_scenarios <- tibble(grid_clean_pct_scenario = c("Current Mix",seq(0.5, 1.0, by = 0.1)))

# --- Expand to All Grid Scenarios ---
facility_grid_long_df <- 
  tidyr::crossing(tech_combined_df, grid_scenarios) %>%
  left_join(grid_mix_df, by = "subregion") %>% 
  mutate(
    # Handle grid % as numeric
    grid_clean_pct_num = suppressWarnings(as.numeric(grid_clean_pct_scenario)),
    # Scale grid emissions based on grid mix 
    scaled_co2e_factor = if_else(
      is.na(grid_clean_pct_num),
      co2e_kg_kwh,
      co2e_kg_kwh * ((1 - grid_clean_pct_num) / current_fossil_share)
    ),
    scaled_nox_factor = if_else(
      is.na(grid_clean_pct_num),
      nox_kg_kwh,
      nox_kg_kwh * ((1 - grid_clean_pct_num) / current_fossil_share)
    ),
    scaled_so2_factor = if_else(
      is.na(grid_clean_pct_num),
      so2_kg_kwh,
      so2_kg_kwh * ((1 - grid_clean_pct_num) / current_fossil_share)
    ),
    scaled_pm25_factor = if_else(
      is.na(grid_clean_pct_num),
      pm25_kg_kwh,
      pm25_kg_kwh * ((1 - grid_clean_pct_num) / current_fossil_share)
    ))

facility_emissions_no_baseline <- 
  facility_grid_long_df %>% 
  filter(tech_scenario != "Baseline") %>% 
  mutate(
    # emissions formula 
    emissions_total_t_co2e = ((change_in_electricity_demand_kwh * scaled_co2e_factor)/1000 + noelec_ghg_emissions),
    emissions_change_kg_nox = (change_in_electricity_demand_kwh * scaled_nox_factor),
    emissions_change_kg_so2 = (change_in_electricity_demand_kwh * scaled_so2_factor),
    emissions_change_kg_pm25 = (change_in_electricity_demand_kwh * scaled_pm25_factor),
  ) 

facility_emissions_baseline <- 
  facility_grid_long_df %>% 
  filter(str_detect(tech_scenario, "Baseline")) %>% 
  mutate(
    emissions_total_t_co2e = baseline_co2e_emissions, 
    emissions_change_kg_nox = 0,
    emissions_change_kg_so2 = 0,
    emissions_change_kg_pm25 = 0
  )

facility_emissions_long_df <- 
  rbind(facility_emissions_baseline, facility_emissions_no_baseline) %>% 
  # label for grid scenario
  mutate(clean_grid_scenario_label = if_else(
    is.na(grid_clean_pct_num),
    "Current Grid Mix",
    paste0(round(grid_clean_pct_num * 100), "% Clean Grid")
  )) %>%
  select(facility_id, facility_name, state, county_fips, latitude, longitude, naics_code, naics_description, 
         sector, subregion, tech_scenario, heat_mmbtu, change_in_electricity_demand_kwh, 
         current_fossil_share, current_clean_share, grid_clean_pct_scenario, 
         baseline_co2e_emissions, elec_ghg_emissions, noelec_ghg_emissions, emissions_total_t_co2e, emissions_change_kg_nox,
         emissions_change_kg_so2, emissions_change_kg_pm25, clean_grid_scenario_label)

# Emissions
county_emissions_summary <- 
  facility_emissions_long_df %>% 
  group_by(county_fips, sector, naics_code, naics_description, tech_scenario, clean_grid_scenario_label) %>% 
  summarise(
    elec_ghg_emissions        = sum(elec_ghg_emissions, na.rm = TRUE),
    noelec_ghg_emissions      = sum(noelec_ghg_emissions, na.rm = TRUE),
    baseline_co2e_emissions   = sum(baseline_co2e_emissions, na.rm = TRUE),
    emissions_total_t_co2e   = sum(emissions_total_t_co2e, na.rm = TRUE),
    emissions_change_kg_nox = sum(emissions_change_kg_nox, na.rm = TRUE),
    emissions_change_kg_so2 = sum(emissions_change_kg_so2, na.rm = TRUE),
    emissions_change_kg_pm25 = sum(emissions_change_kg_pm25, na.rm = TRUE),
    .groups = "drop"
  ) 

state_emissions_summary <- 
  facility_emissions_long_df %>% 
  group_by(sector, naics_code, naics_description, tech_scenario, clean_grid_scenario_label) %>% 
  summarise(
    elec_ghg_emissions        = sum(elec_ghg_emissions, na.rm = TRUE),
    noelec_ghg_emissions      = sum(noelec_ghg_emissions, na.rm = TRUE),
    baseline_co2e_emissions   = sum(baseline_co2e_emissions, na.rm = TRUE),
    emissions_total_t_co2e   = sum(emissions_total_t_co2e, na.rm = TRUE),
    emissions_change_kg_nox = sum(emissions_change_kg_nox, na.rm = TRUE),
    emissions_change_kg_so2 = sum(emissions_change_kg_so2, na.rm = TRUE),
    emissions_change_kg_pm25 = sum(emissions_change_kg_pm25, na.rm = TRUE),
    .groups = "drop"
  ) 

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
  read_excel('state_fact_sheets/data/parameters.xlsx') %>%
  filter(scenario == state)

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

# State & County LCOH
#### NEED TO FIX THIS
# state_lcoh_summary <- 
#   policy_applied_df %>% 
#   group_by(state, sector, naics_code, naics_description, tech_scenario, policy_label) %>% 
#   summarise(
#     weighted_lcoh = sum(lcoh * heat_mmbtu, na.rm = TRUE) / sum(heat_mmbtu, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# county_lcoh_summary <- 
#   policy_applied_df %>% 
#   group_by(county_fips, sector, naics_code, naics_description, tech_scenario, policy_label) %>% 
#   summarise(
#     weighted_lcoh = sum(lcoh * heat_mmbtu, na.rm = TRUE) / sum(heat_mmbtu, na.rm = TRUE),
#     .groups = "drop"
#   )

#### DATA EXPORT ####
writexl::write_xlsx(policy_applied_df, glue("state_fact_sheets/data/modified/state-data/{state}/facility_lcoh_results_{state}_{format(Sys.Date(), '%Y%m%d')}.xlsx")) 
#writexl::write_xlsx(facility_emissions_long_df, glue("state_fact_sheets/data/modified/state-data/{state}/facility_emissions_results_{state}_{format(Sys.Date(), '%Y%m%d')}.xlsx")) 

# writexl::write_xlsx(county_emissions_summary, "state_fact_sheets/data/modified/state-data/MN/250818_county_emissions_results_mn.xlsx")
# writexl::write_xlsx(county_lcoh_summary, "state_fact_sheets/data/modified/state-data/MN/250818_county_lcoh_results_mn.xlsx")

writexl::write_xlsx(state_emissions_summary, glue("state_fact_sheets/data/modified/state-data/{state}/state_emissions_results_{state}_{format(Sys.Date(), '%Y%m%d')}.xlsx")) 

# writexl::write_xlsx(state_lcoh_summary, "state_fact_sheets/data/modified/state-data/MN/250818_state_lcoh_results_mn.xlsx")

#writexl::write_xlsx(tech_combined_df, "state_fact_sheets/data/modified/state-data/MN/250811_facility_option_b_sample_mn.xlsx")
