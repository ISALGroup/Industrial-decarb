# State-Level Emissions & LCOH Calculation #
## September 5, 2025
## Takes state-level longform output, calculates emissions & LCOH outcomes 
## Feeds state_memo_figures 

# Version notes: updates with emissions function, grid intensity change (no grid mix), capex revision 

#### SET STATE ####
# Set state :) 
state <- "MN"

#### SET-UP ####
# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(glue)

# Tech scenario input 
tech_input_df <- 
  read_excel(glue("LCOH modelling/output/longform_{state}.xlsx")) %>%
  mutate(facility_name = tolower(facility_name), 
         baseline_co2e_emissions = elec_ghg_emissions + noelec_ghg_emissions) %>% 
  select(-1, -opex) 

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
    baseline_co2e_emissions, # baseline co2e emissions
    grid_emissions_kg_kwh, # kg of emissions per kwh
    change_in_electricity_demand_kwh, # how much electricity demand changes under electrification
    noelec_ghg_emissions, # Non-electrifiable emissions 
    pollutant_type,
    tech_scenario,
    # Parameters
    grid_clean_pct_scenario # What % cleaner is the grid? should take a value from 0-1
  ) {
    
    # Scale the grid emissions (kg per kwh) to be more clean (or the same, depending on what the user selects). 
    scaled_grid.e_factor <- grid_emissions_kg_kwh * (1-grid_clean_pct_scenario)
    
    case_when(
      # When pollutant is CO2e & we are looking at an electrification scenario... 
      pollutant_type == 'co2e' & !str_detect(tech_scenario, "Baseline") ~ {
        # emissions = the increase in emissions from the grid from electricity demand + non-electrifiable emissions at the facility 
        emissions_total_t = (change_in_electricity_demand_kwh * scaled_grid.e_factor) / 1000 + noelec_ghg_emissions
        
        emissions_total_t
      }, 
      
      # When pollutant is not CO2e 
      pollutant_type != 'co2e' & !str_detect(tech_scenario, "Baseline") ~ {
        emissions_change_kg = change_in_electricity_demand_kwh * scaled_grid.e_factor
        
        emissions_change_kg
      }, 
      
      # When we are looking at the baseline scenario, just return the baseline emissions from GHGRP. 
      str_detect(tech_scenario, "Baseline") ~ {
        baseline_co2e_emissions
      })
  }

#### EMISSIONS CALCULATION #### 

# --- Create Grid Intensity Levels (from 50% to 100% cleaner) ---
grid_scenarios <- tibble(grid_clean_pct_scenario = c(0, seq(0.5, 1.0, by = 0.1)))

# --- Expand to All Grid Scenarios ---
facility_emissions_long_df <- 
  tidyr::crossing(tech_combined_df, grid_scenarios) %>%
  pivot_longer(
    cols = ends_with("_kg_kwh"),
    names_to = "pollutant_type",
    names_pattern = "^(.*)_kg_kwh$",  
    values_to = "grid_emissions_kg_kwh"
  ) %>%
  select(-capex, -heat_mmbtu) %>%
  filter(pollutant_type == 'co2e') %>%   # TEMPORARY -- LET'S GET THIS WORKING FOR NON-CO2E EVENTUALLY
  mutate(
    emissions_total = 
      emissions_func(
        baseline_co2e_emissions,
        grid_emissions_kg_kwh, 
        change_in_electricity_demand_kwh,
        noelec_ghg_emissions,
        pollutant_type, 
        tech_scenario,
        grid_clean_pct_scenario
      ),
    emissions_reduc = baseline_co2e_emissions - emissions_total, 
    clean_grid_scenario_label = if_else(
      grid_clean_pct_scenario == 0,
      "Current Grid Mix",
      paste0(round(grid_clean_pct_scenario * 100), "% Cleaner Grid")
    )
  ) 

state_emissions_summary <-
  facility_emissions_long_df %>%
  group_by(sector, naics_code, naics_description, tech_scenario, clean_grid_scenario_label, pollutant_type) %>%
  summarise(
    elec_ghg_emissions        = sum(elec_ghg_emissions, na.rm = TRUE),
    noelec_ghg_emissions      = sum(noelec_ghg_emissions, na.rm = TRUE),
    baseline_co2e_emissions   = sum(baseline_co2e_emissions, na.rm = TRUE),
    emissions_total = sum(emissions_total, na.rm = TRUE),
    emissions_reduc = sum(emissions_reduc, na.rm = TRUE),
    # emissions_change_kg_nox = sum(emissions_change_kg_nox, na.rm = TRUE),
    # emissions_change_kg_so2 = sum(emissions_change_kg_so2, na.rm = TRUE),
    # emissions_change_kg_pm25 = sum(emissions_change_kg_pm25, na.rm = TRUE),
    .groups = "drop"
  )

# county_emissions_summary <- 
#   facility_emissions_long_df %>% 
#   group_by(county_fips, sector, naics_code, naics_description, tech_scenario, clean_grid_scenario_label) %>% 
#   summarise(
#     elec_ghg_emissions        = sum(elec_ghg_emissions, na.rm = TRUE),
#     noelec_ghg_emissions      = sum(noelec_ghg_emissions, na.rm = TRUE),
#     baseline_co2e_emissions   = sum(baseline_co2e_emissions, na.rm = TRUE),
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


#### DATA EXPORT ####
writexl::write_xlsx(policy_applied_df, glue("state_fact_sheets/data/modified/state-data/{state}/{state}_lcoh_{format(Sys.Date(), '%Y%m%d')}.xlsx")) 

writexl::write_xlsx(state_emissions_summary, glue("state_fact_sheets/data/modified/state-data/{state}/{state}_emissions_{format(Sys.Date(), '%Y%m%d')}.xlsx")) 
