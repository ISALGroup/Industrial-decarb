# August 1. 2025
# EMT
# Version 1.2
# Preparing policy modeling results for webtool dev + lcoh figure generation

# 8/13: added fats/soybeans, changed policy labels, changed emissions_total_mt_co2e to emissions_total_t_co2e
# 8/4 update: added in lat and long, added a current grid mix scenario 

# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(stringr)

# Set working directory
#setwd("~/Documents/Industrial_Decarbonization/Industrial-decarb")

# --- Load Input Data ---

# Tech scenario input
tech_input_df <- read_excel("LCOH modelling/output/longform_mn_wsoybeansnfats_update.xlsx") %>%
  mutate(facility_name = tolower(facility_name)) %>% 
  mutate(baseline_co2e_emissions = elec_ghg_emissions + noelec_ghg_emissions) %>% 
  select(-1, -facility_id, -opex) #removing for now til i get complete results from antoine

# Pull in lat and long from rlps file
facility_lat_long <- read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>% 
  select(facility_id, latitude, longitude) %>% 
  distinct(facility_id, .keep_all = TRUE)

# Pull in facility info, this file doesn't have lat and long...
facility_info <- read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>% 
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
egrid_df <- read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 2) %>% 
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
grid_mix_df <- read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 3) %>%
  clean_names() %>%
  mutate(
    current_fossil_share = coal + oil + gas + other_fossil,
    current_clean_share = hydro + biomass + wind + solar + nuclear #removed geothermal for now due to E, close to 0
  ) %>%
  select(subregion = e_grid_subregion_acronym, current_fossil_share, current_clean_share)

# --- Merge Inputs ---
tech_combined_df <- tech_input_df %>%
  left_join(facility_info, by = "facility_name") %>%
  left_join(egrid_df, by = "subregion") 

# --- Parameters for LCOH ---
r <- 0.065
t <- 15
elec_price <- 0.092
discount_sum <- sum((1 + r)^-(0:(t - 1)))

# --- Calculate Base LCOH ---
tech_combined_lcoh <- tech_combined_df %>%
  mutate(
    opex_base = change_in_electricity_demand_kwh * elec_price,
    lcoh_base = (capex / discount_sum + opex_base) / heat_mmbtu
  )

# --- Create Policy Grid ---

capex_subsidy_seq <- seq(0.0, 1.0, by = 0.1)
elec_price_discounts <- seq(0.0, 1.0, by = 0.25)

policy_grid <- expand.grid(
  capex_subsidy = capex_subsidy_seq,
  elec_discount = elec_price_discounts
)

# --- Apply Policy Grid to Tech Scenarios ---
policy_applied_df <- tidyr::crossing(tech_combined_lcoh, policy_grid) %>%
  mutate(
    capex_adj = capex * (1 - capex_subsidy),
    elec_price_adj = elec_price * (1 - elec_discount),
    opex_adj = change_in_electricity_demand_kwh * elec_price_adj,
    lcoh = (capex_adj / discount_sum + opex_adj) / heat_mmbtu,
    policy_label = paste0("Capex: -", capex_subsidy * 100, "%, Elec: -", elec_discount * 100, "%")
  )

# Add No Policy Scenario
no_policy_df <- tech_combined_lcoh %>%
  mutate(
    capex_subsidy = 1,
    elec_discount = 0,
    capex_adj = capex,
    elec_price_adj = elec_price,
    opex_adj = opex_base,
    lcoh = lcoh_base,
    policy_label = "No Policy"
  )

# Combine All Scenarios
combined_policy_df <- bind_rows(no_policy_df, policy_applied_df)

# --- Create Grid Intensity Levels (from 50% to 100% clean) ---

grid_scenarios <- tibble(grid_clean_pct_scenario = c("Current Mix",seq(0.5, 1.0, by = 0.1)))

# --- Expand to All Grid Scenarios ---
facility_grid_long_df <- tidyr::crossing(combined_policy_df, grid_scenarios) %>%
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

facility_emissions_no_baseline <- facility_grid_long_df %>% 
  filter(tech_scenario != "Baseline") %>% 
  mutate(
    # emissions formula 
    emissions_total_t_co2e = ((change_in_electricity_demand_kwh * scaled_co2e_factor)/1000 + noelec_ghg_emissions),
    emissions_change_kg_nox = (change_in_electricity_demand_kwh * scaled_nox_factor),
    emissions_change_kg_so2 = (change_in_electricity_demand_kwh * scaled_so2_factor),
    emissions_change_kg_pm25 = (change_in_electricity_demand_kwh * scaled_pm25_factor),
  ) 

facility_emissions_baseline <- facility_grid_long_df %>% 
  filter(tech_scenario == "Baseline") %>% 
  mutate(
    emissions_total_t_co2e = baseline_co2e_emissions, 
    emissions_change_kg_nox = 0,
    emissions_change_kg_so2 = 0,
    emissions_change_kg_pm25 = 0
  )

facility_level_long_df <- rbind(facility_emissions_baseline, facility_emissions_no_baseline) %>% 
# label for grid scenario
  mutate(clean_grid_scenario_label = if_else(
    is.na(grid_clean_pct_num),
    "Current Grid Mix",
    paste0(round(grid_clean_pct_num * 100), "% Clean Grid")
  )) %>%
  select(facility_id, facility_name, state, county_fips, latitude, longitude, naics_code, naics_description, 
       sector, subregion, tech_scenario, heat_mmbtu, change_in_electricity_demand_kwh, capex_adj, capex_subsidy, 
       elec_discount, elec_price_adj, opex_adj, lcoh, policy_label, current_fossil_share, current_clean_share, grid_clean_pct_scenario, 
       baseline_co2e_emissions, elec_ghg_emissions, noelec_ghg_emissions, emissions_total_t_co2e, emissions_change_kg_nox,
       emissions_change_kg_so2, emissions_change_kg_pm25, clean_grid_scenario_label)

# ------------ COUNTY AND STATE LEVEL RESULTS ---------------
### Separate emissions and LCOH summarizing

# Emissions
county_emissions_summary <- facility_level_long_df %>% 
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

state_emissions_summary <- facility_level_long_df %>% 
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

# LCOH
state_lcoh_summary <- facility_level_long_df %>% 
  group_by(sector, naics_code, naics_description, tech_scenario, policy_label) %>% 
  summarise(
    change_in_electricity_demand_kwh = sum(change_in_electricity_demand_kwh, na.rm = TRUE),
    capex_adj = sum(capex_adj, na.rm = TRUE),
    total_heat_mmbtu = sum(heat_mmbtu, na.rm = TRUE),
    weighted_LCOH    = sum(heat_mmbtu * lcoh, na.rm = TRUE) / total_heat_mmbtu,
    .groups = "drop"
  )
  
county_lcoh_summary <- facility_level_long_df %>% 
  group_by(county_fips, sector, naics_code, naics_description, tech_scenario, policy_label) %>% 
  summarise(
    change_in_electricity_demand_kwh = sum(change_in_electricity_demand_kwh, na.rm = TRUE),
    capex_adj = sum(capex_adj, na.rm = TRUE),
    total_heat_mmbtu = sum(heat_mmbtu, na.rm = TRUE),
    weighted_LCOH    = sum(heat_mmbtu * lcoh, na.rm = TRUE) / total_heat_mmbtu,
    .groups = "drop"
  )

# --- Save ---
writexl::write_xlsx(facility_level_long_df, "state_fact_sheets/data/modified/state-data/MN/250812_facility_level_results_mn_v2.xlsx") 
writexl::write_xlsx(county_emissions_summary, "state_fact_sheets/data/modified/state-data/MN/250812_county_emissions_results_mn_v2.xlsx")
writexl::write_xlsx(county_lcoh_summary, "state_fact_sheets/data/modified/state-data/MN/250812_county_lcoh_results_mn_v2.xlsx")

writexl::write_xlsx(state_emissions_summary, "state_fact_sheets/data/modified/state-data/MN/250812_state_emissions_results_mn_v2.xlsx")
writexl::write_xlsx(state_lcoh_summary, "state_fact_sheets/data/modified/state-data/MN/250812_state_lcoh_results_mn_v2.xlsx")

writexl::write_xlsx(tech_combined_df, "state_fact_sheets/data/modified/state-data/MN/250811_facility_option_b_sample_mn_v2.xlsx")
