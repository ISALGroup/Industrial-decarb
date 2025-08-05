# August 1. 2025
# EMT
# Version 1.2
# Preparing policy modeling results for webtool dev + lcoh figure generation

# 8/4 update: added in lat and long, added a current grid mix scenario 

# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(here)
library(stringr)

# Set working directory
setwd("~/Documents/Industrial_Decarbonization/Industrial-decarb")

# --- Load Input Data ---

# Tech scenario input
tech_input_df <- read_excel("state_fact_sheets/data/raw/lcoh_policy_modeling_input.xlsx") %>%
  mutate(plant_name = tolower(plant_name))

# Pull in lat and long from rlps file
plant_lat_long <- read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>% 
  select(facility_id, latitude, longitude)

# Pull in plant info, this file doesn't have lat and long...
plant_info <- read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>% 
  clean_names() %>% 
  rename(plant_name = facility_name) %>% 
  rename(naics_code = primary_naics) %>% 
  mutate(plant_name = tolower(plant_name)) %>% 
  select(plant_name, facility_id, state, county_fips, subregion) %>% 
  left_join(plant_lat_long, by = "facility_id")

# eGRID data
egrid_df <- read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 2) %>% 
  clean_names() %>% 
  mutate(co2_kg_kwh = co2 * 0.000453592) %>% 
  mutate(ch4_kg_kwh = ch4 * 0.000453592) %>% 
  mutate(n2o_kg_kwh = n2o * 0.000453592) %>% 
  mutate(nox_kg_kwh = annual_n_ox * 0.000453592) %>% 
  mutate(so2_kg_kwh = so2 * 0.000453592) %>% 
  mutate(pm25_kg_kwh = pm_2_5 * 0.000453592) %>% 
  rename(subregion = e_grid_subregion_acronym) %>% 
  select(subregion, co2_kg_kwh, nox_kg_kwh, so2_kg_kwh, pm25_kg_kwh)

# Resource mix 
grid_mix_df <- read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 3) %>%
  clean_names() %>%
  mutate(
    fossil_share = coal + oil + gas + other_fossil,
    clean_share = hydro + biomass + wind + solar + nuclear #removed geothermal for now due to E, close to 0
  ) %>%
  select(
    subregion = e_grid_subregion_acronym,
    fossil_share,
    clean_share
  )

# --- Merge Inputs ---

tech_combined_df <- tech_input_df %>%
  left_join(plant_info, by = "plant_name") %>%
  left_join(egrid_df, by = "subregion")

# --- Parameters for LCOH ---

r <- 0.065
t <- 15
elec_price <- 0.092
discount_sum <- sum((1 + r)^-(0:(t - 1)))

# --- Calculate Base LCOH ---

tech_combined_lcoh <- tech_combined_df %>%
  mutate(
    opex_base = change_in_elec_demand_kwh * elec_price,
    lcoh_base = (capex / discount_sum + opex_base) / heat_mmbtu
  )

# --- Create Policy Grid ---

capex_cost_shares <- sequence(0:1, by = 0.1)
elec_price_discounts <- c(0.1, 0.2, 0.3, 0.4, 0.5)

policy_grid <- expand.grid(
  capex_share = capex_cost_shares,
  elec_discount = elec_price_discounts
)

# --- Apply Policy Grid to Tech Scenarios ---
policy_applied_df <- tidyr::crossing(tech_combined_lcoh, policy_grid) %>%
  mutate(
    capex_adj = capex * capex_share,
    elec_price_adj = elec_price * (1 - elec_discount),
    opex_adj = change_in_elec_demand_kwh * elec_price_adj,
    lcoh = (capex_adj / discount_sum + opex_adj) / heat_mmbtu,
    policy_label = paste0("Capex: ", capex_share * 100, "%, Elec: -", elec_discount * 100, "%")
  )

# Add No Policy Scenario
no_policy_df <- tech_combined_lcoh %>%
  mutate(
    capex_share = 1,
    elec_discount = 0,
    capex_adj = capex,
    elec_price_adj = elec_price,
    opex_adj = opex_base,
    lcoh = lcoh_base,
    policy_label = "No Policy"
  )

# Combine All Scenarios
combined_policy_df <- bind_rows(no_policy_df, policy_applied_df)

# --- Create Grid Intensity Levels (from 50% to 100% renewables) ---

grid_scenarios <- tibble(grid_clean_pct_scenario = c("Current Mix",seq(0.5, 1.0, by = 0.1)))

# --- Expand to All Grid Scenarios ---
facility_level_long_df <- tidyr::crossing(combined_policy_df, grid_scenarios) %>%
  left_join(grid_mix_df, by = "subregion") %>%
  mutate(
    # Safely create a numeric version of the clean pct column
    grid_clean_pct_num = suppressWarnings(as.numeric(grid_clean_pct_scenario)),
    
    grid_emissions_kg_co2 = if_else(
      is.na(grid_clean_pct_num),
      change_in_elec_demand_kwh * co2_kg_kwh,
      change_in_elec_demand_kwh * co2_kg_kwh * ((1 - grid_clean_pct_num) / fossil_share)
    ),
    grid_emissions_kg_so2 = if_else(
      is.na(grid_clean_pct_num),
      change_in_elec_demand_kwh * so2_kg_kwh,
      change_in_elec_demand_kwh * so2_kg_kwh * ((1 - grid_clean_pct_num) / fossil_share)
    ),
    grid_emissions_kg_nox = if_else(
      is.na(grid_clean_pct_num),
      change_in_elec_demand_kwh * nox_kg_kwh,
      change_in_elec_demand_kwh * nox_kg_kwh * ((1 - grid_clean_pct_num) / fossil_share)
    ),
    grid_emissions_kg_pm25 = if_else(
      is.na(grid_clean_pct_num),
      change_in_elec_demand_kwh * pm25_kg_kwh,
      change_in_elec_demand_kwh * pm25_kg_kwh * ((1 - grid_clean_pct_num) / fossil_share)
    ),
    clean_grid_scenario_label = if_else(
      is.na(grid_clean_pct_num),
      "Current Grid Mix",
      paste0(round(grid_clean_pct_num * 100), "% Clean Grid")
    )
  ) %>% 
  select(-grid_clean_pct_num)



# ------------ COUNTY AND STATE LEVEL RESULTS ---------------
county_results <- facility_level_long_df %>% 
  group_by(county_fips, naics_code, naics_description, sector, tech_scenario, policy_label, fossil_share, clean_share, grid_clean_pct_scenario, clean_grid_scenario_label) %>% 
  summarise(capex, change_in_elec_demand_kwh, heat_mmbtu, opex_base, lcoh_base, capex_adj, opex_adj, lcoh, grid_emissions_kg_co2, grid_emissions_kg_so2, grid_emissions_kg_nox, grid_emissions_kg_pm25)

state_results <- facility_level_long_df %>% 
  group_by(state, naics_code, naics_description, sector, tech_scenario, policy_label, fossil_share, clean_share, grid_clean_pct_scenario, clean_grid_scenario_label) %>% 
  summarise(capex, change_in_elec_demand_kwh, heat_mmbtu, opex_base, lcoh_base, capex_adj, opex_adj, lcoh, grid_emissions_kg_co2, grid_emissions_kg_so2, grid_emissions_kg_nox, grid_emissions_kg_pm25)

# --- Save ---
writexl::write_xlsx(facility_level_long_df, "state_fact_sheets/data/modified/250804_facility_level_results.xlsx") 
writexl::write_xlsx(county_results, "state_fact_sheets/data/modified/250804_county_level_results.xlsx")
writexl::write_xlsx(state_results, "state_fact_sheets/data/modified/250804_state_level_results.xlsx")
