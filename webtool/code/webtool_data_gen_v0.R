#### WEBTOOL DATA OUTPUT #### 

# Version 1.0
# NAM

#### SET-UP ####
# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

# Tech scenario input 
tech_input_df <- 
  read_excel("LCOH modelling/output/longform_il.xlsx") %>%
  bind_rows(read_excel("LCOH modelling/output/longform_mi.xlsx")) %>%
  mutate(facility_name = tolower(facility_name), 
         baseline_co2e_emissions = elec_ghg_emissions + noelec_ghg_emissions, 
         # USING WORST CASE FOR NATURAL GAS 
         capex = if_else(tech_scenario == 'Baseline', (18.11 * (heat_mmbtu/.75) * 293.071) / 8000, capex)) %>% 
  # USING BEST CASE FOR ELECTRIC SCENARIOS 
  filter(str_detect(tech_scenario, 'Best') | tech_scenario == 'Baseline') %>%
  select(-1, -facility_id, -opex) #removing for now til i get complete results from antoine

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
  left_join(egrid_df, by = "subregion") %>%
  left_join(grid_mix_df, by = "subregion")

#### Emissions #### 
