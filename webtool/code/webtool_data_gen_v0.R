#### WEBTOOL DATA OUTPUT #### 

# Version 1.0
# NAM

#### SET-UP ####
# Load Libraries
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(glue)

# Tech scenario input 
tech_input_df.o <- 
  read_excel("LCOH modelling/output/longform_il.xlsx") %>%
  bind_rows(read_excel("LCOH modelling/output/longform_mi.xlsx")) %>%
  select(-1, -opex) 

natgas_best <- 
  tech_input_df.o %>%
  filter(tech_scenario == 'Baseline') %>%
  mutate(tech_scenario = 'BaselineBest', 
         # best case natural gas 
         capex = 4733.79*(heat_mmbtu/54592)^0.8325)

tech_input_df <- 
  tech_input_df.o |>
  mutate(
    tech_scenario = if_else(tech_scenario == 'Baseline', 'BaselineWorst', tech_scenario),
    # worst case natural gas
    capex = if_else(tech_scenario == 'BaselineWorst', 25761.09*(heat_mmbtu/54592)^0.8325, capex)
  ) %>%
  bind_rows(natgas_best) %>%
  mutate(facility_name = tolower(facility_name), 
         facility_id = as.character(facility_id),
         baseline_co2e_emissions = elec_ghg_emissions + noelec_ghg_emissions,
         
         scenario_rank = str_extract(tech_scenario, "(Best|Worst)$"),
         tech_scenario = str_remove(tech_scenario, "(Best|Worst)$")
         ) %>% 
  # AVERAGING THE FUNCTION *INPUTS* ACROSS BEST & WORST CASE
  group_by(facility_id, tech_scenario) %>%
  summarize(
    across(where(is.numeric), mean, na.rm = TRUE),
    across(where(is.character), first),
    .groups = "drop"
  ) %>%
  select(-scenario_rank)

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
    ), 
    facility_id = as.character(facility_id)
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

param <- read_excel('webtool/data/parameters_20250904.xlsx')

# --- Merge Inputs ---
tech_combined_df <- 
  tech_input_df %>%
  left_join(facility_info, by = "facility_id") %>%
  left_join(egrid_df, by = "subregion") 

#### Create & Save Datasets #### 

web_emissions_df <- 
  tech_combined_df |>
  # Drawing pollutant type into a long format column for filtering by the webtool team. 
  pivot_longer(
    cols = ends_with("_kg_kwh"),
    names_to = "pollutant_type",
    names_pattern = "^(.*)_kg_kwh$",  
    values_to = "grid_emissions_kg_kwh"
  ) |>
  select(-capex, -heat_mmbtu)

write_csv(web_emissions_df, glue("webtool/data/webtool_emissions_data_{format(Sys.Date(), '%Y%m%d')}.csv")) 

web_lcoh_df <- 
  tech_combined_df %>%
  left_join(param, by = c('state' = 'scenario')) %>%
  select(-elec_ghg_emissions, -noelec_ghg_emissions, -baseline_co2e_emissions, -ends_with("_kg_kwh")) 

write_csv(web_lcoh_df, glue("webtool/data/webtool_lcoh_data_{format(Sys.Date(), '%Y%m%d')}.csv")) 

  