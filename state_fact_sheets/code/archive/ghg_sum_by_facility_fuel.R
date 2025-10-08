library(readxl)
library(dplyr)
library(janitor)
library(here)
library(tidyr)
library(tidyverse)

naics <- c(311942,325193,311313,322120,322130)

ghgrp_raw_df <- read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 4) %>% 
  clean_names() %>% 
  select(-primary_naics)

plant_info <- read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>% 
  clean_names() %>% 
  select(facility_name, facility_id, state, primary_naics)

# Add facility info and filter MI ONLY CHECK IN EXCEL
mn_ghgrp_filtered <- ghgrp_raw_df %>% 
  left_join(plant_info, by = "facility_id") %>% 
  filter(state == "MN", primary_naics %in% naics)

# group by 

# Add facility info and filter
ghgrp_filtered <- ghgrp_raw_df %>% 
  left_join(plant_info, by = "facility_id") %>% 
  filter(state == "MI", primary_naics %in% naics) %>% 
  filter(
    str_detect(str_to_lower(ghg_name), "total$|\\(co2 eq\\)$") |
      subpart == "AA"
  ) %>% 
  select(
    facility_name, facility_id, primary_naics, unit_type, unit_name,
    fuel_type, ghg_name, ghg_quantity, total_fuel_quantity
  )

# Part 1: Summarize rows with valid fuel_type
ghgrp_with_fuel <- ghgrp_filtered %>%
  filter(!is.na(fuel_type)) %>%
  group_by(facility_name, fuel_type) %>%
  summarize(
    total_co2e = sum(ghg_quantity, na.rm = TRUE),
    ghg_name = "Combined Tons CO2 eq",  # 
    .groups = "drop"
  )

# Part 2: Keep NA fuel_type rows as-is
ghgrp_missing_fuel <- ghgrp_filtered %>%
  filter(is.na(fuel_type)) %>%
  mutate(fuel_type = "NA") %>% 
  select(facility_name, fuel_type, ghg_quantity, ghg_name)

# Part 3: Combine both parts into final result
ghgrp_summary <- bind_rows(
  ghgrp_with_fuel,
  ghgrp_missing_fuel %>% rename(total_co2e = ghg_quantity)
) %>% 
  left_join(plant_info, by = "facility_name") 
  

# Summarize by facility_id, fuel_type, and ghg_name
ghgrp_summary_by_gas <- ghgrp_filtered %>%
  group_by(facility_name, fuel_type, ghg_name) %>%
  summarize(
    total_ghg = sum(ghg_quantity, na.rm = TRUE),
    .groups = "drop"
  )


#writexl::write_xlsx(list(
#  "Emissions by Unit" = ghgrp_filtered,
 # "Emissions by Fclty, Fuel"  = ghgrp_summary,
 # "Emissions by Fclty, Fuel, GHG" = ghgrp_summary_by_gas),
#path = "state_fact_sheets/data/modified/mi_emissions_summary.xlsx")

writexl::write_xlsx(mn_ghgrp_filtered, "state_fact_sheets/data/modified/mm_unit_level_ghgrp_4_naics.xlsx")

