library(readxl)
library(dplyr)
library(janitor)
library(here)
library(tidyr)
library(tidyverse)

naics <- c(311942,325193,311313,322120)

ghgrp_raw_df <- read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 4) %>% 
  clean_names()

plant_info <- read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>% 
  clean_names() %>% 
  filter(state == "MI") %>% 
  filter(primary_naics %in% naics) %>% 
  select(facility_name, facility_id, primary_naics)

ghg_summary <- ghgrp_raw_df %>% 
  filter(primary_naics %in% naics) %>% 
  group_by(facility_id, fuel_type) %>%
  summarize(total_ghg = sum(ghg_quantity, na.rm = TRUE), .groups = "drop") 

ghg_results <- plant_info %>% 
  left_join(ghg_summary, by = "facility_id")

writexl::write_xlsx(ghg_results, "state_fact_sheets/data/modified/MI_ghg_by_fuel_facility.xlsx")
