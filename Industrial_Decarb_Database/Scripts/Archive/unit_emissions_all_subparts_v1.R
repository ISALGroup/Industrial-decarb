##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 6, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create combined dataset for unit level emissions
#
# Notes:  
# 
# Inputs:   
#         
#
# Outputs:   
#
##################################################################

# Clean the environment
rm(list=ls())

# Load libraries
library(here)
library(janitor) 
library(readxl)
library(writexl)
library(tidyverse)
library(openxlsx)

### Functions ###
source(here("Functions", "is_unique_id.R"))
source(here("Functions", "convert_to_numeric.R"))
source(here("Functions", "export_facility_unit_data.R"))

# load data
subpart_aa = read_excel(here("Output", "subpart_aa_emissions_by_unit_v1.xlsx")) |>
  mutate(primary_naics = as.numeric(primary_naics))
subpart_c = read_csv(here("Output", "subpart_c_emissions_and_fuel_by_unit_v3.csv")) 
subpart_ii = read_excel(here("Output", "subpart_ii_emissions_by_unit_v1.xlsx")) |>
  mutate(primary_naics = as.numeric(primary_naics))
relevant_naics = read_excel(here("Data", "target_NAICS.xlsx")) |>
  clean_names() |>
  rename(primary_naics = x6_digit_naics_code) |>
  select(primary_naics) |>
  mutate(keep_naics = 1)

# Append subpart c and subpart aa
unit_level_emissions_combined = subpart_c |>
  bind_rows(subpart_aa) |>
  bind_rows(subpart_ii) |>
  left_join(y = relevant_naics, by = c("primary_naics")) |>
  filter(!is.na(keep_naics) | 
           (primary_naics >=  322110 & primary_naics <= 322139)) |>
  select(-keep_naics) |>
  select(primary_naics,	facility_id,	reporting_year,	unit_type,	unit_name,	anaerobic_process_id,	
         fuel_type,	ghg_gas_name,	ghg_quantity,	total_fuel_quantity,	subpart,	capacity,	
         capacity_utiliziation,	vintage,	floor_space,	temperature,	steam_generation_est,	
         electricity_generation_est,	prime_mover_type,	hrsg_bypass) |>
  arrange(facility_id, reporting_year, unit_name, 
          fuel_type, ghg_gas_name)

### Export ###
write.xlsx(unit_level_emissions_combined, here("Output", "unit_emissions_relevant_naics.xlsx"))


