##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 6, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create combined datasets for plant level emissions
#
# Notes:  _v2: add subparts v, g, and x
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

setwd("/Users/Ben L/Library/CloudStorage/Box-Box/Industrial Plant Raw Data/Industrial-Decarb-Database")

### Functions ###
source(here("Functions", "is_unique_id.R"))
source(here("Functions", "convert_to_numeric.R"))
source(here("Functions", "export_facility_unit_data.R"))

# load data
subpart_aa = read_excel("Output/subpart_aa_emissions_by_facility_v1.xlsx")
subpart_c = read_excel("Output/subpart_c_emissions_by_facility_v2.xlsx") |>
  mutate(primary_naics = as.numeric(primary_naics))
subpart_g = read_excel("Output/subpart_g_emissions_by_facility_v1.xlsx")
subpart_hh = read_excel("Output/subpart_hh_emissions_by_facility_v1.xlsx")
subpart_ii = read_excel("Output/subpart_ii_emissions_by_facility_v1.xlsx")
subpart_s = read_excel("Output/subpart_s_emissions_by_facility_v1.xlsx")
subpart_v = read_excel("Output/subpart_v_emissions_by_facility_v1.xlsx")
subpart_x = read_excel("Output/subpart_x_emissions_by_facility_v1.xlsx")
relevant_naics = read_excel("Data/target_NAICS.xlsx") |>
  clean_names() |>
  rename(primary_naics = x6_digit_naics_code) |>
  select(primary_naics) |>
  mutate(keep_naics = 1)
ethanol_plant_capacity = read_excel("Data/ethyl alcohol manufacturers.xlsx",
                                         sheet = "Crosswalk") |>
  clean_names()|>
  rename(capacity_MMgy_ethanol = capacity_m_mgy) |>
  select(facility_id, capacity_MMgy_ethanol) 
daily_grind = read_excel("Data/beyond_starch_information.xlsx",
                         sheet = "Sheet1") |>
  clean_names()|>
  filter(!is.na(daily_grind_bu_day)) |>
  select(facility_id, daily_grind_bu_day) 

# merge data 
plant_level_emissions_combined = subpart_c |> 
  left_join(y = subpart_aa, by = c("facility_id", "reporting_year")) |>
  left_join(y = subpart_hh, by = c("facility_id", "reporting_year")) |>
  left_join(y = subpart_g, by = c("facility_id", "reporting_year")) |>
  #left_join(y = subpart_ii, by = c("facility_id", "reporting_year")) |> # this subpart is not very useful
  left_join(y = subpart_s, by = c("facility_id", "reporting_year")) |>
  left_join(y = subpart_v, by = c("facility_id", "reporting_year")) |>
  left_join(y = subpart_x, by = c("facility_id", "reporting_year")) |>
  left_join(y = relevant_naics, by = c("primary_naics")) |>
  filter(!is.na(keep_naics) | 
           (primary_naics >=  322110 & primary_naics <= 322139)) |>
  select(-keep_naics) |>
  select(facility_id,	reporting_year,	primary_naics, 
         ends_with(c("subpart_c", "subpart_aa", "subpart_g", "subpart_hh", "subpart_s", 
                     "subpart_v", "subpart_x")),	
         byproducts,	other_pollutants,	pct_co2e_declared_cu,	product_outputs,	
         employment,	annual_production_qty,	plant_size) |>
  left_join(y = ethanol_plant_capacity, by = c("facility_id")) |>
  left_join(y = daily_grind, by = c("facility_id"))

plant_level_emissions_combined_2023 = plant_level_emissions_combined |>
  filter(reporting_year==2023)

### Export ###
write.xlsx(plant_level_emissions_combined, "Output/facility_emissions_relevant_naics.xlsx")
write.xlsx(plant_level_emissions_combined_2023, "Output/facility_emissions_relevant_naics_2023.xlsx")


