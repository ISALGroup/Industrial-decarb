##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 3, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create datast with plant-level emissions information for subpart AA
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

setwd("/Users/Ben L/Library/CloudStorage/Box-Box/Industrial Plant Raw Data/Industrial-Decarb-Database")

### Functions ###
source(here("Functions", "is_unique_id.R"))
source(here("Functions", "convert_to_numeric.R"))
source(here("Functions", "export_facility_unit_data.R"))

# load data
facilities_data = read_excel("Data/rlps_ghg_emitter_facilities.xlsx") |>
  select(facility_id, primary_naics, year) |>
  rename(reporting_year = year) 

subpart_aa_emissions = read_excel("Data/Subpart AA/aa_subpart_level_information.xlsx") |>
  pivot_wider(names_from = ghg_name, values_from = ghg_quantity) |>
  clean_names()  |>
  rename(carbon_dioxide_subpart_aa = carbon_dioxide, biogenic_co2_subpart_aa = biogenic_carbon_dioxide,
         methane_subpart_aa = methane, nitrous_oxide_subpart_aa = nitrous_oxide)

subpart_aa_spent_liquor = read_excel("Data/Subpart AA/aa_spent_liquor_information.xlsx") |>
  group_by(facility_id, reporting_year) |>
  summarize(
    spent_liquor_ch4_subpart_aa = sum(spent_liquor_ch4_emissions, na.rm=TRUE),
    spent_liquor_co2_subpart_aa = sum(spent_liquor_co2_emissions, na.rm=TRUE),
    spent_liquor_n2o_subpart_aa = sum(spent_liquor_n2o_emissions, na.rm=TRUE)
  )

# combine data
subpart_aa_combined = subpart_aa_emissions |>
  full_join(y = subpart_aa_spent_liquor, by = c("facility_id", "reporting_year"))

is_unique_id(subpart_aa_combined, c("facility_id", "reporting_year"))

write_xlsx(subpart_aa_combined, path = "Output/subpart_aa_emissions_by_facility_v1.xlsx")
  





