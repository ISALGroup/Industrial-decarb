##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 12, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create dataset with unit-level process emissions for subpart G and V.
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


### Load Data ###
# Facilities data
facilities_data = read_excel("Data/rlps_ghg_emitter_facilities.xlsx") |>
  rename(reporting_year = year) |>
  select(facility_id, reporting_year, primary_naics)

# subpart g
subpart_g = read_excel("Data/Subpart G/g_non_cems_source_info.xlsx")

# subpart v
subpart_v = read_excel("Data/Subpart V/v_nitric_acid_train.xlsx")


# process data
unit_emissions_g = subpart_g |>
  distinct(co2_emissions, facility_id, unit_name, reporting_year) |>
  rename(ghg_quantity = co2_emissions) |>
  mutate(ghg_name = "Carbon Dioxide") |>
  mutate(subpart = "G") |>
  left_join(y = facilities_data, by = c("facility_id", "reporting_year"))

unit_emissions_v = read_excel("Data/Subpart V/v_nitric_acid_train.xlsx") |>
  select(facility_id, reporting_year, nitric_acid_train_id, annual_unrounded_n2o_emiss) |>
  rename(unit_name = "nitric_acid_train_id") |>
  rename(ghg_quantity = "annual_unrounded_n2o_emiss") |>
  mutate(ghg_name = "Nitrous Oxide") |>
  mutate(subpart = "V") |>
  left_join(y = facilities_data, by = c("facility_id", "reporting_year"))

### Export
write_xlsx(unit_emissions_g, path = "Output/subpart_g_emissions_by_unit_v1.xlsx")
write_xlsx(unit_emissions_v, path = "Output/subpart_v_emissions_by_unit_v1.xlsx")


