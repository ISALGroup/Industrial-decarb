##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 6, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create dataset with unit-level process emissions for subpart II.
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

# subpart ii data
eq_6 = read_excel("Data/Subpart II/ii_equation_ii6.xlsx")
eq_3 = read_excel("Data/Subpart II/ii_equation_ii3.xlsx")

### Wastewater Treatment Emissions
eq_6_clean = eq_6 |>
  select(anaerobic_process_id, annual_mass_methane_emissions, facility_id, reporting_year) |>
  distinct() 

eq_3_clean = eq_3 |>
  select(anaerobic_process_id, annual_mass_methane_emissions, facility_id, reporting_year) |>
  distinct() 

emissions_by_unit = rbind(eq_6_clean, eq_3_clean) |>
  group_by(facility_id, reporting_year, anaerobic_process_id) |>
  summarise(
    ghg_quantity = sum(annual_mass_methane_emissions, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(subpart = "II") |>
  mutate(ghg_gas_name = "Methane") |>
  left_join(y = facilities_data, by=c("facility_id", "reporting_year")) 

write_xlsx(emissions_by_unit, path = "Output/subpart_ii_emissions_by_unit_v1.xlsx")

