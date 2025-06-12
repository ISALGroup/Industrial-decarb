##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 12, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create dataset with unit-level process emissions for subpart X.
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

flare_emissions = read_excel("Data/Subpart X/x_flare_information.xlsx") |>
  mutate(flare_ch4_emissions = as.numeric(flare_ch4_emissions)) |>
  mutate(flare_co2_emissions = as.numeric(flare_co2_emissions)) |>
  mutate(flare_n2o_emissions = as.numeric(flare_n2o_emissions))

mass_balance_emissions = read_excel("Data/Subpart X/x_mass_balance_unit_details.xlsx") 

cems_emissions = read_excel("Data/Subpart X/x_cems_details.xlsx")

# Facilities data
facilities_data = read_excel("Data/rlps_ghg_emitter_facilities.xlsx") |>
  rename(reporting_year = year) |>
  select(facility_id, reporting_year, primary_naics)


### process flare emissions for comparison
flare_emissions_wide = flare_emissions |>
  select(facility_id, reporting_year, unit_name, unit_type, flare_ch4_emissions, 
         flare_co2_emissions, flare_n2o_emissions) |>
  distinct() |>
  group_by(facility_id, reporting_year, unit_name, unit_type) |>
  summarise(
    flare_ch4_emissions = sum(flare_ch4_emissions, na.rm = TRUE),
    flare_co2_emissions = sum(flare_co2_emissions, na.rm = TRUE),
    flare_n2o_emissions = sum(flare_n2o_emissions, na.rm = TRUE)
  ) |>
  ungroup() |>
  pivot_longer(cols = c(flare_ch4_emissions, flare_co2_emissions, flare_n2o_emissions), 
               names_to = "ghg_name", 
               values_to = "ghg_quantity") |>
  mutate(
    ghg_name = case_when(
      ghg_name == "flare_ch4_emissions" ~ "Methane",
      ghg_name == "flare_co2_emissions" ~ "Carbon Dioxide",
      ghg_name == "flare_n2o_emissions" ~ "Nitrous Oxide"
    )
  ) 

### Process mass balance
mass_balance_wide = mass_balance_emissions |>
  select(facility_id, reporting_year, unit_name_mass_balance, unit_type_mass_balance, 
         mass_balance_co2_emissions) |>
  distinct() |>
  group_by(facility_id, reporting_year, unit_name_mass_balance, unit_type_mass_balance) |>
  summarise(
    mass_balance_co2_emissions = sum(mass_balance_co2_emissions, na.rm = TRUE)
  ) |>
  ungroup() |>
  rename(ghg_quantity = mass_balance_co2_emissions) |>
  rename(unit_name = unit_name_mass_balance) |>
  rename(unit_type = unit_type_mass_balance) |>
  mutate(ghg_name = "Carbon Dioxide")

### Process cems
cems_wide = cems_emissions |>
  select(facility_id, reporting_year, cems_co2_emissions, cems_ch4_emissions, 
         cems_n2o_emissions, cml_name, cml_configuration_type, fuel_type) |>
  distinct() |>
  group_by(facility_id, reporting_year, cml_configuration_type, cml_name, fuel_type) |>
  summarise(
    cems_co2_emissions = sum(cems_co2_emissions, na.rm = TRUE),
    cems_ch4_emissions = sum(cems_ch4_emissions, na.rm = TRUE),
    cems_n2o_emissions = sum(cems_n2o_emissions, na.rm = TRUE)
  ) |>
  ungroup() |>
  pivot_longer(cols = c(cems_ch4_emissions, cems_co2_emissions, cems_n2o_emissions), 
               names_to = "ghg_name", 
               values_to = "ghg_quantity") |>
  mutate(
    ghg_name = case_when(
      ghg_name == "cems_ch4_emissions" ~ "Methane",
      ghg_name == "cems_co2_emissions" ~ "Carbon Dioxide",
      ghg_name == "cems_n2o_emissions" ~ "Nitrous Oxide"
    )
  ) |>
  rename(unit_name = "cml_name") |>
  rename(unit_type = "cml_configuration_type")


emissions_by_unit = bind_rows(flare_emissions_wide, mass_balance_wide, cems_wide) |>
  mutate(subpart = "X") |>
  left_join(y = facilities_data, by = c("facility_id", "reporting_year"))

write_xlsx(emissions_by_unit, path = "Output/subpart_x_emissions_by_unit_v1.xlsx")

