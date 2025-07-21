##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 9, 2025               
# Ben Ladabaum                  
#                               
# Description:  Consistency checks for subpart X
#
# Notes:  see https://www.epa.gov/ghgreporting/subpart-x-information-sheet#:~:text=Subpart%20X%20of%20the%20Greenhouse,reporting%2C%20and%20some%20do%20not.
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

### load data
# subpart x
facility_emissions = read_excel("Data/Subpart X/x_subpart_level_information.xlsx") |>
  rename(ghg_quantity_facility = ghg_quantity) |>
  filter(ghg_quantity_facility != 0)

flare_emissions = read_excel("Data/Subpart X/x_flare_information.xlsx") |>
  mutate(flare_ch4_emissions = as.numeric(flare_ch4_emissions)) |>
  mutate(flare_co2_emissions = as.numeric(flare_co2_emissions)) |>
  mutate(flare_n2o_emissions = as.numeric(flare_n2o_emissions))

mass_balance_emissions = read_excel("Data/Subpart X/x_mass_balance_unit_details.xlsx")

cems_emissions = read_excel("Data/Subpart X/x_cems_details.xlsx")

### process flare emissions for comparison
flare_emissions_facility = flare_emissions |>
  select(facility_id, reporting_year, unit_name, flare_ch4_emissions, 
         flare_co2_emissions, flare_n2o_emissions) |>
  distinct() |>
  group_by(facility_id, reporting_year) |>
  summarise(
    flare_ch4_emissions = sum(flare_ch4_emissions, na.rm = TRUE),
    flare_co2_emissions = sum(flare_co2_emissions, na.rm = TRUE),
    flare_n2o_emissions = sum(flare_n2o_emissions, na.rm = TRUE)
  ) |>
  ungroup() |>
  pivot_longer(cols = c(flare_ch4_emissions, flare_co2_emissions, flare_n2o_emissions), 
                           names_to = "ghg_name", 
                           values_to = "ghg_quantity_unit") |>
  mutate(
    ghg_name = case_when(
      ghg_name == "flare_ch4_emissions" ~ "Methane",
      ghg_name == "flare_co2_emissions" ~ "Carbon Dioxide",
      ghg_name == "flare_n2o_emissions" ~ "Nitrous Oxide"
    )
  )

### Process mass balance
mass_balance_facility = mass_balance_emissions |>
  select(facility_id, reporting_year, unit_name_mass_balance, mass_balance_co2_emissions) |>
  distinct() |>
  group_by(facility_id, reporting_year) |>
  summarise(
    mass_balance_co2_emissions = sum(mass_balance_co2_emissions, na.rm = TRUE)
  ) |>
  ungroup() |>
  rename(ghg_quantity_unit = mass_balance_co2_emissions) |>
  mutate(ghg_name = "Carbon Dioxide")

### Process cems
cems_facility = cems_emissions |>
  select(facility_id, reporting_year, cems_co2_emissions, cems_ch4_emissions, 
         cems_n2o_emissions, cml_name, fuel_type) |>
  distinct() |>
  group_by(facility_id, reporting_year) |>
  summarise(
    cems_co2_emissions = sum(cems_co2_emissions, na.rm = TRUE),
    cems_ch4_emissions = sum(cems_ch4_emissions, na.rm = TRUE),
    cems_n2o_emissions = sum(cems_n2o_emissions, na.rm = TRUE)
  ) |>
  ungroup() |>
  pivot_longer(cols = c(cems_ch4_emissions, cems_co2_emissions, cems_n2o_emissions), 
               names_to = "ghg_name", 
               values_to = "ghg_quantity_unit") |>
  mutate(
    ghg_name = case_when(
      ghg_name == "cems_ch4_emissions" ~ "Methane",
      ghg_name == "cems_co2_emissions" ~ "Carbon Dioxide",
      ghg_name == "cems_n2o_emissions" ~ "Nitrous Oxide"
    )
  )


### Compare flare and facility emissions
flare_emissions_comparison = full_join(x = facility_emissions, y = flare_emissions_facility, 
                                 by = c("facility_id", "reporting_year", "ghg_name")) |>
  arrange(facility_id, reporting_year, ghg_name)

### Compare mass_balance and facility emissions
mass_balance_emissions_comparison = full_join(x = facility_emissions, y = mass_balance_facility, 
                                       by = c("facility_id", "reporting_year", "ghg_name")) |>
  arrange(facility_id, reporting_year, ghg_name)

### Compare cems and facility emissions
cems_emissions_comparison = full_join(x = facility_emissions, y = cems_facility, 
                                              by = c("facility_id", "reporting_year", "ghg_name")) |>
  arrange(facility_id, reporting_year, ghg_name)


### compare combined unit level emissions
unit_emissions = mass_balance_facility |>
  bind_rows(cems_facility) |>
  bind_rows(flare_emissions_facility) |> 
  group_by(facility_id, reporting_year, ghg_name) |>
  summarise(
    ghg_quantity_unit = sum(ghg_quantity_unit, na.rm = TRUE)
  ) |>
  ungroup()

### compare facility and unit emissions
unit_emissions_comparison = full_join(x = facility_emissions, y = unit_emissions, 
                                      by = c("facility_id", "reporting_year", "ghg_name")) |>
  arrange(facility_id, reporting_year, ghg_name) |>
  mutate(difference = ghg_quantity_facility - ghg_quantity_unit)

write.xlsx(unit_emissions_comparison, "Output/subpart_x_consistency_checks.xlsx")

