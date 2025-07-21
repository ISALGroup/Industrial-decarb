##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# February 21, 2024               
# Ben Ladabaum                  
#                               
# Description:  Create datast with plant-level emissions information
#
# Notes:  _v2: no longer include emissions by fuel type in facility level data. 
#             Also add declared combustion units
#              
# 
# Inputs:   rlps_ghg_emitter_facilities.xlsx
#           c_subpart_level_information.xlsx
#         
#
# Outputs:   subpart_c_emissions_by_facility_v2.xlsx
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
# test whether set of variables are unique identifiers
is_unique_id <- function(data, vars) {
  n_unique <- nrow(unique(data[vars]))
  n_total <- nrow(data)
  n_unique == n_total
}

# convert variables from string to numeric
convert_to_numeric <- function(data, columns) {
  data[columns] <- lapply(data[columns], function(col) as.numeric(as.character(col)))
  return(data)
}

# report duplicates for set of variables you want to test
report_duplicates <- function(data, vars) {
  data %>%
    group_by(across(all_of(vars))) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(count > 1) %>%
    arrange(desc(count))
}


### Load data and determine unique id variables ###

# Facilities data
facilities_data = read_excel("Data/rlps_ghg_emitter_facilities.xlsx") |>
  select(facility_id, primary_naics, year) |>
  rename(reporting_year = year) 
is_unique_id(facilities_data, c("facility_id", "reporting_year"))

# Total facility combustion emissions by gas 
subpart_level_data = read_excel("Data/Subpart C/c_subpart_level_information.xlsx")
is_unique_id(subpart_level_data, c("facility_id", "ghg_gas_name", "reporting_year"))

# get declared combustion units
fuel_level_data = read_excel("Data/Subpart C/Fuel_level_information.xlsx")

declared_comb_units = fuel_level_data |>
  select(facility_id, reporting_year, unit_name) |>
  distinct() |>
  mutate(declared_combustion_units = 1) |>
  group_by(facility_id, reporting_year) |>
  summarise(
    declared_combustion_units = sum(declared_combustion_units, na.rm = TRUE)
  ) |>
  ungroup()


### Data Build ###
# Merge subpart C emissions data with naics codes, and declared combustion units
facility_emissions = left_join(x = subpart_level_data, facilities_data, by=c("facility_id", "reporting_year")) |>
  rename(annual_facility_ghg_quantity_subpart_c = ghg_quantity) |>
  pivot_wider(names_from = ghg_gas_name, values_from = annual_facility_ghg_quantity_subpart_c) |>
                clean_names() |> 
  select(facility_id, reporting_year, primary_naics, carbon_dioxide, biogenic_carbon_dioxide, methane,
         nitrous_oxide) |>
  left_join(y = declared_comb_units, by = c("facility_id", "reporting_year")) |>
  arrange(facility_id, reporting_year) |>
  mutate(byproducts = "") |>
  mutate(other_pollutants = "") |>
  mutate(pct_co2e_declared_cu = "") |>
  mutate(product_outputs = "") |>
  mutate(employment = "") |>
  mutate(annual_production_qty = "") |>
  mutate(plant_size = "") |>
  rename(carbon_dioxide_subpart_c = carbon_dioxide, biogenic_co2_subpart_c = biogenic_carbon_dioxide,
         methane_subpart_c = methane, nitrous_oxide_subpart_c = nitrous_oxide)

is_unique_id(facility_emissions, c("facility_id", "reporting_year"))

write_xlsx(facility_emissions, path = "Output/subpart_c_emissions_by_facility_v2.xlsx")






