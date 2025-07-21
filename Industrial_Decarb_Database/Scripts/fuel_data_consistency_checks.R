##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# February 22, 2024               
# Ben Ladabaum                  
#                               
# Description:  Compare methods for calculating fuel type by unit
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


# Monthly solid fuel inputs
solid_fuel_monthly_inputs = read_excel("Data/Subpart C/Eqc3c8_monthly_inputs_solid_fuel.xlsx") |>
  convert_to_numeric("fuel_combusted") |>
  distinct()
is_unique_id(solid_fuel_monthly_inputs, c("facility_id", "reporting_year", "unit_name",
                                          "month", "fuel_type", "fuel_combusted"))

## The only duplicate is 1 facility (id = 1004965) where the expected variables are not a unique id 
## This occurs for 12 months in 2022 only, meaning there are 12 duplicate values.
#duplicates = report_duplicates(solid_fuel_monthly_inputs, c("facility_id", "reporting_year", "unit_name",
#                                               "month", "fuel_type"))

# Monthly Liquid fuel inputs
liquid_fuel_monthly_inputs = read_excel("Data/Subpart C/Eqc4c8_monthly_inputs_liquid_fuel.xlsx") |>
  convert_to_numeric("fuel_combusted") |>
  distinct()
is_unique_id(liquid_fuel_monthly_inputs, c("facility_id", "reporting_year", "unit_name",
                                           "month", "fuel_type"))

# Monthly Gaseous fuel inputs
gas_fuel_monthly_inputs = read_excel("Data/Subpart C/Eqc5c8_monthly_inputs_gaseous_fuel.xlsx") |>
  distinct() |> # drop duplicates
  convert_to_numeric("fuel_combusted")
is_unique_id(gas_fuel_monthly_inputs, c("facility_id", "reporting_year", "unit_name",
                                        "month", "fuel_type", "high_heat_value",
                                        "fuel_combusted", "carbon_content"))

# there are 932 duplicate observations. They seem to occur only when fuel type is "Fuel Gas" or "N/A"
# duplicates = report_duplicates(gas_fuel_monthly_inputs, c("facility_id", "reporting_year", "unit_name",
#                                                             "month", "fuel_type"))


# Fuel Level info
fuel_level_data = read_excel("Data/Subpart C/Fuel_level_information.xlsx") |>
  convert_to_numeric(c("tier1_co2_combustion_emissions", "tier2_co2_combustion_emissions", 
                       "tier3_co2_combustion_emissions", "tier1_ch4_emissions_co2e",
                       "tier2_ch4_emissions_co2e", "tier3_ch4_emissions_co2e", 
                       "tier4_ch4_emissions_co2e", "tier1_ch4_combustion_emissions",
                       "tier2_ch4_combustion_emissions", "tier3_ch4_combustion_emissions",
                       "t4ch4combustionemissions", "tier1_n2o_emissions_co2e",
                       "tier2_n2o_emissions_co2e", "tier3_n2o_emissions_co2e",
                       "tier4_n2o_emissions_co2e", "tier1_n2o_combustion_emissions",
                       "tier2_n2o_combustion_emissions", "tier3_n2o_combustion_emissions",
                       "t4n2ocombustionemissions", "tier4_fuel_quantity", 
                       "tier1_fuel_quantity", "tier2_eq_c2a_fuel_qty",
                       "tier3_eq_c3_fuel_qty", "tier3_eq_c4_fuel_qty",
                       "tier3_eq_c5_fuel_qty"))
is_unique_id(fuel_level_data, c("facility_id", "reporting_year", "unit_name", "fuel_type", "tier3_ch4_emissions_co2e"))
#duplicates = report_duplicates(fuel_level_data, c("facility_id", "reporting_year", "unit_name", "fuel_type", 
#"tier3_ch4_emissions_co2e"))



### Data Build ###

# Calculate fuel inputs per year
## gas fuel
annual_gas_fuel = gas_fuel_monthly_inputs |>
  group_by(facility_id, reporting_year, fuel_combusted_uom, fuel_type) |>
  summarise(
    fuel_combusted_annual = sum(fuel_combusted, na.rm = TRUE),
  ) |>
  ungroup() |>
  filter(fuel_combusted_annual!=0) |>
  mutate(fuel_type = ifelse(is.na(fuel_type), "unknown gas fuel", fuel_type))

is_unique_id(annual_gas_fuel, c("facility_id", "reporting_year", "fuel_type"))

## solid_fuel
annual_solid_fuel = solid_fuel_monthly_inputs |>
  group_by(facility_id, reporting_year, fuel_combusted_uom, fuel_type) |>
  summarise(
    fuel_combusted_annual = sum(fuel_combusted, na.rm = TRUE),
  ) |>
  ungroup() |>
  filter(fuel_combusted_annual!=0) |>
  mutate(fuel_type = ifelse(is.na(fuel_type), "unknown solid fuel", fuel_type))

is_unique_id(annual_solid_fuel, c("facility_id", "reporting_year", "fuel_type"))

## liquid_fuel
annual_liquid_fuel = liquid_fuel_monthly_inputs |>
  group_by(facility_id, reporting_year, fuel_combusted_uom, fuel_type) |>
  summarise(
    fuel_combusted_annual = sum(fuel_combusted, na.rm = TRUE),
  ) |>
  ungroup() |>
  filter(fuel_combusted_annual!=0) |>
  mutate(fuel_type = ifelse(is.na(fuel_type), "unknown liquid fuel", fuel_type))

is_unique_id(annual_liquid_fuel, c("facility_id", "reporting_year", "fuel_type"))

# append fuel datasets together
annual_fuel = rbind(annual_gas_fuel, annual_liquid_fuel, annual_solid_fuel)
is_unique_id(annual_fuel, c("facility_id", "reporting_year", "fuel_type"))


# collapse fuel level data by facility_id, reporting year and fuel type
fuel_data_facility_level = fuel_level_data |> 
  mutate(total_fuel_quantity = rowSums(cbind(tier1_fuel_quantity, tier2_eq_c2a_fuel_qty, 
                                             tier3_eq_c3_fuel_qty, tier3_eq_c4_fuel_qty,
                                             tier3_eq_c5_fuel_qty, tier4_fuel_quantity), na.rm = TRUE)) |>
  group_by(facility_id, reporting_year, fuel_type) |>
  summarise(
    total_fuel_quantity = sum(total_fuel_quantity, na.rm = TRUE),
  ) |>
  ungroup() |>
  filter(total_fuel_quantity!=0)


compare_fuel = full_join(x = fuel_data_facility_level, y = annual_fuel, 
                         by = c("facility_id", "reporting_year", "fuel_type"))

## Conclusion: fuel_level_info is much more complete. Generally, the numbers calculated from both datasets are the same,
# although not always. We should use fuel level info in the full dataset.