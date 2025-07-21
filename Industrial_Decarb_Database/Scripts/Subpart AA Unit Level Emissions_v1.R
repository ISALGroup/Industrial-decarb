##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 6, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create datast with unit-level emissions information for subpart AA
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



#spent liquor
# Facilities data
facilities_data = read_excel("Data/rlps_ghg_emitter_facilities.xlsx") |>
  rename(reporting_year = year) |>
  select(facility_id, reporting_year, primary_naics)
is_unique_id(facilities_data, c("facility_id", "reporting_year"))

spent_liquor = read_excel("Data/Subpart AA/aa_spent_liquor_information.xlsx") |>
  convert_to_numeric(c("biomass_ch4_emissions_factor", "biomass_n2o_emissions_factor"))
is_unique_id(spent_liquor, c("facility_id", "reporting_year", "unit_name"))

# fuel data
fossil_fuel_data = read_excel("Data/Subpart AA/aa_fossil_fuel_information.xlsx")
is_unique_id(fossil_fuel_data, c("facility_id", "reporting_year", "unit_name", "fuel_type"))


### Process Emissions ###
spent_liquor_emissions = spent_liquor |>
  rename(biogenic_spent_liquor_co2_emissions = spent_liquor_co2_emissions) |>
  select(facility_id, reporting_year, starts_with("spent_liquor"), biogenic_spent_liquor_co2_emissions, 
         unit_name, unit_type)

emissions_by_unit = left_join(x = fossil_fuel_data, 
                              facilities_data, by=c("facility_id", "reporting_year"))|>
  left_join(y = spent_liquor_emissions, 
                              by = c("facility_id", "reporting_year", "unit_name")) |>
  mutate(fossil_fuel_co2_emissions = rowSums(cbind(tier_1_co2_emissions, tier_2_co2_emissions, 
                                                   tier_3_co2_emissions), na.rm = TRUE)) |>
  mutate(fossil_fuel_ch4_emissions = rowSums(cbind(tier_1_ch4_emissions, tier_2_ch4_emissions, 
                                                   tier_3_ch4_emissions), na.rm = TRUE)) |>
  mutate(fossil_fuel_ch4_emissions_co2e = rowSums(cbind(tier_1_ch4_emissions_co2e, tier_2_ch4_emissions_co2e, 
                                                        tier_3_ch4_emissions_co2e), na.rm = TRUE)) |>
  mutate(fossil_fuel_n2o_emissions = rowSums(cbind(tier_1_n2o_emissions, tier_2_n2o_emissions, 
                                                   tier_3_n2o_emissions), na.rm = TRUE)) |>
  mutate(fossil_fuel_n2o_emissions_co2e = rowSums(cbind(tier_1_n2o_emissions_co2e, tier_2_n2o_emissions_co2e, 
                                                        tier_3_n2o_emissions_co2e), na.rm = TRUE)) |>
  select(primary_naics, facility_id, reporting_year, unit_name, unit_type, fuel_type, 
         fossil_fuel_ch4_emissions, fossil_fuel_ch4_emissions_co2e, 
         fossil_fuel_co2_emissions, biogenic_spent_liquor_co2_emissions, 
         fossil_fuel_n2o_emissions, fossil_fuel_n2o_emissions_co2e, starts_with("spent_liquor")) |>
  pivot_longer(cols = c("fossil_fuel_ch4_emissions", "fossil_fuel_ch4_emissions_co2e", 
                        "fossil_fuel_co2_emissions", "biogenic_spent_liquor_co2_emissions", 
                        "fossil_fuel_n2o_emissions", "fossil_fuel_n2o_emissions_co2e",
                        "spent_liquor_ch4_emissions", "spent_liquor_n2o_emissions"), 
               names_to = "ghg_gas_name",
               values_to = "ghg_quantity")|>
  mutate(
    ghg_gas_name = case_when(
      ghg_gas_name == "fossil_fuel_co2_emissions" ~ "Carbon Dioxide Non-Biogenic",
      ghg_gas_name == "fossil_fuel_ch4_emissions" ~ "Methane",
      ghg_gas_name == "fossil_fuel_ch4_emissions_co2e" ~ "Methane (Co2 eq)",
      ghg_gas_name == "fossil_fuel_n2o_emissions" ~ "Nitrous Oxide",
      ghg_gas_name == "fossil_fuel_n2o_emissions_co2e" ~ "Nitrous Oxide (Co2 eq)",
      ghg_gas_name == "biogenic_spent_liquor_co2_emissions" ~ "Carbon Dioxide Biogenic (Spent Liquor)",
      ghg_gas_name == "spent_liquor_ch4_emissions" ~ "Methane Spent Liquor",
      ghg_gas_name == "spent_liquor_n2o_emissions" ~ "Nitrous Oxide Spent Liquor"
    )
  ) |>
  mutate(subpart = "AA") |>
  group_by(primary_naics, facility_id, reporting_year, unit_name, unit_type, ghg_gas_name, subpart) |>
  summarise(
    ghg_quantity = sum(ghg_quantity, na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(!(ghg_gas_name %in% c("Nitrous Oxide (Co2 eq)", "Methane (Co2 eq)"))) # remove this line 
# once emissions factor issue is resolved


# Export
write_xlsx(emissions_by_unit, path = "Output/subpart_aa_emissions_by_unit_v1.xlsx")

