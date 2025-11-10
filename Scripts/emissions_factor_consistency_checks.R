##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# February 24, 2024               
# Ben Ladabaum                  
#                               
# Description:  Investigate emissions factors for n2o and ch4
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
# convert variables from string to numeric
convert_to_numeric <- function(data, columns) {
  data[columns] <- lapply(data[columns], function(col) as.numeric(as.character(col)))
  return(data)
}

### Load Data ###

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
                       "tier3_eq_c5_fuel_qty")) |>
  select(facility_id, reporting_year, unit_name, fuel_type, tier1_ch4_emissions_co2e,
         tier2_ch4_emissions_co2e, tier3_ch4_emissions_co2e, 
         tier4_ch4_emissions_co2e, tier1_ch4_combustion_emissions,
         tier2_ch4_combustion_emissions, tier3_ch4_combustion_emissions,
         t4ch4combustionemissions, tier1_n2o_emissions_co2e,
         tier2_n2o_emissions_co2e, tier3_n2o_emissions_co2e,
         tier4_n2o_emissions_co2e, tier1_n2o_combustion_emissions,
         tier2_n2o_combustion_emissions, tier3_n2o_combustion_emissions,
         t4n2ocombustionemissions) |>
  mutate(ch4_factor_tier1 = tier1_ch4_emissions_co2e / tier1_ch4_combustion_emissions) |>
  mutate(ch4_factor_tier2 = tier2_ch4_emissions_co2e / tier2_ch4_combustion_emissions) |>
  mutate(ch4_factor_tier3 = tier3_ch4_emissions_co2e / tier3_ch4_combustion_emissions) |>
  mutate(ch4_factor_tier4 = tier4_ch4_emissions_co2e / t4ch4combustionemissions)  |>
  mutate(n2o_factor_tier1 = tier1_n2o_emissions_co2e / tier1_n2o_combustion_emissions) |>
  mutate(n2o_factor_tier2 = tier2_n2o_emissions_co2e / tier2_n2o_combustion_emissions) |>
  mutate(n2o_factor_tier3 = tier3_n2o_emissions_co2e / tier3_n2o_combustion_emissions) |>
  mutate(n2o_factor_tier4 = tier4_n2o_emissions_co2e / t4n2ocombustionemissions)  






