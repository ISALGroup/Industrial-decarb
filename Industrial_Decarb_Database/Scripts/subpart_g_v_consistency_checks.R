##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 12, 2025               
# Ben Ladabaum                  
#                               
# Description:  Consistency checks for subpart G
#
# Notes:  see https://www.epa.gov/ghgreporting/subpart-g-information-sheet 
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
# subpart g
facility_emissions_g = read_excel("Data/Subpart G/g_subpart_level_information.xlsx") |>
  rename(co2_emissions_facility = ghg_quantity) |>
  filter(co2_emissions_facility != 0)

unique(facility_emissions_g$ghg_name)

unit_emissions_g = read_excel("Data/Subpart G/g_non_cems_source_info.xlsx") |>
  distinct(co2_emissions, facility_id, unit_name, reporting_year) |>
  group_by(facility_id, reporting_year) |>
  summarise(
    co2_emissions = sum(co2_emissions, na.rm = TRUE)
  ) |>
  ungroup() |>
  rename(co2_emissions_unit = co2_emissions)

# subpart v
facility_emissions_v = read_excel("Data/Subpart V/v_subpart_level_information.xlsx") |>
  rename(n2o_emissions_facility = ghg_quantity) 

unique(facility_emissions_v$ghg_name)

unit_emissions_v = read_excel("Data/Subpart V/v_nitric_acid_train.xlsx") |>
  group_by(facility_id, reporting_year) |>
  summarise(
    annual_unrounded_n2o_emiss = sum(annual_unrounded_n2o_emiss, na.rm = TRUE)
  ) |>
  ungroup() 

### comparison
# subpart g
emissions_comparison_g = left_join(x = facility_emissions_g, y = unit_emissions_g, 
                                 by = c("facility_id", "reporting_year")) |>
  mutate(difference = co2_emissions_facility - co2_emissions_unit)

# no discrepancies in co2 emisisons

# subpart v
emissions_comparison_v = left_join(x = facility_emissions_v, y = unit_emissions_v, 
                                 by = c("facility_id", "reporting_year")) |>
  mutate(difference = n2o_emissions_facility - annual_unrounded_n2o_emiss)

# no unit information for some plants in 2010-2013
# one large discrepancy for facility_id = 1006100 in 2016. Besides that numbers add up

