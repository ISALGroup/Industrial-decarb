##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 9, 2025               
# Ben Ladabaum                  
#                               
# Description:  Facility emissions data for subpart x
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

# load functinos
source(here("Functions", "is_unique_id.R"))

### load data
# subpart x
subpart_x_emissions = read_excel("Data/Subpart X/x_subpart_level_information.xlsx") |>
pivot_wider(names_from = ghg_name, values_from = ghg_quantity) |>
  clean_names()  |>
  rename(carbon_dioxide_subpart_x = carbon_dioxide, biogenic_co2_subpart_x = biogenic_carbon_dioxide,
         methane_subpart_x = methane, nitrous_oxide_subpart_x = nitrous_oxide)

subpart_g_emissions = read_excel("Data/Subpart G/g_subpart_level_information.xlsx") |>
  pivot_wider(names_from = ghg_name, values_from = ghg_quantity) |>
  clean_names()  |>
  rename(carbon_dioxide_subpart_g = carbon_dioxide, biogenic_co2_subpart_g = biogenic_carbon_dioxide,
         methane_subpart_g = methane, nitrous_oxide_subpart_g = nitrous_oxide)
  
subpart_v_emissions = read_excel("Data/Subpart V/v_subpart_level_information.xlsx") |>
  pivot_wider(names_from = ghg_name, values_from = ghg_quantity) |>
  clean_names()  |>
  rename(nitrous_oxide_subpart_v = nitrous_oxide)

# export
write_xlsx(subpart_x_emissions, path = "Output/subpart_x_emissions_by_facility_v1.xlsx")
write_xlsx(subpart_g_emissions, path = "Output/subpart_g_emissions_by_facility_v1.xlsx")
write_xlsx(subpart_v_emissions, path = "Output/subpart_v_emissions_by_facility_v1.xlsx")




