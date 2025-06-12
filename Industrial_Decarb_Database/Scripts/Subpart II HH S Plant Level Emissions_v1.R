##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# May 3, 2025               
# Ben Ladabaum                  
#                               
# Description:  Facility emissions data forsubparts ii, hh, and s
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

### load data
# subpart ii
subpart_ii_emissions = read_excel("Data/Subpart II/ii_subpart_level_information.xlsx") |>
  clean_names()  |>
  rename(methane_subpart_ii = ghg_quantity) |>
  select(-ghg_name, -facility_name)

# subpart hh data
subpart_hh_emissions = read_excel("Data/Subpart HH/hh_subpart_level_information.xlsx") |>
  pivot_wider(names_from = ghg_name, values_from = ghg_quantity) |>
  clean_names()  |>
  rename(biogenic_co2_subpart_hh = biogenic_carbon_dioxide,
         methane_subpart_hh = methane, nitrous_oxide_subpart_hh = nitrous_oxide) |>
  select(-facility_name)

# subpart s data
subpart_s_emissions = read_excel("Data/Subpart S/s_subpart_level_information.xlsx") |>
  pivot_wider(names_from = ghg_name, values_from = ghg_quantity) |>
  clean_names()  |>
  rename(carbon_dioxide_subpart_s = carbon_dioxide, biogenic_co2_subpart_s = biogenic_carbon_dioxide,
         methane_subpart_s = methane, nitrous_oxide_subpart_s = nitrous_oxide)

# export
write_xlsx(subpart_ii_emissions, path = "Output/subpart_ii_emissions_by_facility_v1.xlsx")
write_xlsx(subpart_hh_emissions, path = "Output/subpart_hh_emissions_by_facility_v1.xlsx")
write_xlsx(subpart_s_emissions, path = "Output/subpart_s_emissions_by_facility_v1.xlsx")