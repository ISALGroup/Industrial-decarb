##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# April 2, 2024               
# Ben Ladabaum                  
#                               
# Description:  Facility emissions data for beet sugar manufacturing
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

# load functions 
source(here("Functions", "export_facility_unit_data.R"))

# load data
ghgrp_facilities = read_excel(here("Data", "rlps_ghg_emitter_facilities.xlsx")) |>
  rename(reporting_year = year)

  # subpart ii
subpart_ii_emissions = read_excel(here("Data/Subpart II", "ii_subpart_level_information.xlsx")) |>
  clean_names()  |>
  rename(methane_subpart_ii = ghg_quantity) |>
  select(-ghg_name, facility_name)

  # subpart s data
subpart_s_emissions = read_excel(here("Data/Subpart S", "s_subpart_level_information.xlsx")) |>
  pivot_wider(names_from = ghg_name, values_from = ghg_quantity) |>
  clean_names()  |>
  rename(carbon_dioxide_subpart_s = carbon_dioxide, biogenic_co2_subpart_s = biogenic_carbon_dioxide,
         methane_subpart_s = methane, nitrous_oxide_subpart_s = nitrous_oxide)


subpart_c_emissions = read_excel(here("Output", "subpart_c_emissions_by_facility_v2.xlsx"))


# merge subpart c, ii, and s
beet_sugar_emissions_by_facility = left_join(x = subpart_c_emissions, y = subpart_s_emissions, 
                                             by = c("facility_id", "reporting_year")) |>
  left_join(y = subpart_ii_emissions, by = c("facility_id", "reporting_year")) |>
  filter(primary_naics %in% c("311313")) |>
  relocate(ends_with("_subpart_s"), .after = nitrous_oxide_subpart_c) |>
  relocate(methane_subpart_ii, .after = nitrous_oxide_subpart_c)

beet_sugar_emissions_by_facility_2023 = beet_sugar_emissions_by_facility |>
  filter(reporting_year==2023)

### export ###
export_facility_unit_data(
  industry = "beet_sugar",
  version = "v1",
  file_type = "facility",
  datasets = list(
    "facility_emissions" = beet_sugar_emissions_by_facility,
    "facility_emissions_2023" = beet_sugar_emissions_by_facility_2023
  )
)