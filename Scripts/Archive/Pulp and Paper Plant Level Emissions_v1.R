##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# February 6, 2024               
# Ben Ladabaum                  
#                               
# Description:  Add process emissions from subpart AA to facility emissions data for pulp and paper industry.
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


# load data
ghgrp_facilities = read_excel(here("Data", "rlps_ghg_emitter_facilities.xlsx"))
is_unique_id(ghgrp_facilities, c("facility_id", "year"))

subpart_aa_emissions = read_excel(here("Data/Subpart AA", "aa_subpart_level_information.xlsx")) |>
  pivot_wider(names_from = ghg_name, values_from = ghg_quantity) |>
  clean_names()  |>
  rename(carbon_dioxide_subpart_aa = carbon_dioxide, biogenic_co2_subpart_aa = biogenic_carbon_dioxide,
         methane_subpart_aa = methane, nitrous_oxide_subpart_aa = nitrous_oxide)

subpart_c_emissions = read_excel(here("Output", "subpart_c_emissions_and_fuel_by_facility_v1.xlsx"))


# merge subpart c and subpart aa
pulp_paper_emissions_by_facility = left_join(x = subpart_c_emissions, y = subpart_aa_emissions, 
                                  by = c("facility_id", "reporting_year")) |>
  filter(primary_naics %in% c("322110", "322120", "322130")) |>
  select(facility_id, reporting_year, primary_naics, carbon_dioxide_subpart_c, biogenic_co2_subpart_c, 
         methane_subpart_c, nitrous_oxide_subpart_c, carbon_dioxide_subpart_aa, biogenic_co2_subpart_aa, 
         methane_subpart_aa, nitrous_oxide_subpart_aa, fuel_type, total_fuel_quantity, byproducts,
         other_pollutants, declared_combustion_units, pct_co2e_declared_cu, product_outputs, 
         employment, annual_production_qty, plant_size) 

  
### export ###
pulp_paper_emissions_by_facility_v1 <- createWorkbook()

addWorksheet(pulp_paper_emissions_by_facility_v1, "facility_emissions")
writeData(pulp_paper_emissions_by_facility_v1, "facility_emissions", pulp_paper_emissions_by_facility)

saveWorkbook(pulp_paper_emissions_by_facility_v1, here("Output", "pulp_paper_emissions_by_facility_v1.xlsx"), 
             overwrite = TRUE)
