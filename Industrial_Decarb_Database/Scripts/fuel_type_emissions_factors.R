##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# October 30, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create unique set of fuel types and unit types
#           
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


setwd("/Users/Ben L/Documents/UCSB Bren/Research/2035 Initiative Industrial Decarbonization/Industrial-decarb")

ghgrp_emissions_2023_unit = read_excel("Industrial_Decarb_Database/Databases/Facility_and_Unit_Emissions_Database_v4.xlsx", 
                                       sheet="unit_emissions") |>
  filter(reporting_year==2023) |>
  filter(primary_naics %in% c(311221, 325193, 311313, 322120, 311224,
                              325110, 325311, 312140, 311611, 311225,
                              325180, 325194, 322130, 322110, 311421,
                              325211, 311513, 311514, 311314, 311942,
                              311613, 312120, 325120, 311423, 325312, 325212,
                              311411, 311615, 322291, 311511, 311422, 311919, 311230)) |>
  select(unit_type, fuel_type) |>
  distinct() |>
  arrange(unit_type, fuel_type)

write.xlsx(ghgrp_emissions_2023_unit, "Industrial_Decarb_Database/Output/fuel_type_emissions_factors.xlsx", sheetName = "Output")
