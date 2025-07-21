##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# April 2, 2025               
# Ben Ladabaum                  
#                               
# Description:  Consistency checks for subpart s: lime manufacturing
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
library(tidyverse)

setwd("/Users/Ben L/Library/CloudStorage/Box-Box/Industrial Plant Raw Data/Industrial-Decarb-Database")

### Functions ###
source(here("Functions", "is_unique_id.R"))
source(here("Functions", "convert_to_numeric.R"))
source(here("Functions", "export_facility_unit_data.R"))


### Load data
subpart_data = read_excel("Data/Subpart S/s_subpart_level_information.xlsx")
cems_details = read_excel("Data/Subpart S/s_cems_details.xlsx")



