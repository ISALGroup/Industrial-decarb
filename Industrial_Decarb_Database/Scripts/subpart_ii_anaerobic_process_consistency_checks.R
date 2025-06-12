##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# April 2, 2025               
# Ben Ladabaum                  
#                               
# Description:  Consistency checks for anaerobic process id methane emissions
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
methane_processes = read_excel("Data/Subpart II/ii_ch4_gen_process.xlsx")
subpart_data = read_excel("Data/Subpart II/ii_subpart_level_information.xlsx")
eq_6 = read_excel("Data/Subpart II/ii_equation_ii6.xlsx")
eq_3 = read_excel("Data/Subpart II/ii_equation_ii3.xlsx")


### Clean data
methane_processes_clean = methane_processes |>
  select(anaerobic_process_id, annual_mass_methane_gen_cod, annual_mass_methane_gen_bod5, facility_id, 
         reporting_year, does_fac_measure_cod_bod_conc) |>
  filter(! is.na(does_fac_measure_cod_bod_conc)) |>
  distinct() |>
  group_by(facility_id, reporting_year) |>
  summarise(
    methane_cod = sum(annual_mass_methane_gen_cod, na.rm = TRUE),
    methane_bod5 = sum(annual_mass_methane_gen_bod5, na.rm = TRUE)
  ) |>
  ungroup()

eq_6_clean = eq_6 |>
  select(anaerobic_process_id, annual_mass_methane_emissions, facility_id, reporting_year) |>
  distinct() |>
  group_by(facility_id, reporting_year) |>
  summarise(
    methane_emissions = sum(annual_mass_methane_emissions, na.rm = TRUE)
  ) |>
  ungroup()

eq_3_clean = eq_3 |>
  select(anaerobic_process_id, annual_mass_methane_emissions, facility_id, reporting_year) |>
  distinct() |>
  group_by(facility_id, reporting_year) |>
  summarise(
    methane_emissions = sum(annual_mass_methane_emissions, na.rm = TRUE)
  ) |>
  ungroup()

eq_3_6_clean = rbind(eq_6_clean, eq_3_clean) |>
  group_by(facility_id, reporting_year) |>
  summarise(
    methane_emissions = sum(methane_emissions, na.rm = TRUE)
  ) |>
  ungroup()
is_unique_id(eq_3_6_clean, c("facility_id", "reporting_year"))

### Merge subpart and methane processes
anaerobic_comparison_processes = full_join(x = subpart_data, y = methane_processes_clean, 
                                 by = c("facility_id", "reporting_year"))

anaerobic_comparison_eq6 = full_join(x = subpart_data, y = eq_6_clean, 
                                     by = c("facility_id", "reporting_year"))

anaerobic_comparison_eq3 = full_join(x = subpart_data, y = eq_3_clean, 
                                     by = c("facility_id", "reporting_year"))

anaerobic_comparison_eq3_6 = full_join(x = subpart_data, y = eq_3_6_clean, 
                                     by = c("facility_id", "reporting_year"))

inconsistencies = anaerobic_comparison_eq3_6 |>
  filter(ghg_quantity != methane_emissions)





