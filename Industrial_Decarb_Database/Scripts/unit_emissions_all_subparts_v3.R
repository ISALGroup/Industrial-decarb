##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 3
# May 6, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create combined dataset for unit level emissions
#
# Notes:    _v2: add subpart v, g, and x
#           _v3: add recovery boiler information
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

# load data
subpart_aa = read_excel("Output/subpart_aa_emissions_by_unit_v1.xlsx") |>
  mutate(primary_naics = as.numeric(primary_naics)) |>
  rename(ghg_name = "ghg_gas_name")

subpart_c = read_csv("Output/subpart_c_emissions_and_fuel_by_unit_v3.csv") |>
  rename(ghg_name = "ghg_gas_name")

subpart_ii = read_excel("Output/subpart_ii_emissions_by_unit_v1.xlsx") |>
  mutate(primary_naics = as.numeric(primary_naics))|>
  rename(ghg_name = "ghg_gas_name")

subpart_g = read_excel("Output/subpart_g_emissions_by_unit_v1.xlsx") |>
  mutate(primary_naics = as.numeric(primary_naics))

subpart_v = read_excel("Output/subpart_v_emissions_by_unit_v1.xlsx") |>
  mutate(primary_naics = as.numeric(primary_naics))

subpart_x = read_excel("Output/subpart_x_emissions_by_unit_v1.xlsx") |>
  mutate(primary_naics = as.numeric(primary_naics))

relevant_naics = read_excel("Data/target_NAICS.xlsx") |>
  clean_names() |>
  rename(primary_naics = x6_digit_naics_code) |>
  select(primary_naics) |>
  mutate(keep_naics = 1)

recovery_boilers = read_excel("Data/blrbac-usa-master-oper-rb-012925_w_unit_name.xls") |>
  clean_names() |>
  select(facility_id, unit_name, unit_age, start_up, mfgr, orig_m_lb_ds_day, current_rating, 
         no_drums, floor_d_decant_sf_slope_to_front_sr_slope_to_rear, design_psig, operate_psig,
         superheat_f) |>
  filter(! is.na(facility_id)) 

# Append subpart c and subpart aa
unit_level_emissions_combined = subpart_c |>
  bind_rows(subpart_aa) |>
  #bind_rows(subpart_ii) |> # subpart ii is not useful
  bind_rows(subpart_g) |>
  bind_rows(subpart_v) |>
  bind_rows(subpart_x) |>
  left_join(y = relevant_naics, by = c("primary_naics")) |>
  filter(!is.na(keep_naics) | 
           (primary_naics >=  322110 & primary_naics <= 322139)) |>
  select(-keep_naics) |>
  select(primary_naics,	facility_id,	reporting_year,	unit_type,	unit_name,	#anaerobic_process_id,	
         fuel_type,	ghg_name,	ghg_quantity,	total_fuel_quantity,	subpart,	capacity,	
         capacity_utiliziation,	vintage,	floor_space,	temperature,	steam_generation_est,	
         electricity_generation_est,	prime_mover_type,	hrsg_bypass) |>
  left_join(y = recovery_boilers, by = c("facility_id", "unit_name")) |>
  arrange(facility_id, reporting_year, unit_name, 
          fuel_type, ghg_name)

unit_level_emissions_combined_2023 = unit_level_emissions_combined |>
  filter(reporting_year==2023)

### Export ###
write.xlsx(unit_level_emissions_combined, "Output/unit_emissions_relevant_naics.xlsx")
write.xlsx(unit_level_emissions_combined_2023, "Output/unit_emissions_relevant_naics_2023.xlsx")


