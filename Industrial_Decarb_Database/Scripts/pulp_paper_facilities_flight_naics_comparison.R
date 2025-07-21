##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# April 15, 2024               
# Ben Ladabaum                  
#                               
# Description:  Compare pulp and paper (as downloaded from flight) with pulp and paper 
#               as identified by naics codes. On the EPA flight dataset, there are around 200 facilities
#               each year that are categorized as pulp and paper. Based on naics code, we only have around 
#               100 in 2021, but 200 in 2022 and 2023. This program aims to determine the cause of this
#               discrepancy.
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
# test whether set of variables are unique identifiers
is_unique_id <- function(data, vars) {
  n_unique <- nrow(unique(data[vars]))
  n_total <- nrow(data)
  n_unique == n_total
}

convert_to_numeric <- function(data, columns) {
  data[columns] <- lapply(data[columns], function(col) as.numeric(as.character(col)))
  return(data)
}

### Load Data ###

### facilities
facilities_data = read_excel("Data/rlps_ghg_emitter_facilities.xlsx") |>
  select(facility_id, primary_naics, year, secondary_naics, add_naics_code) |>
  rename(reporting_year = year) 
is_unique_id(facilities_data, c("facility_id", "reporting_year"))

pulp_paper_naics = facilities_data |>
  filter(primary_naics %in% c("322110", "322120", "322130"))

### Pulp and paper industry as downloaded from GHGRP flight
file_path = "Data/flight_pulp_and_paper_facilities.xls"

# Get the names of all sheets
sheet_names <- excel_sheets(file_path)

# Read and bind all sheets, skipping first 6 rows, and adding 'year'
pulp_and_paper_facilities = lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet, skip = 6) |>
    mutate(sheet = sheet)
}) |>
  bind_rows() |>
  clean_names() |>
  convert_to_numeric(c("reporting_year", "ghgrp_id")) |>
  rename(facility_id = ghgrp_id)


### merge together ### 
naics_identified_only = full_join(x = pulp_and_paper_facilities, y = pulp_paper_naics, 
                                  by = c("facility_id", "reporting_year")) |>
  filter(is.na(sheet)) |>
  select(facility_id, reporting_year, primary_naics, secondary_naics, add_naics_code)|>
  arrange(facility_id, reporting_year, primary_naics, secondary_naics, add_naics_code)

ghgrp_flight_w_naics = left_join(x = pulp_and_paper_facilities, y = facilities_data, 
                                  by = c("facility_id", "reporting_year")) |>
  filter(! is.na(sheet)) |>
  mutate(primary_naics_match = primary_naics %in% c("322110", "322120", "322130")) |>
  arrange(facility_id, reporting_year)


### Export ###
# Create a named list of dataframes
df_list <- list(
  "NAICS_Identified_Only" = naics_identified_only,
  "GHGRP_Flight_With_NAICS" = ghgrp_flight_w_naics
)

# Write to Excel with two tabs
write_xlsx(df_list, path = here("Output", "pulp_paper_facilities_comparison.xlsx"))




