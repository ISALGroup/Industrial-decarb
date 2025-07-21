##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 3
# May 6, 2024               
# Ben Ladabaum                  
#                               
# Description:  Create datast with descriptive plant information
#
# Notes:  _v2: update pulp and paper naics codes to 322110 <=(NAICS code)<= 322139
#         _v3: include more NAICS codes
# 
# Inputs:   
#         
#
# Outputs:   pulp_paper_facilities_descr_info_v1
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


### Functions ###
# test whether set of variables are unique identifiers
is_unique_id <- function(data, vars) {
  n_unique <- nrow(unique(data[vars]))
  n_total <- nrow(data)
  n_unique == n_total
}

# Export function
export_industry_data <- function(version, industry) {
  # Construct workbook name
  wb_name <- paste0(industry, "_descr_info_", version)
  workbook <- createWorkbook()
  
  # Construct variable names dynamically
  industry_data <- get(industry)
  industry_data_2023 <- get(paste0(industry, "_2023"))
  
  # Add worksheets and write data
  addWorksheet(workbook, "descr_info")
  writeData(workbook, "descr_info", industry_data)
  
  addWorksheet(workbook, "descr_info_2023")
  writeData(workbook, "descr_info_2023", industry_data_2023)
  
  # Save the workbook with the correct naming convention
  saveWorkbook(workbook, here("Output", paste0(wb_name, ".xlsx")), overwrite = TRUE)
}



###
# load data
ghgrp_facilities = read_excel(here("Data", "rlps_ghg_emitter_facilities.xlsx")) |>
  mutate(primary_naics = as.numeric(primary_naics))
is_unique_id(ghgrp_facilities, c("facility_id", "year"))

naics_data = read_excel(here("Data", "2022-NAICS-to-SIC-Crosswalk.xlsx")) |>
  clean_names() |>
  select(x2022_naics_code, x2022_naics_title) |>
  distinct() |>
  rename(naics_code = x2022_naics_code, naics_title = x2022_naics_title) 
is_unique_id(naics_data, "naics_code")

relevant_naics = read_excel(here("Data", "target_NAICS.xlsx")) |>
  clean_names() |>
  rename(primary_naics = x6_digit_naics_code) |>
  select(primary_naics) |>
  mutate(keep_naics = 1)

###
# merge facilities and naics data, create blank variables for data we don't yet have, 
# and select relevant variables
ghgrp_facilities_naics_title = left_join(x = ghgrp_facilities,
                                         y = naics_data, 
                                         by = c("primary_naics" = "naics_code")) |>
  mutate(ej_indicator = "") |>
  mutate(egrid_region = "") |>
  mutate(byproduct_fuels = "") |>
  select(parent_company, facility_name, address1, address2, city, county, county_fips, state, 
         state_name, zip, facility_id, primary_naics, naics_title, ej_indicator, egrid_region,
         cogen_unit_emm_ind, cems_used, byproduct_fuels, year, secondary_naics) |>
  left_join(y = relevant_naics, by = c("primary_naics")) |>
  filter(!is.na(keep_naics) | 
           (primary_naics >=  322110 & primary_naics <= 322139)) |>
  select(-keep_naics, -secondary_naics) |>
  arrange(facility_id, year)

ghgrp_facilities_naics_title_2023 = ghgrp_facilities_naics_title |>
  filter(year==2023)

### Export ###
write.xlsx(ghgrp_facilities_naics_title, here("Output", "descr_info_relevant_naics.xlsx"))
write.xlsx(ghgrp_facilities_naics_title_2023, here("Output", "descr_info_relevant_naics_2023.xlsx"))








