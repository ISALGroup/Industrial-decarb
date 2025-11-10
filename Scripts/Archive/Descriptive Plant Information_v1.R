##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# February 4, 2024               
# Ben Ladabaum                  
#                               
# Description:  Create datast with descriptive plant information
#
# Notes:  
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
ghgrp_facilities = read_excel(here("Data", "rlps_ghg_emitter_facilities.xlsx"))
is_unique_id(ghgrp_facilities, c("facility_id", "year"))

naics_data = read_excel(here("Data", "2022-NAICS-to-SIC-Crosswalk.xlsx")) |>
  clean_names() |>
  select(x2022_naics_code, x2022_naics_title) |>
  distinct() |>
  rename(naics_code = x2022_naics_code, naics_title = x2022_naics_title) |>
  mutate(naics_code = as.character(naics_code))
is_unique_id(naics_data, "naics_code")

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
         cogen_unit_emm_ind, byproduct_fuels, year, secondary_naics) |>
  arrange(facility_id, year)

### save output for each industry ###
# 322120	Paper Mills
# 311221 	Wet Corn Milling
# 322130	Paperboard Mills
# 311313	Beet Sugar Manufacturing
# 325193  Ethyl Alcohol Manufacturing
# 311421	Fruit and Vegetable Canning
# 311224	Soybean and Other Oilseed Processing
# 311942	Spice and Extract Manufacturing
# 311514	Dry Condensed and Evaporated Dairy Products
# 311513	Cheese Manufacturing

# pulp and paper
pulp_paper_facilities = ghgrp_facilities_naics_title |>
  filter(primary_naics %in% c("322110", "322120", "322130")) |>
  select(-secondary_naics)
pulp_paper_facilities_2023 = pulp_paper_facilities |>
  filter(year==2023)

# wet corn milling
wet_corn_milling = ghgrp_facilities_naics_title |>
  filter(primary_naics %in% c("311221")) |>
  mutate(secondary_naics_325193 = secondary_naics=="325193") |>
  select(-secondary_naics)
wet_corn_milling_2023 = wet_corn_milling |>
  filter(year==2023)

# beet sugar manufacturing
beet_sugar = ghgrp_facilities_naics_title |>
  filter(primary_naics %in% c("311313")) |>
  select(-secondary_naics)
beet_sugar_2023 = beet_sugar |>
  filter(year==2023)

ethyl_alcohol = ghgrp_facilities_naics_title |>
  filter(primary_naics %in% c("325193")) |>
  select(-secondary_naics)
ethyl_alcohol_2023 = ethyl_alcohol |>
  filter(year==2023)

# fruit and vegetable canning
fruit_vegetable_canning = ghgrp_facilities_naics_title |>
  filter(primary_naics %in% c("311421")) |>
  select(-secondary_naics)
fruit_vegetable_canning_2023 = fruit_vegetable_canning |>
  filter(year==2023)

# soybean oilseed processing
soybean_oilseed = ghgrp_facilities_naics_title |>
  filter(primary_naics %in% c("311224")) |>
  select(-secondary_naics)
soybean_oilseed_2023 = soybean_oilseed |>
  filter(year==2023)

# spice and extract manufacturing
spice_extract = ghgrp_facilities_naics_title |>
  filter(primary_naics %in% c("311942")) |>
  select(-secondary_naics)
spice_extract_2023 = spice_extract |>
  filter(year==2023)

# dry, condensed, and evaporated dairy
dairy_products_dry_cond_evap = ghgrp_facilities_naics_title |>
  filter(primary_naics %in% c("311514")) |>
  select(-secondary_naics)
dairy_products_dry_cond_evap_2023 = dairy_products_dry_cond_evap |>
  filter(year==2023)

# cheese
cheese = ghgrp_facilities_naics_title |>
  filter(primary_naics %in% c("311513")) |>
  select(-secondary_naics)
cheese_2023 = cheese |>
  filter(year==2023)

# animal slaughtering
animal_slaughtering = ghgrp_facilities_naics_title |>
  filter(primary_naics %in% c("311611")) |>
  select(-secondary_naics)
animal_slaughtering_2023 = animal_slaughtering |>
  filter(year==2023)


### export for each industry ###

industries <- c("pulp_paper_facilities", "wet_corn_milling", "beet_sugar", "ethyl_alcohol", 
                "fruit_vegetable_canning", "soybean_oilseed", "spice_extract", 
                "dairy_products_dry_cond_evap", "cheese", "animal_slaughtering")
vn = "v1"
for (industry in industries) {
  export_industry_data(vn, industry)
}


# export file with each facility_id, naics code, and city info
facilities_dedup = ghgrp_facilities_naics_title |>
  select(facility_name, city, county, state, facility_id, primary_naics, naics_title) |>
  distinct() |>
  arrange(facility_name)

write.xlsx(facilities_dedup, here("Output", "facility_naics_city.xlsx"))








