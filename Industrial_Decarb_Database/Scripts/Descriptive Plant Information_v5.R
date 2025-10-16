##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 3
# Oct 16, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create datast with descriptive plant information
#
# Notes:  _v2: update pulp and paper naics codes to 322110 <=(NAICS code)<= 322139
#         _v3: include more NAICS codes
#         _v4: add eGRID mapping files
#         _v5: add naics code 311230 and divisional avg temp
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
library(sf)

setwd("/Users/Ben L/Library/CloudStorage/Box-Box/Industrial Plant Raw Data/Industrial-Decarb-Database")



### Functions ###
# test whether set of variables are unique identifiers
is_unique_id <- function(data, vars) {
  n_unique <- nrow(unique(data[vars]))
  n_total <- nrow(data)
  n_unique == n_total
}


###
# load data
ghgrp_facilities = read_excel("Data/rlps_ghg_emitter_facilities.xlsx") |>
  mutate(primary_naics = as.numeric(primary_naics))
is_unique_id(ghgrp_facilities, c("facility_id", "year"))

naics_data = read_excel("Data/2022-NAICS-to-SIC-Crosswalk.xlsx") |>
  clean_names() |>
  select(x2022_naics_code, x2022_naics_title) |>
  distinct() |>
  rename(naics_code = x2022_naics_code, naics_title = x2022_naics_title) 
is_unique_id(naics_data, "naics_code")

relevant_naics = read_excel("Data/target_NAICS.xlsx") |>
  clean_names() |>
  rename(primary_naics = x6_digit_naics_code) |>
  select(primary_naics) |>
  mutate(keep_naics = 1)

egrid_subregions = st_read("Data/egrid2023_subregions/eGRID2023_Subregions.shp")

# temperature data
setwd("/Users/Ben L/Documents/UCSB Bren/Research/2035 Initiative Industrial Decarbonization/Industrial-decarb/data")
county_to_div = read_excel("NOAA/county-to-climdivs.xlsx") |>
  clean_names() |>
  mutate(division_number = substr(ncdc_fips_id, 1, 3)) |>
  select(-ncdc_fips_id, -climdiv_id) |> 
  rename(county_fips = postal_fips_id) |>
  distinct()

avg_temps = read_excel("NOAA/climdiv-tmpccy-v1.0.0-20250905.xlsx") |>
  mutate(state_div_element_year = as.character(state_div_element_year)) |>
  mutate(state_code = substr(state_div_element_year, 1, 2)) |>
  mutate(division_number = substr(state_div_element_year, 3, 5)) |>
  mutate(element_code = substr(state_div_element_year, 6, 7)) |>
  mutate(year = as.numeric(substr(state_div_element_year, 8, 11))) |>
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "average_temp") |>
  filter(year>=2010 & year<2025) |>
  group_by(division_number, year) |>
  summarize(
    division_average_temp = mean(average_temp, na.rm=TRUE)
  ) |>
  ungroup()

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
         state_name, zip, latitude, longitude, facility_id, primary_naics, secondary_naics, 
         naics_title, ej_indicator, cogen_unit_emm_ind, cems_used, byproduct_fuels, year) |>
  left_join(y = relevant_naics, by = c("primary_naics")) |>
  filter(!is.na(keep_naics) | 
           (primary_naics >=  322110 & primary_naics <= 322139)) |>
  select(-keep_naics) |>
  arrange(facility_id, year)

### Add egrid subregions
sf_use_s2(FALSE)
ghgrp_facilities_naics_title = st_as_sf(ghgrp_facilities_naics_title, 
                                        coords = c("longitude", "latitude"), crs = 4326) |>
  st_join(egrid_subregions, left = TRUE) |>
  st_drop_geometry(ghgrp_facilities_naics_title)

ghgrp_facilities_naics_title_2023 = ghgrp_facilities_naics_title |>
  filter(year==2023)

### Export ###
write.xlsx(ghgrp_facilities_naics_title, "Output/descr_info_relevant_naics.xlsx")
write.xlsx(ghgrp_facilities_naics_title_2023, "Output/descr_info_relevant_naics_2023.xlsx")









