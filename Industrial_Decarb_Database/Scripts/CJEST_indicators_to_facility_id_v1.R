##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# June 13, 2024               
# Ben Ladabaum                  
#                               
# Description:  Create datast that maps CJEST indicators onto each facility id
#
# Notes:  make an EJ indicator dataset by facility with the following columns from this data 
# (include county fips, state, lat and long too):
#
# "Identified as disadvantaged", "Total population", "Is low income?" "PM2.5 in the air (percentile)"
# "PM2.5 in the air" "Current asthma among adults aged greater than or equal to 18 years (percentile)" 
# "Diagnosed diabetes among adults aged greater than or equal to 18 years (percentile)" 
# "Coronary heart disease among adults aged greater than or equal to 18 years (percentile)" 
# "Low life expectancy (percentile)"
#
# 2023 census tract to lat/lon data downloaded here: 
#    https://www2.census.gov/geo/tiger/TIGER2023/TRACT/
# 2010 census tract data downloaded here:
#   https://www2.census.gov/geo/pvs/tiger2010st/ 
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
library(sf)
library(purrr)

setwd("/Users/Ben L/Library/CloudStorage/Box-Box/Industrial Plant Raw Data/Industrial-Decarb-Database")

# load facility data and CJEST data
facility_data = read_excel("Data/rlps_ghg_emitter_facilities.xlsx") |>
  select(facility_id, year, county, county_fips, state, latitude, longitude)

facility_data_sf = st_as_sf(facility_data, coords = c("longitude", "latitude"), crs = 4326)


# "Identified as disadvantaged", "Total population", "Is low income?" "PM2.5 in the air (percentile)"
# "PM2.5 in the air" "Current asthma among adults aged greater than or equal to 18 years (percentile)" 
# "Diagnosed diabetes among adults aged greater than or equal to 18 years (percentile)" 
# "Coronary heart disease among adults aged greater than or equal to 18 years (percentile)" 
# "Low life expectancy (percentile)"
cjest = read_excel("Data/EJ Data/CJEST Community Level Data.xlsx") |>
  clean_names() |>
  select(census_tract_2010_id, identified_as_disadvantaged, total_population, is_low_income,
         pm2_5_in_the_air, current_asthma_among_adults_aged_greater_than_or_equal_to_18_years_percentile,
         diagnosed_diabetes_among_adults_aged_greater_than_or_equal_to_18_years_percentile,
         coronary_heart_disease_among_adults_aged_greater_than_or_equal_to_18_years_percentile,
         low_life_expectancy_percentile)


### import all shape files and append into one dataframe

# Set directory containing the tl_2023_##_tract folders
base_dir = "/Users/Ben L/Library/CloudStorage/Box-Box/Industrial Plant Raw Data/Industrial-Decarb-Database/Data/Census Tracts"  

# List all folder paths starting with tl_2023_ and ending in _tract
tract_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
tract_dirs <- tract_dirs[grepl("tl_2010_\\d{2}_tract10$", tract_dirs)]

# Build full path to each .shp file
shapefile_paths <- file.path(tract_dirs, paste0(basename(tract_dirs), ".shp"))  # folder name = .shp name

# Read and combine
tracts_all_2010 <- map_dfr(shapefile_paths, ~ st_read(.x, quiet = TRUE), .id = "source")

# transform CRS
tracts_all_2010 <- st_transform(tracts_all_2010, crs = 4326)


# merge tracts onto facility_data
facility_w_tract = st_join(facility_data_sf, tracts_all_2010, join=st_intersects)

# merge ej indicators onto facility data
facility_w_ej = left_join(facility_w_tract, cjest,
                          by = c("GEOID10" = "census_tract_2010_id")) |>
  select(facility_id, year, county, county_fips, state, geometry, GEOID10, identified_as_disadvantaged,
         total_population, is_low_income, pm2_5_in_the_air, ends_with("percentile")) |>
  rename(census_tract = "GEOID10",
         coronary_hear_disease_18yrs_older_pct = 
           "coronary_heart_disease_among_adults_aged_greater_than_or_equal_to_18_years_percentile",
         diabetes_18yrs_older_pct = 
           "diagnosed_diabetes_among_adults_aged_greater_than_or_equal_to_18_years_percentile",
         asthma_18yrs_older_pct = "current_asthma_among_adults_aged_greater_than_or_equal_to_18_years_percentile")


# Export
write.xlsx(st_drop_geometry(facility_w_ej), "Output/facility_id_w_EJ_indicators_v1.xlsx")


