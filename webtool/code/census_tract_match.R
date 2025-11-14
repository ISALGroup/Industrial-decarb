#### Census Tract Matching ####

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(tidylog)
library(stringr)
library(janitor)
library(glue)
library(tidycensus)
library(jsonlite)

#### The list of faciilities that we start with.  
## This file is saved in the Box -- let me know if you need me to point you to it 
facility_lcoh_df <- 
  read_csv('lcoh_industrialdecarb_facility_level_v2.csv') |>
  # Name & value clean-up 
  rename(
    naics_code = primary_naics, 
    naics_description = naics_title, 
    lcoh = LCOH, 
    subregion = Subregion, 
  ) |>
  mutate(
    facility_id = as.character(facility_id), 
    sector = case_when(
      str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food & Beverage",
      str_starts(naics_code, "325") ~ "Chemicals",
      str_starts(naics_code, "322") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    )
  ) |>
  # Dropping some facilities with strange GHGRP reporting of combustion units 
  filter(lcoh != 0, 
         !is.na(lcoh)) |>
  # Filter to no-policy 
  filter(PTC == 0 & rate_reduction == 0 & ITC == 0) |> 
  group_by(facility_id, tech_scenario) |>
  summarize(
    facility_name = first(facility_name), 
    naics_code = first(naics_code), 
    naics_description = first(naics_description), 
    sector = first(sector), 
    state = first(state),
    subregion = first(subregion),
  ) |>
  ungroup()

#### The Disadvantaged communities dataset, which is by Census tract -- I think you sent this file to Carrie, 
#### so you should have it. 
dac_df <- 
  read_excel('cejst_2.0-communities.xlsx') |>
  rename(
    census_tract = `Census tract 2010 ID`, 
    county_name = `County Name`, 
    state = `State/Territory`, 
    dac_dum = `Identified as disadvantaged`
  ) |>
  select(census_tract, county_name, state, dac_dum) 
 
#### My attempt to make a facility_id -- census tract crosswalk 
get_fcc_tract <- function(lat, lon) {
  url <- paste0("https://geo.fcc.gov/api/census/area?lat=", lat, "&lon=", lon, "&format=json")
  res <- fromJSON(url)$results$block_fips
  if (length(res) == 0) return(NA_character_)  # nothing returned
  substr(res[1], 1, 11)                        # first 11 digits = tract
}


# Pull in lat and long by facility
## This shit takes forever to run so I will send you my results (facility_census_tract.csv)
facility_lat_long <- 
  read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>%
  select(facility_id, latitude, longitude) %>%
  distinct(facility_id, .keep_all = TRUE) %>%
  mutate(facility_id = as.character(facility_id)) %>%
  filter(facility_id %in% facility_lcoh_df$facility_id) %>%
  rowwise() %>%
  # run Lat and Long through the function, which tries to match lat-long to a census tract
  mutate(census_tract = get_fcc_tract(latitude, longitude)) %>%
  ungroup()

##### Putting it together -- but there are still a lot non-matches. 
# Need you to troubleshoot! I don't see any reason why we shouldn't be able to get a match for every one. 
matches <- 
  facility_lcoh_df |>
  left_join(facility_lat_long, by = 'facility_id') |>
  left_join(dac_df, by = 'census_tract') 

