#### GHGRP - NEI CROSSWALK ####

library(tidyverse)
library(readr)
library(readxl)
library(tidylog)

ghgrp_df <- 
  read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>%
  rename(naics_code = primary_naics) %>%
  rename(naics_description = naics_title) %>%
  #select(facility_id, naics_code, naics_description, county_fips) |>
  mutate(facility_id = as.character(facility_id))

nei_df <- 
  read_excel('data/EPA_National_Emissions_Inventory_2020.xlsx') |>
  rename(nei_id = `EIS Facility ID`) |>
  mutate(nei_id = as.character(nei_id))

nei_unique <- 
  nei_df |>
  distinct(nei_id, .keep_all = TRUE) |>
  select(-c(Pollutant, `Pollutant Type`, `Emissions (Tons)`))

# This is FRS Links, crosswalk from EPA
# Source: https://echo.epa.gov/tools/data-downloads#FRS
frs_links <- read_csv('/Users/nmariano/Downloads/frs_downloads/FRS_PROGRAM_LINKS.csv') 

#### W/ FRS Links #####

# GHGRP-to-FRS
x_ghgrp <- 
  frs_links |>
  filter(PGM_SYS_ACRNM == "E-GGRT") |>
  transmute(REGISTRY_ID, facility_id = PGM_SYS_ID) 

# NEI-to-FRS
x_nei <- 
  frs_links |>
  filter(PGM_SYS_ACRNM %in% c("EIS","NEI")) |>
  transmute(REGISTRY_ID, nei_id = PGM_SYS_ID)

# Append matching nei_id to ghgrp_df using FRS Links where possible. 
frs_crosswalk <- 
  ghgrp_df |>
  left_join(x_ghg, by = 'facility_id') |>
  left_join(x_nei, by = 'REGISTRY_ID') |>
  left_join(nei_unique, by = 'nei_id') |>
  mutate(
    match_type = case_when(
      !is.na(nei_id) ~ 'FRS Link', 
      NULL
    )
  ) 

# There are some facilities where there are multiple NEI matches for a given GHGRP obs. 
# These don't look erroneous, but rather, are instances where a GHGRP facility is coded separately on NEI. 

# dupes <- 
#   ghgrp_df |>
#   left_join(x_ghg, by = 'facility_id') |>
#   left_join(x_nei, by = 'REGISTRY_ID')|>
#   left_join(nei_unique, by = 'nei_id') |>
#   group_by(facility_id, REGISTRY_ID) |>
#   filter(n()>1)

#### BRINGING IN MIKEY'S WORK ####

x_mikey <- 
  read_excel('nei-ghgrp-data-merge/final_pollutant_data.xlsx') |>
  rename(
    nei_id = nei_facility_id, 
    facility_id = ghgrp_facility_id
  ) |>
  mutate(
    match_type = case_when(
      match_type == 'high_confidence' ~ 'Text Match', 
      TRUE ~ match_type
    ), 
    nei_id = as.character(nei_id), 
    facility_id = as.character(facility_id)
  ) |>
  distinct(nei_id, facility_id, match_type, match_score) 

# These are the GHGRP facilities that did not have an FRS Link match. 
# Join in Mikey's crosswalk, to get matches for some of these. 
nei_na <- 
  frs_crosswalk |>
  filter(is.na(nei_id)) |>
  select(-c(nei_id, match_type)) |>
  left_join(x_mikey, by = 'facility_id') |>
  select(nei_id, facility_id, match_type, match_score)

# Join the FRS crosswalk w/ Mikey's matches. 
crosswalk <- 
  frs_crosswalk |>
  filter(!is.na(nei_id)) |>
  bind_rows(nei_na)

match_summary <- 
  ghgrp_df |>
  left_join(crosswalk, by = 'facility_id') |>
  left_join(nei_df, by = 'nei_id') |>
  mutate(
    matched_nei = !is.na(nei_id)
  ) %>%
  distinct(facility_id, matched_nei)

# We get about 85% of facilities matched. 65% from FRS Link, ~20% from Mikey. 
sum(match_summary$matched_nei) / nrow(match_summary)

#### EXPORT THE CROSSWALK ####

write_csv(crosswalk, 'nei-ghgrp-data-merge/ghgrp-nei-crosswalk.csv')

#### W/ AIR EMISSIONS DATASET #####
air_emiss_df <- read_csv('/Users/nmariano/Downloads/POLL_RPT_COMBINED_EMISSIONS.csv')

x_ghg <- 
  air_emiss_df |>
  filter(PGM_SYS_ACRNM == "E-GGRT") |>
  transmute(REGISTRY_ID, facility_id = PGM_SYS_ID) |>
  mutate(facility_id = as.numeric(facility_id)) |>
  distinct(REGISTRY_ID, facility_id)

x_nei <- 
  air_emiss_df |>
  filter(PGM_SYS_ACRNM %in% c("EIS","NEI")) |>
  transmute(REGISTRY_ID, nei_id = PGM_SYS_ID) |>
  distinct(REGISTRY_ID, nei_id)

merge_df <- 
  ghgrp_df |>
  left_join(x_ghg, by = 'facility_id') |>
  left_join(x_nei, by = 'REGISTRY_ID') |>
  left_join(nei_df, by = 'nei_id')

match_summary <- 
  merge_df %>%
  mutate(
    matched_frs = !is.na(REGISTRY_ID),
    matched_nei = !is.na(nei_id)
  ) %>%
  distinct(facility_id, matched_frs, matched_nei) 

sum(match_summary$matched_frs) / nrow(match_summary)

sum(match_summary$matched_nei) / nrow(match_summary)
