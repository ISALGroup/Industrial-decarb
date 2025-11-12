#### WEBTOOL DATA OUTPUT #### 

# Version 1
# NAM
## 2025-09-25

# Version notes:
# Removes copollutant information (for now) 
# Adds the O&M 

#### SET-UP ####
# Load Libraries
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

#### Our data work ####

# Previous version
previous_df <- 
  read_csv('webtool/data/webtool_data_20250925.csv')

facility_lcoh_df <- 
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/lcoh_industrialdecarb_facility_level_v2.csv') |>
  select(-opex) |>
  # Name & value clean-up 
  rename(
    naics_code = primary_naics, 
    naics_description = naics_title, 
    lcoh = LCOH, 
    subregion = Subregion, 
    
    # just use discounted as default opex & capex. when i need non-discounted, i pull "No Policy". 
    opex = discounted_opex, 
    
    capex_equipment = elec_discounted_capex, 
    capex_EE = non_elec_discounted_capex
  ) |>
  mutate(
    capex = capex_equipment + capex_EE, 
    
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
  # selecting variables for the webtool data
  select(
    facility_id, facility_name, naics_code, naics_description, sector,
    state, county_fips, subregion, 
    tech_scenario, scenario_rank, elec_steam_demand,
    rate_reduction, ITC, PTC,
    capex, yearly_opm_costs, change_in_electricity_demand_kwh
  ) |>
  rename(heat_mmbtu = elec_steam_demand,
         capex_subsidy = ITC) |>
  
  group_by(facility_id, tech_scenario) |>
  summarize(
    facility_name = first(facility_name), 
    naics_code = first(naics_code), 
    naics_description = first(naics_description), 
    sector = first(sector), 
    state = first(state),
    subregion = first(subregion),
    
    heat_mmbtu = mean(heat_mmbtu, na.rm = T), 
    capex = mean(capex, na.rm = T), 
    yearly_opm_costs = mean(yearly_opm_costs, na.rm = T), 
    change_in_electricity_demand_kwh = mean(change_in_electricity_demand_kwh, na.rm = T)
  ) |>
  ungroup()

# Base LCOH 
base_lcoh <- 
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/lcoh_industrialdecarb_facility_level_v2.csv') |>
  filter(tech_scenario == 'baseline', 
         !is.na(LCOH)) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    base_lcoh = mean(LCOH, na.rm = T)
  ) |>
  ungroup() |>
  mutate(facility_id = as.character(facility_id)) |>
  select(facility_id, base_lcoh)

## Keeping the data structure for copollutants 
copollutants_df <- 
  read_excel("LCOH modelling/output/copollutant_longform_national.xlsx") %>%
  filter(tech_scenario == 'Baseline') %>%
  mutate(
    facility_id = as.character(facility_id),
  ) %>% 
  rename(
    base_emissions_nox = `base_emissions_Nitrogen Oxides`,
    base_emissions_pm25 = `base_emissions_PM2.5 Primary (Filt + Cond)`, 
    base_emissions_so2 = `base_emissions_Sulfur Dioxide`
  ) %>%
  select(facility_id, base_emissions_nox, base_emissions_pm25, base_emissions_so2)

ghg_df <- 
  read_excel('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/merged_longform.xlsx') |>
  select(-1, -2) |>
  group_by(facility_id) |>
  summarize(
    base_emissions_co2e = sum(unit_ghg_emissions, na.rm = TRUE),
    elec_ghg_emissions = sum(if_else(electrified_option != 'not_electrifiable' & !is_biogenic, unit_ghg_emissions, 0), na.rm = TRUE),
    #biogenic_ghg_emissions = sum(if_else(is_biogenic, unit_ghg_emissions, 0), na.rm = TRUE),
    noelec_ghg_emissions = sum(if_else(electrified_option == 'not_electrifiable', unit_ghg_emissions, 0), na.rm = TRUE), 
  ) |>
  ungroup() |>
  mutate(facility_id = as.character(facility_id)) |>
  filter(facility_id %in% facility_lcoh_df$facility_id) 

emissions_df <- 
  left_join(ghg_df, copollutants_df, by = 'facility_id')


#### Other Datasets ####

fuels_df <-
  read_excel('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/merged_longform.xlsx') |>
  select(-1, -2) |>
  group_by(facility_id) |>
  summarize(
    fuels = paste(unique(fuel_type), collapse = ", ")
  ) |>
  ungroup() |>
  mutate(
    facility_id = as.character(facility_id),
    # Adding some dummies for fuel type
    ng_dum = if_else(fuels == 'Natural Gas (Weighted U.S. Average)', 1, 0),
    fuel_oils_dum = if_else(str_detect(fuels, 'Fuel Oil'), 1, 0),
    coal_dum = if_else(fuels %in% c('Bituminous', 'Subbituminous', 'Anthracite',
                                        'Coal Coke', 'Mixed (Industrial sector)', 'Lignite'),
                       1, 0),
    byproducts_dum = if_else(fuels %in% c('Wood and Wood Residuals (dry basis)', 'Solid Byproducts',
                                              'Agricultural Byproducts', 'Rendered Animal Fat', 'Vegetable Oil'),
                             1, 0),
    propane_dum = if_else(str_detect(fuels, 'Propane'), 1, 0),
    other_dum = if_else(
      !str_detect(fuels, "Natural Gas (Weighted U.S. Average)|Fuel Oil|Bituminous|Subbituminous|Anthracite|Coal Coke|Mixed \\(Industrial sector\\)|Lignite|Wood and Wood Residuals|Solid Byproducts|Agricultural Byproducts|Rendered Animal Fat|Vegetable Oil|Propane"),
      1, 0)
  )



## Do we want to do the fuel type % breakdown? If so, here's a start 
# fuels_df <- 
#   read_excel('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/merged_longform.xlsx') |>
#   select(-1, -2) |>
#   group_by(facility_id, fuel_type) |>
#   summarize(
#     fuel_hhv_mmbtu = sum(fuel_hhv_mmbtu, na.rm = T)
#   ) |>
#   ungroup() |>
#   mutate(
#     # Adding some dummies for fuel type 
#     ng_dum = if_else(fuel_type == 'Natural Gas (Weighted U.S. Average)', 1, 0), 
#     fuel_oils_dum = if_else(str_detect(fuel_type, 'Fuel Oil'), 1, 0), 
#     coal_dum = if_else(fuel_type %in% c('Bituminous', 'Subbituminous', 'Anthracite', 
#                                     'Coal Coke', 'Mixed (Industrial sector)', 'Lignite'), 
#                        1, 0), 
#     byproducts_dum = if_else(fuel_type %in% c('Wood and Wood Residuals (dry basis)', 'Solid Byproducts', 
#                                           'Agricultural Byproducts', 'Rendered Animal Fat', 'Vegetable Oil'), 
#                              1, 0), 
#     propane_dum = if_else(str_detect(fuel_type, 'Propane'), 1, 0),
#     other_dum = if_else(
#       !str_detect(fuel_type, "Natural Gas (Weighted U.S. Average)|Fuel Oil|Bituminous|Subbituminous|Anthracite|Coal Coke|Mixed \\(Industrial sector\\)|Lignite|Wood and Wood Residuals|Solid Byproducts|Agricultural Byproducts|Rendered Animal Fat|Vegetable Oil|Propane"),
#       1, 0)
#   ) |>
#   group_by(facility_id) |>
#   summarize(
#     fuels = paste(unique(fuel_type), collapse = ", ")
#   ) |>
#   ungroup() |>
#   mutate(
#     facility_id = as.character(facility_id), 
# 
#   )

# eGRID data
egrid_df <- 
  read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 2) %>% 
  clean_names() %>% 
  mutate(co2e_kg_kwh = co2e * 0.000453592) %>% 
  mutate(ch4_kg_kwh = ch4 * 0.000453592) %>%
  mutate(n2o_kg_kwh = n2o * 0.000453592) %>%
  mutate(nox_kg_kwh = annual_n_ox * 0.000453592) %>%
  mutate(so2_kg_kwh = so2 * 0.000453592) %>%
  mutate(pm25_kg_kwh = pm_2_5 * 0.000453592) %>%
  rename(subregion = e_grid_subregion_acronym) %>% 
  select(subregion, co2e_kg_kwh, nox_kg_kwh, so2_kg_kwh, pm25_kg_kwh)

# See below for how this was made 
facility_lat_long <- 
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/Industrial-decarb/webtool/data/facility_census_tract.csv') |>
  mutate(facility_id = as.character(facility_id))

param <- 
  read_csv('national_results/data/parameters_fuelrange.csv') |>
  # just using the average between best & worst case 
  mutate(
    ng_price = (ng_price_high + ng_price_low) / 2
  ) |>
  rename(
    elec_price = elec_price_mean
  ) |>
  select(
    state, ng_price, elec_price, r, t
  )

dac_df <- 
  read_excel('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/cejst_2.0-communities.xlsx') |>
  rename(
    census_tract = `Census tract 2010 ID`, 
    county_name = `County Name`, 
    state = `State/Territory`, 
    dac_dum = `Identified as disadvantaged`
  ) |>
  select(census_tract, county_name, state, dac_dum) 

#### Create & Save Datasets #### 

# --- Merge Inputs ---
webtool_df <- 
  facility_lcoh_df %>%
  left_join(egrid_df, by = "subregion") %>%
  left_join(param, by = c('state')) %>%
  left_join(emissions_df, by = 'facility_id') |>
  left_join(facility_lat_long, by = 'facility_id') |>
  ### DAC merge, leave off for now 
  #left_join(dac_df, by = 'census_tract') |>
  
  ### if we want to just get rid of copollutants, we could do it this way  
  # mutate(
  #   facility_emissions = case_when(
  #     tech_scenario == 'baseline' ~ base_emissions_co2e, 
  #     tech_scenario != 'baseline' ~ noelec_ghg_emissions
  #   )
  # ) |>
  pivot_longer(
    cols = ends_with("_kg_kwh"),
    names_to = "pollutant_type",
    names_pattern = "^(.*)_kg_kwh$",  
    values_to = "grid_emissions_kg_kwh"
  ) %>%
  mutate(
    # make one base emissions variable, calibrated to copollutant 
    base_emissions = case_when(
      pollutant_type == 'co2e' ~ base_emissions_co2e, 
      pollutant_type == 'nox' ~ base_emissions_nox, 
      pollutant_type == 'so2' ~ base_emissions_so2, 
      pollutant_type == 'pm25' ~ base_emissions_pm25,
    ),
    # make one emissions outcome variable, calibrated to copollutant & scenario
    facility_emissions = case_when(
      pollutant_type == 'co2e' & tech_scenario == 'baseline' ~ base_emissions_co2e, 
      pollutant_type == 'nox' & tech_scenario == 'baseline' ~ base_emissions_nox, 
      pollutant_type == 'so2' & tech_scenario == 'baseline' ~ base_emissions_so2, 
      pollutant_type == 'pm25' & tech_scenario == 'baseline' ~ base_emissions_pm25,
      
      # just going to fill in with the 
      pollutant_type == 'co2e' & tech_scenario != 'baseline' ~ noelec_ghg_emissions, 
      pollutant_type == 'nox' & tech_scenario != 'baseline' ~ 0, 
      pollutant_type == 'so2' & tech_scenario != 'baseline'  ~ 0, 
      pollutant_type == 'pm25' & tech_scenario != 'baseline' ~ 0
    )
  ) %>%
  select(-noelec_ghg_emissions, -matches('base_emissions_'))

write_csv(webtool_df, glue("webtool/data/webtool_data_{format(Sys.Date(), '%Y%m%d')}.csv")) 


#### Census Tract Analysis ####
get_fcc_tract <- function(lat, lon) {
  url <- paste0("https://geo.fcc.gov/api/census/area?lat=", lat, "&lon=", lon, "&format=json")
  res <- fromJSON(url)$results$block_fips
  if (length(res) == 0) return(NA_character_)  # nothing returned
  substr(res[1], 1, 11)                        # first 11 digits = tract
}

# Pull in lat and long from rlps file
facility_lat_long <- 
  read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>%
  select(facility_id, latitude, longitude) %>%
  distinct(facility_id, .keep_all = TRUE) %>%
  mutate(facility_id = as.character(facility_id)) %>%
  filter(facility_id %in% facility_lcoh_df$facility_id) %>%
  rowwise() %>%
  mutate(census_tract = get_fcc_tract(latitude, longitude)) %>%
  ungroup()

write_csv(facility_lat_long, 'webtool/data/facility_census_tract.csv')
