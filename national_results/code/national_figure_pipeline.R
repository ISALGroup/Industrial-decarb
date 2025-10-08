# Integrated National Data & Figure Code #
## October 7, 2025 ##

## Works with unit-level data 

#### INITIAL SET-UP #### 
# Load Libraries
library(readxl)
library(readr)
library(writexl)
library(dplyr)
library(tidyr)
library(tidylog)
library(stringr)
library(janitor)
library(glue)
library(ggplot2)

# Tech scenario input 
longform <- 
  read_excel("LCOH modelling/output/longform_heat_demand.xlsx") |>
  select(-1)

unit_input_df <- 
  longform |>
  mutate(
    process_unit_heat_demand_ee = process_unit_heat_demand * .9, 
    
    # Creating capex variables 
    ng_capex_low = case_when(
      combustion_unit_category == 'generic' ~ 24573.01928*(process_unit_heat_demand/49132.8)^0.8431, 
      TRUE ~ NA_integer_
    ),
    ng_capex_high = case_when(
      combustion_unit_category == 'generic' ~ 294796.2541*(process_unit_heat_demand/40944)^0.8431, 
      TRUE ~ NA_integer_
    ), 
    ## Placeholder: using the same capex as ng for eb
    eb_capex_low = case_when(
      combustion_unit_category == 'generic' ~ 24573.01928*(process_unit_heat_demand_ee/49132.8)^0.8431, 
      TRUE ~ NA_integer_
    ),
    eb_capex_high = case_when(
      combustion_unit_category == 'generic' ~ 294796.2541*(process_unit_heat_demand_ee/40944)^0.8431, 
      TRUE ~ NA_integer_
    ), 
    ## Placeholder: just multiplying capex by 2 for hp scenario 
    hp_capex_low = case_when(
      combustion_unit_category == 'generic' ~ (24573.01928*(process_unit_heat_demand_ee/49132.8)^0.8431)*2, 
      TRUE ~ NA_integer_
    ),
    hp_capex_high = case_when(
      combustion_unit_category == 'generic' ~ (294796.2541*(process_unit_heat_demand_ee/40944)^0.8431)*2, 
      TRUE ~ NA_integer_
    ), 
    
    # Electricity 
    ## For eb, all units have 99% efficiency
    eb_electricity_demand_kwh = case_when(
      combustion_unit_electrifiable == 'Y' ~ (process_unit_heat_demand_ee * 294.071) / .99, 
      combustion_unit_electrifiable == 'N' ~ 0
    ), 
    
    ## For best case, just inputting 3 as COP for now. other units have 99% efficiency
    hp_electricity_demand_kwh_low = case_when(
      combustion_unit_category == 'generic' & combustion_unit_electrifiable == 'Y' ~ (process_unit_heat_demand_ee * 294.071) / 3,
      combustion_unit_category != 'generic' & combustion_unit_electrifiable == 'Y' ~ (process_unit_heat_demand_ee * 294.071) / .99,
      combustion_unit_electrifiable == 'N' ~ 0
    ), 
    
    ## For hp worst case, input 1.5 as COP. other units have 99% efficiency. 
    hp_electricity_demand_kwh_high = case_when(
      combustion_unit_category == 'generic' & combustion_unit_electrifiable == 'Y' ~ (process_unit_heat_demand_ee * 294.071) / 1.5,
      combustion_unit_category != 'generic' & combustion_unit_electrifiable == 'Y' ~ (process_unit_heat_demand_ee * 294.071) / .99,
      combustion_unit_electrifiable == 'N' ~ 0
    )
  ) |>
  rename(naics_code = primary_naics) |>
  rename(naics_description = naics_title) |>
  mutate(
    facility_id = as.character(facility_id), 
    sector = case_when(
      str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food & Beverage",
      str_starts(naics_code, "325") ~ "Chemicals",
      str_starts(naics_code, "322") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    ), 
    industry_clean = case_when(
      naics_description == 'Beet Sugar Manufacturing' ~ 'Beet Sugar', 
      naics_description == 'Ethyl Alcohol Manufacturing' ~ 'Ethyl Alcohol', 
      naics_description == 'Fats and Oils Refining and Blending' ~ 'Fats & Oils', 
      naics_description == 'Paper Mills' ~ 'Pulp & Paper', 
      naics_description == 'Paperboard Mills' ~ 'Pulp & Paper',
      naics_description == 'Pulp Mills' ~ 'Pulp & Paper',
      naics_description == 'Soybean and Other Oilseed Processing' ~ 'Soybeans', 
      naics_description == 'Spice and Extract Manufacturing' ~ 'Spices', 
      naics_description == 'Animal (except Poultry) Slaughtering' ~ 'Meat (non-poultry)', 
      naics_description == 'Distilleries' ~ 'Distilleries', 
      naics_description == 'Wet Corn Milling and Starch Manufacturing' ~ 'Wet Corn Milling', 
      naics_description == 'Rendering and Meat Byproduct Processing' ~ 'Rendering', 
      naics_description == 'Cheese Manufacturing' ~ 'Cheese'
    )
  )

facility_info <-
  read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>%
  clean_names() %>%
  mutate(facility_id = as.character(facility_id)) |>
  select(facility_id, county_fips, subregion) 

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

# --- Merge Inputs ---
unit_combined_df <- 
  unit_input_df %>%
  left_join(facility_info, by = "facility_id") %>%
  left_join(egrid_df, by = "subregion") 

rm(egrid_df, facility_info, longform, unit_input_df)

#### COLLAPSING OPERATIONS ####

## This makes a facility-level dataset that can be used with old figure code, basically
## Except: we're splitting out boiler electrification from full electrification 
facility_df <-
  unit_combined_df |>
  # Expand for different technology scenarios 
  crossing(tech_scenario = c('ng_boiler', 'ng_full', 'eb_boiler', 'eb_full', 'hp_boiler', 'hp_full')) |>
  # Expand for  best & worst case
  crossing(scenario_rank = c('best', 'worst')) |>
  group_by(facility_id, tech_scenario, scenario_rank) |>
  # Grouping by facility-scenario 
  summarize(
    # Main info vars 
    facility_name = first(facility_name), 
    naics_code = first(naics_code), 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    # Capex. Varies by technology, best/worst, and boiler vs full electrification
    capex = case_when(
      tech_scenario == 'ng_boiler' & scenario_rank == 'best' ~ 
        sum(ng_capex_low[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'ng_full'   & scenario_rank == 'best' ~ 
        sum(ng_capex_low, na.rm = TRUE),
      
      tech_scenario == 'ng_boiler' & scenario_rank == 'worst' ~ 
        sum(ng_capex_high[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'ng_full'   & scenario_rank == 'worst' ~ 
        sum(ng_capex_high, na.rm = TRUE),
      
      tech_scenario == 'eb_boiler' & scenario_rank == 'best' ~ 
        sum(eb_capex_low[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'eb_full'   & scenario_rank == 'best' ~ 
        sum(eb_capex_low, na.rm = TRUE),
      
      tech_scenario == 'eb_boiler' & scenario_rank == 'worst' ~ 
        sum(eb_capex_high[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'eb_full'   & scenario_rank == 'worst' ~ 
        sum(eb_capex_high, na.rm = TRUE),
      
      tech_scenario == 'hp_boiler' & scenario_rank == 'best' ~ 
        sum(hp_capex_low[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'hp_full'   & scenario_rank == 'best' ~ 
        sum(hp_capex_low, na.rm = TRUE),
      
      tech_scenario == 'hp_boiler' & scenario_rank == 'worst' ~ 
        sum(hp_capex_high[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'hp_full'   & scenario_rank == 'worst' ~ 
        sum(hp_capex_high, na.rm = TRUE)
    ),
    
    # Change in electricity demand 
    change_in_electricity_demand_kwh = case_when(
      str_detect(tech_scenario, 'ng') ~ 0,
      
      tech_scenario == 'eb_boiler' ~ 
        sum(eb_electricity_demand_kwh[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'eb_full' ~ 
        sum(eb_electricity_demand_kwh, na.rm = TRUE),
      
      tech_scenario == 'hp_boiler' & scenario_rank == 'best' ~ 
        sum(hp_electricity_demand_kwh_low[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'hp_full'   & scenario_rank == 'best' ~ 
        sum(hp_electricity_demand_kwh_low, na.rm = TRUE),
      
      tech_scenario == 'hp_boiler' & scenario_rank == 'worst' ~ 
        sum(hp_electricity_demand_kwh_high[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'hp_full'   & scenario_rank == 'worst' ~ 
        sum(hp_electricity_demand_kwh_high, na.rm = TRUE)
    ),
    
    # Heat demand. Only varies by boiler vs full electrification
    heat_mmbtu = case_when(
      tech_scenario == 'ng_boiler' ~ sum(process_unit_heat_demand[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'ng_full'   ~ sum(process_unit_heat_demand, na.rm = TRUE),
      
      tech_scenario == 'eb_boiler' ~ sum(process_unit_heat_demand_ee[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'eb_full'   ~ sum(process_unit_heat_demand_ee, na.rm = TRUE),
      
      tech_scenario == 'hp_boiler' ~ sum(process_unit_heat_demand_ee[combustion_unit_category == 'generic'], na.rm = TRUE),
      tech_scenario == 'hp_full'   ~ sum(process_unit_heat_demand_ee, na.rm = TRUE)
    ),
    
    # Adding up emissions 
    base_emissions_co2e = sum(ghg_quantity, na.rm = TRUE), 
    elec_ghg_emissions = sum(ghg_quantity[combustion_unit_electrifiable == 'Y' & is_biogenic == FALSE], na.rm = TRUE), 
    biogenic_ghg_emissions = sum(ghg_quantity[is_biogenic == TRUE], na.rm = TRUE), 
    noelec_ghg_emissions = sum(ghg_quantity[combustion_unit_electrifiable == 'N'], na.rm = TRUE),
    
    # Other info vars
    county_fips = first(county_fips), 
    subregion = first(subregion), 
    .groups = "drop"
  ) |>
  distinct(facility_id, tech_scenario, scenario_rank, .keep_all = TRUE)


# unit_df

# process_df 

#### GRAPHICS SET-UP ####

## Graphics settings
sector_colors <- c(
  "Pulp & Paper" = "#6d7d33",
  "Beet Sugar" = "#ef5645",
  "Ethyl Alcohol" = "#2CA02C", 
  "Fats & Oils" = "#047c91", 
  "Soybeans" = "#c9bf9d", 
  "Spices" = "#9370DB", 
  "Meat (non-poultry)" = "#8B0000", 
  "Distilleries" = "#D2691E", 
  "Rendering" = "#8C564B",  
  "Wet Corn Milling"    = "#febc11", 
  'Cheese' = "#FFD95B"
)

tech_colors <- c(
  "ASHP"      = "#CC79A7", 
  "E-Boiler"  = "#56B4E9",
  "NG Boiler" = "#4D4D4D"  
)

scenario_colors <- c(
  "Baseline"   = "#fb9a99",
  "Scenario1"  = "#1f78b4",
  "Scenario2"  = "#33a02c",
  "Scenario3"  = "#a6cee3",
  "Scenario4"  = "#b2df8a"
)

scenario_labels <- c(
  "Baseline"   = "Baseline",
  "Scenario1"  = "1: E-Boiler",
  "Scenario2"  = "2: ASHP",
  "Scenario3"  = "3: E-Boiler + EE",
  "Scenario4"  = "4: ASHP + EE"
)

#####