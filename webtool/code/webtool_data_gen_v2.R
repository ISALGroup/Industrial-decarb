#### WEBTOOL DATA OUTPUT #### 

# Version 1
# NAM
## 2025-09-25

# Version notes:
# Adds fuel type dummies
# Consolidates into one dataset, for the "in the money results" 

#### SET-UP ####
# Load Libraries
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(glue)

# Tech scenario input 
tech_input_df.o <- 
  read_excel("LCOH modelling/output/copollutant_longform_national.xlsx") %>%
  select(-1, -opex) 

natgas_best <- 
  tech_input_df.o %>%
  filter(tech_scenario == 'Baseline') %>%
  mutate(tech_scenario = 'BaselineBest', 
         # best case natural gas 
         capex = 4733.79*(heat_mmbtu/54592)^0.8325)

tech_input_df <- 
  tech_input_df.o |>
  mutate(
    tech_scenario = if_else(tech_scenario == 'Baseline', 'BaselineWorst', tech_scenario),
    # worst case natural gas
    capex = if_else(tech_scenario == 'BaselineWorst', 25761.09*(heat_mmbtu/54592)^0.8325, capex)
  ) %>%
  bind_rows(natgas_best) %>%
  mutate(#facility_name = tolower(facility_name), 
         facility_id = as.character(facility_id),
         base_emissions_co2e = elec_ghg_emissions + noelec_ghg_emissions,
         
         scenario_rank = str_extract(tech_scenario, "(Best|Worst)$"),
         tech_scenario = str_remove(tech_scenario, "(Best|Worst)$") 
         
         ) %>% 
  rename(
    base_emissions_nox = `base_emissions_Nitrogen Oxides`,
    base_emissions_pm25 = `base_emissions_PM2.5 Primary (Filt + Cond)`, 
    base_emissions_so2 = `base_emissions_Sulfur Dioxide`
  ) %>%
  
  # AVERAGING THE FUNCTION *INPUTS* ACROSS BEST & WORST CASE
  group_by(facility_id, tech_scenario) %>%
  summarize(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    across(where(is.character), \(x) first(x)),
    .groups = "drop"
  ) %>%
  select(-scenario_rank) %>%
  # Adding some dummies for fuel type 
  mutate(
    ng_dum = if_else(str_detect(fuels, 'Natural Gas'), 1, 0), 
    fuel_oils_dum = if_else(str_detect(fuels, 'Fuel Oil'), 1, 0), 
    coal_dum = if_else(fuels %in% c('Bituminous', 'Subbituminous', 'Anthracite', 
                                    'Coal Coke', 'Mixed (Industrial sector)', 'Lignite'), 
                       1, 0), 
    byproducts_dum = if_else(fuels %in% c('Wood and Wood Residuals (dry basis)', 'Solid Byproducts', 
                            'Agricultural Byproducts', 'Rendered Animal Fat', 'Vegetable Oil'), 
                            1, 0), 
    propane_dum = if_else(str_detect(fuels, 'Propane'), 1, 0),
    other_dum = if_else(
      !str_detect(fuels, "Natural Gas|Fuel Oil|Bituminous|Subbituminous|Anthracite|Coal Coke|Mixed \\(Industrial sector\\)|Lignite|Wood and Wood Residuals|Solid Byproducts|Agricultural Byproducts|Rendered Animal Fat|Vegetable Oil|Propane"),
      1, 0), 
    
    across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x))
  )

# Pull in lat and long from rlps file
facility_lat_long <- 
  read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>% 
  select(facility_id, latitude, longitude) %>% 
  distinct(facility_id, .keep_all = TRUE)

# Pull in facility info, this file doesn't have lat and long... (maybe add in the future)
facility_info <- 
  read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>% 
  clean_names() %>% 
  rename(naics_code = primary_naics) %>% 
  rename(naics_description = naics_title) %>% 
  select(facility_id, naics_code, naics_description, county_fips, subregion) %>% 
  inner_join(facility_lat_long, by = "facility_id") %>% 
  mutate(
    sector = case_when(
      str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food & Beverage",
      str_starts(naics_code, "325") ~ "Chemicals",
      str_starts(naics_code, "322") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    ), 
    facility_id = as.character(facility_id)
  )

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

param <- read_csv('state_fact_sheets/data/parameters.csv')

# --- Merge Inputs ---
tech_combined_df <- 
  tech_input_df %>%
  left_join(facility_info, by = "facility_id") %>%
  left_join(egrid_df, by = "subregion") %>%
  left_join(param, by = c('state'))

#### BASELINE LCOH FUNCTION ####
base_lcoh_func <- function(
  ## Variables from the costs dataset  
  capex,                                   # Up-front equipment cost
  heat_mmbtu,                              # Quantity of heat used at the facility 
  
  ## Variables from the parameters dataset 
  r,                   # Discount factor
  ng_price,          # Natural gas price 
  t,                   # Time horizon for LCOH calculation 
  ngboiler_om_best,    # Best case natgas O&M costs
  ngboiler_om_worst   # Worst case natgas O&M costs
){
  
  # Time discounting formula 
  discount_sum <- (1 - (1 + r)^(-t)) / r
  
  opex_ng_worst <- (heat_mmbtu / 0.75) * ng_price
  opex_ng_best  <- (heat_mmbtu / 0.90) * ng_price
  opex_ng       <- (opex_ng_worst + opex_ng_best) / 2
  
  opex_om_worst <- ngboiler_om_worst * capex
  opex_om_best  <- ngboiler_om_best  * capex
  opex_om       <- (opex_om_best + opex_om_worst) / 2
  
  numerator   <- capex + (opex_om + opex_ng) * discount_sum
  denominator <- heat_mmbtu * discount_sum
  numerator / denominator
}

#### Create & Save Datasets #### 

webtool_df <- 
  tech_combined_df |>
  # Drawing pollutant type into a long format column for filtering by the webtool team. 
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
      pollutant_type == 'co2e' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_co2e, 
      pollutant_type == 'nox' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_nox, 
      pollutant_type == 'so2' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_so2, 
      pollutant_type == 'pm25' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_pm25,
      
      pollutant_type == 'co2e' & str_detect(tech_scenario, 'Scenario') ~ noelec_ghg_emissions, 
      pollutant_type == 'nox' & str_detect(tech_scenario, 'Scenario') ~ nox_emissions, 
      pollutant_type == 'so2' & str_detect(tech_scenario, 'Scenario')  ~ so2_emissions, 
      pollutant_type == 'pm25' & str_detect(tech_scenario, 'Scenario') ~ pm25_emissions
    ), 
    # base LCOH
    base_lcoh = 
      base_lcoh_func(
        ## LCOH dataset variables 
        capex,
        heat_mmbtu,
        # Parameters
        r, 
        ng_price,
        t, 
        ngboiler_om_best, 
        ngboiler_om_worst
      )  
  ) %>%
  select(-contains('base_emissions_'), -elec_ghg_emissions, -noelec_ghg_emissions, 
         -nox_emissions, -so2_emissions, -pm25_emissions, -contains('match'), -nei_id, -fuel_reduction)

write_csv(webtool_df, glue("webtool/data/webtool_data_{format(Sys.Date(), '%Y%m%d')}.csv")) 

  