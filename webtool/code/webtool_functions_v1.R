
#### Functions for 2035 Webtool: Emissions & LCOH ####
## Nate M
## 2025-09-18

#### SET-UP ####
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

#### EMISSIONS FUNCTION #### 
# This takes user inputs & facility-level data to produce a facility-level emissions estimate.  
# It does *not* apply the filtering to the dataset, which must be done beforehand. 

emissions_func <- 
  function(
    # From emissions dataset
    facility_emissions, # facility emissions
    grid_emissions_kg_kwh, # kg of emissions per kwh
    change_in_electricity_demand_kwh, # how much electricity demand changes under electrification
    
    # User input
    grid_clean_pct_scenario # What % cleaner is the grid? should take a value from 0-1
  ) {
    
    # Scale the grid emissions (kg per kwh) to be more clean (or the same, depending on what the user selects). 
    scaled_grid.e_factor <- grid_emissions_kg_kwh * grid_clean_pct_scenario
    
    
    # emissions = the increase in emissions from the grid from electricity demand + non-electrifiable emissions at the facility 
    total_emissions_t = (change_in_electricity_demand_kwh * scaled_grid.e_factor) / 1000 + facility_emissions
    
    total_emissions_t
  }

#### EMISSIONS EXAMPLE ####
# Here is an example of how this could applied in a dplyr pipeline below. 

emissions_df <- read_csv('webtool/data/webtool_emissions_data_20250925.csv')

# Say the user inputs Scenario1, 80% cleaner grid, co2e, and natural gas fuel type 
user_tech_scenario <- "Scenario1"
user_grid_clean_pct <- .8
user_pollutant_type <- "co2e"
user_fuel_type <- 'ng'

emissions_example_results <- 
  emissions_df %>%
  # Filter dataset, per user input 
  filter(
    tech_scenario == user_tech_scenario & 
    pollutant_type == user_pollutant_type &
    !!sym(paste0(user_fuel_type, "_dum")) == 1
  ) %>%
  # Use function inside mutate() to calculate emissions outcome for each observation
  mutate(
    emissions_out = 
      emissions_func(
        # dataset variables
        facility_emissions, 
        grid_emissions_kg_kwh,
        change_in_electricity_demand_kwh, 
        
        # user inputs 
        user_grid_clean_pct
      ), 
    # Now making a dummy to show if emissions went up or down with electrification (for saves emissions toggle) 
    saves_emissions = if_else(emissions_out < base_emissions, 1, 0)
  ) %>%
  # (example) group_by state/county to get the results for a pop-up box when a user clicks on a geographic area.
  # For now, emissions will display as totals. 
  group_by(state, naics_code) %>%
  summarize(
    emissions_out = sum(emissions_out, na.rm = TRUE)
  ) %>%

  mutate(

  )

# For reference -- filtering variables from the emissions dataset: 
## tech_scenario --> Electrification scenario 
## county_fips --> County
## state --> State 
## naics_code / sector --> Sector 
## pollutant_type --> co2e, nox, so2, pm25 
## fuel type --> use fuel type dummies 

#### LCOH FUNCTION ####

# This takes user inputs & facility-level data to produce a facility-level "levelized cost of heat" (LCOH) estimate.
# Works same as emissions function (see above comments). 

lcoh_func <- function(
  ## User inputs 
  tech_scenario, # Electrification scenario (Baseline, E-Boiler, etc.)
  capex_subsidy, # Proportion of capex subsidized by govt (should take value 0-1)
  elec_discount, # Proportion of electricity price reduction (should take value 0-1)
  
  ## Variables from the costs dataset  
  capex,                                   # Up-front equipment cost
  heat_mmbtu,                              # Quantity of heat used at the facility 
  change_in_electricity_demand_kwh,        # Change in electricity demand under electrification 
  
  ## Variables from the parameters dataset 
  r,                   # Discount factor
  elec_price,          # Electricity price 
  ng_price,            # Natural gas price 
  t,                   # Time horizon for LCOH calculation 
  ngboiler_om_best,    # Best case natgas O&M costs
  ngboiler_om_worst,   # Worst case natgas O&M costs
  eboiler_om_best,     # Best case e-boiler O&M costs
  eboiler_om_worst,    # Worst case e-boiler O&M costs
  hthp_om_best,        # Best case heat pump O&M costs
  hthp_om_worst       # Worst case heat pump O&M costs
  ){
  
  # Time discounting formula 
  discount_sum <- (1 - (1 + r)^(-t)) / r
  
  ## Inputting different parameters for different tech scenarios 
  case_when(
    
    # Baseline LCOH
    str_detect(tech_scenario, "Baseline") ~ {
      opex_ng_worst <- (heat_mmbtu / 0.75) * ng_price
      opex_ng_best  <- (heat_mmbtu / 0.90) * ng_price
      opex_ng       <- (opex_ng_worst + opex_ng_best) / 2
      
      opex_om_worst <- ngboiler_om_worst * capex
      opex_om_best  <- ngboiler_om_best  * capex
      opex_om       <- (opex_om_best + opex_om_worst) / 2
      
      numerator   <- capex + (opex_om + opex_ng) * discount_sum
      denominator <- heat_mmbtu * discount_sum
      numerator / denominator
    },
    
    # E-boiler LCOH
    str_detect(tech_scenario, "Scenario1|Scenario3") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1 - elec_discount))
      
      opex_om_worst <- eboiler_om_worst * capex
      opex_om_best  <- eboiler_om_best * capex
      opex_om       <- (opex_om_best + opex_om_worst) / 2
      
      numerator   <- capex_adj + (opex_om + opex_elec) * discount_sum
      denominator <- heat_mmbtu * discount_sum
      numerator / denominator
    },
    
    # Heat pump LCOH
    str_detect(tech_scenario, "Scenario2|Scenario4") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1 - elec_discount))
      
      opex_om_worst <- hthp_om_worst * capex
      opex_om_best  <- hthp_om_best  * capex
      opex_om       <- (opex_om_best + opex_om_worst) / 2
      
      numerator   <- capex_adj + (opex_om + opex_elec) * discount_sum
      denominator <- heat_mmbtu * discount_sum
      numerator / denominator
    }
  )
}

#### LCOH EXAMPLE #### 

lcoh_df <- read_csv('webtool/data/webtool_lcoh_data_20250918.csv')

# Hypothetical user inputs
user_tech_scenario <- 'Scenario2'
user_capex_subsidy <- .5
user_elec_discount <- .25

lcoh_example_results <- 
  lcoh_df %>%
  filter(
    tech_scenario == user_tech_scenario
  ) %>%
  mutate(
    lcoh_out = lcoh_func(
      ## User inputs
      user_tech_scenario,
      user_capex_subsidy, 
      user_elec_discount, 
      
      ## LCOH dataset variables 
      capex,
      heat_mmbtu,
      change_in_electricity_demand_kwh,
      # Parameters
      r, 
      elec_price,
      ng_price,
      t, 
      ngboiler_om_best, 
      ngboiler_om_worst, 
      eboiler_om_best, 
      eboiler_om_worst, 
      hthp_om_best, 
      hthp_om_worst
    )  
  ) %>%
  # Example: summarizing for county results. LCOH should display as average. 
  group_by(county_fips, naics_code) %>%
  summarize(
    lcoh_out = mean(lcoh_out, na.rm = TRUE),
  )

# Filtering variables on LCOH dataset: 
## tech_scenario --> Electrification scenario 
## fuels --> Fuel type at the facility 
## county_fips --> County
## state --> State 
## naics_code / sector --> Sector 
## (no need to filter by emissions type) 

  
      