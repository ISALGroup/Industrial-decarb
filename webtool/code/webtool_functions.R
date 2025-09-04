
#### SET-UP ####
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(glue)

tech_combined_df <- read_excel('file_path')

#### EMISSIONS FUNCTION #### 
# This takes user inputs & facility-level data to produce a facility-level emissions estimate.  

# You could apply this inside a dplyr pipe, e.g.:
## 1. Filter dataset, per user input 
## 2. Use function inside mutate() to calculate emissions outcome for each observation
## 3a. Sum across all observations to get the total emissions for a given scenario (top bar). 
## 3b. Group_by state/county to get the results for a pop-up box when a user clicks on a geographic area.

# Filtering variables on the emissions dataset: 
## tech_scenario --> Electrification scenario 
## fuels --> Fuel type at the facility 
## county_fips --> County
## state --> State 
## naics_code / sector --> Sector 
## pollutant_type --> co2e, nox, so2, pm25 

emissions_func <- 
  function(# From Emissions Dataset
           grid_emissions_kg_kwh, # kg of emissions per kwh
           change_in_electricity_demand_kwh, # how much electricity demand changes under electrification
           
           # User Inputs 
           tech_scenario, # Which electrification scenario are we looking at? (Baseline, E-Boiler, etc.)
           grid_clean_pct, # What % cleaner is the grid? should take a value from 0-1
           pollutant_type # For now, let's only do CO2e 
           ) {

    # Scale the grid emissions (kg per kwh) to be more clean (or the same, depending on what the user selects). 
    scaled_grid.e_factor <- grid_emissions_kg_kwh * (1-grid_clean_pct)
    
    case_when(
      # When pollutant is CO2e & we are looking at an electrification scenario... 
      pollutant_type == 'co2e' & tech_scenario != "Baseline" ~ {
        # emissions = the increase in emissions from the grid from electricity demand + non-electrifiable emissions at the facility 
        emissions_total_t = (change_in_electricity_demand_kwh * scaled_grid.e_factor) / 1000 + noelec_ghg_emissions
        
        emissions_total_t
      }, 
      
      # When pollutant is not CO2e... (ignore for now) 
      tech_scenario != "Baseline" ~ {
        emissions_change_kg = change_in_electricity_demand_kwh * scaled_grid.e_factor
        
        emissions_change_kg
      }, 
      
      # When we are looking at the baseline scenario, just return the baseline emissions from GHGRP. 
      tech_scenario == "Baseline" ~ {
        baseline_co2e_emissions
      })
  }


#### COST FUNCTION ####

# This takes user inputs & facility-level data to produce a facility-level "levelized cost of heat" (LCOH) estimate.  


lcoh_func <- function(
  ## User inputs 
  tech_scenario,
  capex_subsidy, 
  elec_discount,
  
  ## Variables from the facility-level dataset  
  capex,
  heat_mmbtu,
  change_in_electricity_demand_kwh,
  
  ## Variables from the parameters dataset 
  r, 
  elec_price,
  ng_price,
  t, # for now, assume same lifetime across equipment, but we can change this by technology later 
  # nat gas boiler assumptions
  #t_ngboiler,
  ngboiler_om_best, 
  ngboiler_om_worst, 
  # e-boiler assumptions
  #t_eboiler, 
  eboiler_om_best, 
  eboiler_om_worst, 
  # hp assumptions
  #t_hthp, 
  hthp_om_best, 
  hthp_om_worst
  ){
  # time discounting formula 
  discount_sum <- sum((1 + r)^-(1:t))
  
  ## Inputting different parameters for different tech scenarios 
  case_when(
    str_detect(tech_scenario, "Baseline") ~ {
      opex_ng_worst <- (heat_mmbtu/.75) * ng_price      # energy costs
      opex_om_worst <- ngboiler_om_worst * capex    # o&m costs
      
      opex_ng_best <- (heat_mmbtu/.9) * ng_price       # energy costs
      opex_om_best <- ngboiler_om_best * capex    # o&m costs
      
      numerator_worst <- capex + ((opex_om_worst + opex_ng_worst) * discount_sum)
      numerator_best <- capex + ((opex_om_best + opex_ng_best) * discount_sum)
      
      denominator <- heat_mmbtu * discount_sum
      
      mean((numerator_worst/denominator), (numerator_best/denominator))
    }, 
    str_detect(tech_scenario, "Scenario1|Scenario3") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      
      opex_om_worst <- eboiler_om_worst * capex
      opex_om_best <- eboiler_om_best * capex
      
      numerator_worst <- capex_adj + ((opex_om_worst + opex_elec) * discount_sum)
      numerator_best <- capex_adj + ((opex_om_best + opex_elec) * discount_sum)
      
      denominator <- heat_mmbtu * discount_sum
      
      mean((numerator_worst/denominator), (numerator_best/denominator))
    }, 
    str_detect(tech_scenario, "Scenario2|Scenario4") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      
      opex_om_worst <- hthp_om_worst * capex
      opex_om_best <- hthp_om_best * capex
      
      numerator_worst <- capex_adj + ((opex_om_worst + opex_elec) * discount_sum)
      numerator_best <- capex_adj + ((opex_om_best + opex_elec) * discount_sum)
      
      denominator <- heat_mmbtu * discount_sum
      
      mean((numerator_worst/denominator), (numerator_best/denominator))
    }
  )
}