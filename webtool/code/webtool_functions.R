
#### SET-UP ####
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(glue)

tech_combined_df <- read_excel('file_path')


#### EMISSIONS FUNCTION #### 
# This takes user inputs & facility-level data to produce a facility-level emissions estimate 
# Data can then be averaged by sector, technology scenario, and/or state to produce the visual layer

emissions_func <- 
  function(df, 
           
           # User Inputs 
           grid_clean_pct, 
           pollutant, #co2e, nox, so2, or pm25
           tech_scenario
           #total_or_relative
           ) {
      
    in_col  <- paste0(pollutant, "_kg_kwh")

    df <- 
      df %>%
      # Scale grid emissions based on grid mix already present in tech_df
      mutate(
        scaled_emissions_factor := .data[[in_col]] * ((1 - grid_clean_pct) / current_fossil_share)
      ) %>%
      
      # calculate change in 
    
  
  # Non-baseline
  facility_emissions_no_baseline <- facility_grid_df %>%
    filter(tech_scenario != "Baseline") %>%
    mutate(
      emissions_total_t_co2e = (change_in_electricity_demand_kwh * scaled_co2e_factor) / 1000 + noelec_ghg_emissions,
      emissions_change_kg_nox = change_in_electricity_demand_kwh * scaled_nox_factor,
      emissions_change_kg_so2 = change_in_electricity_demand_kwh * scaled_so2_factor,
      emissions_change_kg_pm25 = change_in_electricity_demand_kwh * scaled_pm25_factor
    )
  
  # Baseline
  facility_emissions_baseline <- facility_grid_df %>%
    filter(str_detect(tech_scenario, "Baseline")) %>%
    mutate(
      emissions_total_t_co2e = baseline_co2e_emissions,
      emissions_change_kg_nox = 0,
      emissions_change_kg_so2 = 0,
      emissions_change_kg_pm25 = 0
    )
  }

#### COST FUNCTION ####

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