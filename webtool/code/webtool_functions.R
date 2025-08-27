tech_combined_df <- read_excel('file_path')

library(glue)

#### EMISSIONS FUNCTION #### 
# This takes user inputs & facility-level data to produce a facility-level emissions estimate 
# Data can then be averaged by sector, technology scenario, and/or state to produce the visual layer

emissions_func <- 
  function(# User Inputs 
           grid_clean_pct, 
           pollutant, #co2e, nox, so2, or pm25
           emissions_range, 
           total_or_relative, 
           
           # From facility data 
           current_fossil_share, 
           noelec_ghg_emissions,
           change_in_electricity_demand_kwh) {
      
    # Scale grid emissions based on grid mix already present in tech_df
    scaled_co2e_factor <- co2e_kg_kwh * ((1 - grid_clean_pct) / current_fossil_share)
    
    scaled_nox_factor <- nox_kg_kwh * ((1 - grid_clean_pct) / current_fossil_share)
    
    scaled_so2_factor <- so2_kg_kwh * ((1 - grid_clean_pct) / current_fossil_share)
    
    scaled_pm25_factor <- pm25_kg_kwh * ((1 - grid_clean_pct) / current_fossil_share)
  
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
  
  bind_rows(facility_emissions_baseline, facility_emissions_no_baseline) %>%
    mutate(
      clean_grid_scenario_label = if_else(
        is.na(grid_clean_pct_num),
        "Current Grid Mix",
        paste0(round(grid_clean_pct_num * 100), "% Clean Grid")
      )
    ) %>%
    select(
      facility_id, facility_name, state, county_fips, latitude, longitude,
      naics_code, naics_description, sector, subregion, tech_scenario, heat_mmbtu,
      change_in_electricity_demand_kwh, current_fossil_share, current_clean_share,
      grid_clean_pct_scenario, baseline_co2e_emissions, elec_ghg_emissions, noelec_ghg_emissions,
      emissions_total_t_co2e, emissions_change_kg_nox, emissions_change_kg_so2,
      emissions_change_kg_pm25, clean_grid_scenario_label
    )
}
