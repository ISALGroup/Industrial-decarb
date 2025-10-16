##### RMI DATA WORK ####

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
tech_input_df.o <- 
  read_excel("LCOH modelling/output/copollutant_longform_national_wtemps_addedsectors_30sept.xlsx") |>
  #read_excel("LCOH modelling/output/copollutant_longform_MI.xlsx") |>
  filter(state %in% c('MI', 'IL')) |>
  select(-1, -opex) 

# TEMPORARY: natgas capex calculation
natgas_best <- 
  tech_input_df.o |>
  filter(tech_scenario == 'Baseline') |>
  mutate(tech_scenario = 'BaselineBest', 
         # best case natural gas 
         capex = 4733.79*(heat_mmbtu/49132.8)^0.8325)

tech_input_df <- 
  tech_input_df.o |>
  mutate(
    tech_scenario = if_else(tech_scenario == 'Baseline', 'BaselineWorst', tech_scenario),
    # worst case natural gas
    capex = if_else(tech_scenario == 'BaselineWorst', 25761.09*(heat_mmbtu/54592)^0.8325, capex)
  ) |>
  bind_rows(natgas_best) |>
  mutate(#facility_name = tolower(facility_name), 
    facility_id = as.character(facility_id),
    base_emissions_co2e = elec_ghg_emissions + noelec_ghg_emissions
    
    # scenario_rank = str_extract(tech_scenario, "(Best|Worst)$"),
    # tech_scenario = str_remove(tech_scenario, "(Best|Worst)$") 
  ) |> 
  rename(
    base_emissions_nox = `base_emissions_Nitrogen Oxides`,
    base_emissions_pm25 = `base_emissions_PM2.5 Primary (Filt + Cond)`, 
    base_emissions_so2 = `base_emissions_Sulfur Dioxide`
  )

# # Pull in lat and long from rlps file
# facility_lat_long <-
#   read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") |>
#   select(facility_id, latitude, longitude) |>
#   distinct(facility_id, .keep_all = TRUE)

# Pull in facility info, this file doesn't have lat and long... (maybe add in the future)
facility_info <-
  read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) |>
  clean_names() |>
  rename(naics_code = primary_naics) |>
  rename(naics_description = naics_title) |>
  select(facility_id, naics_code, naics_description, county_fips, subregion) |>
  #inner_join(facility_lat_long, by = "facility_id") |>
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

# eGRID data
egrid_df <-
  read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 2) |>
  clean_names() |>
  mutate(co2e_kg_kwh = co2e * 0.000453592) |>
  mutate(ch4_kg_kwh = ch4 * 0.000453592) |>
  mutate(n2o_kg_kwh = n2o * 0.000453592) |>
  mutate(nox_kg_kwh = annual_n_ox * 0.000453592) |>
  mutate(so2_kg_kwh = so2 * 0.000453592) |>
  mutate(pm25_kg_kwh = pm_2_5 * 0.000453592) |>
  rename(subregion = e_grid_subregion_acronym) |>
  select(subregion, co2e_kg_kwh, nox_kg_kwh, so2_kg_kwh, pm25_kg_kwh)

# --- Merge Inputs ---
tech_combined_df <- 
  tech_input_df |>
  left_join(facility_info, by = "facility_id") |>
  left_join(egrid_df, by = "subregion") 

rm(egrid_df, facility_info, natgas_best, tech_input_df, tech_input_df.o)

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

#### LCOH DATA WORK & SET-UP ####
lcoh_func <- function(
    ## parameters
  r, 
  elec_price_low,
  elec_price_high,
  ng_price,
  t, # for now, assume same lifetime across equipment, but we can change this by technology later 
  # nat gas boiler assumptions
  #t_ngboiler,
  ngboiler_om_low, 
  ngboiler_om_high, 
  # e-boiler assumptions
  #t_eboiler, 
  eboiler_om_low, 
  eboiler_om_high, 
  # hp assumptions
  #t_hthp, 
  hthp_om_low, 
  hthp_om_high, 
  
  ## tech scenario + calculations 
  tech_scenario,
  capex,
  heat_mmbtu,
  change_in_electricity_demand_kwh,
  
  ## policy scenarios
  capex_subsidy, 
  elec_discount){
  # time discounting formula 
  discount_sum <- sum((1 + r)^-(1:t))
  
  ## Inputting different parameters for different tech scenarios 
  case_when(
    str_detect(tech_scenario, "BaselineWorst") ~ {
      opex_ng <- (heat_mmbtu/.75) * ng_price      # energy costs
      opex_om <- ngboiler_om_high * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "BaselineBest") ~ {
      opex_ng <- (heat_mmbtu/.9) * ng_price       # energy costs
      opex_om <- ngboiler_om_low * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario1Best|Scenario3Best") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price_low * (1-elec_discount))
      opex_om <- eboiler_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario1Worst|Scenario3Worst") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price_high * (1-elec_discount))
      opex_om <- eboiler_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario2Best|Scenario4Best") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price_low * (1-elec_discount))
      opex_om <- hthp_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario2Worst|Scenario4Worst") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price_high * (1-elec_discount))
      opex_om <- hthp_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }
  )
}

# Import parameters 
param <- 
  read_csv('state_fact_sheets/data/parameters.csv') |>
  filter(state %in% c('IL', 'MI'))

# --- Create Policy Grid ---
policy_grid <- expand.grid(
  capex_subsidy = seq(0.0, 1.0, by = 0.1),
  elec_discount = seq(0.0, 1.0, by = 0.25)
)

# --- Apply Policy Grid to Tech Scenarios ---
facility_lcoh_df <- 
  tidyr::crossing(tech_combined_df, policy_grid) |>
  mutate(
    lcoh = lcoh_func(
      param$r, 
      param$elec_price_low,
      param$elec_price_high,
      param$ng_price,
      param$t, 
      param$ngboiler_om_low, 
      param$ngboiler_om_high, 
      param$eboiler_om_low, 
      param$eboiler_om_high, 
      param$hthp_om_low, 
      param$hthp_om_high, 
      ## tech scenario + calculations 
      tech_scenario,
      capex,
      heat_mmbtu,
      change_in_electricity_demand_kwh,
      ## policy scenarios
      capex_subsidy, 
      elec_discount 
    ), 
    
    policy_label = paste0("Capex: -", capex_subsidy * 100, "%, Elec: -", elec_discount * 100, "%"), 
    policy_label = if_else(
      policy_label == 'Capex: -0%, Elec: -0%', 'No Policy', policy_label
    )) #|>
#mutate(industry_clean = factor(industry_clean, levels = order_levels))

ng_min <- min(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineBest'])
ng_max <- max(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineWorst'])

#### RMI LCOH V ELECTRICITY DATA ####
x_vals <- seq(0, 0.15, length.out = 200)

elec_plot_df_mi <- 
  facility_lcoh_df |>
  # Filter to no policy support, Scenario4 outcomes, pulp & paper
  filter(str_detect(tech_scenario, "Scenario4"), 
         policy_label == 'No Policy', 
         industry_clean == 'Pulp & Paper', 
         state == 'MI') |>
  
  # Summarize at the scenario level. 
  group_by(#sector, 
    tech_scenario) |>
  summarize(
    #sector = min(sector), 
    capex = mean(capex), 
    change_in_electricity_demand_kwh = mean(change_in_electricity_demand_kwh), 
    heat_mmbtu = mean(heat_mmbtu), 
    tech_scenario = min(tech_scenario),
    capex_subsidy = mean(capex_subsidy), # will be zero
    elec_discount = mean(elec_discount), # will be zero
  ) |>
  ungroup() |>
  tidyr::crossing(x = x_vals) |>
  mutate(
    lcoh = lcoh_func(
      param$r, 
      # We provide x for high and low electricity price, since we're just making a prediction
      x,
      x,
      param$ng_price,
      param$t, 
      param$ngboiler_om_low, 
      param$ngboiler_om_high, 
      param$eboiler_om_low, 
      param$eboiler_om_high, 
      param$hthp_om_low, 
      param$hthp_om_high, 
      ## tech scenario + calculations 
      tech_scenario,
      capex,
      heat_mmbtu,
      change_in_electricity_demand_kwh,
      ## policy scenarios
      capex_subsidy, 
      elec_discount 
    ),
    x = x*100, 
    scenario_rank = str_extract(tech_scenario, "(Best|Worst)$")
  ) |>
  select(x, scenario_rank, lcoh) |>
  rename(lcoh_mi = lcoh)

ng_min_mi <- 
  facility_lcoh_df |>
  filter(state == "MI", tech_scenario == "BaselineBest") |>
  summarise(val = min(lcoh, na.rm = TRUE)) |>
  pull(val)

ng_max_mi <- 
  facility_lcoh_df |>
  filter(state == "MI", tech_scenario == "BaselineWorst") |>
  summarise(val = max(lcoh, na.rm = TRUE)) |>
  pull(val)
# Need to re-do the import before making this 
elec_plot_df_il <- 
  facility_lcoh_df |>
  # Filter to no policy support, Scenario4 outcomes, pulp & paper
  filter(str_detect(tech_scenario, "Scenario4"), 
         policy_label == 'No Policy', 
         industry_clean == 'Wet Corn Milling', 
         state == 'IL') |>
  # Summarize at the scenario level. 
  group_by(#sector, 
    tech_scenario) |>
  summarize(
    #sector = min(sector), 
    capex = mean(capex), 
    change_in_electricity_demand_kwh = mean(change_in_electricity_demand_kwh), 
    heat_mmbtu = mean(heat_mmbtu), 
    tech_scenario = min(tech_scenario),
    capex_subsidy = mean(capex_subsidy), # will be zero
    elec_discount = mean(elec_discount), # will be zero
  ) |>
  ungroup() |>
  tidyr::crossing(x = x_vals) |>
  mutate(
    lcoh = lcoh_func(
      param$r, 
      x,
      x,
      param$ng_price,
      param$t, 
      param$ngboiler_om_low, 
      param$ngboiler_om_high, 
      param$eboiler_om_low, 
      param$eboiler_om_high, 
      param$hthp_om_low, 
      param$hthp_om_high, 
      ## tech scenario + calculations 
      tech_scenario,
      capex,
      heat_mmbtu,
      change_in_electricity_demand_kwh,
      ## policy scenarios
      capex_subsidy, 
      elec_discount 
    ),
    x = x*100, 
    scenario_rank = str_extract(tech_scenario, "(Best|Worst)$")
  ) |>
  select(x, scenario_rank, lcoh) |>
  rename(lcoh_il = lcoh)

ng_min_il <- 
  facility_lcoh_df |>
  filter(state == "IL", tech_scenario == "BaselineBest") |>
  summarise(val = min(lcoh, na.rm = TRUE)) |>
  pull(val)

ng_max_il <- 
  facility_lcoh_df |>
  filter(state == "IL", tech_scenario == "BaselineWorst") |>
  summarise(val = max(lcoh, na.rm = TRUE)) |>
  pull(val)

elec_plot_df <- 
  left_join(elec_plot_df_mi, elec_plot_df_il, by = c('x', 'scenario_rank'))

write_csv(elec_plot_df, 'state_fact_sheets/outputs/RMI Materials/elec_plot_data.csv')

param_rmi <- 
  read_csv('state_fact_sheets/data/parameters.csv') |>
  filter(state %in% c('IL', 'MI')) |>
  mutate(
    ng_min = case_when(
      state == 'IL' ~ ng_min_il,
      state == 'MI' ~ ng_min_mi
    ), 
    ng_max = case_when(
      state == 'IL' ~ ng_max_il, 
      state == 'MI' ~ ng_max_mi
    )
  )

write_csv(param_rmi, 'state_fact_sheets/outputs/RMI Materials/rmi_parameters.csv')

## See rmi_figure_code.R for the actual figures 

