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
library(ggforce)
library(ggraph)
library(patchwork)
library(purrr)

# Tech scenario input 
longform <- 
  read_excel("LCOH modelling/output/fixed_long_form_data_2.xlsx") |>
  select(-c(`plant type`, is_redundant, auxiliary)) |>
  bind_rows(
    read_excel("LCOH modelling/output/longform_heat_demand.xlsx") |>
      select(-c(has_chem_recovery, has_lime_kiln, `plant type`, is_redundant, process_unit)) |>
      rename(process_unit = individual_process_unit)
  ) |>
  select(-1)

ee_parameters <- 
  read_excel('national_results/data/EE Parameters.xlsx') |>
  mutate(naics_sector = as.character(naics_sector))

unit_input_df <- 
  longform |>
  mutate(naics_sector = substr(primary_naics, 1, 3)) |>
  left_join(ee_parameters, by = 'naics_sector') |>
  # Name & value clean-up 
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
      naics_description == 'Cheese Manufacturing' ~ 'Cheese', 
      naics_description == 'Fluid Milk Manufacturing' ~ 'Milk', 
      naics_description == 'Dry, Condensed, and Evaporated Dairy Product Manufacturing' ~ 'Other Dairy', 
      naics_description == 'Breweries' ~ 'Breweries', 
      naics_description == 'Sanitary Paper Product Manufacturing' ~ 'Toilet Paper', 
      naics_description == 'Synthetic Rubber Manufacturing' ~ 'Rubber', 
      naics_description == 'Nitrogenous Fertilizer Manufacturing' ~ 'Fertilizers', 
      naics_description == 'Phosphatic Fertilizer Manufacturing' ~ 'Fertilizers'
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

## Graphics settings
subsector_colors <- c(
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

sector_colors <- c(
  "Pulp & Paper" = "#6d7d33",
  "Chemicals" = "#047c91", 
  "Food & Beverage"= "#febc11"
)

tech_colors <- c(
  "Air-Source HP"      = "#CC79A7", 
  "E-Boiler"           = "#56B4E9",
  "NG Boiler"          = "#4D4D4D"  
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

#### UNIT DATA ####
# --- Merge Inputs ---
unit_combined_df <- 
  unit_input_df %>%
  left_join(facility_info, by = "facility_id") %>%
  left_join(egrid_df, by = "subregion") 

rm(egrid_df, facility_info, unit_input_df)

# Making a massive unit-scenario level dataset
unit_scenario_df <-
  unit_combined_df |>
  # Expand for different technology scenarios 
  crossing(tech_scenario = c('ng_boiler', 'ng_boiler_ee', 'ng_full', 
                             'eb_boiler', 'eb_boiler_ee', 'eb_full', 'eb_full_ee',  
                             'hp_boiler','hp_boiler_ee', 'hp_full', 'hp_full_ee')) |>
  # Expand for  best & worst case
  crossing(scenario_rank = c('best', 'worst')) |>
  
  # Calculate heat demand, electricity, and capex for different scenarios 
  mutate(
    # Calculating reductions in steam demand w/ buckets 1 & 2 of the energy efficiency measures. 
    process_unit_heat_demand_ee = case_when(
      combustion_unit_category == 'generic' & str_detect(tech_scenario, 'ee') ~ 
        process_unit_heat_demand * (1 - (bucket1_pct + bucket2_pct)), 
      TRUE ~ process_unit_heat_demand
    ),
    
    unit_electricity_demand_kwh = case_when(
      # generic (boiler) units 
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler', 'eb_full') ~
        (process_unit_heat_demand * 294.071) / 0.99,
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler_ee', 'eb_full_ee') ~
        (process_unit_heat_demand_ee * 294.071) / 0.99,
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler','hp_full') & scenario_rank == 'best' ~
        (process_unit_heat_demand * 294.071) / 3,
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler_ee','hp_full_ee') & scenario_rank == 'best' ~
        (process_unit_heat_demand_ee * 294.071) / 3,
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler','hp_full') & scenario_rank == 'worst' ~
        (process_unit_heat_demand * 294.071) / 1.5,
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler_ee','hp_full_ee') & scenario_rank == 'worst' ~
        (process_unit_heat_demand_ee * 294.071) / 1.5,
      
      # In "full" scenarios, non-combustion units electrify too
      combustion_unit_category != 'generic' & combustion_unit_electrifiable == 'Y' & 
        tech_scenario %in% c('eb_full', 'hp_full') ~
        (process_unit_heat_demand * 294.071) / 0.99,
      ## In "EE" scenarios, the electricity demand of these units goes down 
      combustion_unit_category != 'generic' & combustion_unit_electrifiable == 'Y' &
        tech_scenario %in% c('eb_full_ee', 'hp_full_ee') ~
        ((process_unit_heat_demand * 294.071) / 0.99) * (1 - bucket3_pct),
      
      combustion_unit_electrifiable == 'N' ~ 0,
      TRUE ~ 0
    ),
    
    # Capex for the actual unit. Account for EE here because it makes the unit smaller 
    unit_capex = case_when(
      combustion_unit_category == 'generic' & tech_scenario %in% c('ng_boiler', 'ng_full') & scenario_rank == 'best' ~ 
        24573.01928*(process_unit_heat_demand/49132.8)^0.8431, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('ng_boiler', 'ng_full') & scenario_rank == 'worst' ~ 
        294796.2541*(process_unit_heat_demand/40944)^0.8431, 
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('ng_boiler_ee', 'ng_full_ee') & scenario_rank == 'best' ~ 
        24573.01928*(process_unit_heat_demand_ee/49132.8)^0.8431, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('ng_boiler_ee', 'ng_full_ee') & scenario_rank == 'worst' ~ 
        294796.2541*(process_unit_heat_demand_ee/40944)^0.8431, 
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler', 'eb_full')  & scenario_rank == 'best' ~ 
        24573.01928*(process_unit_heat_demand/49132.8)^0.8431, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler', 'eb_full')  & scenario_rank == 'worst' ~ 
        294796.2541*(process_unit_heat_demand/40944)^0.8431, 
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler_ee', 'eb_full_ee') & scenario_rank == 'best' ~ 
        24573.01928*(process_unit_heat_demand_ee/49132.8)^0.8431, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler_ee', 'eb_full_ee')  & scenario_rank == 'worst' ~ 
        294796.2541*(process_unit_heat_demand_ee/40944)^0.8431, 
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler', 'hp_full')  & scenario_rank == 'best' ~ 
        24573.01928*((process_unit_heat_demand/49132.8)^0.8431)*2, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler', 'hp_full')  & scenario_rank == 'worst' ~ 
        294796.2541*((process_unit_heat_demand/40944)^0.8431)*2, 
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler_ee', 'hp_full_ee') & scenario_rank == 'best' ~ 
        24573.01928*((process_unit_heat_demand_ee/49132.8)^0.8431)*2, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler_ee', 'hp_full_ee') & scenario_rank == 'worst' ~ 
        294796.2541*((process_unit_heat_demand_ee/40944)^0.8431)*2, 
      
      TRUE ~ 0
    ), 
    
    # Cost of EE upgrades
    ee_capex = case_when(
      ## EE Capex for generic units = mmbtus saved with energy efficiency * cost per mmbtu, for each bucket 
      combustion_unit_category == 'generic' & str_detect(tech_scenario, 'ee') ~ 
        (process_unit_heat_demand * bucket1_pct * bucket1_cost) + (process_unit_heat_demand * bucket2_pct * bucket2_cost),
      
      ## EE Capex for non-generic units = base kwh * % kwh saved * cost per kwh
      combustion_unit_category != 'generic' & combustion_unit_electrifiable == 'Y' & str_detect(tech_scenario, 'ee') ~ 
        (process_unit_heat_demand * 294.071 / .99) * bucket3_pct * bucket3_cost,
      
      ## Non-electrifiable units / non-EE scenarios have no EE capex 
      TRUE ~ 0
    )
  )

#### FACILITY DATA ####
# This makes a facility-level dataset that can be used with old figure code, basically

# getting facility emissions from unit_combined, so as not to dupe rows w/ different scenarios 
facility_emissions_df <- 
  unit_combined_df |>
  group_by(facility_id) |>
  summarize(
    base_emissions_co2e = sum(ghg_quantity, na.rm = TRUE),
    elec_ghg_emissions = sum(if_else(combustion_unit_electrifiable == 'Y' & !is_biogenic, ghg_quantity, 0), na.rm = TRUE),
    biogenic_ghg_emissions = sum(if_else(is_biogenic, ghg_quantity, 0), na.rm = TRUE),
    noelec_ghg_emissions = sum(if_else(combustion_unit_electrifiable == 'N', ghg_quantity, 0), na.rm = TRUE),
  )

# Calculate full & boiler scenarios separately
facility_full_df <- 
  unit_scenario_df |>
  filter(combustion_unit_category %in% c('generic', 'specific (incl)'), 
         str_detect(tech_scenario, 'full')) |>
  # Grouping by facility-scenario
  group_by(facility_id, tech_scenario, scenario_rank) |>
  summarize(
    # Main info vars 
    facility_name = first(facility_name), 
    naics_code = first(naics_code), 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    nonboiler_elec_unit_dum = any(
      combustion_unit_category != "generic" & combustion_unit_electrifiable == "Y",
      na.rm = TRUE
    ),
    
    capex = sum(
      case_when(
        !str_detect(tech_scenario, 'ee') ~ unit_capex, 
        str_detect(tech_scenario, 'ee') ~ unit_capex + ee_capex,
        TRUE ~ 0
      ), 
      na.rm = TRUE
    ),
    
    # The original (pre-energy efficiency) heat demand 
    heat_mmbtu_orig = sum(process_unit_heat_demand, na.rm = TRUE),
    # Heat demand w/ ee 
    heat_mmbtu_ee = sum(
      case_when(
        ## no energy efficiency for ng, non-EE scenarios
        !str_detect(tech_scenario, 'ee') ~ process_unit_heat_demand,
        str_detect(tech_scenario, 'ee') ~ process_unit_heat_demand_ee, 
        TRUE ~ 0
      ), 
      na.rm = TRUE
    ),
    
    # Change in electricity demand 
    ## EE is accounted for already in unit_electricity_demand_kwh calculation 
    change_in_electricity_demand_kwh = sum(unit_electricity_demand_kwh, na.rm = T),
    
    # Other info vars
    county_fips = first(county_fips), 
    subregion = first(subregion), 
    .groups = "drop"
  ) 


facility_boiler_df <-
  unit_scenario_df |>
  filter(combustion_unit_category == 'generic', 
         str_detect(tech_scenario, 'boiler')) |>
  # Grouping by facility-scenario
  group_by(facility_id, tech_scenario, scenario_rank) |>
  summarize(
    # Main info vars
    facility_name = first(facility_name),
    naics_code = first(naics_code),
    industry_clean = first(industry_clean),
    sector = first(sector),
    state = first(state),

    nonboiler_elec_unit_dum = any(
      combustion_unit_category != "generic" & combustion_unit_electrifiable == "Y",
      na.rm = TRUE
    ),

    capex = sum(
      case_when(
        !str_detect(tech_scenario, 'ee') ~ unit_capex,
        str_detect(tech_scenario, 'ee') ~ unit_capex + ee_capex,
        TRUE ~ 0
      ),
      na.rm = TRUE
    ),

    # The original (pre-energy efficiency) heat demand
    heat_mmbtu_orig = sum(process_unit_heat_demand, na.rm = TRUE),

    # Heat demand w/ ee
    heat_mmbtu_ee = sum(
      case_when(
        ## no energy efficiency for ng, non-EE scenarios
        !str_detect(tech_scenario, 'ee') ~ process_unit_heat_demand,
        str_detect(tech_scenario, 'ee') ~ process_unit_heat_demand_ee,
        TRUE ~ 0
      ),
      na.rm = TRUE
    ),

    # Change in electricity demand
    ## EE is accounted for already in unit_electricity_demand_kwh calculation
    change_in_electricity_demand_kwh = sum(unit_electricity_demand_kwh, na.rm = T),

    # Other info vars
    county_fips = first(county_fips),
    subregion = first(subregion),
    .groups = "drop"
  )


facility_scenario_df <-
  bind_rows(facility_full_df, facility_boiler_df) |>
  # bring in emissions
  left_join(facility_emissions_df, by = 'facility_id') |>
  arrange(facility_id, tech_scenario, scenario_rank)

#### LCOH DATA WORK & SET-UP ####
lcoh_func <- function(
  ## parameters
  r, 
  elec_price,
  #elec_price_high,
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
  scenario_rank,
  capex,
  heat_mmbtu_ee,
  heat_mmbtu_orig,
  change_in_electricity_demand_kwh,
  
  ## policy scenarios
  capex_subsidy, 
  elec_discount){
  # time discounting formula 
  discount_sum <- sum((1 + r)^-(1:t))
  
  ## Inputting different parameters for different tech scenarios 
  ## Note: for non-ee scenarios, heat_mmbtu_ee in numerator will be the same as heat_mmbtu_orig. For ee, it's less.   
  case_when(
    str_detect(tech_scenario, 'ng') & scenario_rank == 'worst' ~ {
      opex_ng <- (heat_mmbtu_ee/.75) * ng_price      # energy costs. ng_boiler_ee has lower heat demand 
      opex_om <- ngboiler_om_high * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum 
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, 'ng') & scenario_rank == 'best' ~ {
      opex_ng <- (heat_mmbtu_ee/.9) * ng_price       # energy costs
      opex_om <- ngboiler_om_low * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum
      
      numerator / denominator
    }, 
    
    str_detect(tech_scenario, 'eb') & scenario_rank == 'worst' ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      # EE measures already accounted for in change_in_electricity_demand_kwh
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- eboiler_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, 'eb')  & scenario_rank == 'best' ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- eboiler_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum
      
      numerator / denominator
    }, 

    str_detect(tech_scenario, 'hp')  & scenario_rank == 'worst' ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- hthp_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum
      
      numerator / denominator
    },
    str_detect(tech_scenario, 'hp') & scenario_rank == 'best' ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- hthp_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum
      
      numerator / denominator
    }
  )
}

# Import parameters 
param <- 
  read_csv('state_fact_sheets/data/parameters.csv') 

# --- Create Policy Grid ---
policy_grid <- expand.grid(
  capex_subsidy = seq(0.0, 1.0, by = 0.1),
  elec_discount = seq(0.0, 1.0, by = 0.25)
)

# --- Apply Policy Grid to Tech Scenarios ---
facility_lcoh_df <- 
  tidyr::crossing(facility_scenario_df, policy_grid) %>%
  left_join(param, by = 'state') |>
  mutate(
    lcoh = lcoh_func(
      param$r[1], # need to pass r & t as scalars 
      elec_price,
      #elec_price_high,
      ng_price,
      param$t[1], 
      ngboiler_om_low, 
      ngboiler_om_high, 
      eboiler_om_low, 
      eboiler_om_high, 
      hthp_om_low, 
      hthp_om_high, 
      ## tech scenario + calculations 
      tech_scenario,
      scenario_rank,
      capex,
      heat_mmbtu_ee,
      heat_mmbtu_orig,
      change_in_electricity_demand_kwh,
      ## policy scenarios
      capex_subsidy, 
      elec_discount 
    ), 
    
    policy_label = case_when(
      elec_discount > 0 & capex_subsidy > 0 ~ 
        paste0("Capex: -", capex_subsidy * 100, "%, Elec: -", elec_discount * 100, "%"),
      elec_discount == 0 & capex_subsidy > 0 ~
        paste0("Capex: -", capex_subsidy * 100, "%"),
      elec_discount > 0 & capex_subsidy == 0 ~
        paste0("Elec: -", capex_subsidy * 100, "%"),
      elec_discount == 0 & capex_subsidy == 0 ~ 
        'No Policy' 
    )
  )|>
  select(-any_of(setdiff(names(param), "state")))

#### FIG: LCOH BY TECH SCENARIO ####
ng_boiler_min <-
  facility_lcoh_df |>
  filter(tech_scenario == "ng_boiler", scenario_rank == "best") |>
  summarise(min(lcoh, na.rm = TRUE)) |>
  pull()

ng_boiler_max <-
  facility_lcoh_df |>
  filter(tech_scenario == "ng_boiler", scenario_rank == "worst") |>
  summarise(max(lcoh, na.rm = TRUE)) |>
  pull()

tech_lcoh_data <- 
  facility_lcoh_df %>%
  filter(
    policy_label == "No Policy",
    #nonboiler_elec_unit_dum == F,
    # drop non-EE 
    str_detect(tech_scenario, 'ee')  
  ) %>%
  mutate(
    scenario_group = factor(
      case_when(
        str_detect(tech_scenario, 'boiler') ~ 'Steam Network', 
        str_detect(tech_scenario, 'full') ~ 'Full Electrification'
      ), 
      levels = c('Steam Network', 'Full Electrification')
    ),
    scenario_label = factor(
      tech_scenario,
      levels = c('ng_boiler_ee', 'eb_boiler_ee', 'hp_boiler_ee', 'eb_full_ee', 'hp_full_ee'),
      labels = c("EE Only", "E-Boiler", "Air-Source HP", "E-Boiler", "Air-Source HP")
    ))

# Technology scenario x sector plot 
tech_lcoh_plot <- 
  ggplot(tech_lcoh_data,
         aes(x = scenario_label, y = lcoh, color = scenario_group)) +
  
  # # Natural gas range display 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_boiler_min, ymax = ng_boiler_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_boiler_min, linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = ng_boiler_max, linetype = "dotted", color = "black", size = 0.5) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 30)) +
  
  labs(
    x = NULL,
    y = "Levelized Cost of Heat ($/MMBtu)",
    color = "Scenario"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.83, 0.32),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

tech_lcoh_plot
  
#### FIG: LCOH BY TECH SCENARIO V2 ####
tech_lcoh_data_2 <- 
  facility_lcoh_df %>%
  filter(
    policy_label == "No Policy"            
  ) %>%
  mutate(
    scenario_group = factor(
      tech_scenario,
      levels = c('ng_boiler', 'ng_boiler_ee','ng_full', 
                 'eb_boiler', 'eb_boiler_ee', 'eb_full', 'eb_full_ee',
                 'hp_boiler', 'hp_boiler_ee', 'hp_full', 'hp_full_ee'), 
      labels = c("Baseline", "Baseline", "Baseline", 
                 "E-Boiler", "E-Boiler", "E-Boiler", "E-Boiler",
                 "Air-Source HP", "Air-Source HP", "Air-Source HP", "Air-Source HP")
    ), 
    scenario_label = factor(
      case_when(
        str_detect(tech_scenario, 'boiler') & !str_detect(tech_scenario, 'ee') ~ 'Boiler', 
        str_detect(tech_scenario, 'boiler') & str_detect(tech_scenario, 'ee') ~ 'Boiler + EE', 
        str_detect(tech_scenario, 'full') & !str_detect(tech_scenario, 'ee') ~ 'Facility', 
        str_detect(tech_scenario, 'full') & str_detect(tech_scenario, 'ee') ~ 'Facility + EE', 
      ), 
      levels = c('Boiler', 'Boiler + EE', 'Facility', 'Facility + EE')
      )
    )

# Technology scenario x sector plot 
tech_lcoh_plot_2 <- 
  ggplot(tech_lcoh_data_2,
         aes(x = scenario_group, y = lcoh, color = scenario_label)) +
    
  # # # Natural gas range display 
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_full_min, ymax = ng_full_max,
  #          fill = "grey90", alpha = 0.3) +
  # geom_hline(yintercept = ng_boiler_min, linetype = "dotted", color = "black", size = 0.5) +
  # geom_hline(yintercept = ng_boiler_max, linetype = "dotted", color = "black", size = 0.5) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 30)) +
  
  labs(
    x = NULL,
    y = "Levelized Cost of Heat ($/MMBtu)",
    color = "Scenario"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.83, 0.32),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

tech_lcoh_plot_2

#### FIG: DELTA LCOH PLOT ####

baseline_lcoh <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario %in% c('ng_boiler', 'ng_full')) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_ng = mean(lcoh, na.rm = T)
  ) |>
  ungroup() |>
  mutate(facility_scenario = str_extract(tech_scenario, "(?<=_).*")) |>
  select(-tech_scenario)

delta_lcoh_data <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario %in% c('eb_boiler_ee', 'eb_full_ee', 'hp_boiler_ee', 'hp_full_ee') 
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    # Main info vars 
    facility_name = first(facility_name), 
    naics_code = first(naics_code), 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    lcoh = mean(lcoh, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    facility_scenario = str_extract(tech_scenario, "(?<=_).*"),
    facility_scenario = str_remove(facility_scenario, "_ee"), 
    tech_scenario = str_extract(tech_scenario, "^[^_]+")
  ) |>
  left_join(baseline_lcoh, by = c('facility_id', 'facility_scenario')) |>
  mutate(
    lcoh_delta = lcoh - lcoh_ng, 
    tech_scenario = factor(
      tech_scenario,
      levels = c('eb', 'hp'), 
      labels = c("E-Boiler", "Air-Source HP")
    ), 
    facility_scenario = factor(
      facility_scenario, 
      levels = c('boiler', 'full'), 
      labels = c('Boiler', 'Facility')
    )
  ) |>
  # let's just look at boiler outcomes for now
  filter(
    facility_scenario == 'Boiler'
  )

# Technology scenario x sector plot 
delta_lcoh_plot   <- 
  ggplot(delta_lcoh_data,
         aes(x = sector, y = lcoh_delta, color = tech_scenario)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = tech_colors) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  #scale_y_continuous(limits = c(5, 30)) +
  
  labs(
    x = NULL,
    y = "Δ Levelized Cost of Heat ($/MMBtu)",
    color = "Technology:", 
    shape = "Electrifying:"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.83, 0.32),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

delta_lcoh_plot


#### FIG: DELTA LCOH BUBBLE PLOT ####
baseline_bubble <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'ng_full', 
         scenario_rank == 'worst'
         ) |>
  rename(lcoh_ng = lcoh) |>
  select(facility_id, lcoh_ng)

delta_bubble_data_good <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario %in% c('hp_full_ee') , 
    scenario_rank == 'best'
  ) |>
  left_join(
    baseline_bubble, by = 'facility_id'
  ) |>
  mutate(lcoh_delta = lcoh - lcoh_ng) |>
  group_by(state, industry_clean) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    # UNWEIGHTED lcoh delta
    lcoh_delta = mean(lcoh_delta, na.rm = T), 
    base_emissions_co2e = sum(base_emissions_co2e, na.rm = T)
  ) |>
  ungroup() |>
  mutate(name = paste(state, industry_clean, sep = " "), 
         label = str_wrap(name, width = 10)) |>
  select(name, label, lcoh_delta, base_emissions_co2e) |>
  slice_min(order_by = lcoh_delta, n = 15) |>
  tbl_graph(nodes = _, edges = NULL) 

delta_bubble_data_bad <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario %in% c('hp_full_ee') , 
    scenario_rank == 'best'
  ) |>
  left_join(
    baseline_bubble, by = 'facility_id'
  ) |>
  mutate(lcoh_delta = lcoh - lcoh_ng) |>
  group_by(state, industry_clean) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    # UNWEIGHTED lcoh delta
    lcoh_delta = mean(lcoh_delta, na.rm = T), 
    base_emissions_co2e = sum(base_emissions_co2e, na.rm = T)
  ) |>
  ungroup() |>
  mutate(name = paste(state, industry_clean, sep = " "), 
         label = str_wrap(name, width = 10)) |>
  select(name, label, lcoh_delta, base_emissions_co2e) |>
  slice_max(order_by = lcoh_delta, n = 15) |>
  tbl_graph(nodes = _, edges = NULL) 

# --- compute shared limits across both graphs ---
fill_lims <- range(c(delta_bubble_data_good %N>% pull(lcoh_delta),
                     delta_bubble_data_bad %N>% pull(lcoh_delta)), na.rm = TRUE)
size_lims <- range(c(delta_bubble_data_good %N>% pull(base_emissions_co2e),
                     delta_bubble_data_bad %N>% pull(base_emissions_co2e)), na.rm = TRUE)

# --- shared scales ---
fill_scale <- scale_fill_gradient2(
  limits = fill_lims,
  low = "#1f78b4", mid = "white", high = "#fb9a99", midpoint = 0,
  name = "Δ LCOH ($/mmbtu)"
)
size_scale <- scale_size_continuous(
  limits = size_lims,
  range = c(3, 15),
  name = "Base CO₂e emissions",
  guide = "none"     
)

# --- small helper function ---
base_plot <- function(graph, title, show_legend = TRUE) {
  ggraph(graph, layout = "circlepack", weight = base_emissions_co2e) +
    geom_node_circle(
      aes(fill = lcoh_delta, size = base_emissions_co2e),
      color = "grey40", linewidth = 0.3, alpha = 0.9
    ) +
    geom_node_text(
      aes(label = label), size = 3, color = "black",
      hjust = 0.5, vjust = 0.5, check_overlap = TRUE
    ) +
    fill_scale + size_scale +
    ggtitle(title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = if (show_legend) "right" else "none"
    ) +
    coord_equal()
}

p1 <- base_plot(delta_bubble_data_good, "Most Competitive")
p2 <- base_plot(delta_bubble_data_bad, "Least Competitive")


(p1 + p2) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right")


# edges <- 
#   delta_bubble_data |> 
#   mutate(node = paste(state, industry_clean, sep = "_")) |> 
#   select(from = sector, to = node)
# 
# # Combine with unique nodes (so sectors appear as roots)
# nodes <- 
#   tibble(name = unique(c(edges$from, edges$to))) |> 
#   left_join(delta_bubble_data |> mutate(name = paste(state, industry_clean, sep = "_")), by = "name")
# 
# graph <- tbl_graph(nodes = nodes, edges = edges)

# ggraph(graph, layout = "circlepack", weight = base_emissions_co2e) +
#   geom_node_circle(aes(fill = delta_lcoh, r = size), color = "grey30", alpha = 0.8) +
#   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
#   geom_node_text(aes(label = name), repel = TRUE, size = 3) +
#   theme_void()

#### FIG: STATE EMISSIONS "IN THE MONEY" ####
fig_policies <- c(
  "Total Emissions", 
  "No Policy",
  "Capex: -100%, Elec: -0%", 
  "Capex: -0%, Elec: -25%", 
  "Capex: -0%, Elec: -50%"
)

fig_states <- c(
  'CA', 'CO', 'FL', 'IA', 'IL', 
  'LA', 'MD', 'ME', 'MI', 'MN', 
  'NY', 'OH', 'OR', 'TX', 'WA', 'WI'
)


lcoh_tech_base <- 
  facility_lcoh_df %>%
  filter(
    policy_label == "No Policy" &
    tech_scenario == 'ng_full' &
    scenario_rank == 'best'
  ) %>%
  select(facility_id, lcoh)

eim_df <- 
  facility_lcoh_df %>%
  filter(
    tech_scenario == 'hp_full_ee' &
    scenario_rank == 'best' &
    policy_label %in% fig_policies & 
    state %in% fig_states
  ) %>%
  left_join(
    lcoh_tech_base |> rename(lcoh_ng = lcoh),
    by = 'facility_id'
  ) %>%
  mutate(
    in_money = if_else(lcoh < lcoh_ng, 1, 0), 
    policy_label = factor(policy_label, levels = fig_policies),
    state = factor(state, levels = rev(sort(unique(as.character(state)))))
  ) %>%
  group_by(state, policy_label) %>%
  summarize(
    eim = sum(elec_ghg_emissions[in_money == 0], #+ 
                # noelec_ghg_emissions[in_money == 0] +
                # biogenic_ghg_emissions[in_money == 0],
              na.rm = TRUE),
    total_co2e_ghg = sum(base_emissions_co2e, na.rm = TRUE),
    total_elec_ghg = sum(elec_ghg_emissions, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  # ✅ Drop consecutive duplicates within each state
  group_by(state) %>%
  group_modify(~ {
    total_val <- mean(.x$total_elec_ghg, na.rm = TRUE)
    bind_rows(.x, tibble(state = unique(.x$state),
                         policy_label = "Total Emissions",
                         eim = total_val))
  }) %>%
  arrange(desc(state), factor(policy_label, levels = fig_policies)) %>%
  filter(eim != lag(eim) | is.na(lag(eim))) %>%
  ungroup() %>%
  mutate(
    eim_Mt = eim / 1e6,
    eim_prop = (eim / total_elec_ghg) * 100, 
    policy_label = factor(policy_label, levels = fig_policies)
  ) 

eim_plot <- 
  ggplot() +
  
  # geom_col(data = eim_df,
  #          aes(x = total_elec_ghg, y = state)) +
  
  geom_col(data = eim_df,
           aes(x = eim, y = state, fill = policy_label),
           position = position_dodge(width = 0.5), 
           width = 0.2) +

  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  scale_fill_manual(
    values = c(
      "Total Emissions" = "grey30",
      "No Policy" = "#1b9e77",
      "Capex: -100%, Elec: -0%" = "#d95f02",
      "Capex: -0%, Elec: -25%" = "#7570b3", 
      "Capex: -0%, Elec: -50%" = "lightblue"
      
    ),
    breaks = c(
      "Total Emissions",
      "No Policy",
      "Capex: -100%, Elec: -0%",
      "Capex: -0%, Elec: -25%", 
      "Capex: -0%, Elec: -50%"
    ),
    labels = c(
      "Total Emissions",
      "No Policy",
      "Capex: -100%, Elec: -0%",
      "Capex: -0%, Elec: -25%", 
      "Capex: -0%, Elec: -50%"
    )
  ) +
  
  labs(x = "Emissions (MtCO2e)", y = NULL, fill = "Policy" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.7, 0.95),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

eim_plot

#### FIG: PAYBACK ####

fig_statesubs <- 
  c(
    'WA Pulp & Paper', 
    'NY Spices', 
    'PA Ethyl Alcohol', 
    'MD Rendering' 
  )

fig_policies <- 
  c(
    "Elec -50%", 
    "Elec -25%", 
    "Capex -100%", 
    "Capex -50%", 
    "No Policy"
  )

fig_state_colors <- list(
  "WA Pulp & Paper" = c(
    "Elec -50%"   = "#E1EAC6",
    "Elec -25%"   = "#ACBF7C",
    "Capex -100%" = "#8DA452",
    "Capex -50%"  = "#6D7D33",
    "No Policy"    = "#3B471C"
  ),
  "NY Spices" = c(
    "Elec -50%"   = "#FDE6E0",
    "Elec -25%"   = "#F8B3A4",
    "Capex -100%" = "#F27D63",
    "Capex -50%"  = "#EF5645",
    "No Policy"    = "#9B2C24"
  ),
  "MD Rendering" = c(
    "Elec -50%"   = "#FDE6E0",
    "Elec -25%"   = "#F8B3A4",
    "Capex -100%" = "#F27D63",
    "Capex -50%"  = "#EF5645",
    "No Policy"    = "#9B2C24"
  ),
  "PA Ethyl Alcohol" = c(
    "Elec -50%"   = "#D4EEF3",
    "Elec -25%"   = "#A0D7E0",
    "Capex -100%" = "#5EBAC3",
    "Capex -50%"  = "#09847A",
    "No Policy"    = "#064E58"
  )
)


policy_grid <- 
  expand.grid(
    capex_subsidy = c(0, .5, 1),
    elec_discount = c(0, .25, .5)
  )

payback_data_ng <- 
  facility_scenario_df |>
  filter(
    tech_scenario == 'ng_full' & 
      scenario_rank == 'best'
  ) |>
  left_join(param, by = 'state') |>
  mutate(
    opex_ng = ((heat_mmbtu_orig/.9) * ng_price) + (ngboiler_om_low * capex)
  ) |>
  select(facility_id, opex_ng)

payback_data_hp <- 
  facility_scenario_df |>
  crossing(policy_grid) |>
  mutate(
    policy_label = case_when(
      capex_subsidy == 0 & elec_discount == 0 ~ 'No Policy', 
      capex_subsidy == .5 & elec_discount == 0 ~ "Capex -50%",
      capex_subsidy == 1 & elec_discount == 0 ~ "Capex -100%",
      capex_subsidy == 0 & elec_discount == .25 ~ "Elec -25%",
      capex_subsidy == 0 & elec_discount == .5 ~ "Elec -50%",
      TRUE ~ 'NA'
    )
  )|>
  filter(
    tech_scenario == 'hp_full_ee' & 
      scenario_rank == 'best' & 
      policy_label %in% fig_policies
  ) |>
  left_join(param, by = 'state') |>
  mutate(
    opex_hp = (change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))) + (hthp_om_low * capex)
  )

payback_data <- 
  left_join(payback_data_hp, payback_data_ng, by = 'facility_id') |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    payback_years = if_else(annual_savings > 0, capex / annual_savings, NA_real_)
  ) |>
  group_by(state, industry_clean, policy_label) |>
  summarize(
    capex = mean(capex, na.rm = T),
    opex_ng = mean(opex_ng, na.rm = T), 
    opex_hp = mean(opex_hp, na.rm = T), 
    annual_savings = mean(annual_savings, na.rm = T),
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    statesub = paste(state, industry_clean, sep = " "), 
    policy_label = factor(policy_label, levels = fig_policies)
  ) |>
  filter(statesub %in% fig_statesubs) |>
  arrange(statesub, factor(policy_label, levels = fig_policies)) |>
  group_by(statesub) |>
  mutate(radius = row_number()) |>
  ungroup()


#payback_fig <- 
plots <- map(fig_statesubs, function(s) {
  ggplot(payback_data |> filter(statesub == s)) +
    geom_link(aes(x = radius, xend = radius,
                  y = 0, yend = 1),
              size = 5, lineend = "round", color = "grey95") +
    geom_link(aes(x = radius, xend = radius,
                  y = 0, yend = payback_years),
              size = 5, lineend = "round", color = "grey20") +
    geom_link(aes(x = radius, xend = radius,
                  y = 0, yend = payback_years, color = policy_label),
              size = 4, lineend = "round") +
    geom_label(aes(radius, y = 1, 
                   label = paste0(policy_label, ": ", round(payback_years, 2), "yrs"),
                   hjust = 1.1), size = 2.5) +
    scale_x_continuous(limits = c(0, 5)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = fig_state_colors[[s]]) +
    guides(color = "none") +
    coord_polar(theta = "y") +
    theme_void() +
    theme(
      plot.margin = margin(10, 10, 10, 10)  # top, right, bottom, left (in "pt" by default)
    ) +
    ggtitle(s)
})

# Combine into 2×2 grid
(plots[[1]] | plots[[2]]) /
(plots[[3]] | plots[[4]])

payback_fig

#### FIG: SECTOR/POLICY LCOH FIGURE  #####

# select policy scenarios to show 
fig_policies <- c(
  'No Policy',
  'Capex: -50%, Elec: -0%', 
  'Capex: -100%, Elec: -0%', 
  'Capex: -0%, Elec: -25%', 
  'Capex: -0%, Elec: -50%', 
  'Capex: -50%, Elec: -50%', 
  'Capex: -100%, Elec: -50%'
)

sector_policy_lcoh_data <- 
  facility_lcoh_df |>
  filter(str_detect(tech_scenario, "hp_full_ee"), 
         policy_label %in% fig_policies) |>
  mutate(
    policy_label = factor(policy_label, levels = fig_policies)
  ) 

# Policy scenario x sector plot (S4 only) 
sector_policy_lcoh_plot <- 
  ggplot(sector_policy_lcoh_data,
         aes(x = policy_label, y = lcoh, color = sector)) +
  
  # # NG baseline band
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_min, ymax = ng_max,
  #          fill = "grey90", alpha = 0.3) +
  # geom_hline(yintercept = ng_min, linetype = "dotted", color = "black", size = 0.5) +
  # geom_hline(yintercept = ng_max, linetype = "dotted", color = "black", size = 0.5) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  
  scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(0, 21)) +
  
  labs(
    y = "Levelized Cost of Heat ($/MMBtu)",
    x = NULL,
    color = "Sector"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = c(0.75, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

sector_policy_lcoh_plot

#### FIG: SUBSECTOR LCOH FIGURE  #####
subsector_lcoh_data <- 
  facility_lcoh_df |>
  filter(str_detect(tech_scenario, "hp_full_ee"))

# Policy scenario x sector plot (S4 only) 
subsector_lcoh_plot <- 
  ggplot(subsector_lcoh_data,
         aes(x = industry_clean, y = lcoh)) +
  
  # # NG baseline band
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_min, ymax = ng_max,
  #          fill = "grey90", alpha = 0.3) +
  # geom_hline(yintercept = ng_min, linetype = "dotted", color = "black", size = 0.5) +
  # geom_hline(yintercept = ng_max, linetype = "dotted", color = "black", size = 0.5) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  
  scale_color_manual(values = subsector_colors) +
  #scale_y_continuous(limits = c(0, 21)) +
  
  labs(
    y = "Levelized Cost of Heat ($/MMBtu)",
    x = NULL,
    color = "Sector"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = c(0.75, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

subsector_lcoh_plot

#### FIG: ABATEMENT COST CURVE ####
fig_policies <- 
  c(
    "Elec -50%", 
    "Elec -25%", 
    "Capex -100%", 
    "Capex -50%", 
    "No Policy"
  )

acc_data <- 
  facility_lcoh_df |>
  filter(tech_scenario %in% c('ng_full', 'hp_full_ee') & 
         scenario_rank == "best" & 
         policy_label %in% fig_policies) |>
  group_by(industry_clean, technology_scenario)
  select(facility_id, state, industry_clean, policy_label,
         lcoh, lcoh_ng, base_emissions_co2e, elec_ghg_emissions, noelec_ghg_emissions) |>
  mutate(
    emissions_ng = base_emissions_co2e,
    emissions_hp = elec_ghg_emissions + noelec_ghg_emissions,
    abatement = emissions_ng - emissions_hp,  # tCO2e reduction
    delta_cost = (lcoh - lcoh_ng) * heat_demand_mmbtu,  # $/yr difference
    abatement_cost = delta_cost / abatement
  ) |>
  filter(abatement > 0)  # keep feasible reductions

acc_curve <- acc_data |>
  arrange(abatement_cost) |>
  mutate(
    cumulative_abatement = cumsum(abatement) / 1e6,  # in MtCO2e
    abatement_cost = pmin(abatement_cost, 1000)      # cap for plotting readability
  )

ggplot(acc_curve, aes(x = cumulative_abatement, y = abatement_cost,
                      fill = industry_clean)) +
  geom_col(width = 0.9, color = "grey60") +
  scale_y_continuous("Abatement cost ($/tCO₂e)", expand = c(0, 0)) +
  scale_x_continuous("Cumulative abatement potential (MtCO₂e)", expand = c(0, 0)) +
  facet_wrap(~ policy_label, ncol = 2) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
#### FIG: COP VS. SPARK GAP ####



#### FIG: CAPEX PLOT ####
capex_kw_data <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy') |>
  mutate(
    heat_kw = heat_mmbtu * 293.071 / 8000,
    
    scenario_group = factor(
      tech_scenario,
      levels = c('ng_boiler', 'ng_boiler_ee','ng_full', 
                 'eb_boiler', 'eb_boiler_ee', 'eb_full', 'eb_full_ee',
                 'hp_boiler', 'hp_boiler_ee', 'hp_full', 'hp_full_ee'), 
      labels = c("Baseline", "Baseline", "Baseline", 
                 "E-Boiler", "E-Boiler", "E-Boiler", "E-Boiler",
                 "Air-Source HP", "Air-Source HP", "Air-Source HP", "Air-Source HP")
    ), 
    scenario_label = factor(
      case_when(
        str_detect(tech_scenario, 'boiler') & !str_detect(tech_scenario, 'ee') ~ 'Boiler', 
        str_detect(tech_scenario, 'boiler') & str_detect(tech_scenario, 'ee') ~ 'Boiler + EE', 
        str_detect(tech_scenario, 'full') & !str_detect(tech_scenario, 'ee') ~ 'Facility', 
        str_detect(tech_scenario, 'full') & str_detect(tech_scenario, 'ee') ~ 'Facility + EE', 
      ), 
      levels = c('Boiler', 'Boiler + EE', 'Facility', 'Facility + EE')
    ), 
    
    scenario_rank = str_extract(tech_scenario, "(Best|Worst)$"),
    tech_scenario = str_remove(tech_scenario, "(Best|Worst)$"), 
    technology = case_when(
      tech_scenario == 'Baseline' ~ 'NG Boiler', 
      tech_scenario %in% c('Scenario1', 'Scenario3') ~ 'E-Boiler', 
      tech_scenario %in% c('Scenario2', 'Scenario4') ~ 'ASHP'
    ), 
    tech_scenario = factor(tech_scenario, levels = c("NG Boiler", "E-Boiler", "ASHP")), 
    capex_per_kw = capex / heat_kw
  ) |>
  select(facility_id, state, industry_clean, capex_per_kw, technology, tech_scenario, scenario_rank)

# Capex Plot 
capex_kw_plot <-
  ggplot(capex_kw_plot, aes(x = technology, y = capex_per_kw, fill = technology)) +
  geom_boxplot(
    outlier.shape = 21,       # filled circle with outline
    outlier.size  = 1.8,
    outlier.stroke = 0.25,
    outlier.alpha = 0.8, 
    width = 0.6,
    position = position_dodge2(width = 1, preserve = "single", reverse = TRUE)
  ) +
  
  scale_fill_manual(
    values = tech_colors,
    breaks = c("NG Boiler", "E-Boiler", "ASHP"),
    limits = c("NG Boiler", "E-Boiler", "ASHP"),
    name = "Technology" 
  ) +
  #scale_y_continuous(limits = c(0, 90)) +
  
  labs(x = NULL, y = "CAPEX ($/kW)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = c(0.8, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  ) +
  guides(fill = "none")

capex_plot_kw

#### DATA CHECKS ####

check <- 
  unit_combined_df |>
  group_by(facility_id) |>
  summarize(
    sector = first(sector), 
    nonboiler_elec_unit_dum = max(nonboiler_elec_unit_dum, na.rm = TRUE)
  ) |>
  group_by(sector) |>
  summarize(
    n_facilities = n(),
    n_nonboiler = sum(nonboiler_elec_unit_dum),
    prop_nonboiler = mean(nonboiler_elec_unit_dum)
  )

check <-
  unit_combined_df |>
  # Expand for different technology scenarios 
  crossing(tech_scenario = c('baseline', 'eb', 'hp')) |>
  # Expand for  best & worst case
  crossing(scenario_rank = c('best', 'worst')) |>
  filter(combustion_unit_category %in% c('generic', 'specific (incl)')) |>
  select(unit_type, unit_name, combustion_unit_category, 
         tech_scenario, scenario_rank) |>
  mutate(
    unit_type = if_else(combustion_unit_category == "generic", "boiler", unit_type),
    unit_name = if_else(combustion_unit_category == "generic", "boiler", unit_name),
    capex_formula = NA, 
    efficiency = NA
  ) |>
  distinct()

#write_csv(check, '/Users/nmariano/Downloads/elec_unit_data_2.csv')

check2 <- 
  longform |>
  mutate(facility_id = as.character(facility_id)) |>
  right_join(
    tech_lcoh_data_2 |>
      filter(scenario_group != 'Baseline', 
             lcoh < 5 | lcoh > 60) |>
      select(facility_id, lcoh),
    by = c('facility_id')
  ) 

#write_csv(check2, '/Users/nmariano/Downloads/weird_lcoh_data.csv')

check3 <- 
  tech_lcoh_data_2 |>
  filter(!str_detect(tech_scenario,'ng'))

check4 <- 
  unit_scenario_df |>
  filter(facility_id == '1003993')

write_csv(check4, '/Users/nmariano/Downloads/eldorado.csv')



unit_combined_df |>
  filter(sector = pulp & paper) |>
  group_by(process_unit) |>
  summarize(
    total_demand = sum(process_unit_heat_demand) 
  )
#### DATA ARCHIVE ####

## SET-UP
# Tech scenario input 
longform <- 
  read_excel("lcoh modelling/output/fixed_long_form_data_2.xlsx") |>
  select(-c(`plant type`, is_redundant, auxiliary)) |>
  bind_rows(
    read_excel("lcoh modelling/output/longform_heat_demand.xlsx") |>
      select(-c(has_chem_recovery, has_lime_kiln, `plant type`, is_redundant, process_unit)) |>
      rename(process_unit = individual_process_unit)
  ) |>
  select(-1)

ee_parameters <- 
  read_excel('national_results/data/EE Parameters.xlsx') |>
  mutate(naics_sector = as.character(naics_sector))

unit_input_df <- 
  longform |>
  mutate(naics_sector = substr(primary_naics, 1, 3)) |>
  left_join(ee_parameters, by = 'naics_sector') |>
  # Name & value clean-up 
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
      naics_description == 'Cheese Manufacturing' ~ 'Cheese', 
      naics_description == 'Fluid Milk Manufacturing' ~ 'Milk', 
      naics_description == 'Dry, Condensed, and Evaporated Dairy Product Manufacturing' ~ 'Other Dairy', 
      naics_description == 'Breweries' ~ 'Breweries', 
      naics_description == 'Sanitary Paper Product Manufacturing' ~ 'Toilet Paper', 
      naics_description == 'Synthetic Rubber Manufacturing' ~ 'Rubber', 
      naics_description == 'Nitrogenous Fertilizer Manufacturing' ~ 'Fertilizers', 
      naics_description == 'Phosphatic Fertilizer Manufacturing' ~ 'Fertilizers'
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

## Graphics settings
subsector_colors <- c(
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

sector_colors <- c(
  "Pulp & Paper" = "#6d7d33",
  "Chemicals" = "#047c91", 
  "Food & Beverage"= "#febc11"
)

tech_colors <- c(
  "Air-Source HP"      = "#CC79A7", 
  "E-Boiler"           = "#56B4E9",
  "NG Boiler"          = "#4D4D4D"  
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

## UNIT DATA ##
# --- Merge Inputs ---
unit_combined_df <- 
  unit_input_df %>%
  left_join(facility_info, by = "facility_id") %>%
  left_join(egrid_df, by = "subregion") 

rm(egrid_df, facility_info, unit_input_df)

# Making a massive unit-scenario level dataset
unit_scenario_df <-
  unit_combined_df |>
  # Expand for different technology scenarios 
  crossing(tech_scenario = c('ng_boiler', 'ng_boiler_ee', 'ng_full', 
                             'eb_boiler', 'eb_boiler_ee', 'eb_full', 'eb_full_ee',  
                             'hp_boiler','hp_boiler_ee', 'hp_full', 'hp_full_ee')) |>
  # Expand for  best & worst case
  crossing(scenario_rank = c('best', 'worst')) |>
  
  # Calculate heat demand, electricity, and capex for different scenarios 
  mutate(
    # Calculating reductions in steam demand w/ buckets 1 & 2 of the energy efficiency measures. 
    process_unit_heat_demand_ee = case_when(
      combustion_unit_category == 'generic' & str_detect(tech_scenario, 'ee') ~ 
        process_unit_heat_demand * (1 - (bucket1_pct + bucket2_pct)), 
      TRUE ~ process_unit_heat_demand
    ),
    
    unit_electricity_demand_kwh = case_when(
      # generic (boiler) units 
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler', 'eb_full') ~
        (process_unit_heat_demand * 294.071) / 0.99,
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler_ee', 'eb_full_ee') ~
        (process_unit_heat_demand_ee * 294.071) / 0.99,
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler','hp_full') & scenario_rank == 'best' ~
        (process_unit_heat_demand * 294.071) / 3,
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler_ee','hp_full_ee') & scenario_rank == 'best' ~
        (process_unit_heat_demand_ee * 294.071) / 3,
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler','hp_full') & scenario_rank == 'worst' ~
        (process_unit_heat_demand * 294.071) / 1.5,
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler_ee','hp_full_ee') & scenario_rank == 'worst' ~
        (process_unit_heat_demand_ee * 294.071) / 1.5,
      
      # In "full" scenarios, non-combustion units electrify too
      combustion_unit_category != 'generic' & combustion_unit_electrifiable == 'Y' & 
        tech_scenario %in% c('eb_full', 'hp_full') ~
        (process_unit_heat_demand * 294.071) / 0.99,
      ## In "EE" scenarios, the electricity demand of these units goes down 
      combustion_unit_category != 'generic' & combustion_unit_electrifiable == 'Y' &
        tech_scenario %in% c('eb_full_ee', 'hp_full_ee') ~
        ((process_unit_heat_demand * 294.071) / 0.99) * (1 - bucket3_pct),
      
      combustion_unit_electrifiable == 'N' ~ 0,
      TRUE ~ 0
    ),
    
    # Capex for the actual unit. Account for EE here because it makes the unit smaller 
    unit_capex = case_when(
      combustion_unit_category == 'generic' & tech_scenario %in% c('ng_boiler', 'ng_full') & scenario_rank == 'best' ~ 
        24573.01928*(process_unit_heat_demand/49132.8)^0.8431, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('ng_boiler', 'ng_full') & scenario_rank == 'worst' ~ 
        294796.2541*(process_unit_heat_demand/40944)^0.8431, 
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('ng_boiler_ee', 'ng_full_ee') & scenario_rank == 'best' ~ 
        24573.01928*(process_unit_heat_demand_ee/49132.8)^0.8431, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('ng_boiler_ee', 'ng_full_ee') & scenario_rank == 'worst' ~ 
        294796.2541*(process_unit_heat_demand_ee/40944)^0.8431, 
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler', 'eb_full')  & scenario_rank == 'best' ~ 
        24573.01928*(process_unit_heat_demand/49132.8)^0.8431, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler', 'eb_full')  & scenario_rank == 'worst' ~ 
        294796.2541*(process_unit_heat_demand/40944)^0.8431, 
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler_ee', 'eb_full_ee') & scenario_rank == 'best' ~ 
        24573.01928*(process_unit_heat_demand_ee/49132.8)^0.8431, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('eb_boiler_ee', 'eb_full_ee')  & scenario_rank == 'worst' ~ 
        294796.2541*(process_unit_heat_demand_ee/40944)^0.8431, 
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler', 'hp_full')  & scenario_rank == 'best' ~ 
        24573.01928*((process_unit_heat_demand/49132.8)^0.8431)*2, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler', 'hp_full')  & scenario_rank == 'worst' ~ 
        294796.2541*((process_unit_heat_demand/40944)^0.8431)*2, 
      
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler_ee', 'hp_full_ee') & scenario_rank == 'best' ~ 
        24573.01928*((process_unit_heat_demand_ee/49132.8)^0.8431)*2, 
      combustion_unit_category == 'generic' & tech_scenario %in% c('hp_boiler_ee', 'hp_full_ee') & scenario_rank == 'worst' ~ 
        294796.2541*((process_unit_heat_demand_ee/40944)^0.8431)*2, 
      
      TRUE ~ 0
    ), 
    
    # Cost of EE upgrades
    ee_capex = case_when(
      ## EE Capex for generic units = MMBtus saved with energy efficiency * cost per MMBtu, for each bucket 
      combustion_unit_category == 'generic' & str_detect(tech_scenario, 'ee') ~ 
        (process_unit_heat_demand * bucket1_pct * bucket1_cost) + (process_unit_heat_demand * bucket2_pct * bucket2_cost),
      
      ## EE Capex for non-generic units = base kwh * % kwh saved * cost per kwh
      combustion_unit_category != 'generic' & combustion_unit_electrifiable == 'Y' & str_detect(tech_scenario, 'ee') ~ 
        (process_unit_heat_demand * 294.071 / .99) * bucket3_pct * bucket3_cost,
      
      ## Non-electrifiable units / non-EE scenarios have no EE capex 
      TRUE ~ 0
    )
  )

## FACILITY DATA 
# This makes a facility-level dataset that can be used with old figure code, basically

# getting facility emissions from unit_combined, so as not to dupe rows w/ different scenarios 
facility_emissions_df <- 
  unit_combined_df |>
  group_by(facility_id) |>
  summarize(
    base_emissions = sum(ghg_quantity, na.rm = TRUE),
    elec_ghg_emissions = sum(if_else(combustion_unit_electrifiable == 'Y' & !is_biogenic, ghg_quantity, 0), na.rm = TRUE),
    biogenic_ghg_emissions = sum(if_else(is_biogenic, ghg_quantity, 0), na.rm = TRUE),
    noelec_ghg_emissions = sum(if_else(combustion_unit_electrifiable == 'N', ghg_quantity, 0), na.rm = TRUE),
  )

# Calculate full & boiler scenarios separately
facility_full_df <- 
  unit_scenario_df |>
  filter(combustion_unit_category %in% c('generic', 'specific (incl)'), 
         str_detect(tech_scenario, 'full')) |>
  # Grouping by facility-scenario
  group_by(facility_id, tech_scenario, scenario_rank) |>
  summarize(
    # Main info vars 
    facility_name = first(facility_name), 
    naics_code = first(naics_code), 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    nonboiler_elec_unit_dum = any(
      combustion_unit_category != "generic" & combustion_unit_electrifiable == "Y",
      na.rm = TRUE
    ),
    
    capex = sum(
      case_when(
        !str_detect(tech_scenario, 'ee') ~ unit_capex, 
        str_detect(tech_scenario, 'ee') ~ unit_capex + ee_capex,
        TRUE ~ 0
      ), 
      na.rm = TRUE
    ),
    
    # The original (pre-energy efficiency) heat demand 
    heat_mmbtu_orig = sum(process_unit_heat_demand, na.rm = TRUE),
    # Heat demand w/ ee 
    heat_mmbtu_ee = sum(
      case_when(
        ## no energy efficiency for ng, non-EE scenarios
        !str_detect(tech_scenario, 'ee') ~ process_unit_heat_demand,
        str_detect(tech_scenario, 'ee') ~ process_unit_heat_demand_ee, 
        TRUE ~ 0
      ), 
      na.rm = TRUE
    ),
    
    # Change in electricity demand 
    ## EE is accounted for already in unit_electricity_demand_kwh calculation 
    change_in_electricity_demand_kwh = sum(unit_electricity_demand_kwh, na.rm = T),
    
    # Other info vars
    county_fips = first(county_fips), 
    subregion = first(subregion), 
    .groups = "drop"
  ) 


facility_boiler_df <-
  unit_scenario_df |>
  filter(combustion_unit_category == 'generic', 
         str_detect(tech_scenario, 'boiler')) |>
  # Grouping by facility-scenario
  group_by(facility_id, tech_scenario, scenario_rank) |>
  summarize(
    # Main info vars
    facility_name = first(facility_name),
    naics_code = first(naics_code),
    industry_clean = first(industry_clean),
    sector = first(sector),
    state = first(state),
    
    nonboiler_elec_unit_dum = any(
      combustion_unit_category != "generic" & combustion_unit_electrifiable == "Y",
      na.rm = TRUE
    ),
    
    capex = sum(
      case_when(
        !str_detect(tech_scenario, 'ee') ~ unit_capex,
        str_detect(tech_scenario, 'ee') ~ unit_capex + ee_capex,
        TRUE ~ 0
      ),
      na.rm = TRUE
    ),
    
    # The original (pre-energy efficiency) heat demand
    heat_mmbtu_orig = sum(process_unit_heat_demand, na.rm = TRUE),
    
    # Heat demand w/ ee
    heat_mmbtu_ee = sum(
      case_when(
        ## no energy efficiency for ng, non-EE scenarios
        !str_detect(tech_scenario, 'ee') ~ process_unit_heat_demand,
        str_detect(tech_scenario, 'ee') ~ process_unit_heat_demand_ee,
        TRUE ~ 0
      ),
      na.rm = TRUE
    ),
    
    # Change in electricity demand
    ## EE is accounted for already in unit_electricity_demand_kwh calculation
    change_in_electricity_demand_kwh = sum(unit_electricity_demand_kwh, na.rm = T),
    
    # Other info vars
    county_fips = first(county_fips),
    subregion = first(subregion),
    .groups = "drop"
  )


facility_scenario_df <-
  bind_rows(facility_full_df, facility_boiler_df) |>
  # bring in emissions
  left_join(facility_emissions_df, by = 'facility_id') |>
  arrange(facility_id, tech_scenario, scenario_rank)

## lcoh DATA WORK & SET-UP 
lcoh_func <- function(
    ## parameters
  r, 
  elec_price,
  #elec_price_high,
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
  scenario_rank,
  capex,
  heat_mmbtu_ee,
  heat_mmbtu_orig,
  change_in_electricity_demand_kwh,
  
  ## policy scenarios
  ITC, 
  rate_reduction){
  # time discounting formula 
  discount_sum <- sum((1 + r)^-(1:t))
  
  ## Inputting different parameters for different tech scenarios 
  ## Note: for non-ee scenarios, heat_mmbtu_ee in numerator will be the same as heat_mmbtu_orig. For ee, it's less.   
  case_when(
    str_detect(tech_scenario, 'ng') & scenario_rank == 'worst' ~ {
      opex_ng <- (heat_mmbtu_ee/.75) * ng_price      # energy costs. ng_boiler_ee has lower heat demand 
      opex_om <- ngboiler_om_high * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum 
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, 'ng') & scenario_rank == 'best' ~ {
      opex_ng <- (heat_mmbtu_ee/.9) * ng_price       # energy costs
      opex_om <- ngboiler_om_low * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum
      
      numerator / denominator
    }, 
    
    str_detect(tech_scenario, 'eb') & scenario_rank == 'worst' ~ {
      capex_adj <- capex * (1 - ITC)
      # EE measures already accounted for in change_in_electricity_demand_kwh
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-rate_reduction))
      opex_om <- eboiler_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, 'eb')  & scenario_rank == 'best' ~ {
      capex_adj <- capex * (1 - ITC)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-rate_reduction))
      opex_om <- eboiler_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum
      
      numerator / denominator
    }, 
    
    str_detect(tech_scenario, 'hp')  & scenario_rank == 'worst' ~ {
      capex_adj <- capex * (1 - ITC)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-rate_reduction))
      opex_om <- hthp_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum
      
      numerator / denominator
    },
    str_detect(tech_scenario, 'hp') & scenario_rank == 'best' ~ {
      capex_adj <- capex * (1 - ITC)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-rate_reduction))
      opex_om <- hthp_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu_orig * discount_sum
      
      numerator / denominator
    }
  )
}

# Import parameters 
param <- 
  read_csv('state_fact_sheets/data/parameters.csv') 

# --- Create Policy Grid ---
policy_grid <- expand.grid(
  ITC = seq(0.0, 1.0, by = 0.1),
  rate_reduction = seq(0.0, 1.0, by = 0.25)
)

# --- Apply Policy Grid to Tech Scenarios ---
facility_lcoh_df <- 
  tidyr::crossing(facility_scenario_df, policy_grid) %>%
  left_join(param, by = 'state') |>
  mutate(
    lcoh = lcoh_func(
      param$r[1], # need to pass r & t as scalars 
      elec_price,
      #elec_price_high,
      ng_price,
      param$t[1], 
      ngboiler_om_low, 
      ngboiler_om_high, 
      eboiler_om_low, 
      eboiler_om_high, 
      hthp_om_low, 
      hthp_om_high, 
      ## tech scenario + calculations 
      tech_scenario,
      scenario_rank,
      capex,
      heat_mmbtu_ee,
      heat_mmbtu_orig,
      change_in_electricity_demand_kwh,
      ## policy scenarios
      ITC, 
      rate_reduction 
    ), 
    
    policy_label = case_when(
      rate_reduction > 0 & ITC > 0 ~ 
        paste0("Capex -", ITC * 100, "%, Elec -", rate_reduction * 100, "%"),
      rate_reduction == 0 & ITC > 0 ~
        paste0("Capex -", ITC * 100, "%"),
      rate_reduction > 0 & ITC == 0 ~
        paste0("Elec -", ITC * 100, "%"),
      rate_reduction == 0 & ITC == 0 ~ 
        'No Policy' 
    )
  )|>
  select(-any_of(setdiff(names(param), "state")))

#### FIG ARCHIVE ####

## FIG: lcoh BY TECH SCENARIO 
ng_boiler_min <-
  facility_lcoh_df |>
  filter(tech_scenario == "ng_boiler", scenario_rank == "best") |>
  summarise(min(lcoh, na.rm = TRUE)) |>
  pull()

ng_boiler_max <-
  facility_lcoh_df |>
  filter(tech_scenario == "ng_boiler", scenario_rank == "worst") |>
  summarise(max(lcoh, na.rm = TRUE)) |>
  pull()

tech_lcoh_data <- 
  facility_lcoh_df %>%
  filter(
    policy_label == "No Policy",
    #nonboiler_elec_unit_dum == F,
    # drop non-EE 
    str_detect(tech_scenario, 'ee')  
  ) %>%
  mutate(
    scenario_group = factor(
      case_when(
        str_detect(tech_scenario, 'boiler') ~ 'Steam Network', 
        str_detect(tech_scenario, 'full') ~ 'Full Electrification'
      ), 
      levels = c('Steam Network', 'Full Electrification')
    ),
    scenario_label = factor(
      tech_scenario,
      levels = c('ng_boiler_ee', 'eb_boiler_ee', 'hp_boiler_ee', 'eb_full_ee', 'hp_full_ee'),
      labels = c("EE Only", "E-Boiler", "Air-Source HP", "E-Boiler", "Air-Source HP")
    ))

# Technology scenario x sector plot 
tech_lcoh_plot <- 
  ggplot(tech_lcoh_data,
         aes(x = scenario_label, y = lcoh, color = scenario_group)) +
  
  # # Natural gas range display 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_boiler_min, ymax = ng_boiler_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_boiler_min, linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = ng_boiler_max, linetype = "dotted", color = "black", size = 0.5) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 30)) +
  
  labs(
    x = NULL,
    y = "Levelized Cost of Heat ($/MMBtu)",
    color = "Scenario"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.83, 0.32),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

tech_lcoh_plot

## FIG: lcoh BY TECH SCENARIO
tech_lcoh_data_2 <- 
  facility_lcoh_df %>%
  filter(
    policy_label == "No Policy"            
  ) %>%
  mutate(
    scenario_group = factor(
      tech_scenario,
      levels = c('baseline',  
                 'eb_boiler', 'eb_boiler_ee',
                 'hp_boiler', 'hp_boiler_ee'), 
      labels = c("Baseline",  
                 "E-Boiler", "E-Boiler", 
                 "Air-Source HP", "Air-Source HP")
    ), 
    scenario_label = factor(
      case_when(
        str_detect(tech_scenario, 'boiler') & !str_detect(tech_scenario, 'ee') ~ 'Boiler', 
        str_detect(tech_scenario, 'boiler') & str_detect(tech_scenario, 'ee') ~ 'Boiler + EE', 
        str_detect(tech_scenario, 'full') & !str_detect(tech_scenario, 'ee') ~ 'Facility', 
        str_detect(tech_scenario, 'full') & str_detect(tech_scenario, 'ee') ~ 'Facility + EE', 
      ), 
      levels = c('Boiler', 'Boiler + EE', 'Facility', 'Facility + EE')
    )
  )

# Technology scenario x sector plot 
tech_lcoh_plot_2 <- 
  ggplot(tech_lcoh_data_2,
         aes(x = scenario_group, y = lcoh, color = scenario_label)) +
  
  # # # Natural gas range display 
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_full_min, ymax = ng_full_max,
  #          fill = "grey90", alpha = 0.3) +
  # geom_hline(yintercept = ng_boiler_min, linetype = "dotted", color = "black", size = 0.5) +
  # geom_hline(yintercept = ng_boiler_max, linetype = "dotted", color = "black", size = 0.5) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 30)) +
  
  labs(
    x = NULL,
    y = "Levelized Cost of Heat ($/MMBtu)",
    color = "Scenario"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.83, 0.32),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

tech_lcoh_plot_2



