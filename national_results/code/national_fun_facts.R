# NATIONAL POLICY ROADMAP FUN FACTS #
## November 13, 2025 ##

#### INITIAL SET-UP #### 
# Load Libraries
library(colorspace)
library(dplyr)
library(forcats)
library(ggforce)
library(ggraph)
library(ggpattern)
library(ggplot2)
library(glue)
library(igraph)
library(janitor)
library(packcircles)
library(patchwork)
library(purrr)
library(readr)
library(readxl)
library(scales)
library(stringr)
library(tidygraph)
library(tidyr)
library(tidylog)

data_wd <- c('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/dataset_v6_chpnmore/')

r <- 0.065
t_npv <- 10

facility_lcoh_df <- 
  read_csv(glue(data_wd, 'lcoh_industrialdecarb_facility_level.csv')) |>
  select(-opex) |>
  # Name & value clean-up 
  rename(
    naics_code = primary_naics, 
    naics_description = naics_title, 
    lcoh = LCOH, 
    
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
    ), 
    industry_clean = case_when(
      naics_description == 'Animal (except Poultry) Slaughtering' ~ 'Meat (non-poultry)',
      naics_description == 'Beet Sugar Manufacturing' ~ 'Beet Sugar',
      naics_description == 'Breakfast Cereal Manufacturing' ~ 'Breakfast Cereal',
      naics_description == 'Breweries' ~ 'Breweries',
      naics_description == 'Cane Sugar Manufacturing' ~ 'Cane Sugar',
      naics_description == 'Cheese Manufacturing' ~ 'Cheese',
      naics_description == 'Cyclic Crude, Intermediate, and Gum and Wood Chemical Manufacturing' ~ 'Wood Chemicals',
      naics_description == 'Distilleries' ~ 'Distilleries',
      naics_description == 'Dried and Dehydrated Food Manufacturing' ~ 'Dried Foods',
      naics_description == 'Dry, Condensed, and Evaporated Dairy Product Manufacturing' ~ 'Other Dairy',
      naics_description == 'Ethyl Alcohol Manufacturing' ~ 'Ethyl Alcohol',
      naics_description == 'Fats and Oils Refining and Blending' ~ 'Fats & Oils',
      naics_description == 'Fluid Milk Manufacturing' ~ 'Milk',
      naics_description == 'Frozen Fruit, Juice, and Vegetable Manufacturing' ~ 'Frozen Foods',
      naics_description == 'Fruit and Vegetable Canning' ~ 'Canning',
      naics_description == 'Industrial Gas Manufacturing' ~ 'Industrial Gas',
      naics_description == 'Nitrogenous Fertilizer Manufacturing' ~ 'Fertilizers',
      naics_description == 'Other Basic Inorganic Chemical Manufacturing' ~ 'Inorganic Chemicals',
      naics_description == 'Other Snack Food Manufacturing' ~ 'Snack Foods',
      naics_description == 'Paper Mills' ~ 'Pulp & Paper',
      naics_description == 'Paperboard Mills' ~ 'Pulp & Paper',
      naics_description == 'Petrochemical Manufacturing' ~ 'Petrochemicals',
      naics_description == 'Phosphatic Fertilizer Manufacturing' ~ 'Fertilizers',
      naics_description == 'Plastics Material and Resin Manufacturing' ~ 'Plastics & Resins',
      naics_description == 'Poultry Processing' ~ 'Poultry',
      naics_description == 'Pulp Mills' ~ 'Pulp & Paper',
      naics_description == 'Rendering and Meat Byproduct Processing' ~ 'Rendering',
      naics_description == 'Sanitary Paper Product Manufacturing' ~ 'Toilet Paper',
      naics_description == 'Soybean and Other Oilseed Processing' ~ 'Soybeans',
      naics_description == 'Specialty Canning' ~ 'Specialty Canning',
      naics_description == 'Spice and Extract Manufacturing' ~ 'Spices',
      naics_description == 'Synthetic Rubber Manufacturing' ~ 'Rubber',
      naics_description == 'Wet Corn Milling and Starch Manufacturing' ~ 'Wet Corn Milling',
      TRUE ~ naics_description
    ), 
    
    policy_label = case_when(
      rate_reduction == 0 & ITC == 0 & PTC == 0 ~
        'No Policy', 
      rate_reduction == 0 & ITC > 0 & PTC == 0 ~
        paste0("Capex -", ITC * 100, "%"),
      rate_reduction > 0 & ITC == 0 & PTC == 0 ~
        paste0("Elec -", rate_reduction * 100, "%"),
      rate_reduction > 0 & ITC > 0 & PTC == 0~ 
        paste0("Capex -", ITC * 100, "%, Elec -", rate_reduction * 100, "%"),
      PTC == 5 ~
        'PTC $5/MMBtu', 
      PTC == 10 ~
        'PTC $10/MMBtu', 
      PTC == 15 ~ 
        'PTC $15/MMBtu'
    ), 
    
    tech_scenario_label = case_when(
      tech_scenario == 'baseline' ~ 'Baseline', 
      tech_scenario == 'eb_boiler' ~ 'Drop-In Elec', 
      tech_scenario == 'eb_boiler_ee' ~ 'Drop-In Elec (EE+)', 
      tech_scenario == 'hp_boiler' ~ 'Adv Elec', 
      tech_scenario == 'hp_boiler_ee' ~ 'Adv Elec (EE+)'
    )
  ) |>
  filter(
    # Only keep PTC rows where rate_reduction & ITC are zero 
    !(PTC != 0 & (rate_reduction != 0 | ITC != 0))
  ) |>
  # Dropping some facilities with strange GHGRP reporting of combustion units 
  filter(lcoh != 0)

payback_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  rename(
    capex_ng = capex,
    opex_ng = opex
  ) |>
  ungroup() |>
  select(facility_id, scenario_rank, capex_ng, opex_ng)

facility_lcoh_df <- 
  facility_lcoh_df |>
  
  left_join(payback_data_ng, by = c('facility_id', 'scenario_rank')) |>
  
  mutate(
    annual_savings = opex_ng - opex,
    capex_diff = capex - capex_ng, 
    payback_years = case_when(
      tech_scenario == 'baseline' ~ NA_real_, 
      annual_savings >= 0 ~ capex_diff / annual_savings, 
      annual_savings < 0 ~ NA_real_,
      TRUE ~ NA_real_
    ), 
    
    # payback using total capex (no baseline subtraction)
    payback_years_nobase = case_when(
      tech_scenario == "baseline" ~ NA_real_,
      annual_savings > 0 ~ capex / annual_savings,   # use absolute capex
      TRUE ~ NA_real_
    ), 
    
    npv = -capex - opex * ((1 - (1 + r)^(-t_npv)) / r)
  ) 

r <- 0.065
t <- 25

emissions_df <-
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/total_emissions_by_facility_scope_category_251113.csv') |>
  mutate(
    # Apply a per-year discount factor
    discount_factor = 1 / (1 + r)^(policy_year - 2025),  # assuming 2025 is year 0, 
    in_scope = if_else(tech_scenario == 'baseline', in_scope, 0)
  ) |>
  group_by(facility_id, tech_scenario, scenario_rank) |>
  summarize(
    # Present-day emissions figures
    baseline_co2e_mt_25 = co2e_baseline_mt[policy_year == 2025],
    clean_grid_co2e_mt_25 = cambium95_grid_co2e_emission_mt[policy_year == 2025],
    # mid_grid_co2e_mt_25 = cambiummid_grid_co2e_emission_mt[policy_year == 2025],
    bau_grid_co2e_mt_25 = bau_grid_co2e_emission_mt[policy_year == 2025],
    in_scope_mt_25 = in_scope[policy_year == 2025], 
    chp_mt_25 = chp[policy_year == 2025], 
    biogenic_mt_25 = biogenic[policy_year == 2025], 
    no_direct_mt_25 = no_direct_replacement[policy_year == 2025], 
    hightemp_mt_25 = utilities_temp_too_high[policy_year == 2025], 
    
    # Lifetime totals
    lifetime_baseline_co2e_mt = sum(co2e_baseline_mt, na.rm = TRUE),
    lifetime_clean_grid_co2e_mt = sum(cambium95_grid_co2e_emission_mt, na.rm = TRUE),
    lifetime_bau_grid_co2e_mt = sum(bau_grid_co2e_emission_mt, na.rm = TRUE),
    lifetime_in_scope_mt = sum(in_scope, na.rm = TRUE),
    lifetime_chp_mt = sum(chp, na.rm = TRUE),
    lifetime_biogenic_mt = sum(biogenic, na.rm = TRUE),
    lifetime_no_direct_mt = sum(no_direct_replacement, na.rm = TRUE),
    lifetime_hightemp_mt = sum(utilities_temp_too_high, na.rm = TRUE),
    
    # # Discounted lifetime totals — year-by-year discounting
    lifetime_baseline_co2e_mt_dc = sum(co2e_baseline_mt * discount_factor, na.rm = TRUE),
    lifetime_clean_grid_co2e_mt_dc = sum(cambium95_grid_co2e_emission_mt * discount_factor, na.rm = TRUE),
    lifetime_bau_grid_co2e_mt_dc = sum(bau_grid_co2e_emission_mt * discount_factor, na.rm = TRUE),
    lifetime_in_scope_mt_dc = sum(in_scope * discount_factor, na.rm = TRUE),
    lifetime_chp_mt_dc = sum(chp * discount_factor, na.rm = T),
    lifetime_biogenic_mt_dc = sum(biogenic * discount_factor, na.rm = T),
    lifetime_no_direct_mt_dc = sum(no_direct_replacement * discount_factor, na.rm = TRUE),
    lifetime_hightemp_mt_dc = sum(utilities_temp_too_high * discount_factor, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    facility_id = as.character(facility_id),
    
    # Annual reductions at t=2025
    # emissions_reduc_mt_25 = (clean_grid_co2e_mt_25 + in_scope_mt_25 + hard_elec_mt_25 + biogenic_mt_25) - baseline_co2e_mt_25,
    
    # Lifetime reductions
    lifetime_emissions_reduc_mt = (lifetime_clean_grid_co2e_mt + lifetime_in_scope_mt + lifetime_chp_mt + lifetime_biogenic_mt + lifetime_no_direct_mt + lifetime_hightemp_mt) - lifetime_baseline_co2e_mt,
    lifetime_emissions_reduc_mt_dc = (lifetime_clean_grid_co2e_mt_dc + lifetime_in_scope_mt_dc + lifetime_chp_mt_dc + lifetime_biogenic_mt_dc 
                                      + lifetime_no_direct_mt_dc + lifetime_hightemp_mt_dc) - lifetime_baseline_co2e_mt_dc
  ) |>
  filter(facility_id %in% facility_lcoh_df$facility_id)

#### Fuel Fun Facts ####

tech_scenario_labels <- 
  c(
    "Baseline",
    "Drop-In Elec",
    "Drop-In Elec (EE+)",
    "Adv Elec",
    "Adv Elec (EE+)"
  )

fuel_colors <- c(
  "Electricity" = '#FEBC11',
  "Natural Gas" = "#9CBEBE",
  "Fuel Oil"    = "#6D4C41",
  "Coal"        = "#424242",
  "Byproducts"  = '#A67C52',
  "Propane"     = "#9575CD",
  "Other"       = "#BDBDBD"
)

facility_fuels_df <-
  read_excel(glue(data_wd ,'merged_longform.xlsx')) |>
  select(-1, -2) |>
  mutate(
    facility_id = as.character(facility_id), 
    sector = case_when(
      str_starts(primary_naics, "311") | str_starts(primary_naics, "312") ~ "Food & Beverage",
      str_starts(primary_naics, "325") ~ "Chemicals",
      str_starts(primary_naics, "322") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    ), 
    
    fuel_label = case_when(
      fuel_type == 'Natural Gas (Weighted U.S. Average)' ~ 'Natural Gas', 
      str_detect(fuel_type, 'Fuel Oil') ~ 'Fuel Oil', 
      fuel_type %in% c('Bituminous', 'Subbituminous', 'Anthracite',
                       'Coal Coke', 'Mixed (Industrial sector)', 'Lignite') ~ 'Coal', 
      fuel_type %in% c('Wood and Wood Residuals (dry basis)', 'Solid Byproducts',
                       'Agricultural Byproducts', 'Rendered Animal Fat', 'Vegetable Oil', 'North American Hardwood') ~ 'Byproducts', 
      str_detect(fuel_type, 'Propane') ~ 'Propane', 
      TRUE ~ 'Other'
    ), 
    
    electrifiable = if_else(electrified_option != 'not_electrifiable', 1, 0)
  ) |>
  group_by(facility_id, fuel_label, electrifiable) |>
  summarize(
    fuel_hhv_mmbtu = sum(fuel_hhv_mmbtu, na.rm = T)
  ) |>
  ungroup() 

elec_fuel_df <- 
  facility_lcoh_df |>
  filter(
    policy_label == 'No Policy', 
    tech_scenario != 'baseline'
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    tech_scenario_label = first(tech_scenario_label), 
    sector = first(sector), 
    
    fuel_hhv_mmbtu = mean(change_in_electricity_demand_kwh, na.rm = T) / 293.071
  ) |>
  ungroup() |>
  mutate(fuel_label = 'Electricity')

fuels_df <-
  facility_lcoh_df |>
  filter(
    policy_label == 'No Policy'
  ) |>
  distinct(facility_id, tech_scenario, tech_scenario_label, sector) |>
  left_join(facility_fuels_df, by = c('facility_id'), relationship = 'many-to-many') |>
  filter(
    !(tech_scenario != 'baseline' & electrifiable == 1) 
  ) |>
  select(-electrifiable) |>
  bind_rows(elec_fuel_df) |>
  group_by(sector, tech_scenario_label, fuel_label) |>
  summarize(
    fuel_hhv_tbtu = sum(fuel_hhv_mmbtu, na.rm = T) / 1000000
  ) |>
  ungroup() |>
  mutate(
    tech_scenario_label = factor(
      tech_scenario_label, 
      levels = rev(tech_scenario_labels)
    ), 
    
    fuel_label = factor(
      fuel_label,
      levels = rev(names(fuel_colors))
    ), 
    
    sector = paste0(sector, '*'),
    sector = factor(sector, levels = rev(sort(unique(as.character(sector)))))
  )

fuels_fun_fact <- 
  fuels_df %>%
  filter(tech_scenario_label %in% c("Baseline", "Adv Elec (EE+)")) %>%
  
  # Step 1 — collapse full detail to national totals
  group_by(tech_scenario_label, fuel_label) %>%
  summarize(tbtu = sum(fuel_hhv_tbtu, na.rm = TRUE), .groups = "drop") %>%
  
  # Step 2 — pivot so Baseline and Adv side-by-side
  tidyr::pivot_wider(
    names_from = tech_scenario_label,
    values_from = tbtu
  ) %>%
  
  # Step 3 — compute reductions
  mutate(
    reduction_tbtu = Baseline - `Adv Elec (EE+)`,
    reduction_pct  = 100 * reduction_tbtu / Baseline,
    
    group = case_when(
      fuel_label == "Natural Gas" ~ "Natural Gas",
      fuel_label %in% c("Fuel Oil", "Coal", "Propane", "Other") ~ "Other Fossil",
      TRUE ~ "Non-Fossil"
    )
  ) %>%
  
  # Step 4 — collapse to the categories you want
  group_by(group) %>%
  summarize(
    baseline_tbtu = sum(Baseline, na.rm = TRUE),
    adv_tbtu      = sum(`Adv Elec (EE+)`, na.rm = TRUE),
    reduction_tbtu = sum(reduction_tbtu, na.rm = TRUE),
    reduction_pct  = 100 * (1 - adv_tbtu / baseline_tbtu),
    .groups = "drop"
  ) %>%
  
  # Step 5 — keep only the categories you asked for
  filter(group %in% c("Natural Gas", "Other Fossil"))

#### Capex Fun Facts ####
fig_scenarios <-
  c("Baseline", 
    "Drop-In\nElec", 
    "Drop-In\nElec (EE+)", 
    "Advanced\nElec", 
    "Advanced\nElec (EE+)")

sector_colors <- 
  c(
    "Chemicals"     = "#09847A",
    "Food & Beverage" = "#EF5645",
    "Pulp & Paper"  = "#A67C52"
  )

capex_data <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario != 'baseline') |>
  # left_join(baseline_capex, by = c('facility_id', 'scenario_rank')) |>
  mutate(
    capex_delta = (capex - capex_ng) / 1e6, 
    capex_delta_pct = ((capex - capex_ng) / capex_ng) * 100,
    tech_scenario = recode(
      tech_scenario,
      "baseline"           = fig_scenarios[1],
      "eb_boiler"          = fig_scenarios[2],
      "eb_boiler_ee"       = fig_scenarios[3],
      "hp_boiler"          = fig_scenarios[4],
      "hp_boiler_ee"       = fig_scenarios[5]
    ), 
    tech_scenario = factor(tech_scenario, levels = fig_scenarios) 
    #capex_mill = capex / 1000000
  ) |>
  select(facility_id, state, sector, industry_clean, capex, capex_ng, capex_delta, capex_delta_pct, tech_scenario, scenario_rank)

# 

# Median capex by tech type, no EE
capex_median <- tibble::tibble(
  description = c(
    "Median natural gas capex",
    "Median e-boiler capex",
    "Median heat pump capex"
  ),
  value = c(
    median(capex_data$capex_ng, na.rm = TRUE) / 1e6,
    median(capex_data$capex[capex_data$tech_scenario == "Drop-In\nElec"], na.rm = TRUE)  / 1e6,
    median(capex_data$capex[capex_data$tech_scenario == "Advanced\nElec"], na.rm = TRUE) / 1e6
  )
)

# Percent of facilities where capex is less under electrification 
# i am using both best and worst cases here... seems fine since it's a % anyway
capex_pct <- 
  capex_data |>
  filter(tech_scenario != 'baseline') |>
  mutate(
    capex_less = if_else(capex < capex_ng, 1, 0)
  ) |>
  group_by(tech_scenario) |>
  summarize(
    pct_less = sum(capex_less) / n()
  )


#### Payback Fun Facts -- No Policy ####
sector_colors <- c(
  "Chemicals*"      = "#09847A",
  "Food & Beverage*" = "#EF5645",
  "Pulp & Paper*"   = "#A67C52"
)

tech_scenario_labels <- c(
  "Drop-In\nElec",
  "Drop-In\nElec (EE+)",
  "Adv\nElec",
  "Adv\nElec (EE+)"
)

in_payback_data_nopol <- 
  facility_lcoh_df |>
  filter(
    policy_label == 'No Policy' 
  ) |>
  group_by(facility_id, tech_scenario_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    payback_years = mean(payback_years, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    payback_sub_5 = if_else(payback_years <= 5, 1, 0), 
    payback_sub_15 = if_else(payback_years <= 15 & payback_years > 5, 1, 0), 
    payback_plus_15 = if_else(!is.na(payback_years) & payback_years > 15, 1, 0)
  ) |>
  group_by(tech_scenario_label, sector) |>
  summarize(
    n_facilities = n(),
    n_sub_5 = sum(payback_sub_5, na.rm = TRUE),
    n_sub_15 = sum(payback_sub_15, na.rm = TRUE), 
    n_plus_15 = sum(payback_plus_15, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    pct_sub_5 = (n_sub_5 / n_facilities) * 100, 
    pct_sub_15 = (n_sub_15 / n_facilities) * 100, 
    pct_plus_15 = (n_plus_15 / n_facilities) * 100
  ) |>
  pivot_longer(
    cols = starts_with('pct'), 
    names_to = "pct_group", 
    names_prefix = "pct_", 
    values_to = "pct_in_payback"
  ) |>
  mutate(
    sector = paste0(sector, "*"),
    tech_scenario_label = factor(
      tech_scenario_label, 
      levels = c(
        "Drop-In Elec",
        "Drop-In Elec (EE+)",
        "Adv Elec",
        "Adv Elec (EE+)"
      )
    )
  ) |>
  filter(tech_scenario_label != 'Baseline')

# How many facilities are in payback? 
in_payback_no_pol_fact <- 
  in_payback_data_nopol |>
  distinct(tech_scenario_label, sector, n_facilities, n_sub_5, n_sub_15, n_plus_15) |>
  group_by(tech_scenario_label) |>
  summarize(
    n_facilities = sum(n_facilities, na.rm = TRUE),
    n_sub_5 = sum(n_sub_5, na.rm = TRUE),
    n_sub_15 = sum(n_sub_15, na.rm = TRUE), 
    n_plus_15 = sum(n_plus_15, na.rm = TRUE)
  )

#### NPV Fun Facts -- No Policy ####

tech_scenario_labels <- c(
  "Drop-In Elec",
  "Drop-In Elec (EE+)",
  "Adv Elec",
  "Adv Elec (EE+)"
)

tech_scenario_colors <- c(
  "Drop-In Elec"       = "#1f78b4",
  "Drop-In Elec (EE+)" = "#a6cee3",
  "Adv Elec"           = "#33a02c",
  "Adv Elec (EE+)"     = "#b2df8a"
)

baseline_npv <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  rename(
    npv_ng = npv
  ) |>
  ungroup() |>
  select(facility_id, scenario_rank, npv_ng)

delta_npv_data <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario != 'baseline' 
  ) |>
  left_join(baseline_npv, by = c('facility_id', 'scenario_rank')) |>
  mutate(
    npv_delta = npv - npv_ng, 
    npv_delta_pct = 100 * (npv - npv_ng) / npv_ng,
    tech_scenario_label = factor(
      tech_scenario_label, 
      levels = tech_scenario_labels
    ), 
    sector = paste0(sector, '*')
  ) 

npv_fun_fact <- 
  delta_npv_data |>
  filter(tech_scenario == 'hp_boiler_ee') |>
  summarize(
    pct_negative = mean(npv_delta > 0, na.rm = TRUE) * 100
  )


#### Abatement Fun Facts  ####

fig_subsector_colors <- c(
  "Cane Sugar"           = "#E41A1C",  # bright red
  "Pulp & Paper"         = "#A67C52",  
  "Fats & Oils"          = "#984EA3",  # purple
  "Spices"               = "#FF7F00",  # orange
  "Wet Corn Milling"     = "#D73027",  # deep red ✅
  "Rubber"               = "#4575B4",  # blue
  "Plastics & Resins"    = "#F781BF",  # pink
  "Ethyl Alcohol"        = "#00A7A0",  # teal ✅
  "Specialty Canning"    = "#FFD92F",  # yellow
  "Breweries"            = "#1B9E77",  # dark green-teal
  "Milk"                 = "#E6AB02",  # gold
  "Soybeans"             = "#984EA3",  # purple (reused, alternate tone OK)
  "Fertilizers"          = "#4C7D8D",  # brown-gold
  "Distilleries"         = "#7570B3",  # indigo
  "Beet Sugar"           = "#66C2A5",  # light aqua
  "Cheese"               = "#E7298A",  # fuchsia
  "Breakfast Cereal"     = "#B2DF8A",  # light green
  "Meat (non-poultry)"   = "#FFB300",  # orange-yellow
  "Toilet Paper"         = "#01665E",  # dark teal
  "Frozen Foods"         = "#DE77AE",  # mauve-pink
  "Poultry"              = "#A6CEE3",  # sky blue
  "Rendering"            = "#B2182B",  # crimson
  "Dried Foods"          = "#F46D43",  # orange-coral
  "Other Dairy"          = "#8DD3C7",  # mint
  "Snack Foods"          = "#E6F598",  # pale lime
  "Canning"              = "#3288BD",   # blue
  "Industrial Gas"        = "#00A6C2",  # teal-blue
  "Inorganic Chemicals"   = "#4B3F72",  # indigo-violet
  "Petrochemicals"        = "#70543E",  # warm brown-gray
  "Phosphatic Fertilizer" = "#D9467E",  # rose-magenta
  "Wood Chemicals"        = "#8C6A4E"   # natural wood tone
)


### IF YOU CHANGE THIS, NEED TO CHANGE FOR EMISSIONS_DF TOO *********
r <- .065
t <- 25
discount_sum <- (1 - (1 + r)^(-t)) / r

acc_data_ng <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'baseline', 
    policy_label == 'No Policy'
  ) |>
  group_by(facility_id) |>
  summarize(
    industry_clean = first(industry_clean), 
    
    opex_ng = mean(opex, na.rm = T), 
    capex_ng = mean(capex, na.rm = T), 
    elec_orig_process_unit_heat_demand = mean(elec_orig_process_unit_heat_demand, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    lifetime_cost_ng = capex_ng + (opex_ng * discount_sum)
  ) 

acc_data_hp <- 
  facility_lcoh_df |>
  filter(tech_scenario == 'hp_boiler_ee', 
         policy_label == 'No Policy') |>
  group_by(facility_id) |>
  summarize(
    opex_hp = mean(opex, na.rm = T), 
    capex_hp = mean(capex, na.rm = T), 
    elec_orig_process_unit_heat_demand = mean(elec_orig_process_unit_heat_demand, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    lifetime_cost_hp = capex_hp + (opex_hp * discount_sum)
  ) |>
  select(facility_id, lifetime_cost_hp)

acc_data <- 
  inner_join(acc_data_ng, acc_data_hp, by = 'facility_id') |>
  inner_join(emissions_df, by = 'facility_id') |>
  mutate(
    lifetime_cost = lifetime_cost_hp - lifetime_cost_ng, 
    mac = lifetime_cost / lifetime_emissions_reduc_mt_dc
  )

Z <- 100   # number of cheapest facilities to consider

abatement_thresholds_df <- 
  acc_data %>%
  filter(lifetime_emissions_reduc_mt_dc > 0) %>%
  summarize(
    emissions_mmt_100 = sum(lifetime_emissions_reduc_mt_dc[mac <= 100], na.rm = TRUE) / 1e6,
    emissions_mmt_200 = sum(lifetime_emissions_reduc_mt_dc[mac <= 200], na.rm = TRUE) / 1e6
  ) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = "threshold",
    values_to = "emissions_mmt"
  ) %>%
  mutate(
    threshold = recode(threshold,
                       "emissions_mmt_100" = "≤100_per_t",
                       "emissions_mmt_200" = "≤200_per_t"
    ),
    threshold = factor(threshold, levels = c("≤100_per_t", "≤200_per_t"))
  )

cumulative_mac_df <- 
  acc_data %>%
  filter(lifetime_emissions_reduc_mt_dc > 0) %>%
  arrange(mac) %>%
  mutate(
    facility_index = row_number(),
    cumulative_mmt = cumsum(lifetime_emissions_reduc_mt_dc) / 1e6
  )

electrify_Z_df <- 
  cumulative_mac_df %>%
  filter(facility_index == Z) %>%
  select(facility_index, cumulative_mmt)
  

#### Payback Fun Facts -- Policy ####
sector_colors <- c(
  "Chemicals*"      = "#09847A",
  "Food & Beverage*" = "#EF5645",
  "Pulp & Paper*"   = "#A67C52"
)

fig_policies <- c(
  'No Policy',
  'Elec -25%', 
  'Capex -30%, Elec -25%',
  'Elec -50%', 
  'PTC $10/MMBtu'
)

policy_labels <- c(
  'No Policy' = 'No Policy',
  'Elec -25%' =  'Elec -25%',
  'Capex -30%, Elec -25%' = 'Capex -30%,\nElec -25%',
  'Elec -50%' = 'Elec -50%', 
  'PTC $10/MMBtu' = 'PTC\n$10/MMBtu'
)

in_payback_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee', 
    policy_label %in% fig_policies
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  
  mutate(
    sub_5 = if_else(payback_years <= 5, 1, 0), 
    sub_15 = if_else(payback_years <= 15 & payback_years > 5, 1, 0), 
    plus_15 = if_else(!is.na(payback_years) & payback_years > 15, 1, 0)
  ) |>
  group_by(policy_label, sector) |>
  summarize(
    n_facilities = n(),
    n_sub_5 = sum(sub_5, na.rm = TRUE),
    n_sub_15 = sum(sub_15, na.rm = TRUE), 
    n_plus_15 = sum(plus_15, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    pct_sub_5 = (n_sub_5 / n_facilities) * 100, 
    pct_sub_15 = (n_sub_15 / n_facilities) * 100, 
    pct_plus_15 = (n_plus_15 / n_facilities) * 100
  ) |>
  pivot_longer(
    cols = starts_with('pct'), 
    names_to = "pct_group", 
    names_prefix = "pct_", 
    values_to = "pct_in_payback"
  ) |>
  mutate(
    sector = paste0(sector, "*"),
    policy_label = factor(
      policy_label, 
      levels = fig_policies
    )
  ) 

policy_payback_summary <- 
  in_payback_data |> 
  group_by(policy_label) |> 
  summarize(
    n_facilities    = sum(n_facilities),
    n_sub_5    = sum(n_sub_5),
    n_sub_15    = sum(n_sub_15),
    n_plus_15    = sum(n_plus_15)
  ) |> 
  ungroup() |>
  mutate(
    pct_in_payback_sub_5 = n_sub_5 / n_facilities, 
    pct_in_payback_sub_15 = n_sub_15 / n_facilities,
    pct_in_payback_plus_15 = n_plus_15 / n_facilities,
    total_in_payback = (n_sub_5 + n_sub_15 + n_plus_15) / n_facilities
  )

## 25% elec stuff
# .42 - .15 = .38 (38%)
# 162 new sub 15 facilities / 426 new facilities in payback total = .38






#### Payback fun facts - statesubs - policy  ####

fig_policies <- 
  c(
    "PTC $10/MMBtu",
    "Capex -30%", 
    "Elec -25%", 
    "No Policy"
  )

fig_policy_colors <- c(
  "No Policy"     = '#FEBC11',
  "Elec -25%"     = "#C2E4E6",
  "Capex -30%"   = "#09847A", 
  "PTC $10/MMBtu" = '#003660'
)

top_payback_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == "hp_boiler_ee",
    policy_label %in% fig_policies
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  group_by(state, industry_clean, policy_label) |>
  filter(n() >= 2) |>   # keep only statesubs with +2 facilities
  summarize(
    sector = first(sector),
    
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    statesub = paste(state, industry_clean, sep = " "), 
    statesub_label = str_wrap(statesub, width = 10), 
    policy_label = factor(
      policy_label, 
      levels = fig_policies
    )
  ) |>
  filter(!is.na(payback_years)) |>
  group_by(statesub) |>
  mutate(
    payback_no_policy = payback_years[policy_label == "No Policy"][1], 
    payback_ptc = payback_years[policy_label == "PTC $10/MMBtu"][1]
  ) |>
  ungroup() |>
  filter(
    !is.na(payback_no_policy),
    payback_ptc < 10, 
    payback_no_policy >= 20, 
    # excluding this because of funky outcomes stemming from facilities moving in and out of payback in best/worse scenarios, 
    # which causes the electricity discount to look worse (because facilities get into payback in the worst case, bringing the facility
    # average up for the electricity policy)
    !statesub %in% c('NM Cheese', 'TX Petrochemicals')
  ) |>
  select(-payback_no_policy, -payback_ptc)


payback_nopolicy <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == "hp_boiler_ee",
    policy_label %in% c("No Policy")
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state),
    payback_years_nopol = mean(payback_years, na.rm = TRUE),
    .groups = "drop"
  ) |>
  
  # create statesub label
  mutate(statesub = paste(state, industry_clean)) |>
  
  # average across facilities within each statesub-policy combo
  group_by(statesub) |>
  summarize(
    sector = first(sector),
    payback_years_nopol = mean(payback_years_nopol, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup()
  
payback_effect <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == "hp_boiler_ee",
    policy_label %in% c("Capex -30%")
  ) |>
  group_by(facility_id) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state),
    payback_years_capex = mean(payback_years, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup() |>
  # create statesub label
  mutate(statesub = paste(state, industry_clean)) |>
  
  # average across facilities within each statesub-policy combo
  group_by(statesub) |>
  summarize(
    sector = first(sector),
    payback_years_capex = mean(payback_years_capex, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup() |>
  left_join(payback_nopolicy, by = 'statesub') |>
  mutate(
    flipped = if_else(payback_years_nopol > 5 & payback_years_capex <=5, 1, 0)
  )


#### Emissions in the money fun facts ####
fig_policies <- c(
  "Baseline", 
  "No Policy",
  "Capex -100%", 
  "Elec -25%", 
  "Elec -50%", 
  "PTC $10/MMBtu"
)

fig_emissions_colors <- c(
  "Scope 1 (No Direct Replacement)" = "#C9BF9D", 
  "Scope 1 (High Temperature)"    = "#EF5645",
  "Scope 1 (CHP)"                 = "#4575B4", 
  "Scope 1 (Biogenic)"            = "#A67C52",  
  "Scope 1 (Low-Med Temp)"  = "#09847A",  
  "Scope 2" = "#FEBC11"
)

total_elec_ghg_emissions_nat <- 
  emissions_df %>%
  filter(tech_scenario == 'baseline') %>%
  group_by(facility_id) %>% 
  summarize(
    lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = TRUE),
    lifetime_in_scope_mt = mean(lifetime_in_scope_mt, na.rm = TRUE),
    lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = TRUE),
    lifetime_chp_mt = mean(lifetime_chp_mt, na.rm = TRUE),
    lifetime_hightemp_mt = mean(lifetime_hightemp_mt, na.rm = TRUE),
    lifetime_no_direct_mt = mean(lifetime_no_direct_mt, na.rm = TRUE)
  ) %>%
  left_join(facility_lcoh_df |>
              distinct(facility_id, sector), 
            by = 'facility_id') |>
  group_by(sector) %>%          
  summarize(
    lifetime_clean_grid_co2e_mmt = sum(lifetime_clean_grid_co2e_mt, na.rm = TRUE)/ 1e6, 
    lifetime_in_scope_mmt = sum(lifetime_in_scope_mt, na.rm = TRUE)/ 1e6, 
    lifetime_chp_mmt = sum(lifetime_chp_mt, na.rm = TRUE)/ 1e6, 
    lifetime_biogenic_mmt = sum(lifetime_biogenic_mt, na.rm = TRUE)/ 1e6, 
    lifetime_no_direct_mmt = sum(lifetime_no_direct_mt, na.rm = TRUE)/ 1e6, 
    lifetime_hightemp_mmt = sum(lifetime_hightemp_mt, na.rm = TRUE)/ 1e6,
    policy_label = "Baseline"
  ) 

eim_data_nat <- 
  facility_lcoh_df |>
  filter(
    policy_label %in% fig_policies, 
    tech_scenario %in% c('hp_boiler_ee')
  ) |>
  left_join(emissions_df, by = c('facility_id', 'tech_scenario', 'scenario_rank')) |>
  # Average best & worst case 
  group_by(facility_id, tech_scenario, policy_label) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = TRUE),
    lifetime_in_scope_mt = mean(lifetime_in_scope_mt, na.rm = TRUE),
    lifetime_chp_mt = mean(lifetime_chp_mt, na.rm = TRUE),
    lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = TRUE),
    lifetime_no_direct_mt = mean(lifetime_no_direct_mt, na.rm = TRUE),
    lifetime_hightemp_mt = mean(lifetime_hightemp_mt, na.rm = TRUE),
    
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    payback_years = ifelse(is.nan(payback_years), NA, payback_years),
    in_money = if_else(!is.na(payback_years) & payback_years < 15, 1, 0)
  ) %>%
  
  # Add baseline emissions for every facility
  left_join(
    emissions_df |>
      filter(tech_scenario == 'baseline') |>
      group_by(facility_id) |>
      summarize(
        lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = TRUE),
        lifetime_in_scope_mt = mean(lifetime_in_scope_mt, na.rm = TRUE),
        lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = TRUE),
        lifetime_chp_mt = mean(lifetime_chp_mt, na.rm = TRUE),
        lifetime_hightemp_mt = mean(lifetime_hightemp_mt, na.rm = TRUE),
        lifetime_no_direct_mt = mean(lifetime_no_direct_mt, na.rm = TRUE)
      ) |>
      ungroup() |>
      rename(
        base_lifetime_clean_grid_co2e_mt = lifetime_clean_grid_co2e_mt, 
        base_lifetime_in_scope_mt = lifetime_in_scope_mt, 
        base_lifetime_biogenic_mt = lifetime_biogenic_mt, 
        base_lifetime_chp_mt = lifetime_chp_mt,
        base_lifetime_hightemp_mt = lifetime_hightemp_mt,
        base_lifetime_no_direct_mt = lifetime_no_direct_mt
      ),
    by = 'facility_id'
  ) |>
  
  mutate(
    lifetime_clean_grid_effective = if_else(in_money == 1,
                                            lifetime_clean_grid_co2e_mt,
                                            base_lifetime_clean_grid_co2e_mt),
    lifetime_low_med_effective = if_else(in_money == 1,
                                         lifetime_in_scope_mt,
                                         base_lifetime_in_scope_mt)
  ) |>
  
  # Getting sector-policy emissions in the money 
  group_by(sector, policy_label) %>%
  summarize(
    lifetime_clean_grid_co2e_mmt = sum(lifetime_clean_grid_effective, na.rm = T) / 1e6, 
    lifetime_in_scope_mmt = sum(lifetime_low_med_effective, na.rm = T) / 1e6, 
    lifetime_chp_mmt = sum(lifetime_chp_mt, na.rm = TRUE)/ 1e6, 
    lifetime_biogenic_mmt = sum(lifetime_biogenic_mt, na.rm = TRUE)/ 1e6, 
    lifetime_no_direct_mmt = sum(lifetime_no_direct_mt, na.rm = TRUE)/ 1e6, 
    lifetime_hightemp_mmt = sum(lifetime_hightemp_mt, na.rm = TRUE)/ 1e6
  ) %>%
  ungroup() %>%
  # Add the sector totals
  bind_rows(total_elec_ghg_emissions_nat) %>%
  
  # Get to one emissions variable 
  pivot_longer(
    cols = starts_with("lifetime_"), 
    names_to = "emissions_type", 
    values_to = "emissions_mmt"
  ) |>
  
  mutate(
    emissions_label = case_when(
      emissions_type == 'lifetime_no_direct_mmt' ~ names(fig_emissions_colors)[1], 
      emissions_type == 'lifetime_hightemp_mmt' ~ names(fig_emissions_colors)[2], 
      emissions_type == 'lifetime_chp_mmt' ~ names(fig_emissions_colors)[3], 
      emissions_type == 'lifetime_biogenic_mmt' ~ names(fig_emissions_colors)[4], 
      emissions_type == 'lifetime_in_scope_mmt' ~ names(fig_emissions_colors)[5], 
      emissions_type == 'lifetime_clean_grid_co2e_mmt' ~ names(fig_emissions_colors)[6]
    ), 
    
    emissions_label = factor(
      emissions_label, 
      levels = names(fig_emissions_colors)
    ), 
    
    policy_label = factor(
      policy_label,
      levels = rev(fig_policies)
    ), 
    
    sector = paste0(sector, '*'),
    sector = factor(sector, levels = rev(sort(unique(as.character(sector)))))
  )  %>%
  arrange(desc(sector), factor(policy_label, levels = fig_policies))

eim_fun_fact_base <- 
  eim_data_nat |>
  filter(policy_label == 'Baseline',
         emissions_type %in% c('lifetime_in_scope_mmt', 'lifetime_clean_grid_co2e_mmt')) |>
  group_by(policy_label) |>
  summarize(
    total = 'total', 
    total_emissions_mmt_base = sum(emissions_mmt,na.rm = T)) |>
  ungroup()

eim_fun_fact <- 
  eim_data_nat |>
  filter(policy_label == 'Elec -25%',
         emissions_type %in% c('lifetime_in_scope_mmt', 'lifetime_clean_grid_co2e_mmt')) |>
  group_by(policy_label) |>
  summarize(
    total = 'total',
    total_emissions_mmt_elec25 = sum(emissions_mmt,na.rm = T)) |>
  ungroup() |>
  left_join(eim_fun_fact_base, by = 'total') |>
  mutate(
    emissions_avoided = total_emissions_mmt_base - total_emissions_mmt_elec25
  )

# of facilities in the money under our policy

eim_fun_fact_2 <- 
  facility_lcoh_df |>
  filter(
    policy_label %in% 'Elec -25%', 
    tech_scenario %in% c('hp_boiler_ee')
  ) |>
  left_join(emissions_df, by = c('facility_id', 'tech_scenario', 'scenario_rank')) |>
  # Average best & worst case 
  group_by(facility_id, tech_scenario, policy_label) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    payback_years = ifelse(is.nan(payback_years), NA, payback_years),
    in_money = if_else(!is.na(payback_years) & payback_years < 15, 1, 0)
  ) |>
  group_by(sector) |>
  summarize(
    n_facilities = n(), 
    n_in_money = sum(in_money, na.rm = T)
  ) |>
  ungroup() |>
  mutate(pct_in_money = n_in_money / n_facilities)
  
  
