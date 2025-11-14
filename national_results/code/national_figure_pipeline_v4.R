# Integrated National Data & Figure Code #
## October 24, 2025 ##

## Works with lcoh_industrialdecarb_facility_level.csv

#### INITIAL SET-UP #### 
# Load Libraries
library(colorspace)
library(dplyr)
library(forcats)
library(ggforce)
library(ggraph)
library(ggplot2)
library(glue)
library(janitor)
library(packcircles)
library(patchwork)
library(purrr)
library(readr)
library(readxl)
library(scales)
library(stringr)
library(tidyr)
library(tidylog)

facility_lcoh_df <- 
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/lcoh_industrialdecarb_facility_level_v2.csv') |>
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
      rate_reduction > 0 & ITC > 0 & PTC == 0~ 
        paste0("Capex -", ITC * 100, "%, Elec -", rate_reduction * 100, "%"),
      rate_reduction == 0 & ITC > 0 & PTC == 0 ~
        paste0("Capex -", ITC * 100, "%"),
      rate_reduction > 0 & ITC == 0 & PTC == 0 ~
        paste0("Elec -", rate_reduction * 100, "%"),
      rate_reduction == 0 & ITC == 0 & PTC == 0 ~ 
        'No Policy', 
      PTC == 5 ~
        'PTC $5/MMBtu', 
      PTC == 10 ~
        'PTC $10/MMBtu', 
      PTC == 15 ~ 
        'PTC $15/MMBtu'
    ) 
  ) |>
  filter(
    # Only keep PTC rows where rate_reduction & ITC are zero 
    !(PTC != 0 & (rate_reduction != 0 | ITC != 0))
  ) |>
  # Dropping some facilities with strange GHGRP reporting of combustion units 
  filter(lcoh != 0)

# # Dropping some duplicates 
# dupes <-
#   facility_lcoh_df |>
#   group_by(facility_id, scenario_rank, policy_label, tech_scenario) |>
#   filter(n() > 1) |>
#   ungroup() |>
#   filter(steam_network_losses != 0)
# 
# facility_lcoh_df <-
#   facility_lcoh_df |>
#   anti_join(dupes, by = c('facility_id', 'tech_scenario', 'scenario_rank', 'policy_label')) |>
#   bind_rows(dupes) |>


# TEMPORARY: just using these emissions figures, not sure if they're right
emissions_df <- 
  read_excel('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/merged_longform.xlsx') |>
  select(-1, -2) |>
  group_by(facility_id) |>
  summarize(
    base_emissions_co2e = sum(unit_ghg_emissions, na.rm = TRUE),
    elec_ghg_emissions = sum(if_else(electrified_option != 'not_electrifiable' & !is_biogenic, unit_ghg_emissions, 0), na.rm = TRUE),
    biogenic_ghg_emissions = sum(if_else(is_biogenic, unit_ghg_emissions, 0), na.rm = TRUE),
    noelec_ghg_emissions = sum(if_else(electrified_option == 'not_electrifiable', unit_ghg_emissions, 0), na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    facility_id = as.character(facility_id)
  ) %>%
  filter(facility_id %in% facility_lcoh_df$facility_id)

#### FIG: CAPEX PLOT ####
fig_scenarios <-
  c("NG Boiler +\nOther Units", 
    "E-Boiler +\nOther Elec Units", 
    "E-Boiler +\nOther Elec Units +\nEnergy Efficiency", 
    "Air-Source HP +\nOther Elec Units", 
    "Air-Source HP +\nOther Elec Units +\nEnergy Efficiency")

sector_colors <- 
  c(
    "Chemicals"     = "#09847A",
    "Food & Beverage" = "#EF5645",
    "Pulp & Paper"  = "#6D7D33"
  )

capex_data <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy') |>
  mutate(
    tech_scenario = recode(
      tech_scenario,
      "baseline"           = fig_scenarios[1],
      "eb_boiler"          = fig_scenarios[2],
      "eb_boiler_ee"       = fig_scenarios[3],
      "hp_boiler"          = fig_scenarios[4],
      "hp_boiler_ee"       = fig_scenarios[5]
    ), 
    tech_scenario = factor(tech_scenario, levels = fig_scenarios), 
    capex_mill = capex / 1000000
  ) |>
  select(facility_id, state, sector, industry_clean, capex_mill, tech_scenario, scenario_rank)

# Capex Plot 
capex_plot <-
  ggplot(capex_data, aes(x = tech_scenario, y = capex_mill, color = sector)) +
  geom_boxplot(
    outlier.shape = 21,
    outlier.size  = 1.8,
    outlier.stroke = 0.25,
    outlier.alpha = 0.8,
    width = 0.2,
    position = position_dodge(width = 0.4)  # widen or shrink this to adjust spacing
    
    #position = position_dodge2(width = .5, preserve = "single", reverse = TRUE)
  ) +
  scale_y_continuous(limits = c(0, 250)) +
  scale_color_manual(
    name = "Sector",
    values = sector_colors
  ) +
  labs(x = NULL, y = "CAPEX ($ millions)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(hjust = .5, size = 10),
    legend.position = c(0.03, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

capex_plot

#### FIG: DELTA OPEX  ####

scenario_labels <- c(
  "eb_boiler"     = "E-Boiler",
  "eb_boiler_ee"  = "E-Boiler + EE",
  "hp_boiler"     = "Air-Source HP",
  "hp_boiler_ee"  = "Air-Source HP + EE"
)

scenario_colors <- c(
  "E-Boiler"     = "#1f78b4",
  "E-Boiler + EE"  = "#a6cee3",
  "Air-Source HP"     = "#33a02c",
  "Air-Source HP + EE"  = "#b2df8a"
)

baseline_opex <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup() |>
  select(-tech_scenario)

delta_opex_data <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario %in% c('eb_boiler', 'eb_boiler_ee', 'hp_boiler', 'hp_boiler_ee') 
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    # Main info vars 
    facility_name = first(facility_name), 
    naics_code = first(naics_code), 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    opex = mean(opex, na.rm = T)
  ) |>
  ungroup() |>
  left_join(baseline_opex, by = 'facility_id') |>
  mutate(
    opex_delta = opex - opex_ng, 
    opex_delta_mill = opex_delta / 1000000,
    tech_scenario = factor(
      tech_scenario,
      levels = names(scenario_labels),
      labels = scenario_labels
    )
  ) 

# Technology scenario x sector delta opex plot 
delta_opex_plot   <- 
  ggplot(delta_opex_data, 
         aes(x = sector, y = opex_delta_mill, color = tech_scenario)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = scenario_colors, labels = scenario_labels) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  
  scale_y_continuous(limits = c(-25, 100))+
  
  labs(
    x = NULL,
    y = "Δ Annual OPEX ($ Millions)",
    color = "Technology"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.83, 0.32),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

delta_opex_plot




#### FIG: COP VS. SPARK GAP ####

unit_df.o <- 
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/dataset_baseline.csv') |>
  # Name & value clean-up 
  rename(
    naics_code = primary_naics, 
    naics_description = naics_title, 
    lcoh = LCOH
  ) |>
  mutate(
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
    )
  )

spark_gap_df <- 
  read_csv('national_results/data/parameters_fuelrange.csv') |>
  select(state, elec_price_high, elec_price_low, ng_price_latest, ng_price_high, ng_price_low) |>
  mutate(
    across(!state, as.numeric), 
    
    #kwh per Mcf
    ng_price_high = ng_price_high / (1.037 * 293.071), 
    ng_price_low = ng_price_low / (1.037 * 293.071),
    
    spark_gap_high = elec_price_high / ng_price_high, 
    spark_gap_low = elec_price_low / ng_price_low, 
    
    spark_gap = case_when(
      spark_gap_high < spark_gap_low ~ spark_gap_high, 
      spark_gap_low < spark_gap_high ~ spark_gap_low
    )
  ) |>
  select(state, spark_gap)

sg_ratio_data <- 
  unit_df.o |>
  filter(
    technology == 'electric boiler' & tech_scenario == 'eb_boiler' |
      technology == 'air source heat pump' & tech_scenario == 'hp_boiler' |
      technology == 'waste heat pump' & tech_scenario == 'hp_boiler_ee', 
    scenario_rank == 'best'
  ) |>
  select(facility_id, industry_clean, state, tech_scenario, technology, efficiency) |>
  distinct() |>
  group_by(state, industry_clean, technology) |>
  summarize(
    efficiency = mean(efficiency, na.rm = T) 
  ) |>
  ungroup() |>
  left_join(spark_gap_df, by = 'state') |> 
  mutate(
    sg_ratio = efficiency / spark_gap, 
    technology = case_when(
      technology == 'air source heat pump' ~ 'Air-Source HP', 
      technology == 'waste heat pump' ~ 'Waste HP', 
      technology == 'electric boiler' ~ 'E-Boiler'
    ), 
    state_sub = paste(state, industry_clean, sep = " ")
  ) |>
  # drop virgin islands
  filter(state != 'VI')

sg_ratio_order <- 
  sg_ratio_data |>
  filter(technology == "Air-Source HP") |>
  select(state_sub, sg_ratio_order = sg_ratio)

sg_ratio_data <- 
  sg_ratio_data |>
  left_join(sg_ratio_order, by = "state_sub") |>
  mutate(state_sub = forcats::fct_reorder(state_sub, sg_ratio_order))

scenario_colors <- c(
  "E-Boiler"     = "#1f78b4",
  "Air-Source HP"     = "#33a02c",
  "Waste HP" = "#FEBC11"
)

# Technology scenario x sector delta opex plot 
sg_ratio_plot   <- 
  ggplot(sg_ratio_data,
         aes(x = state_sub, y = sg_ratio, color = technology)) +
  
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 0.8) +
  
  scale_color_manual(values = scenario_colors) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", size = 0.5) +
  # scale_y_continuous(limits = c(-100, 800), 
  #                    breaks = c(-100, pretty(delta_opex_data$opex_delta, n = 6))) +
  
  labs(
    x = "State-Subsector",
    y = "COP-to-Spark Gap Ratio",
    color = "Technology"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    # Remove tick labels and ticks
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    
    # Remove all grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

sg_ratio_plot



#### FIG: DELTA LCOH PLOT ####

scenario_labels <- c(
  "eb_boiler"     = "E-Boiler",
  "eb_boiler_ee"  = "E-Boiler + EE",
  "hp_boiler"     = "Air-Source HP",
  "hp_boiler_ee"  = "Air-Source HP + EE"
)

scenario_colors <- c(
  "E-Boiler"     = "#1f78b4",
  "E-Boiler + EE"  = "#a6cee3",
  "Air-Source HP"     = "#33a02c",
  "Air-Source HP + EE"  = "#b2df8a"
)

baseline_lcoh <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_ng = mean(lcoh, na.rm = T)
  ) |>
  ungroup() |>
  select(-tech_scenario)

delta_lcoh_data <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario %in% c('eb_boiler', 'eb_boiler_ee', 'hp_boiler', 'hp_boiler_ee') 
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
  left_join(baseline_lcoh, by = 'facility_id') |>
  mutate(
    lcoh_delta = lcoh - lcoh_ng, 
    tech_scenario = factor(
      tech_scenario,
      levels = names(scenario_labels),
      labels = scenario_labels
    )
  ) 

# Technology scenario x sector delta LCOH plot 
delta_lcoh_plot   <- 
  ggplot(delta_lcoh_data,
         aes(x = sector, y = lcoh_delta, color = tech_scenario)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = scenario_colors, labels = scenario_labels) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  
  scale_y_continuous(limits = c(-25, 75)) +
  
  labs(
    x = NULL,
    y = "Δ Levelized Cost of Heat ($/MMBtu)",
    color = "Technology"
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
sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#6D7D33"
)

baseline_bubble <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline'
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_ng = mean(lcoh, na.rm = T)
  ) |>
  select(facility_id, lcoh_ng)

delta_bubble_data <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario == 'hp_boiler_ee' 
  ) |>
  left_join(emissions_df |>
              select(facility_id, elec_ghg_emissions), by = 'facility_id') |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    lcoh = mean(lcoh, na.rm = T), 
    elec_ghg_emissions = mean(elec_ghg_emissions, na.rm = T)
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
    elec_ghg_emissions = sum(elec_ghg_emissions, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    name = paste(state, industry_clean, sep = " "), 
    label = str_wrap(name, width = 10) 
  ) |>
  select(name, label, sector, lcoh_delta, elec_ghg_emissions) 

delta_bubble_data_fnb <- 
  delta_bubble_data |>
  filter(sector == 'Food & Beverage') |>
  slice_min(order_by = lcoh_delta, n = 10) |>
  tbl_graph(nodes = _, edges = NULL) 

delta_bubble_data_pnp <- 
  delta_bubble_data |>
  filter(sector == 'Pulp & Paper') |>
  slice_min(order_by = lcoh_delta, n = 10) |>
  tbl_graph(nodes = _, edges = NULL) 

delta_bubble_data_chem <- 
  delta_bubble_data |>
  filter(sector == 'Chemicals') |>
  slice_min(order_by = lcoh_delta, n = 10) |>
  tbl_graph(nodes = _, edges = NULL) 


# --- compute shared limits across both graphs ---
fill_lims <- 
  range(c(delta_bubble_data_fnb %N>% pull(lcoh_delta),
          delta_bubble_data_pnp %N>% pull(lcoh_delta),
          delta_bubble_data_chem %N>% pull(lcoh_delta)
          ), 
        na.rm = TRUE)

size_lims <-
  range(c(delta_bubble_data_fnb %N>% pull(elec_ghg_emissions),
          delta_bubble_data_pnp %N>% pull(elec_ghg_emissions),
          delta_bubble_data_chem %N>% pull(elec_ghg_emissions)
  ), 
  na.rm = TRUE)

# --- shared scales ---
fill_scale <- 
  scale_fill_gradient2(
  limits = fill_lims,
  low = "#003660", mid = "white", high = "#FEBC11", midpoint = 0,
  name = "Δ LCOH\n($/MMBtu)"
)

size_scale <- 
  scale_size_continuous(
  limits = size_lims,
  range = c(3, 15),
  name = "Base CO2e emissions",
  guide = "none"     
)

# --- normalize elec_ghg_emissions globally so circle sizes are comparable ---
global_min <- 
  min(c(
      delta_bubble_data_fnb %N>% pull(elec_ghg_emissions),
      delta_bubble_data_pnp %N>% pull(elec_ghg_emissions),
      delta_bubble_data_chem %N>% pull(elec_ghg_emissions)
    ), na.rm = TRUE)

global_max <- 
  max(c(
      delta_bubble_data_fnb %N>% pull(elec_ghg_emissions),
      delta_bubble_data_pnp %N>% pull(elec_ghg_emissions),
      delta_bubble_data_chem %N>% pull(elec_ghg_emissions)
  ), na.rm = TRUE)

rescale_emissions <- function(x) {
  (x - global_min) / (global_max - global_min)
}

delta_bubble_data_fnb <- 
  delta_bubble_data_fnb %N>% 
  mutate(weight_scaled = rescale_emissions(elec_ghg_emissions))

delta_bubble_data_pnp  <- 
  delta_bubble_data_pnp  %N>% 
  mutate(weight_scaled = rescale_emissions(elec_ghg_emissions))

delta_bubble_data_chem  <- 
  delta_bubble_data_chem  %N>% 
  mutate(weight_scaled = rescale_emissions(elec_ghg_emissions))


base_plot <- function(graph, title, show_legend = TRUE) {
  ggraph(graph, layout = "circlepack", weight = elec_ghg_emissions) +
    # Circles
    geom_node_circle(
      aes(fill = lcoh_delta),
      color = "grey40", linewidth = 0.3, alpha = 0.9,
      show.legend = TRUE
    ) +
    # Text labels — scaled and filtered for tiny bubbles
    geom_node_text(
      data = function(d) d %>%
        mutate(
          # scale label size roughly with bubble radius
          label_size = rescale(sqrt(pmax(elec_ghg_emissions, 0)),
                               to = c(1.6, 4.0)),  # smaller overall range
          label = str_wrap(label, width = 10)
        ) %>%
        filter(leaf, !is.na(label), label != ""),
      aes(label = label, size = label_size),
      color = "black", lineheight = 0.9,
      hjust = 0.5, vjust = 0.5, check_overlap = TRUE,
      show.legend = FALSE
    ) +
    fill_scale +
    ggtitle(title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = if (show_legend) "right" else "none"
    ) +
    coord_equal() +
    scale_size_identity()
}

delta_bubble_plot <- 
  (base_plot(delta_bubble_data_fnb, "Food & Beverage") + 
   base_plot(delta_bubble_data_pnp, "Pulp & Paper") +
   base_plot(delta_bubble_data_chem, 'Chemicals')
  ) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# delta_bubble_plot2 <- 
#   (base_plot2(delta_bubble_data_fnb, "Food & Beverage") + 
#    base_plot2(delta_bubble_data_pnp, "Pulp & Paper") +
#    base_plot2(delta_bubble_data_chem, 'Chemicals')
#   ) + 
#   plot_layout(guides = "collect") &
#   theme(legend.position = "right")

delta_bubble_plot
# delta_bubble_plot2

#### FIG: PAYBACK PLOT NO POLICY ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#6D7D33"
)

payback_data_ng_nopol <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline', 
         scenario_rank == 'best') |>
  rename(
    opex_ng = opex, 
    capex_ng = capex
  ) |>
  select(facility_id, scenario_rank, opex_ng, capex_ng)


payback_data_nopol <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee' &
      policy_label == 'No Policy' &
      scenario_rank == 'best'
  ) |>
  rename(
    opex_hp = opex, 
    capex_hp = capex
  ) |>
  left_join(payback_data_ng_nopol, by = 'facility_id') |>
  
  group_by(state, industry_clean) |>
  summarize(
    opex_hp = mean(opex_hp, na.rm = T), 
    capex_hp = mean(capex_hp, na.rm = T), 
    opex_ng = mean(opex_ng, na.rm = T), 
    capex_ng = mean(capex_ng, na.rm = T), 
    
    sector = first(sector)
  ) |>
  ungroup() |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng, 
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_),
    
    state_sub = paste(state, industry_clean, sep = " ")
  ) |>
  filter(!is.na(payback_years), 
         payback_years < 5) 

payback_plot_nopol <- 
  ggplot(payback_data_nopol,
         aes(x = reorder(state_sub, payback_years, decreasing = TRUE),
             y = payback_years,
             fill = sector) 
         ) +
  
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  
  scale_fill_manual(values = sector_colors) +
  
  #scale_y_continuous(limits = c(0,10)) +
  
  # scale_y_continuous(
  #   breaks = seq(0, ceiling(max(payback_data_nopol$payback_years, na.rm = TRUE) / 25) * 25, 25)
  # ) +
  
  labs(
    x = NULL,
    y = "Average Payback (Years)",
    fill = "Sector"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.position = c(0.95, 0.7),     
    legend.justification = c(1, 0),      
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.2),
    axis.text.y = element_text(size = 7)
  )

payback_plot_nopol


#### FIG: ABATEMENT COST CURVE ####

fig_industry_colors <- c(
  # --- Chemicals (teal family) ---
  "Ethyl Alcohol"              = "#5EBAC3",
  "Fertilizers"                = "#09847A",
  "Industrial Gas"             = "#4FA19E",
  "Inorganic Chemicals"        = "#3D8A86",
  "Petrochemicals"             = "#2E726E",
  "Phosphatic Fertilizer"      = "#65C2BA",
  "Plastics & Resins"          = "#0FA396",
  "Rubber"                     = "#065C54",
  "Soybeans"                   = "#7AD1C8",
  "Wood Chemicals"             = "#046F6E",
  
  # --- Food & Beverage (coral family) ---
  "Beet Sugar"                 = "#FDE1DA",
  "Breakfast Cereal"           = "#FBC4B3",
  "Breweries"                  = "#F9B4A3",
  "Cane Sugar"                 = "#F8A18C",
  "Canning"                    = "#F58C73",
  "Cheese"                     = "#F1735E",
  "Distilleries"               = "#F58F7A",
  "Dried Foods"                = "#F27D63",
  "Fats & Oils"                = "#EF5645",
  "Frozen Foods"               = "#EC3E32",
  "Meat (non-poultry)"         = "#E53828",
  "Milk"                       = "#C7402D",
  "Other Dairy"                = "#B13222",
  "Poultry"                    = "#D4503C",
  "Rendering"                  = "#9B2C24",
  "Snack Foods"                = "#F8A18C",
  "Specialty Canning"          = "#FBC4B3",
  "Spices"                     = "#FDE1DA",
  "Wet Corn Milling"           = "#F8A18C",
  
  # --- Pulp & Paper (olive family) ---
  "Pulp & Paper"               = "#6D7D33",
  "Toilet Paper"               = "#8DA452"
)


r <- .065
t <- 30
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
  group_by(industry_clean) |>
  summarize(
    total_heat = sum(elec_orig_process_unit_heat_demand, na.rm = T),
    lifetime_cost_hp = sum(lifetime_cost_hp, na.rm = T), 
    lifetime_cost_ng = sum(lifetime_cost_ng, na.rm = T), 
    elec_ghg_emissions = sum(elec_ghg_emissions, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    lifetime_cost = lifetime_cost_hp - lifetime_cost_ng, 
    lifetime_emissions_reduc = elec_ghg_emissions * discount_sum, 
    mac = lifetime_cost / lifetime_emissions_reduc, 
    
    # some diagnostics
    emissions_avoided_per_heat = elec_ghg_emissions / total_heat,
    implied_factor = total_heat / lifetime_emissions_reduc  
  ) |>
  arrange(mac) |>
  mutate(
    x_min_t  = dplyr::lag(cumsum(lifetime_emissions_reduc), default = 0),
    x_max_t  = x_min_t + lifetime_emissions_reduc,
    x_min_mt = x_min_t / 1e6,
    x_max_mt = x_max_t / 1e6,
    y_min    = 0,
    y_max    = mac, 
    
    industry_clean = forcats::fct_reorder(industry_clean, mac, .desc = FALSE)
  ) |>
  # When the sector is too small, mac ends up looking really high. we'll drop a few 
  filter(mac < 1000)

acc_plot <- 
  ggplot(acc_data) +
  geom_rect(
    aes(xmin = x_min_mt, xmax = x_max_mt, ymin = pmin(y_min, y_max), ymax = pmax(y_min, y_max),
        fill = industry_clean),
    color = "grey60"
  ) +
  scale_fill_manual(
    name = "Subsector (ordered by cost)",          
    values = fig_industry_colors
    # guide = guide_legend(reverse = TRUE)  
  ) +
  # optional outline along the tops (emphasize "steps")
  geom_segment(
    aes(x = x_min_mt, xend = x_max_mt, y = y_max, yend = y_max),
    linewidth = 0.4, color = "grey20"
  ) +
  scale_x_continuous("Cumulative Abatement Potential (MtCO2e)", expand = c(0, 0)) +
  scale_y_continuous("Abatement Cost ($/tCO2e)", expand = c(0, 0)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"), 
    legend.text = element_text(size = 8),       # smaller legend text
    legend.title = element_text(size = 9),      # smaller legend title
    legend.key.size = unit(0.4, "cm"),          # smaller color boxes
    legend.box.spacing = unit(0.2, "cm")        # less space between legends
  )

acc_plot

#### FIG: DELTA LCOH -- POLICY PLOT ####

# select policy scenarios to show 
fig_policies <- c(
  'No Policy',
  'Capex -30%', 
  'Capex -100%', 
  'Elec -25%', 
  'Elec -50%', 
  'Capex -30%, Elec -25%', 
  'PTC $10/MMBtu'
)

sector_colors <-
  c(
    "Chemicals"     = "#09847A",
    "Food & Beverage" = "#EF5645",
    "Pulp & Paper"  = "#6D7D33"
  )

baseline_lcoh <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_ng = mean(lcoh, na.rm = T)
  ) |>
  ungroup() |>
  select(-tech_scenario)

delta_policy_data <- 
  facility_lcoh_df |>
  filter(
    policy_label %in% fig_policies, 
    tech_scenario == 'hp_boiler_ee'
  ) |>
  group_by(facility_id, policy_label) |>
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
  left_join(baseline_lcoh, by = 'facility_id') |>
  mutate(
    lcoh_delta = (lcoh - lcoh_ng), 
    policy_label = factor(policy_label, levels = fig_policies)
  ) 

delta_policy_plot   <- 
  ggplot(delta_policy_data,
         aes(x = policy_label, y = lcoh_delta, color = sector)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = sector_colors) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  
  scale_y_continuous(limits = c(-20, 50)) +
  
  labs(
    x = NULL,
    y = "Δ Levelized Cost of Heat ($/MMBtu)",
    color = "Sector"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    # legend.position = c(0.81, 0.94),
    # legend.justification = c(0, 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.background = element_rect(color = "black", linewidth = 0.2),
    # legend.key.size = unit(0.4, "lines"),        # shrink symbol size
    # legend.key.height = unit(0.4, "lines"),      # shorter legend keys
    # legend.key.width = unit(0.6, "lines"),
    # legend.spacing.y = unit(0.1, "lines"),       # reduce vertical space between keys
    # legend.spacing.x = unit(0.2, "lines"),       # reduce horizontal space between keys
    legend.margin = margin(2.5, 2.5, 2.5, 2.5)           # tighten internal margins of the box
  )

delta_policy_plot


#### FIG: DELTA LCOH BUBBLE PTC PLOT ####
sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#6D7D33"
)

baseline_bubble <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline'
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_ng = mean(lcoh, na.rm = T)
  ) |>
  select(facility_id, lcoh_ng)

delta_bubble_data <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario == 'hp_boiler_ee' 
  ) |>
  left_join(emissions_df |>
              select(facility_id, elec_ghg_emissions), by = 'facility_id') |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    lcoh = mean(lcoh, na.rm = T), 
    elec_ghg_emissions = mean(elec_ghg_emissions, na.rm = T)
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
    elec_ghg_emissions = sum(elec_ghg_emissions, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    name = paste(state, industry_clean, sep = " "), 
    label = str_wrap(name, width = 10) 
  ) |>
  select(name, label, sector, lcoh_delta, elec_ghg_emissions) 

delta_bubble_data_fnb <- 
  delta_bubble_data |>
  filter(sector == 'Food & Beverage') |>
  slice_min(order_by = lcoh_delta, n = 10) |>
  tbl_graph(nodes = _, edges = NULL) 

delta_bubble_data_pnp <- 
  delta_bubble_data |>
  filter(sector == 'Pulp & Paper') |>
  slice_min(order_by = lcoh_delta, n = 10) |>
  tbl_graph(nodes = _, edges = NULL) 

delta_bubble_data_chem <- 
  delta_bubble_data |>
  filter(sector == 'Chemicals') |>
  slice_min(order_by = lcoh_delta, n = 10) |>
  tbl_graph(nodes = _, edges = NULL) 


# --- compute shared limits across both graphs ---
fill_lims <- 
  range(c(delta_bubble_data_fnb %N>% pull(lcoh_delta),
          delta_bubble_data_pnp %N>% pull(lcoh_delta),
          delta_bubble_data_chem %N>% pull(lcoh_delta)
  ), 
  na.rm = TRUE)

size_lims <-
  range(c(delta_bubble_data_fnb %N>% pull(elec_ghg_emissions),
          delta_bubble_data_pnp %N>% pull(elec_ghg_emissions),
          delta_bubble_data_chem %N>% pull(elec_ghg_emissions)
  ), 
  na.rm = TRUE)

# --- shared scales ---
fill_scale <- 
  scale_fill_gradient2(
    limits = fill_lims,
    low = "#003660", mid = "white", high = "#FEBC11", midpoint = 0,
    name = "Δ LCOH\n($/MMBtu)"
  )

size_scale <- 
  scale_size_continuous(
    limits = size_lims,
    range = c(3, 15),
    name = "Base CO2e emissions",
    guide = "none"     
  )

# --- normalize elec_ghg_emissions globally so circle sizes are comparable ---
global_min <- 
  min(c(
    delta_bubble_data_fnb %N>% pull(elec_ghg_emissions),
    delta_bubble_data_pnp %N>% pull(elec_ghg_emissions),
    delta_bubble_data_chem %N>% pull(elec_ghg_emissions)
  ), na.rm = TRUE)

global_max <- 
  max(c(
    delta_bubble_data_fnb %N>% pull(elec_ghg_emissions),
    delta_bubble_data_pnp %N>% pull(elec_ghg_emissions),
    delta_bubble_data_chem %N>% pull(elec_ghg_emissions)
  ), na.rm = TRUE)

rescale_emissions <- function(x) {
  (x - global_min) / (global_max - global_min)
}

delta_bubble_data_fnb <- 
  delta_bubble_data_fnb %N>% 
  mutate(weight_scaled = rescale_emissions(elec_ghg_emissions))

delta_bubble_data_pnp  <- 
  delta_bubble_data_pnp  %N>% 
  mutate(weight_scaled = rescale_emissions(elec_ghg_emissions))

delta_bubble_data_chem  <- 
  delta_bubble_data_chem  %N>% 
  mutate(weight_scaled = rescale_emissions(elec_ghg_emissions))


base_plot <- function(graph, title, show_legend = TRUE) {
  ggraph(graph, layout = "circlepack", weight = elec_ghg_emissions) +
    # Circles
    geom_node_circle(
      aes(fill = lcoh_delta),
      color = "grey40", linewidth = 0.3, alpha = 0.9,
      show.legend = TRUE
    ) +
    # Text labels — scaled and filtered for tiny bubbles
    geom_node_text(
      data = function(d) d %>%
        mutate(
          # scale label size roughly with bubble radius
          label_size = rescale(sqrt(pmax(elec_ghg_emissions, 0)),
                               to = c(1.6, 4.0)),  # smaller overall range
          label = str_wrap(label, width = 10)
        ) %>%
        filter(leaf, !is.na(label), label != ""),
      aes(label = label, size = label_size),
      color = "black", lineheight = 0.9,
      hjust = 0.5, vjust = 0.5, check_overlap = TRUE,
      show.legend = FALSE
    ) +
    fill_scale +
    ggtitle(title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = if (show_legend) "right" else "none"
    ) +
    coord_equal() +
    scale_size_identity()
}

delta_bubble_plot <- 
  (base_plot(delta_bubble_data_fnb, "Food & Beverage") + 
     base_plot(delta_bubble_data_pnp, "Pulp & Paper") +
     base_plot(delta_bubble_data_chem, 'Chemicals')
  ) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# delta_bubble_plot2 <- 
#   (base_plot2(delta_bubble_data_fnb, "Food & Beverage") + 
#    base_plot2(delta_bubble_data_pnp, "Pulp & Paper") +
#    base_plot2(delta_bubble_data_chem, 'Chemicals')
#   ) + 
#   plot_layout(guides = "collect") &
#   theme(legend.position = "right")

delta_bubble_plot
# delta_bubble_plot2

#### FIG: NATIONAL EMISSIONS "IN THE MONEY" ####
fig_policies <- c(
  "Total In-Scope Emissions", 
  "No Policy",
  "Capex -100%", 
  "Elec -25%", 
  "Elec -50%", 
  "PTC $10/MMBtu"
)

fig_policy_colors <- c(
  "Total In-Scope Emissions" = "grey70",
  "No Policy"     = lighten('#09847A', amount = 0.45),  # lightest
  "Capex -100%"   = lighten('#09847A', amount = 0.25),
  "Elec -25%"     = '#09847A',
  "Elec -50%"     = darken('#09847A', amount = 0.25), 
  "PTC $10/MMBtu" = darken('#09847A', amount = 0.45)
)

baseline_eim_nat <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  # Average best & worst case
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_ng = mean(lcoh, na.rm = T)
  ) |>
  ungroup() |>
  select(-tech_scenario)

total_elec_ghg_emissions_nat <- 
  emissions_df %>%
  left_join(facility_lcoh_df |>
              distinct(facility_id, sector), 
            by = 'facility_id') |>
  group_by(sector) %>%          
  summarize(
    notinmoney_co2e = sum(elec_ghg_emissions, na.rm = T),
    policy_label = "Total In-Scope Emissions"
  ) %>%
  select(sector, policy_label, notinmoney_co2e)

eim_data_nat <- 
  facility_lcoh_df |>
  filter(
    policy_label %in% fig_policies, 
    tech_scenario %in% c('hp_boiler_ee')
  ) |>
  # Average best & worst case 
  group_by(facility_id, tech_scenario, policy_label) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    lcoh = mean(lcoh, na.rm = T)
  ) |>
  left_join(emissions_df, by = 'facility_id') |>
  left_join(baseline_eim_nat, by = 'facility_id') |>
  mutate(
    in_money = if_else(lcoh < lcoh_ng, 1, 0)
  ) %>%
  
  # Getting sector-policy emissions in the money 
  group_by(sector, policy_label) %>%
  summarize(
    lcoh = mean(lcoh, na.rm = T), 
    notinmoney_co2e = sum(elec_ghg_emissions[in_money == 0], #+ 
                          # noelec_ghg_emissions[in_money == 0] +
                          # biogenic_ghg_emissions[in_money == 0],
                          na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Add the sector totals
  bind_rows(total_elec_ghg_emissions_nat) %>%
  
  mutate(
    notinmoney_co2e_Mt = notinmoney_co2e / 1e6,
    #notinmoney_co2e_prop = (notinmoney_co2e / total_elec_ghg) * 100, 
    policy_label = factor(
      policy_label,
      levels = rev(fig_policies)
    ), 
    sector = factor(sector, levels = rev(sort(unique(as.character(sector)))))
  )  %>%
  arrange(desc(sector), factor(policy_label, levels = fig_policies))


eim_plot_nat <- 
  ggplot() +
  
  geom_col(data = eim_data_nat,
           aes(x = notinmoney_co2e_Mt, y = sector, fill = policy_label),
           position = position_dodge(width = 0.6), 
           width = .4) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  scale_fill_manual(
    values = fig_policy_colors,
    breaks = fig_policies,
    labels = fig_policies,
    name = NULL
  ) +
  
  labs(x = "Emissions (MtCO2e)", y = NULL, fill = "Policy" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.95, 0.05),     
    legend.justification = c(1, 0),      
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.3, "cm"),         # shrink legend boxes
    legend.key.height = unit(0.3, "cm"),       # optional: shorter boxes
    legend.key.width = unit(0.3, "cm"),
    legend.spacing.y = unit(0.2, "cm")         # tighter vertical spacing
  )

eim_plot_nat

#### FIG: STATE EMISSIONS "IN THE MONEY" ####
fig_policies <- c(
  "Total In-Scope Emissions", 
  "No Policy",
  "Capex -100%", 
  "Elec -25%", 
  "Elec -50%", 
  "PTC $10/MMBtu"
)

fig_policy_colors <- c(
  "Total In-Scope Emissions" = "grey70",
  "No Policy"     = lighten('#09847A', amount = 0.45),  # lightest
  "Capex -100%"   = lighten('#09847A', amount = 0.25),
  "Elec -25%"     = '#09847A',
  "Elec -50%"     = darken('#09847A', amount = 0.25), 
  "PTC $10/MMBtu" = darken('#09847A', amount = 0.45)
)

fig_states <- c(
  'CA', 'IL', 'NY', 'MD', 'MN', 'MA', 
  'WI', 'MI', 'PA', 'CO', 'OR'
)

baseline_eim <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  # Average best & worst case
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_ng = mean(lcoh, na.rm = T)
  ) |>
  ungroup() |>
  select(-tech_scenario)

total_elec_ghg_emissions <- 
  emissions_df %>%
  left_join(facility_lcoh_df |>
              distinct(facility_id, state), 
            by = 'facility_id') |>
  group_by(state) %>%          
  summarize(
    notinmoney_co2e = sum(elec_ghg_emissions, na.rm = T),
    policy_label = "Total In-Scope Emissions"
  ) %>%
  filter(state %in% fig_states) %>%
  select(state, policy_label, notinmoney_co2e)

eim_data <- 
  facility_lcoh_df |>
  filter(
    policy_label %in% fig_policies, 
    tech_scenario %in% c('hp_boiler_ee')
  ) |>
  # Average best & worst case 
  group_by(facility_id, tech_scenario, policy_label) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    lcoh = mean(lcoh, na.rm = T)
  ) |>
  left_join(emissions_df, by = 'facility_id') |>
  left_join(baseline_eim, by = 'facility_id') |>
  mutate(
    in_money = if_else(lcoh < lcoh_ng, 1, 0)
  ) %>%
  
  # Getting state-policy emissions in the money 
  group_by(state, policy_label) %>%
  summarize(
    lcoh = mean(lcoh, na.rm = T), 
    notinmoney_co2e = sum(elec_ghg_emissions[in_money == 0], #+ 
                          # noelec_ghg_emissions[in_money == 0] +
                          # biogenic_ghg_emissions[in_money == 0],
                          na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Add the state totals
  bind_rows(total_elec_ghg_emissions) %>%
  filter(state %in% fig_states) %>%
  
  left_join(
    total_elec_ghg_emissions %>%
      rename(total_emis = notinmoney_co2e) %>%
      select(state, total_emis),
    by = "state"
  ) %>%
  mutate(
    state = fct_reorder(state, total_emis, .desc = FALSE),  # now orders from high to low
    policy_label = factor(policy_label, levels = rev(fig_policies)), 
    notinmoney_co2e_Mt = notinmoney_co2e / 1e6
  )
  
eim_plot <- 
  ggplot() +
  
  geom_col(data = eim_data,
           aes(x = notinmoney_co2e_Mt, y = state, fill = policy_label),
           position = position_dodge(width = 0.6), 
           width = .4) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  scale_fill_manual(
    values = fig_policy_colors,
    breaks = fig_policies,
    labels = fig_policies,
    name = NULL
  ) +
  
  labs(x = "Emissions (MtCO2e)", y = NULL, fill = "Policy" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.95, 0.05),     
    legend.justification = c(1, 0),      
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.2),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

eim_plot

#### FIG: PAYBACK POLICY PLOT  ####

fig_policies <- 
  c(
    "Capex -50%", 
    "Elec -25%", 
    "No Policy"
  )

fig_policy_colors <- c(
  "No Policy"     = '#FEBC11',
  "Elec -25%"     = "#09847A",
  "Capex -50%"   = '#003660'
)

payback_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline', 
         scenario_rank == 'best') |>
  rename(
    opex_ng = opex, 
    capex_ng = capex
  ) |>
  select(facility_id, opex_ng, capex_ng)

check <- 
  facility_lcoh_df |>
  group_by(facility_id, tech_scenario, policy_label, scenario_rank) |>
  filter(n()>1)

check <- 
  facility_lcoh_df |>
  group_by(facility_id) |>
  summarize(has_baseline = any(tech_scenario == "baseline")) |>
  filter(!has_baseline)

payback_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == "hp_boiler_ee",
    policy_label %in% fig_policies,
    scenario_rank == "best"
  ) |>
  rename(
    opex_hp = opex,
    capex_hp = capex
  ) |>
  left_join(payback_data_ng, by = "facility_id") |>
  group_by(state, industry_clean, policy_label) |>
  summarize(
    opex_hp = mean(opex_hp, na.rm = TRUE),
    capex_hp = mean(capex_hp, na.rm = TRUE),
    opex_ng = mean(opex_ng, na.rm = TRUE),
    capex_ng = mean(capex_ng, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng,
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_),
    state_sub = paste(state, industry_clean, sep = " "),
    policy_label = factor(policy_label, levels = fig_policies)
  ) |>
  filter(!is.na(payback_years)) |>
  group_by(state_sub) |>
  mutate(
    payback_no_policy = payback_years[policy_label == "No Policy"][1]
  ) |>
  ungroup() |>
  filter(
    !is.na(payback_no_policy),
    payback_no_policy >= 5,
    payback_no_policy <= 15
  ) |>
  select(-payback_no_policy)

payback_plot <- 
  ggplot(payback_data,
         aes(x = reorder(state_sub, payback_years, decreasing = TRUE),
             y = payback_years,
             fill = policy_label)
         ) +
  
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  
  scale_fill_manual(values = fig_policy_colors) +
  
  labs(
    x = NULL,
    y = "Average Payback (Years)",
    fill = "Policy"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.position = c(0.95, 0.7),     
    legend.justification = c(1, 0),      
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.2),
    axis.text.y = element_text(size = 9)
  )

payback_plot


#### FIG: PERCENT IN PAYBACK -- POLICY ####

# select policy scenarios to show 
fig_policies <- c(
  'No Policy',
  'Elec -25%', 
  'Elec -50%', 
  'PTC $10/MMBtu'
)

fig_policy_colors <- c(
  "No Policy"                = "#BFBFBF",  # neutral gray
  "Elec -25%"                = "#31A354",  # medium green
  "Elec -50%"                = "#006D2C",  # dark green
  "PTC $10/MMBtu"            = "#E6550D"   # orange
)

scenario_labels <- c(
  "eb_boiler"     = "E-Boiler",
  "eb_boiler_ee"  = "E-Boiler (EE+)",
  "hp_boiler"     = "ASHP",
  "hp_boiler_ee"  = "ASHP (EE+)"
)

baseline_opex <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup() |>
  select(-tech_scenario)

in_payback_data <- 
  facility_lcoh_df |>
  filter(
    policy_label %in% fig_policies, 
    tech_scenario %in% c('eb_boiler', 'eb_boiler_ee', 'hp_boiler', 'hp_boiler_ee') 
  ) |>
  group_by(facility_id, tech_scenario, policy_label) |>
  summarize(
    # Main info vars 
    facility_name = first(facility_name), 
    naics_code = first(naics_code), 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    opex = mean(opex, na.rm = T)
  ) |>
  ungroup() |>
  left_join(baseline_opex, by = 'facility_id') |>
  mutate(
    opex_delta = opex - opex_ng, 
    in_payback = if_else(opex_delta <= 0, 1, 0),
    tech_scenario = factor(
      tech_scenario,
      levels = names(scenario_labels),
      labels = scenario_labels
    ), 
    policy_label = factor(policy_label, levels = fig_policies)
  ) |>
  group_by(tech_scenario, policy_label) |>
  summarize(
    n_facilities = n(),
    n_in_payback = sum(in_payback, na.rm = TRUE),
    pct_in_payback = (n_in_payback / n_facilities) * 100
  ) |>
  ungroup()

# Technology scenario x sector delta opex plot 
in_payback_plot   <- 
  ggplot() +
  
  geom_col(data = in_payback_data,
           aes(x = tech_scenario, y = pct_in_payback, fill = policy_label),
           position = position_dodge(width = 0.6), 
           width = .4) +
  
  #scale_color_manual(values = sector_colors) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(
    values = fig_policy_colors,
    breaks = fig_policies,
    labels = fig_policies,
    name = NULL
  ) +
  
  labs(x = NULL , y = "Percent of Facilities W/ Positive Payback", fill = "Policy" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.2, 0.7),     
    legend.justification = c(1, 0),      
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    # legend.key.size = unit(0.3, "cm"),         # shrink legend boxes
    # legend.key.height = unit(0.3, "cm"),       # optional: shorter boxes
    # legend.key.width = unit(0.3, "cm"),
    # legend.spacing.y = unit(0.2, "cm")         # tighter vertical spacing
  )

in_payback_plot


#### FIG: PERCENT OF HEAT IN-SCOPE ####
heat_df <- 
  read_excel('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/merged_longform.xlsx') |>
  select(-1, -2) |>
  rename(
    naics_code = primary_naics, 
    naics_description = naics_title
  ) |>
  mutate(
    facility_id = as.character(facility_id), 
    sector = case_when(
      str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food & Beverage",
      str_starts(naics_code, "325") ~ "Chemicals",
      str_starts(naics_code, "322") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    ), 
    
    scope_category = case_when(
      `Direct electrification` == 'Y' ~ 'In Scope', 
      `Direct electrification` == 'N' & is_biogenic == FALSE ~ 'High Temperature Process', 
      `Direct electrification` == 'N' & is_biogenic == TRUE ~ 'Biogenic Fueled Process' 
    )
  ) |>
  group_by(sector, scope_category) |>
  summarize(
    heat_total = sum(process_unit_heat_demand, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(sector) |>
  mutate(
    pct_heat = 100 * heat_total / sum(heat_total, na.rm = TRUE)
  ) |>
  ungroup()

heat_plot <- 
  ggplot(heat_df, aes(x = sector, y = pct_heat, fill = scope_category)) +
  geom_col(position = "fill") +   # or position = "stack" for raw totals
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Share of Total Process Heat",
    fill = "Scope Category"
  ) +
  theme_bw(base_size = 14)

heat_plot
  
#### SAVE FIGURES ####

safe_save <- function(plot_obj, width, height, dpi = 300,
                      dir = "national_results/outputs") {
  obj_name <- deparse(substitute(plot_obj))
  file_name <- glue::glue("{dir}/{obj_name}_{format(Sys.Date(), '%Y%m%d')}.png")
  
  if (exists(obj_name, envir = parent.frame()) && inherits(plot_obj, "gg")) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    ggplot2::ggsave(file_name, plot_obj, width = width, height = height, dpi = dpi)
    message(glue::glue("Saved {obj_name} → {file_name}"))
  } else {
    message(glue::glue("Skipped: {obj_name} not found or not a ggplot object."))
  }
}

safe_save(capex_plot, width = 8, height = 5)
safe_save(delta_lcoh_plot, width = 8, height = 5)
safe_save(delta_bubble_plot, width = 8, height = 5)
safe_save(delta_policy_plot, width = 8, height = 5)
safe_save(delta_opex_plot, width = 8, height = 5)
safe_save(eim_plot, width = 8, height = 5)
safe_save(eim_plot_nat, width = 8, height = 5)
safe_save(payback_plot_nopol, width = 5, height = 8)
safe_save(payback_plot, width = 8, height = 5)
safe_save(acc_plot, width = 10, height = 5)
safe_save(acc_statesub_plot, width = 10, height = 5)
safe_save(sg_ratio_plot, width = 8, height = 5)

#### DATA CHECKS ####

delta_bubble_data

eim_data
acc_statesub_data


r <- .065
t <- 30
disc_ord <- (1 - (1 + r)^(-t)) / r
disc_due <- disc_ord * (1 + r)   # match Python's i = 0..t-1

check_1 <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline'
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_ng = mean(lcoh, na.rm = T), 
    opex_ng = mean(opex, na.rm = T), 
    capex_ng = mean(capex, na.rm = T), 
    heat_ng = mean(elec_orig_process_unit_heat_demand, na.rm = T)
  ) |>
  mutate(
    lifetime_cost_ng = capex_ng + (opex_ng * disc_due)
  )

check_2 <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario == 'hp_boiler_ee' 
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_hp = mean(lcoh, na.rm = T), 
    opex_hp = mean(opex, na.rm = T), 
    capex_hp = mean(capex, na.rm = T), 
    heat_hp = mean(elec_orig_process_unit_heat_demand, na.rm = T)
  ) |>
  mutate(
    lifetime_cost_hp = capex_hp + (opex_hp * disc_due)
  ) |>
  left_join(
    check_1, by = 'facility_id'
  ) |>
  mutate(
    capex_delta = capex_hp - capex_ng, 
    opex_delta = opex_hp - opex_ng,
    lcoh_delta = lcoh_hp - lcoh_ng, 
    lifetime_cost_delta = lifetime_cost_hp - lifetime_cost_ng, 
    heat_delta = heat_hp - heat_ng
  ) |>
  filter(abs(lcoh_delta) > 1e-3 | abs(lifetime_cost_delta) > 1e-3) |>
  filter(
    lcoh_delta > 0, 
    lifetime_cost_delta < 0
  )

check <- check_2 |>
  mutate(
    lcoh_hp_recalc = (capex_hp + opex_hp * sum((1 / (1 + r))^(0:(t - 1)))) / 
      sum((heat_hp / (1 + r))^(0:(t - 1))),
    lcoh_ng_recalc = (capex_ng + opex_ng * sum((1 / (1 + r))^(0:(t - 1)))) / 
      sum((heat_ng / (1 + r))^(0:(t - 1))),
    lcoh_delta_recalc = lcoh_hp_recalc - lcoh_ng_recalc
  )

check_test <- 
  check_2 |>
  mutate(
    # ordinary (R lifetime_cost way)
    lcoh_hp_ord = (capex_hp + opex_hp * disc_ord) / (heat_hp * disc_ord),
    lcoh_ng_ord = (capex_ng + opex_ng * disc_ord) / (heat_ng * disc_ord),
    lcoh_delta_ord = lcoh_hp_ord - lcoh_ng_ord,
    
    # due (Python LCOH way)
    lcoh_hp_due = (capex_hp + opex_hp * disc_due) / (heat_hp * disc_due),
    lcoh_ng_due = (capex_ng + opex_ng * disc_due) / (heat_ng * disc_due),
    lcoh_delta_due = lcoh_hp_due - lcoh_ng_due
  )

# Compare signs vs lifetime_cost under each convention
check_test |>
  summarize(
    flips_vs_ord = sum(sign(lcoh_delta_ord) != sign(lifetime_cost_delta), na.rm = TRUE),
    flips_vs_due = sum(sign(lcoh_delta_due) != sign(lifetime_cost_delta), na.rm = TRUE)
  )


write_csv(check_2, '/Users/nmariano/Downloads/mac_lcoh_check.csv')

write_csv(facility_lcoh_df |>
            filter(tech_scenario %in% c('baseline', 'hp_boiler_ee'), 
                   policy_label == 'No Policy'), '/Users/nmariano/Downloads/facility_lcoh_df.csv')

check <- 
  facility_lcoh_df |>
  group_by(facility_id) |>
  filter(any(is.na(lcoh)))

check <- 
  facility_lcoh_df |>
  filter(lcoh == 0 | is.na(lcoh)) |>
  distinct(facility_id, .keep_all = TRUE)

#### FIG: ABATEMENT COST CURVE - statesubs ####

fig_industry_colors <- c(
  # --- Chemicals (teal family) ---
  "Ethyl Alcohol"              = "#5EBAC3",
  "Fertilizers"                = "#09847A",
  "Industrial Gas"             = "#4FA19E",
  "Inorganic Chemicals"        = "#3D8A86",
  "Petrochemicals"             = "#2E726E",
  "Phosphatic Fertilizer"      = "#65C2BA",
  "Plastics & Resins"          = "#0FA396",
  "Rubber"                     = "#065C54",
  "Soybeans"                   = "#7AD1C8",
  "Wood Chemicals"             = "#046F6E",
  
  # --- Food & Beverage (coral family) ---
  "Beet Sugar"                 = "#FDE1DA",
  "Breakfast Cereal"           = "#FBC4B3",
  "Breweries"                  = "#F9B4A3",
  "Cane Sugar"                 = "#F8A18C",
  "Canning"                    = "#F58C73",
  "Cheese"                     = "#F1735E",
  "Distilleries"               = "#F58F7A",
  "Dried Foods"                = "#F27D63",
  "Fats & Oils"                = "#EF5645",
  "Frozen Foods"               = "#EC3E32",
  "Meat (non-poultry)"         = "#E53828",
  "Milk"                       = "#C7402D",
  "Other Dairy"                = "#B13222",
  "Poultry"                    = "#D4503C",
  "Rendering"                  = "#9B2C24",
  "Snack Foods"                = "#F8A18C",
  "Specialty Canning"          = "#FBC4B3",
  "Spices"                     = "#FDE1DA",
  "Wet Corn Milling"           = "#F8A18C",
  
  # --- Pulp & Paper (olive family) ---
  "Pulp & Paper"               = "#6D7D33",
  "Toilet Paper"               = "#8DA452"
)


r <- .065
t <- 30
discount_sum <- (1 - (1 + r)^(-t)) / r

acc_statesub_data_ng <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'baseline', 
    policy_label == 'No Policy'
  ) |>
  group_by(facility_id) |>
  summarize(
    industry_clean = first(industry_clean), 
    state = first(state),
    
    opex_ng = mean(opex, na.rm = T), 
    capex_ng = mean(capex, na.rm = T), 
    elec_orig_process_unit_heat_demand = mean(elec_orig_process_unit_heat_demand, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    lifetime_cost_ng = capex_ng + (opex_ng * discount_sum)
  ) 

acc_statesub_data_hp <- 
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

acc_statesub_data <- 
  left_join(acc_statesub_data_ng, acc_statesub_data_hp, by = 'facility_id') |>
  left_join(emissions_df, by = 'facility_id') |>
  group_by(state, industry_clean) |>
  summarize(
    total_heat = sum(elec_orig_process_unit_heat_demand, na.rm = T),
    lifetime_cost_hp = sum(lifetime_cost_hp, na.rm = T), 
    lifetime_cost_ng = sum(lifetime_cost_ng, na.rm = T), 
    elec_ghg_emissions = sum(elec_ghg_emissions, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    lifetime_cost = lifetime_cost_hp - lifetime_cost_ng, 
    lifetime_emissions_reduc = elec_ghg_emissions * discount_sum, 
    mac = lifetime_cost / lifetime_emissions_reduc, 
    
    # some diagnostics
    emissions_avoided_per_heat = elec_ghg_emissions / total_heat,
    implied_factor = total_heat / lifetime_emissions_reduc, 
    
    name = paste(state, industry_clean, sep = " ")
  ) |>
  arrange(mac) |>
  mutate(
    x_min_t  = dplyr::lag(cumsum(lifetime_emissions_reduc), default = 0),
    x_max_t  = x_min_t + lifetime_emissions_reduc,
    x_min_mt = x_min_t / 1e6,
    x_max_mt = x_max_t / 1e6,
    y_min    = 0,
    y_max    = mac, 
    
    industry_clean = forcats::fct_reorder(industry_clean, mac, .desc = FALSE)
  ) |>
  # When the sector is too small, mac ends up looking really high. we'll drop a few 
  filter(mac < 500)

acc_statesub_plot <- 
  ggplot(acc_statesub_data) +
  geom_rect(
    aes(xmin = x_min_mt, xmax = x_max_mt, ymin = pmin(y_min, y_max), ymax = pmax(y_min, y_max),
        fill = industry_clean),
    color = "grey60", 
    linewidth = 0.2
  ) +
  scale_fill_manual(
    name = "Subsector",
    values = fig_industry_colors
    # guide = guide_legend(reverse = TRUE)
  ) +
  # optional outline along the tops (emphasize "steps")
  geom_segment(
    aes(x = x_min_mt, xend = x_max_mt, y = y_max, yend = y_max),
    linewidth = 0.42, color = "grey20"
  ) +
  labs(x = "Cumulative Abatement Potential (MtCO2e)", y = "Abatement Cost ($/tCO2e)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"), 
    legend.text = element_text(size = 8),       # smaller legend text
    legend.title = element_text(size = 9),      # smaller legend title
    legend.key.size = unit(0.4, "cm"),          # smaller color boxes
    legend.box.spacing = unit(0.2, "cm")        # less space between legends
  )

acc_statesub_plot

#### FIG: DELTA LCOH BUBBLE PLOT - WEIGHTED AVERAGE ####
baseline_bubble_wgt <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline'
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_ng = mean(lcoh, na.rm = T)
  ) |>
  select(facility_id, lcoh_ng)

delta_bubble_wgt_data_good <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario %in% c('hp_boiler_ee') 
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    lcoh = mean(lcoh, na.rm = T), 
    elec_ghg_emissions = mean(elec_ghg_emissions, na.rm = T), 
    elec_orig_process_unit_heat_demand = mean(elec_orig_process_unit_heat_demand, na.rm = T)
  ) |>
  left_join(
    baseline_bubble_wgt, by = 'facility_id'
  ) |>
  mutate(lcoh_delta = lcoh - lcoh_ng) |>
  group_by(state, industry_clean) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    # WEIGHTED lcoh delta
    lcoh_delta = weighted.mean(lcoh_delta, elec_ghg_emissions, na.rm = T), 
    elec_ghg_emissions = sum(elec_ghg_emissions, na.rm = T)
  ) |>
  ungroup() |>
  mutate(name = paste(state, industry_clean, sep = " "), 
         label = str_wrap(name, width = 10)) |>
  select(state, industry_clean, name, label, lcoh_delta, elec_ghg_emissions) |>
  slice_min(order_by = lcoh_delta, n = 15) |>
  tbl_graph(nodes = _, edges = NULL) 

delta_bubble_wgt_data_bad <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario %in% c('hp_boiler_ee') 
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    lcoh = mean(lcoh, na.rm = T), 
    elec_ghg_emissions = mean(elec_ghg_emissions, na.rm = T), 
    elec_orig_process_unit_heat_demand = mean(elec_orig_process_unit_heat_demand, na.rm = T)
  ) |>
  left_join(
    baseline_bubble_wgt, by = 'facility_id'
  ) |>
  mutate(lcoh_delta = lcoh - lcoh_ng) |>
  group_by(state, industry_clean) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    # WEIGHTED lcoh delta
    lcoh_delta = weighted.mean(lcoh_delta, elec_ghg_emissions, na.rm = T), 
    elec_ghg_emissions = sum(elec_ghg_emissions, na.rm = T)
  ) |>
  ungroup() |>
  mutate(name = paste(state, industry_clean, sep = " "), 
         label = str_wrap(name, width = 10)) |>
  select(state, industry_clean, name, label, lcoh_delta, elec_ghg_emissions) |>
  slice_max(order_by = lcoh_delta, n = 15) |>
  tbl_graph(nodes = _, edges = NULL) 

# --- compute shared limits across both graphs ---
fill_lims <- range(c(delta_bubble_wgt_data_good %N>% pull(lcoh_delta),
                     delta_bubble_wgt_data_bad %N>% pull(lcoh_delta)), na.rm = TRUE)
size_lims <- range(c(delta_bubble_wgt_data_good %N>% pull(elec_ghg_emissions),
                     delta_bubble_wgt_data_bad %N>% pull(elec_ghg_emissions)), na.rm = TRUE)

# --- shared scales ---
fill_scale <- scale_fill_gradient2(
  limits = fill_lims,
  low = "#1f78b4", mid = "white", high = "#fb9a99", midpoint = 0,
  name = "Δ LCOH ($/MMBtu)"
)
size_scale <- scale_size_continuous(
  limits = size_lims,
  range = c(3, 15),
  name = "Base CO₂e emissions",
  guide = "none"     
)

# --- normalize elec_ghg_emissions globally so circle sizes are comparable ---
global_min <- min(c(
  delta_bubble_wgt_data_good %N>% pull(elec_ghg_emissions),
  delta_bubble_wgt_data_bad  %N>% pull(elec_ghg_emissions)
), na.rm = TRUE)
global_max <- max(c(
  delta_bubble_wgt_data_good %N>% pull(elec_ghg_emissions),
  delta_bubble_wgt_data_bad  %N>% pull(elec_ghg_emissions)
), na.rm = TRUE)

rescale_emissions <- function(x) {
  (x - global_min) / (global_max - global_min)
}

delta_bubble_wgt_data_good <- 
  delta_bubble_wgt_data_good %N>% 
  mutate(weight_scaled = rescale_emissions(elec_ghg_emissions))

delta_bubble_wgt_data_bad  <- 
  delta_bubble_wgt_data_bad  %N>% 
  mutate(weight_scaled = rescale_emissions(elec_ghg_emissions))

# --- small helper function ---
base_wgt_plot <- function(graph, title, show_legend = TRUE) {
  ggraph(graph, layout = "circlepack", weight = elec_ghg_emissions) +
    geom_node_circle(
      aes(fill = lcoh_delta, size = elec_ghg_emissions),
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

delta_bubble_wgt_plot <- 
  (base_wgt_plot(delta_bubble_wgt_data_good, "Top 15 'Hot Spots'") + 
     base_wgt_plot(delta_bubble_wgt_data_bad, "Top 15 'Challenge Spots'")
  ) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

delta_bubble_wgt_plot

ggsave(glue("national_results/outputs/delta_bubble_wgt_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       delta_bubble_wgt_plot,
       width = 8, height = 5, dpi = 300)


#### FIG: PAYBACK RADIAL ####

fig_policies <- 
  c(
    "Elec -50%", 
    "Elec -25%", 
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
  "LA Petrochemicals" = c(
    "Elec -50%"   = "#D4EEF3",
    "Elec -25%"   = "#A0D7E0",
    "Capex -100%" = "#5EBAC3",
    "Capex -50%"  = "#09847A",
    "No Policy"    = "#064E58"
  ),
  "IL Distilleries" = c(
    "Elec -50%"   = "#FDE6E0",
    "Elec -25%"   = "#F8B3A4",
    "Capex -100%" = "#F27D63",
    "Capex -50%"  = "#EF5645",
    "No Policy"    = "#9B2C24"
  ),
  "NY Ethyl Alcohol" = c(
    "Elec -50%"   = "#D4EEF3",
    "Elec -25%"   = "#A0D7E0",
    "Capex -100%" = "#5EBAC3",
    "Capex -50%"  = "#09847A",
    "No Policy"    = "#064E58"
  )
)

payback_data_ng <-
  facility_lcoh_df |>
  filter(
    tech_scenario == 'baseline' &
      scenario_rank == 'best'
  ) |>
  rename(
    opex_ng = opex, 
    capex_ng = capex
  ) |>
  select(facility_id, opex_ng, capex_ng) 

payback_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee' &
      scenario_rank == 'best' &
      policy_label %in% fig_policies
  ) |>
  rename(
    opex_hp = opex, 
    capex_hp = capex
  ) |>
  left_join(payback_data_ng, by = 'facility_id') |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng, 
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_)
  ) |>
  group_by(state, industry_clean, policy_label) |>
  summarize(
    payback_years = mean(payback_years, na.rm = T), 
    base_emissions = sum(base_emissions, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    statesub = paste(state, industry_clean, sep = " "), 
    policy_label = factor(policy_label, levels = fig_policies)
  ) |>
  filter(statesub %in% names(fig_state_colors)) |>
  arrange(statesub, factor(policy_label, levels = fig_policies)) |>
  group_by(statesub) |>
  mutate(radius = row_number()) |>
  ungroup()

circle_limit <- 10

#payback_fig <- 
plots <- 
  map(names(fig_state_colors), function(s) {
    ggplot(payback_data |> filter(statesub == s)) +
      geom_link(aes(x = radius, xend = radius,
                    y = 0, yend = circle_limit),
                size = 5, lineend = "round", color = "grey95") +
      geom_link(aes(x = radius, xend = radius,
                    y = 0, yend = payback_years),
                size = 5, lineend = "round", color = "grey20") +
      geom_link(aes(x = radius, xend = radius,
                    y = 0, yend = payback_years, color = policy_label),
                size = 4, lineend = "round") +
      geom_label(aes(radius, y = circle_limit, 
                     label = paste0(policy_label, ": ", round(payback_years, 2), "yrs"),
                     hjust = 1.1), fill = alpha("white", 0.8), size = 2.5) +
      scale_x_continuous(limits = c(0, 5)) +
      scale_y_continuous(limits = c(0, circle_limit)) +
      scale_color_manual(values = fig_state_colors[[s]]) +
      guides(color = "none") +
      coord_polar(theta = "y") +
      theme_void() +
      theme(
        plot.margin = margin(t = 0, r = 0, b = 0, l = 30)  # top, right, bottom, left (in points)
      ) +
      ggtitle(s)
  })

# Combine into 2×2 grid

payback_plot <- 
  (plots[[1]] | plots[[2]]) /
  (plots[[3]] | plots[[4]])

payback_plot 

ggsave(glue("national_results/outputs/payback_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       payback_plot,
       width = 8, height = 5, dpi = 300)

#### FIG: PAYBACK PLOT ####

# Weirdness here where "worst case" looks better sometimes bc 
# it's relatively better than the ng worst case. 

fig_policies <- 
  c(
    "Elec -50%", 
    "Elec -25%", 
    "Capex -50%", 
    "No Policy"
  )

fig_policy_colors <- c(
  "No Policy"     = lighten('#09847A', amount = 0.45),  # lightest
  "Capex -50%"   = lighten('#09847A', amount = 0.25),
  "Elec -25%"     = '#09847A',
  "Elec -50%"     = darken('#09847A', amount = 0.25)    # darkest
)

payback_data_ng <-
  facility_lcoh_df |>
  filter(
    tech_scenario == 'baseline'
  ) |>
  rename(
    opex_ng = opex, 
    capex_ng = capex
  ) |>
  select(facility_id, scenario_rank, opex_ng, capex_ng) |>
  bind_rows(
    facility_lcoh_df %>%
      filter(tech_scenario == 'baseline') |>
      group_by(facility_id) |>
      summarise(
        capex_ng = mean(capex, na.rm = T),
        opex_ng = mean(opex, na.rm = T)) |>
      ungroup() |>
      mutate(scenario_rank = "average")
  )

payback_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee' &
      policy_label %in% fig_policies
  ) |>
  rename(
    opex_hp = opex, 
    capex_hp = capex
  ) |>
  bind_rows(
    facility_lcoh_df |>
      filter(
        tech_scenario == 'hp_boiler_ee' &
          policy_label %in% fig_policies
      ) |>
      group_by(facility_id, policy_label) |>
      summarise(
        capex_hp = mean(capex, na.rm = T),
        opex_hp = mean(opex, na.rm = T), 
        across(c(state, industry_clean), first)) |>
      ungroup() |>
      mutate(scenario_rank = "average")
  ) |>
  left_join(payback_data_ng, by = c('facility_id', 'scenario_rank')) |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng, 
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_)
  ) |>
  group_by(state, industry_clean, policy_label, scenario_rank) |>
  summarize(
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    state_sub = paste(state, industry_clean, sep = " "), 
    policy_label = factor(policy_label, levels = fig_policies)
  ) |>
  #group_by(state_sub, policy_label) |>
  pivot_wider(
    id_cols = c(state_sub, policy_label),             
    names_from = scenario_rank,
    values_from = payback_years,
    names_prefix = "payback_years_"
  ) |>
  ungroup()

payback_data_order <- 
  payback_data |>
  filter(policy_label == "No Policy") |>
  select(state_sub, payback_years_order = payback_years)

payback_data <- 
  payback_data |>
  left_join(payback_data_order, by = "state_sub") |>
  mutate(state_sub = forcats::fct_reorder(state_sub, payback_years_order))

# Technology scenario x sector delta opex plot 
payback_plot   <- 
  ggplot(payback_data,
         aes(x = state_sub, y = payback_years, color = policy_label)) +
  
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 0.8) +
  
  scale_color_manual(values = fig_policy_colors) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", size = 0.5) +
  # scale_y_continuous(limits = c(-100, 800), 
  #                    breaks = c(-100, pretty(delta_opex_data$opex_delta, n = 6))) +
  
  labs(
    x = "State-Subsector",
    y = "Payback Years",
    color = "Policy"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    # Remove tick labels and ticks
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    
    # Remove all grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

payback_plot 

ggsave(glue("national_results/outputs/payback_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       payback_plot,
       width = 8, height = 5, dpi = 300)


#### FIG: DELTA OPEX V1 ####

scenario_labels <- c(
  "eb_boiler"     = "E-Boiler",
  "eb_boiler_ee"  = "E-Boiler + EE",
  "hp_boiler"     = "Air-Source HP",
  "hp_boiler_ee"  = "Air-Source HP + EE"
)

scenario_colors <- c(
  "E-Boiler"     = "#1f78b4",
  "E-Boiler + EE"  = "#a6cee3",
  "Air-Source HP"     = "#33a02c",
  "Air-Source HP + EE"  = "#b2df8a"
)

baseline_opex <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup() |>
  select(facility_id, opex_ng)

delta_opex_data <- 
  facility_lcoh_df |>
  filter(
    policy_label == "No Policy", 
    tech_scenario %in% c('eb_boiler', 'eb_boiler_ee', 'hp_boiler', 'hp_boiler_ee') 
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    # Main info vars 
    facility_name = first(facility_name), 
    naics_code = first(naics_code), 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    opex = mean(opex, na.rm = T)
  ) |>
  ungroup() |>
  left_join(baseline_opex, by = 'facility_id') |>
  group_by(state, sector, tech_scenario) |>
  summarize(
    # Main info vars
    naics_code = first(naics_code),
    industry_clean = first(industry_clean),
    sector = first(sector),
    state = first(state),
    
    opex_ng = sum(opex_ng, na.rm = T),
    opex = sum(opex, na.rm = T)
  ) |>
  ungroup() |>
  mutate( 
    opex_delta = opex - opex_ng, 
    tech_scenario = factor(
      tech_scenario,
      levels = names(scenario_labels),
      labels = scenario_labels
    ), 
    state_sec = paste(state, sector, sep = " ")
  ) 

hp_order <- 
  delta_opex_data |>
  filter(tech_scenario == "Air-Source HP + EE") |>
  select(state_sec, hp_rank_value = opex_delta)

delta_opex_data <- 
  delta_opex_data |>
  left_join(hp_order, by = "state_sec") |>
  mutate(state_sec = forcats::fct_reorder(state_sec, hp_rank_value))

# Technology scenario x sector delta opex plot 
delta_opex_plot   <- 
  ggplot(delta_opex_data,
         aes(x = state_sec, y = opex_delta, color = tech_scenario)) +
  
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 0.8) +
  
  scale_color_manual(values = scenario_colors, labels = scenario_labels) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  # scale_y_continuous(limits = c(-100, 800), 
  #                    breaks = c(-100, pretty(delta_opex_data$opex_delta, n = 6))) +
  
  labs(
    x = NULL,
    y = "Δ Annual OPEX ($)",
    color = "Technology"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.83, 0.32),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

delta_opex_plot

# ggsave(glue("national_results/outputs/delta_opex_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
#        delta_opex_plot,
#        width = 8, height = 5, dpi = 300)

#### FIG: SECTOR/POLICY LCOH FIGURE  #####

# select policy scenarios to show 
fig_policies <- c(
  'No Policy',
  'Capex -50%, Elec -0%', 
  'Capex -100%, Elec -0%', 
  'Capex -0%, Elec -25%', 
  'Capex -0%, Elec -50%', 
  'Capex -50%, Elec -50%', 
  'Capex -100%, Elec -50%'
)

sector_policy_lcoh_data <- 
  facility_lcoh_df |>
  filter(tech_scenario == 'hp_boiler_ee', 
         policy_label %in% fig_policies) |>
  mutate(
    policy_label = factor(policy_label, levels = fig_policies)
  ) 

# Policy scenario x sector plot (S4 only) 
sector_policy_lcoh_plot <- 
  ggplot(sector_policy_lcoh_data,
         aes(x = policy_label, y = lcoh, color = sector)) +
  
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
