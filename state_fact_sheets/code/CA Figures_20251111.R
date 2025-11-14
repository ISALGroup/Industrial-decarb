#### INITIAL SET-UP #### 
# Load Libraries
library(colorspace)
library(dplyr)
library(forcats)
library(ggforce)
library(ggraph)
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

data_wd <- c('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/dataset_v5_no_partial_chp/')

facility_lcoh_df_ca <- 
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
        paste0("ITC Capex -", ITC * 100, "%"),
      rate_reduction > 0 & ITC == 0 & PTC == 0 ~
        paste0("Elec -", rate_reduction * 100, "%"),
      rate_reduction > 0 & ITC > 0 & PTC == 0~ 
        paste0("ITC Capex -", ITC * 100, "%, Elec -", rate_reduction * 100, "%"),
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
  filter(
    lcoh != 0, 
    state == 'CA')

payback_data_ng <-
  facility_lcoh_df_ca |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  rename(
    capex_ng = capex,
    opex_ng = opex
  ) |>
  ungroup() |>
  select(facility_id, scenario_rank, capex_ng, opex_ng)

facility_lcoh_df_ca <- 
  facility_lcoh_df_ca |>
  
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
    )
  ) 

r <- 0.065
t <- 25

emissions_df <-
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/total_emissions_by_facility_year_251111.csv') |>
  mutate(
    # Apply a per-year discount factor
    discount_factor = 1 / (1 + r)^(policy_year - 2025),  # assuming 2025 is year 0, 
    elec_mt = if_else(tech_scenario == 'baseline', elec_mt, 0)
  ) |>
  group_by(facility_id, tech_scenario, scenario_rank) |>
  summarize(
    # Present-day emissions figures
    baseline_co2e_mt_25 = co2e_baseline_mt[policy_year == 2025],
    clean_grid_co2e_mt_25 = cambium95_grid_co2e_emission_mt[policy_year == 2025],
    mid_grid_co2e_mt_25 = cambiummid_grid_co2e_emission_mt[policy_year == 2025],
    bau_grid_co2e_mt_25 = bau_grid_co2e_emission_mt[policy_year == 2025],
    elec_mt_25 = elec_mt[policy_year == 2025], 
    hard_elec_mt_25 = hard_elec_mt[policy_year == 2025], 
    biogenic_mt_25 = biogenic_mt[policy_year == 2025],
    
    # Lifetime totals
    lifetime_baseline_co2e_mt = sum(co2e_baseline_mt, na.rm = TRUE),
    lifetime_clean_grid_co2e_mt = sum(cambium95_grid_co2e_emission_mt, na.rm = TRUE),
    lifetime_mid_grid_co2e_mt = sum(cambiummid_grid_co2e_emission_mt, na.rm = TRUE),
    lifetime_bau_grid_co2e_mt = sum(bau_grid_co2e_emission_mt, na.rm = TRUE),
    lifetime_elec_mt = sum(elec_mt, na.rm = TRUE),
    lifetime_hard_elec_mt = sum(hard_elec_mt, na.rm = TRUE),
    lifetime_biogenic_mt = sum(biogenic_mt, na.rm = TRUE),
    
    # Discounted lifetime totals — year-by-year discounting
    lifetime_baseline_co2e_mt_dc = sum(co2e_baseline_mt * discount_factor, na.rm = TRUE),
    lifetime_clean_grid_co2e_mt_dc = sum(cambium95_grid_co2e_emission_mt * discount_factor, na.rm = TRUE),
    lifetime_mid_grid_co2e_mt_dc = sum(cambiummid_grid_co2e_emission_mt * discount_factor, na.rm = TRUE),
    lifetime_bau_grid_co2e_mt_dc = sum(bau_grid_co2e_emission_mt * discount_factor, na.rm = TRUE),
    lifetime_elec_mt_dc = sum(elec_mt * discount_factor, na.rm = TRUE),
    lifetime_hard_elec_mt_dc = sum(hard_elec_mt * discount_factor, na.rm = TRUE),
    lifetime_biogenic_mt_dc = sum(biogenic_mt * discount_factor, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    facility_id = as.character(facility_id),
    
    # Annual reductions at t=2025
    emissions_reduc_mt_25 = (clean_grid_co2e_mt_25 + elec_mt_25 + hard_elec_mt_25 + biogenic_mt_25) - baseline_co2e_mt_25,
    
    # Lifetime reductions
    lifetime_emissions_reduc_mt = (lifetime_clean_grid_co2e_mt + lifetime_elec_mt + lifetime_hard_elec_mt + lifetime_biogenic_mt) - lifetime_baseline_co2e_mt,
    lifetime_emissions_reduc_mt_dc = (lifetime_clean_grid_co2e_mt_dc + lifetime_elec_mt_dc + lifetime_hard_elec_mt_dc + lifetime_biogenic_mt_dc) - lifetime_baseline_co2e_mt_dc
  ) |>
  filter(facility_id %in% facility_lcoh_df_ca$facility_id)



# TEMPORARY: just using these emissions figures, not sure if they're right
# emissions_df <-
#   read_excel('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/merged_longform.xlsx') |>
#   select(-1, -2) |>
#   group_by(facility_id) |>
#   summarize(
#     base_emissions_co2e = sum(unit_ghg_emissions, na.rm = TRUE),
#     elec_ghg_emissions = sum(if_else(electrified_option != 'not_electrifiable' & !is_biogenic, unit_ghg_emissions, 0), na.rm = TRUE),
#     biogenic_ghg_emissions = sum(if_else(is_biogenic, unit_ghg_emissions, 0), na.rm = TRUE),
#     noelec_ghg_emissions = sum(if_else(electrified_option == 'not_electrifiable', unit_ghg_emissions, 0), na.rm = TRUE)
#   ) |>
#   ungroup() |>
#   mutate(
#     facility_id = as.character(facility_id)
#   ) %>%
#   filter(facility_id %in% facility_lcoh_df_ca$facility_id)

#### FIG: PERCENT OF HEAT IN-SCOPE ####

heat_df <- 
  read_excel(glue(data_wd ,'merged_longform.xlsx')) |>
  select(-1, -2) |>
  filter(state == 'CA') |>
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
      electrified_option != 'not_electrifiable' ~ 'In Scope', 
      electrified_option == 'not_electrifiable' & is_biogenic == FALSE ~ 'Hard-to-Electrify', 
      electrified_option == 'not_electrifiable' & is_biogenic == TRUE ~ 'Biogenic' 
    )
  ) |>
  group_by(sector, scope_category) |>
  summarize(
    heat_total = sum(process_unit_heat_demand, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(sector) |>
  mutate(
    pct_heat = 100 * heat_total / sum(heat_total, na.rm = TRUE), 
    scope_category = factor(
      scope_category,
      levels = c("Hard-to-Electrify", "Biogenic", "In Scope")  # bottom → top
    )
  ) |>
  ungroup()

heat_plot <- 
  ggplot(heat_df, aes(x = sector, y = pct_heat, fill = scope_category)) +
  geom_col(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "In Scope" = "#09847A",
      "Biogenic" = "#C9BF9D",
      "Hard-to-Electrify" = "#EF5645"
    ),
    breaks = c("In Scope", "Biogenic", "Hard-to-Electrify"),  # legend order
    name = "Scope Category"
  ) +
  labs(
    x = NULL,
    y = "Share of Total Process Heat"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(hjust = 0.5, size = 10),
    legend.position = "right",                     # ← move legend to right side
    legend.justification = "center",               # center vertically
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(
      color = "black",
      linewidth = 0.2,
      fill = alpha("white", 0.8)
    )
  )

heat_plot

heat_plot_2 <- 
  ggplot(heat_df, aes(x = sector, y = heat_total / 1000000, fill = scope_category)) +
  geom_col(position = "stack", width = 0.6) +
  #scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "In Scope" = "#09847A",
      "Biogenic" = "#C9BF9D",
      "Hard-to-Electrify" = "#EF5645"
    ),
    breaks = c("In Scope", "Biogenic", "Hard-to-Electrify"),  # legend order
    name = "Scope Category"
  ) +
  labs(
    x = NULL,
    y = "Total Process Heat (Million MMBtu)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(hjust = 0.5, size = 10),
    legend.position = "right",                     # ← move legend to right side
    legend.justification = "center",               # center vertically
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(
      color = "black",
      linewidth = 0.2,
      fill = alpha("white", 0.8)
    )
  )

heat_plot_2


#### FIG: FUEL DEMAND ####

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
  filter(state == 'CA') |>
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
  facility_lcoh_df_ca |>
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
  facility_lcoh_df_ca |>
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
    
    sector = factor(sector, levels = rev(sort(unique(as.character(sector)))))
  )


fuels_plot <- 
  ggplot() +
  
  geom_col(data = fuels_df,
           aes(x = fuel_hhv_tbtu, y = tech_scenario_label, fill = fuel_label),
           position = position_stack(), 
           width = .4) +
  
  facet_wrap(~ sector, nrow = 1) +
  
  scale_fill_manual(
    values = fuel_colors,
    breaks = names(fuel_colors),
    labels = names(fuel_colors),
    name = NULL
  ) +
  
  labs(x = "Fuel Demand (TBtu)", y = NULL, fill = "Fuel Type" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",
      linewidth = 0.2
    ),
    legend.text = element_text(size = 8),       # small but readable
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.3, "cm"),          # balanced key size
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.4, "cm"),         # a touch wider for color visibility
    legend.spacing.x = unit(0.2, "cm"),         # moderate spacing between boxes
    #legend.margin = margin(t = 2, b = 2, unit = "pt"),  # trim padding
    #legend.box.margin = margin(t = 0, b = 0, unit = "cm")  # tighter around legend
  )

fuels_plot


#### FIG: NATIONAL EMISSIONS  ####

fig_emissions_labels <- 
  c('On-Site (in scope)', 'On-Site (hard to electrify)', 'Biogenic', 'Grid')

tech_scenario_labels <- 
  c(
    "Baseline",
    "Drop-In Elec",
    "Drop-In Elec (EE+)",
    "Adv Elec",
    "Adv Elec (EE+)"
  )

fig_emissions_colors <- 
  c('On-Site (in scope)' = '#09847A', 
    'On-Site (hard to electrify)' = '#EF5645', 
    'Biogenic' = '#A67C52', 
    'Grid' = '#FEBC11')

emissions_data_nat <- 
  facility_lcoh_df_ca |>
  filter(
    policy_label == 'No Policy'
  ) |>
  left_join(emissions_df, by = c('facility_id', 'tech_scenario', 'scenario_rank')) |>
  # Average best & worst case 
  group_by(facility_id, tech_scenario_label) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = T), 
    lifetime_elec_mt = mean(lifetime_elec_mt, na.rm = T), 
    lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = T), 
    lifetime_hard_elec_mt = mean(lifetime_hard_elec_mt, na.rm = T)
  ) |>
  ungroup() |>
  
  # Getting sector-scenario emissions 
  group_by(sector, tech_scenario_label) %>%
  summarize(
    lifetime_clean_grid_co2e_gt = sum(lifetime_clean_grid_co2e_mt, na.rm = T) / 1e9, 
    lifetime_elec_mt = sum(lifetime_elec_mt, na.rm = T) / 1e9, 
    lifetime_biogenic_gt = sum(lifetime_biogenic_mt, na.rm = T) / 1e9, 
    lifetime_hard_elec_gt = sum(lifetime_hard_elec_mt, na.rm = T) / 1e9
  ) %>%
  ungroup() %>%
  # Add the sector totals
  
  # Get to one emissions variable 
  pivot_longer(
    cols = starts_with("lifetime_"), 
    names_to = "emissions_type", 
    values_to = "emissions_gt"
  ) |>
  
  mutate(
    emissions_label = case_when(
      emissions_type == 'lifetime_elec_mt' ~ fig_emissions_labels[1], 
      emissions_type == 'lifetime_hard_elec_gt' ~ fig_emissions_labels[2],
      emissions_type == 'lifetime_biogenic_gt' ~ fig_emissions_labels[3], 
      emissions_type == 'lifetime_clean_grid_co2e_gt' ~ fig_emissions_labels[4]
    ), 
    
    emissions_label = factor(
      emissions_label, 
      levels = fig_emissions_labels
    ), 
    
    tech_scenario_label = factor(
      tech_scenario_label, 
      levels = rev(tech_scenario_labels)
    ), 
    
    sector = factor(sector, levels = rev(sort(unique(as.character(sector)))))
  )  %>%
  arrange(desc(sector), factor(tech_scenario_label, levels = rev(tech_scenario_labels)))


emissions_plot_nat <- 
  ggplot() +
  
  geom_col(data = emissions_data_nat,
           aes(x = emissions_gt, y = tech_scenario_label, fill = emissions_label),
           position = position_stack(), 
           width = .4) +
  
  facet_wrap(~ sector, nrow = 1) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  scale_fill_manual(
    values = fig_emissions_colors,
    breaks = fig_emissions_labels,
    labels = fig_emissions_labels,
    name = NULL
  ) +
  
  labs(x = "Emissions (GtCO2e)", y = NULL, fill = "Emissions Source" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",
      linewidth = 0.2
    ),
    legend.text = element_text(size = 8),       # small but readable
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.3, "cm"),          # balanced key size
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.4, "cm"),         # a touch wider for color visibility
    legend.spacing.x = unit(0.2, "cm"),         # moderate spacing between boxes
    #legend.margin = margin(t = 2, b = 2, unit = "pt"),  # trim padding
    #legend.box.margin = margin(t = 0, b = 0, unit = "cm")  # tighter around legend
  )

emissions_plot_nat



#### TABLE: ELECTRICITY DEMAND ####

electricity_data <- 
  facility_lcoh_df_ca |>
  filter(
    policy_label == 'No Policy'
  ) |>
  # Average best & worst case 
  group_by(facility_id, tech_scenario_label) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    change_in_electricity_demand_kwh = mean(change_in_electricity_demand_kwh, na.rm = T)
  ) |>
  ungroup() |>
  
  # Getting sector-scenario emissions 
  group_by(sector, tech_scenario_label) %>%
  summarize(
    change_in_electricity_demand_gwh = (sum(change_in_electricity_demand_kwh, na.rm = T) / 1e6)
  ) %>%
  ungroup() 

#write_csv(electricity_data, 'national_results/outputs/electricity_demand_table.csv')


#### FIG: CAPEX PLOT ####
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
  facility_lcoh_df_ca |>
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
  labs(x = NULL, y = "Total Up-Front Costs ($ millions)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(hjust = 0.5, size = 10),
    legend.position = "right",                     
    legend.justification = "center",               
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(
      color = "black",
      linewidth = 0.2,
      fill = alpha("white", 0.8)
    )
  )

capex_plot

#### FIG: PERCENT IN PAYBACK (SECTOR) -- NO POLICY ####

sector_pct_colors <- 
  c(
    "Chemicals (<5 yrs)"     = "#09847A",
    "Chemicals (<15 yrs)"     = lighten("#09847A", amount = .4),
    "Chemicals (15+ yrs)"     = lighten("#09847A", amount = .8),
    "Food & Beverage (<5 yrs)" = "#EF5645",
    "Food & Beverage (<15 yrs)" = lighten("#EF5645", amount = .4),
    "Food & Beverage (15+ yrs)" = lighten("#EF5645", amount = .8),
    "Pulp & Paper (<5 yrs)" = "#A67C52",
    "Pulp & Paper (<15 yrs)" = lighten("#A67C52", amount = .4),
    "Pulp & Paper (15+ yrs)" = lighten("#A67C52", amount = .8)
  )

# scenario_labels <- c(
#   "eb_boiler"     = "E-Boiler",
#   "eb_boiler_ee"  = "E-Boiler\n(EE+)",
#   "hp_boiler"     = "Air-Source\nHTHP",
#   "hp_boiler_ee"  = "Air-Source\nHTHP (EE+)"
# )

tech_scenario_labels <- c(
  "Drop-In\nElec",
  "Drop-In\nElec (EE+)",
  "Adv\nElec",
  "Adv\nElec (EE+)"
)

in_payback_data_nopol <- 
  facility_lcoh_df_ca |>
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
    sector_pct = case_when(
      sector == 'Chemicals' & pct_group == 'sub_5' ~ 'Chemicals (<5 yrs)', 
      sector == 'Chemicals' & pct_group == 'sub_15' ~ 'Chemicals (<15 yrs)',
      sector == 'Chemicals' & pct_group == 'plus_15' ~ 'Chemicals (15+ yrs)', 
      sector == 'Food & Beverage' & pct_group == 'sub_5' ~ 'Food & Beverage (<5 yrs)', 
      sector == 'Food & Beverage' & pct_group == 'sub_15' ~ 'Food & Beverage (<15 yrs)',
      sector == 'Food & Beverage' & pct_group == 'plus_15' ~ 'Food & Beverage (15+ yrs)', 
      sector == 'Pulp & Paper' & pct_group == 'sub_5' ~ 'Pulp & Paper (<5 yrs)', 
      sector == 'Pulp & Paper' & pct_group == 'sub_15' ~ 'Pulp & Paper (<15 yrs)',
      sector == 'Pulp & Paper' & pct_group == 'plus_15' ~ 'Pulp & Paper (15+ yrs)' 
    ), 
    sector_pct = factor(
      sector_pct,
      levels = c(
        "Chemicals (15+ yrs)", "Chemicals (<15 yrs)", "Chemicals (<5 yrs)",
        "Food & Beverage (15+ yrs)", "Food & Beverage (<15 yrs)", "Food & Beverage (<5 yrs)",
        "Pulp & Paper (15+ yrs)", "Pulp & Paper (<15 yrs)", "Pulp & Paper (<5 yrs)"
      )
    ), 
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

in_payback_plot_nopol   <- 
  ggplot() +
  
  geom_col(data = in_payback_data_nopol,
           aes(x = tech_scenario_label, y = pct_in_payback, fill = sector_pct),
           position = "stack", width = 0.6)+
  
  facet_wrap(~ sector, nrow = 1) +
  
  scale_fill_manual(
    values = sector_pct_colors,
    name = "Payback (Years)",
    breaks = c(
      "Chemicals (<5 yrs)", "Chemicals (<15 yrs)", "Chemicals (15+ yrs)",
      "Food & Beverage (<5 yrs)", "Food & Beverage (<15 yrs)", "Food & Beverage (15+ yrs)",
      "Pulp & Paper (<5 yrs)", "Pulp & Paper (<15 yrs)", "Pulp & Paper (15+ yrs)"
    ),
    labels = c("<5 yrs", "5–15 yrs", "15+ yrs",
               "<5 yrs", "5–15 yrs", "15+ yrs",
               "<5 yrs", "5–15 yrs", "15+ yrs")
  ) +
  
  scale_x_discrete(labels = tech_scenario_labels) +
  
  #scale_color_manual(values = sector_colors) +
  scale_y_continuous(limits = c(0, 100)) +
  
  labs(x = NULL , y = "Percent of Facilities W/ Positive Payback", fill = "Payback (Years)" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "white"),
    legend.position = "right",
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.text.x = element_text(
      size = 7,              # smaller text
      angle = 45,            # rotate 45 degrees
      hjust = 1,             # right-align so they don’t overlap
      vjust = 1              # vertically align nicely under ticks
    )  
  )

in_payback_plot_nopol




#### FIG: TOP PAYBACK -- NO POLICY ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)

payback_data_nopol <- 
  facility_lcoh_df_ca |>
  filter(
    tech_scenario == 'hp_boiler_ee' &
      policy_label == 'No Policy' 
  ) |> 
  group_by(facility_id) |>
  summarize(
    sector = first(sector), 
    state = first(state), 
    industry_clean = first(industry_clean), 
    
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  filter(!is.na(payback_years)) |>
  group_by(state, industry_clean) |>
  filter(n() >= 2) |>   # keep only statesubs with +2 facilities
  summarize(
    sector = first(sector), 
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    statesub = paste(state, industry_clean, sep = " ")
  ) |>
  filter(payback_years < 15) 

payback_plot_nopol <- 
  ggplot(payback_data_nopol,
         aes(x = reorder(statesub, payback_years, decreasing = TRUE),
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
    axis.text.x = element_text(hjust = 0.5, size = 10),
    
    legend.position = c(0.98, 0.98),     # near top-right corner
    legend.justification = c(1, 1),      # anchor legend’s top-right to that point
    
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(
      color = "black",
      linewidth = 0.2,
      fill = alpha("white", 0.8)
    )
  )

payback_plot_nopol


#### FIG: PAYBACK BUBBLE X/Y -- CAPEX ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)

fig_states <- c(
  'MI', 'PA', 'MN', 'CO'
)

fig_policies <- c(
  'No Policy', 
  'ITC Capex -30%', 
  'ITC Capex -50%'
)

fig_subsector_colors <- c(
  "Cane Sugar"           = "#E41A1C",  # bright red
  "Pulp & Paper"         = "#4DAF4A",  # vivid green ✅
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
  "Fertilizers"          = "#A6761D",  # brown-gold
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
  "Canning"              = "#3288BD"   # blue
)

payback_capex_xy_data <- 
  facility_lcoh_df_ca |>
  filter(
    tech_scenario == 'hp_boiler_ee', 
    policy_label %in% fig_policies #, 
    #state %in% fig_states
  ) |>
  left_join(emissions_df |>
              filter(tech_scenario == 'baseline') |>
              select(facility_id, scenario_rank, elec_mt_25), 
            by = c('facility_id', 'scenario_rank')
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    payback_years = mean(payback_years, na.rm = T),
    elec_mt_25 = mean(elec_mt_25, na.rm = T)
  ) |>
  ungroup() |>
  filter(!is.na(payback_years)) |>
  group_by(state, industry_clean, policy_label) |>
  filter(n() > 2) |>
  summarize(
    sector = first(sector),
    
    payback_years = mean(payback_years, na.rm = T), 
    elec_mt_25 = sum(elec_mt_25, na.rm = T) / 1000000
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
  select(state, industry_clean, sector, policy_label, payback_years, elec_mt_25) |>
  filter(!is.na(payback_years),
         payback_years < 50
  )

payback_capex_xy_plot <- 
  ggplot(payback_capex_xy_data,
         aes(x = policy_label,
             y = payback_years,
             size = elec_mt_25,
             fill = industry_clean)) +
  
  geom_point(shape = 21, color = "grey30", alpha = 0.85, stroke = 0.3,
             position = position_jitter(width = 0.2, height = 0)) +
  # facet_wrap(~ policy_label, nrow = 1) +
  
  scale_fill_manual(values = fig_subsector_colors, name = "Subsector") +
  scale_size_continuous(
    range = c(3, 15),
    name = "In-Scope CO2e\nEmissions",
    labels = function(x) sprintf("%.1f Mt", x)
  ) +
  # scale_y_continuous(limits = c(0, 20), expand = c(0, 0.5)) +
  labs(
    x = NULL,
    y = "Payback Period (Years)",
  ) +
  theme_bw(base_size = 13) +
  guides(
    fill = guide_legend(
      title = "Subsector",
      title.position = "top",
      keywidth = 0.7, keyheight = 0.7,
      label.theme = element_text(size = 8),
      ncol = 1,        # or 2 if you want two columns
      byrow = TRUE
    ),
    size = guide_legend(
      title.position = "top",
      label.position = "right"
    )
  ) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

payback_capex_xy_plot

#### FIG: PAYBACK BUBBLE X/Y -- OPEX ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)

fig_states <- c(
  'IL', 'MI', 'WI', 'OH'
)

fig_policies <- c(
  'No Policy', 
  'Elec -25%', 
  'Elec -50%', 
  'PTC $10/MMBtu'
)

fig_subsector_colors <- c(
  "Cane Sugar"           = "#E41A1C",  # bright red
  "Pulp & Paper"         = "#4DAF4A",  # vivid green ✅
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
  "Fertilizers"          = "#A6761D",  # brown-gold
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
  "Canning"              = "#3288BD"   # blue
)

payback_opex_xy_data <- 
  facility_lcoh_df_ca |>
  filter(
    tech_scenario == 'hp_boiler_ee', 
    policy_label %in% fig_policies #, 
    #state %in% fig_states
  ) |>
  left_join(emissions_df |>
              filter(tech_scenario == 'baseline') |>
              select(facility_id, scenario_rank, elec_mt_25), 
            by = c('facility_id', 'scenario_rank')
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    payback_years = mean(payback_years, na.rm = T),
    elec_mt_25 = mean(elec_mt_25, na.rm = T)
  ) |>
  ungroup() |>
  filter(!is.na(payback_years)) |>
  group_by(state, industry_clean, policy_label) |>
  filter(n() > 2) |>
  summarize(
    sector = first(sector),
    
    payback_years = mean(payback_years, na.rm = T), 
    elec_mt_25 = sum(elec_mt_25, na.rm = T) / 1000000
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
  select(state, industry_clean, sector, policy_label, payback_years, elec_mt_25) |>
  filter(!is.na(payback_years),
         payback_years < 50
  )

payback_opex_xy_plot <-
  ggplot(payback_opex_xy_data,
         aes(x = policy_label,
             y = payback_years,
             size = elec_mt_25,
             fill = industry_clean))+
  geom_point(shape = 21, color = "grey30", alpha = 0.85, stroke = 0.3,
             position = position_jitter(width = 0.2, height = 0)) +
  #facet_wrap(~ policy_label, nrow = 1) +
  
  scale_fill_manual(values = fig_subsector_colors, name = "Subsector") +
  scale_size_continuous(
    range = c(3, 15),
    name = "In-Scope CO2e\nEmissions",
    labels = function(x) sprintf("%.1f Mt", x)
  ) +
  # scale_y_continuous(limits = c(0, 20), expand = c(0, 0.5)) +
  labs(
    x = NULL,
    y = "Payback Period (Years)",
  ) +
  theme_bw(base_size = 13) +
  guides(
    fill = guide_legend(
      title = "Subsector",
      title.position = "top",
      keywidth = 0.7, keyheight = 0.7,
      label.theme = element_text(size = 8),
      ncol = 1,        # or 2 if you want two columns
      byrow = TRUE
    ),
    size = guide_legend(
      title.position = "top",
      label.position = "right"
    )
  ) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

payback_opex_xy_plot

#### FIG: PAYBACK BUBBLE X/Y -- COMBINED ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)

fig_states <- c(
  'IL', 'MI', 'WI', 'OH'
)

fig_policies <- c(
  'No Policy', 
  'ITC Capex -50%',
  'Elec -25%', 
  'PTC $10/MMBtu'
)

fig_subsector_colors <- c(
  "Cane Sugar"           = "#E41A1C",  # bright red
  "Pulp & Paper"         = "#4DAF4A",  # vivid green ✅
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
  "Fertilizers"          = "#A6761D",  # brown-gold
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
  "Canning"              = "#3288BD"   # blue
)

payback_xy_data <- 
  facility_lcoh_df_ca |>
  filter(
    tech_scenario == 'hp_boiler_ee', 
    policy_label %in% fig_policies #, 
    #state %in% fig_states
  ) |>
  left_join(emissions_df |>
              filter(tech_scenario == 'baseline') |>
              select(facility_id, scenario_rank, elec_mt_25), 
            by = c('facility_id', 'scenario_rank')
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    payback_years = mean(payback_years, na.rm = T),
    elec_mt_25 = mean(elec_mt_25, na.rm = T)
  ) |>
  ungroup() |>
  filter(!is.na(payback_years)) |>
  group_by(state, industry_clean, policy_label) |>
  filter(n() > 2) |>
  summarize(
    sector = first(sector),
    
    payback_years = mean(payback_years, na.rm = T), 
    elec_mt_25 = sum(elec_mt_25, na.rm = T) / 1000000
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
  select(state, industry_clean, sector, policy_label, payback_years, elec_mt_25) |>
  filter(!is.na(payback_years),
         payback_years < 50
  )

payback_xy_plot <- 
  ggplot(payback_xy_data,
         aes(x = policy_label,
             y = payback_years,
             size = elec_mt_25,
             fill = industry_clean))+
  geom_point(shape = 21, color = "grey30", alpha = 0.85, stroke = 0.3,
             position = position_jitter(width = 0.2, height = 0)) +
  #facet_wrap(~ policy_label, nrow = 1) +
  
  scale_fill_manual(values = fig_subsector_colors, name = "Subsector") +
  scale_size_continuous(
    range = c(3, 15),
    name = "In-Scope CO2e\nEmissions",
    labels = function(x) sprintf("%.1f Mt", x)
  ) +
  # scale_y_continuous(limits = c(0, 20), expand = c(0, 0.5)) +
  labs(
    x = NULL,
    y = "Payback Period (Years)",
  ) +
  theme_bw(base_size = 13) +
  guides(
    fill = guide_legend(
      title = "Subsector",
      title.position = "top",
      keywidth = 0.7, keyheight = 0.7,
      label.theme = element_text(size = 8),
      ncol = 1,        # or 2 if you want two columns
      byrow = TRUE
    ),
    size = guide_legend(
      title.position = "top",
      label.position = "right"
    )
  ) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

payback_xy_plot

#### FIG: PERCENT IN PAYBACK -- POLICY ####
# select policy scenarios to show 
fig_policies <- c(
  'No Policy',
  'Elec -25%', 
  'Capex -50%, Elec -25%',
  'Elec -50%', 
  'PTC $10/MMBtu'
)

policy_labels <- c(
  'No Policy' = 'No Policy',
  'Elec -25%' =  'Elec -25%',
  'Capex -50%, Elec -25%' = 'Capex -50%,\nElec -25%',
  'Elec -50%' = 'Elec -50%', 
  'PTC $10/MMBtu' = 'PTC\n$10/MMBtu'
)

sector_pct_colors <- 
  c(
    "Chemicals (<5 yrs)"     = "#09847A",
    "Chemicals (<15 yrs)"     = lighten("#09847A", amount = .4),
    "Chemicals (15+ yrs)"     = lighten("#09847A", amount = .8),
    "Food & Beverage (<5 yrs)" = "#EF5645",
    "Food & Beverage (<15 yrs)" = lighten("#EF5645", amount = .4),
    "Food & Beverage (15+ yrs)" = lighten("#EF5645", amount = .8),
    "Pulp & Paper (<5 yrs)" = "#A67C52",
    "Pulp & Paper (<15 yrs)" = lighten("#A67C52", amount = .4),
    "Pulp & Paper (15+ yrs)" = lighten("#A67C52", amount = .8)
  )

# fig_policy_colors <- c(
#   "No Policy"                = "#BFBFBF",  # neutral gray
#   "Elec -25%"                = "#31A354",  # medium green
#   "Elec -50%"                = "#006D2C",  # dark green
#   "PTC $10/MMBtu"            = "#E6550D"   # orange
# )

in_payback_data <- 
  facility_lcoh_df_ca |>
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
    sector_pct = case_when(
      sector == 'Chemicals' & pct_group == 'sub_5' ~ 'Chemicals (<5 yrs)', 
      sector == 'Chemicals' & pct_group == 'sub_15' ~ 'Chemicals (<15 yrs)',
      sector == 'Chemicals' & pct_group == 'plus_15' ~ 'Chemicals (15+ yrs)', 
      sector == 'Food & Beverage' & pct_group == 'sub_5' ~ 'Food & Beverage (<5 yrs)', 
      sector == 'Food & Beverage' & pct_group == 'sub_15' ~ 'Food & Beverage (<15 yrs)',
      sector == 'Food & Beverage' & pct_group == 'plus_15' ~ 'Food & Beverage (15+ yrs)', 
      sector == 'Pulp & Paper' & pct_group == 'sub_5' ~ 'Pulp & Paper (<5 yrs)', 
      sector == 'Pulp & Paper' & pct_group == 'sub_15' ~ 'Pulp & Paper (<15 yrs)',
      sector == 'Pulp & Paper' & pct_group == 'plus_15' ~ 'Pulp & Paper (15+ yrs)' 
    ), 
    sector_pct = factor(
      sector_pct,
      levels = c(
        "Chemicals (15+ yrs)", "Chemicals (<15 yrs)", "Chemicals (<5 yrs)",
        "Food & Beverage (15+ yrs)", "Food & Beverage (<15 yrs)", "Food & Beverage (<5 yrs)",
        "Pulp & Paper (15+ yrs)", "Pulp & Paper (<15 yrs)", "Pulp & Paper (<5 yrs)"
      )
    ), 
    
    policy_label = factor(
      policy_label, 
      levels = fig_policies
    )
  ) 

in_payback_plot   <- 
  ggplot() +
  
  geom_col(data = in_payback_data,
           aes(x = policy_label, y = pct_in_payback, fill = sector_pct),
           position = "stack", width = 0.6)+
  
  facet_wrap(~ sector, nrow = 1) +
  
  scale_fill_manual(
    values = sector_pct_colors,
    name = "Payback (Years)",
    breaks = c(
      "Chemicals (<5 yrs)", "Chemicals (<15 yrs)", "Chemicals (15+ yrs)",
      "Food & Beverage (<5 yrs)", "Food & Beverage (<15 yrs)", "Food & Beverage (15+ yrs)",
      "Pulp & Paper (<5 yrs)", "Pulp & Paper (<15 yrs)", "Pulp & Paper (15+ yrs)"
    ),
    labels = c("<5 yrs", "5–15 yrs", "15+ yrs",
               "<5 yrs", "5–15 yrs", "15+ yrs",
               "<5 yrs", "5–15 yrs", "15+ yrs")
  ) +
  
  scale_x_discrete(labels = policy_labels) +
  
  #scale_color_manual(values = sector_colors) +
  scale_y_continuous(limits = c(0, 100)) +
  
  labs(x = NULL , y = "Percent of Facilities W/ Positive Payback", fill = "Payback (Years)" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "white"),
    legend.position = "right",
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.text.x = element_text(
      size = 8,              # smaller text
      angle = 45,            # rotate 45 degrees
      hjust = 1,             # right-align so they don’t overlap
      vjust = 1              # vertically align nicely under ticks
    )  
  )

in_payback_plot

#### FIG: TOP PAYBACK -- POLICY  ####

fig_policies <- 
  c(
    "PTC $10/MMBtu",
    "ITC Capex -50%", 
    "Elec -25%", 
    "No Policy"
  )

fig_policy_colors <- c(
  "No Policy"     = '#FEBC11',
  "Elec -25%"     = "#C2E4E6",
  "ITC Capex -50%"   = "#09847A", 
  "PTC $10/MMBtu" = '#003660'
)

payback_data <- 
  facility_lcoh_df_ca |>
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
    payback_no_policy = payback_years[policy_label == "No Policy"][1]
  ) |>
  ungroup() |>
  filter(
    !is.na(payback_no_policy),
    payback_no_policy >= 5,
    payback_no_policy <= 20, 
    # excluding this because of funky outcomes stemming from facilities moving in and out of payback in best/worse scenarios, 
    # which causes the electricity discount to look worse (because facilities get into payback in the worst case, bringing the facility
    # average up for the electricity policy)
    !statesub %in% c('NM Cheese', 'TX Petrochemicals')
  ) |>
  select(-payback_no_policy)

payback_plot <- 
  ggplot(payback_data,
         aes(x = reorder(statesub, payback_years, decreasing = TRUE),
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
    legend.position = c(0.95, 0.6),     
    legend.justification = c(1, 0),      
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.2),
    axis.text.y = element_text(size = 9)
  )

payback_plot



#### FIG: ABATEMENT COST CURVE ####

fig_subsector_colors <- c(
  # --- Chemicals (now mixed teal / blue / purple for stronger separation) ---
  "Ethyl Alcohol"         = "#09847A",  # deep teal
  "Fertilizers"           = "#4B9CD3",  # medium blue
  "Industrial Gas"        = "#7B68EE",  # lavender-blue
  "Inorganic Chemicals"   = "#2E5984",  # navy blue
  "Petrochemicals"        = "#5EBAC3",  # light aqua
  "Phosphatic Fertilizer" = "#8A2BE2",  # violet
  "Plastics & Resins"     = "#3CB371",  # medium sea green
  "Rubber"                = "#483D8B",  # dark slate blue
  "Soybeans"              = "#6A5ACD",  # periwinkle
  "Wood Chemicals"        = "#0FA396",  # greenish teal
  
  # --- Food & Beverage (expanded coral family with orange/yellow contrast) ---
  "Beet Sugar"            = "#FEBC11",  # golden yellow
  "Breakfast Cereal"      = "#F6A623",  # warm orange
  "Breweries"             = "#F58C73",  # coral
  "Cane Sugar"            = "#E65C47",  # deep coral-red
  "Canning"               = "#F1735E",  # reddish orange
  "Cheese"                = "#FDD38E",  # light gold
  "Distilleries"          = "#E98B2A",  # amber orange
  "Dried Foods"           = "#F27D63",  # salmon
  "Fats & Oils"           = "#EF5645",  # red-orange
  "Frozen Foods"          = "#FFD166",  # light yellow
  "Meat (non-poultry)"    = "#D4503C",  # muted red
  "Milk"                  = "#F8A18C",  # pale coral
  "Other Dairy"           = "#B13222",  # dark brick
  "Poultry"               = "#E37D1E",  # pumpkin
  "Rendering"             = "#9B2C24",  # deep red-brown
  "Snack Foods"           = "#F9A55B",  # light orange
  "Specialty Canning"     = "#FBE7A1",  # pale yellow
  "Spices"                = "#E9891A",  # golden orange
  "Wet Corn Milling"      = "#EC3E32",  # red
  
  # --- Pulp & Paper (same olive base, slightly more contrast) ---
  "Pulp & Paper"          = "#A67C52",  # olive green
  "Toilet Paper"          = "#A2B86B"   # lighter olive
)

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
  facility_lcoh_df_ca |>
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
  facility_lcoh_df_ca |>
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
    lifetime_emissions_reduc_mt_dc = sum(lifetime_emissions_reduc_mt_dc, na.rm = T) * -1
  ) |>
  ungroup() |>
  mutate(
    lifetime_cost = lifetime_cost_hp - lifetime_cost_ng, 
    mac = lifetime_cost / lifetime_emissions_reduc_mt_dc
  ) |>
  arrange(mac) |>
  mutate(
    x_min_t  = dplyr::lag(cumsum(lifetime_emissions_reduc_mt_dc), default = 0),
    x_max_t  = x_min_t + lifetime_emissions_reduc_mt_dc,
    x_min_mt = x_min_t / 1e6,
    x_max_mt = x_max_t / 1e6,
    y_min    = 0,
    y_max    = mac, 
    
    industry_clean = forcats::fct_reorder(industry_clean, mac, .desc = FALSE)
  ) |>
  # When the sector is too small, mac ends up looking really high. we'll drop a few 
 filter(mac < 2500)

acc_plot <- 
  ggplot(acc_data) +
  geom_rect(
    aes(xmin = x_min_mt, xmax = x_max_mt, ymin = pmin(y_min, y_max), ymax = pmax(y_min, y_max),
        fill = industry_clean),
    color = "grey60"
  ) +
  scale_fill_manual(
    name = "Subsector (left to right)",          
    values = fig_subsector_colors
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

#### FIG: NATIONAL EMISSIONS "IN THE MONEY" ####
fig_policies <- c(
  "Baseline", 
  "No Policy",
  "ITC Capex -100%", 
  "Elec -25%", 
  "Elec -50%", 
  "PTC $10/MMBtu"
)

# fig_policy_colors <- c(
#   "Baseline In-Scope Emissions" = "grey70",
#   "No Policy"     = lighten('#09847A', amount = 0.45),  # lightest
#   "ITC Capex -100%"   = lighten('#09847A', amount = 0.25),
#   "Elec -25%"     = '#09847A',
#   "Elec -50%"     = darken('#09847A', amount = 0.25), 
#   "PTC $10/MMBtu" = darken('#09847A', amount = 0.45)
# )

fig_emissions_labels <- 
  c('On-Site (in scope)', 'On-Site (hard to electrify)', 'Biogenic', 'Grid')

fig_emissions_colors <- 
  c('On-Site (in scope)' = '#09847A', 
    'On-Site (hard to electrify)' = '#EF5645', 
    'Biogenic' = '#A67C52', 
    'Grid' = '#FEBC11')

total_elec_ghg_emissions_nat <- 
  emissions_df %>%
  filter(tech_scenario == 'baseline') %>%
  group_by(facility_id) %>% 
  summarize(
    lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = T), 
    lifetime_elec_mt = mean(lifetime_elec_mt, na.rm = T), 
    lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = T), 
    lifetime_hard_elec_mt = mean(lifetime_hard_elec_mt, na.rm = T),
  ) %>%
  left_join(facility_lcoh_df_ca |>
              distinct(facility_id, sector), 
            by = 'facility_id') |>
  group_by(sector) %>%          
  summarize(
    lifetime_clean_grid_co2e_gt = sum(lifetime_clean_grid_co2e_mt, na.rm = T) / 1e9, 
    lifetime_elec_mt = sum(lifetime_elec_mt, na.rm = T) / 1e9, 
    lifetime_biogenic_gt = sum(lifetime_biogenic_mt, na.rm = T) / 1e9, 
    lifetime_hard_elec_gt = sum(lifetime_hard_elec_mt, na.rm = T) / 1e9,
    policy_label = "Baseline"
  ) 

eim_data_nat <- 
  facility_lcoh_df_ca |>
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
    
    lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = T), 
    lifetime_elec_mt = mean(lifetime_elec_mt, na.rm = T), 
    lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = T), 
    lifetime_hard_elec_mt = mean(lifetime_hard_elec_mt, na.rm = T),
    
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    in_money = if_else(!is.na(payback_years), 1, 0)
  ) %>%
  
  # Add baseline emissions for every facility
  left_join(
    emissions_df |>
      filter(tech_scenario == 'baseline') |>
      group_by(facility_id) |>
      summarize(
        lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = T), 
        lifetime_elec_mt = mean(lifetime_elec_mt, na.rm = T), 
        lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = T), 
        lifetime_hard_elec_mt = mean(lifetime_hard_elec_mt, na.rm = T),
      ) |>
      ungroup() |>
      rename(
        base_lifetime_clean_grid_co2e_mt = lifetime_clean_grid_co2e_mt, 
        base_lifetime_elec_mt = lifetime_elec_mt, 
        base_lifetime_biogenic_mt = lifetime_biogenic_mt, 
        base_lifetime_hard_elec_mt = lifetime_hard_elec_mt
      ),
    by = 'facility_id'
  ) |>
  
  mutate(
    lifetime_clean_grid_effective = if_else(in_money == 1,
                                            lifetime_clean_grid_co2e_mt,
                                            base_lifetime_clean_grid_co2e_mt),
    lifetime_low_med_effective = if_else(in_money == 1,
                                         lifetime_elec_mt,
                                         base_lifetime_elec_mt),  
    lifetime_non_elec_effective = if_else(in_money == 1,
                                          lifetime_hard_elec_mt,
                                          base_lifetime_hard_elec_mt),
    lifetime_biogenic_effective = if_else(in_money == 1,
                                          lifetime_biogenic_mt,
                                          base_lifetime_biogenic_mt)
  ) |>
  
  # Getting sector-policy emissions in the money 
  group_by(sector, policy_label) %>%
  summarize(
    lifetime_clean_grid_co2e_gt = sum(lifetime_clean_grid_effective, na.rm = T) / 1e9, 
    lifetime_elec_mt = sum(lifetime_low_med_effective, na.rm = T) / 1e9, 
    lifetime_biogenic_gt = sum(lifetime_biogenic_effective, na.rm = T) / 1e9, 
    lifetime_hard_elec_gt = sum(lifetime_non_elec_effective, na.rm = T) / 1e9
  ) %>%
  ungroup() %>%
  # Add the sector totals
  bind_rows(total_elec_ghg_emissions_nat) %>%
  
  # Get to one emissions variable 
  pivot_longer(
    cols = starts_with("lifetime_"), 
    names_to = "emissions_type", 
    values_to = "emissions_gt"
  ) |>
  
  mutate(
    emissions_label = case_when(
      emissions_type == 'lifetime_elec_mt' ~ fig_emissions_labels[1], 
      emissions_type == 'lifetime_hard_elec_gt' ~ fig_emissions_labels[2],
      emissions_type == 'lifetime_biogenic_gt' ~ fig_emissions_labels[3], 
      emissions_type == 'lifetime_clean_grid_co2e_gt' ~ fig_emissions_labels[4]
    ), 
    
    emissions_label = factor(
      emissions_label, 
      levels = fig_emissions_labels
    ), 
    
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
           aes(x = emissions_gt, y = policy_label, fill = emissions_label),
           position = position_stack(), 
           width = .4) +
  
  facet_wrap(~ sector, nrow = 1) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  scale_fill_manual(
    values = fig_emissions_colors,
    breaks = fig_emissions_labels,
    labels = fig_emissions_labels,
    name = NULL
  ) +
  
  labs(x = "Emissions (GtCO2e)", y = NULL, fill = "Emissions Source" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",                 
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    #legend.key.size = unit(0.4, "cm"),         # slightly larger boxes look better at bottom
    legend.spacing.x = unit(0.3, "cm"),        # space between legend items horizontally
    legend.box = "horizontal"                  # arrange legend items in one horizontal row
  )

eim_plot_nat


#### FIG: STATE EMISSIONS "IN THE MONEY" ####
fig_policies <- c(
  "Baseline", 
  "No Policy",
  "ITC Capex -100%", 
  "Elec -25%", 
  "Elec -50%", 
  "PTC $10/MMBtu"
)

fig_emissions_labels <- 
  c('On-Site (in scope)', 'On-Site (hard to electrify)', 'Biogenic', 'Grid')

fig_emissions_colors <- 
  c('On-Site (in scope)' = '#09847A', 
    'On-Site (hard to electrify)' = '#EF5645', 
    'Biogenic' = '#A67C52', 
    'Grid' = '#FEBC11')

fig_states <- c(
  'CA'
)

total_elec_ghg_emissions_state <- 
  emissions_df %>%
  filter(tech_scenario == 'baseline') %>%
  group_by(facility_id) %>% 
  summarize(
    lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = T), 
    lifetime_elec_mt = mean(lifetime_elec_mt, na.rm = T), 
    lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = T), 
    lifetime_hard_elec_mt = mean(lifetime_hard_elec_mt, na.rm = T),
  ) %>%
  left_join(facility_lcoh_df_ca |>
              distinct(facility_id, state), 
            by = 'facility_id') |>
  group_by(state) %>%          
  summarize(
    lifetime_clean_grid_co2e_gt = sum(lifetime_clean_grid_co2e_mt, na.rm = T) / 1e9, 
    lifetime_elec_mt = sum(lifetime_elec_mt, na.rm = T) / 1e9, 
    lifetime_biogenic_gt = sum(lifetime_biogenic_mt, na.rm = T) / 1e9, 
    lifetime_hard_elec_gt = sum(lifetime_hard_elec_mt, na.rm = T) / 1e9,
    policy_label = "Baseline"
  ) |>
  ungroup() |>
  filter(state %in% fig_states)

eim_data_state <- 
  facility_lcoh_df_ca |>
  filter(
    policy_label %in% fig_policies, 
    tech_scenario %in% c('hp_boiler_ee'), 
    state %in% fig_states
  ) |>
  left_join(emissions_df, by = c('facility_id', 'tech_scenario', 'scenario_rank')) |>
  # Average best & worst case 
  group_by(facility_id, tech_scenario, policy_label) |>
  summarize(
    # Main info vars 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    
    lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = T), 
    lifetime_elec_mt = mean(lifetime_elec_mt, na.rm = T), 
    lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = T), 
    lifetime_hard_elec_mt = mean(lifetime_hard_elec_mt, na.rm = T),
    
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
        lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = T), 
        lifetime_elec_mt = mean(lifetime_elec_mt, na.rm = T), 
        lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = T), 
        lifetime_hard_elec_mt = mean(lifetime_hard_elec_mt, na.rm = T),
      ) |>
      ungroup() |>
      rename(
        base_lifetime_clean_grid_co2e_mt = lifetime_clean_grid_co2e_mt, 
        base_lifetime_elec_mt = lifetime_elec_mt, 
        base_lifetime_biogenic_mt = lifetime_biogenic_mt, 
        base_lifetime_hard_elec_mt = lifetime_hard_elec_mt
      ),
    by = 'facility_id'
  ) |>
  
  mutate(
    lifetime_clean_grid_effective = if_else(in_money == 1,
                                            lifetime_clean_grid_co2e_mt,
                                            base_lifetime_clean_grid_co2e_mt),
    lifetime_low_med_effective = if_else(in_money == 1,
                                         lifetime_elec_mt,
                                         base_lifetime_elec_mt),  
    lifetime_non_elec_effective = if_else(in_money == 1,
                                          lifetime_hard_elec_mt,
                                          base_lifetime_hard_elec_mt),
    lifetime_biogenic_effective = if_else(in_money == 1,
                                          lifetime_biogenic_mt,
                                          base_lifetime_biogenic_mt)
  ) |>
  
  # Getting sector-policy emissions in the money 
  group_by(state, policy_label) %>%
  summarize(
    lifetime_clean_grid_co2e_gt = sum(lifetime_clean_grid_effective, na.rm = T) / 1e9, 
    lifetime_elec_mt = sum(lifetime_low_med_effective, na.rm = T) / 1e9, 
    lifetime_biogenic_gt = sum(lifetime_biogenic_effective, na.rm = T) / 1e9, 
    lifetime_hard_elec_gt = sum(lifetime_non_elec_effective, na.rm = T) / 1e9
  ) %>%
  ungroup() %>%
  # Add the sector totals
  bind_rows(total_elec_ghg_emissions_state) %>%
  
  # Get to one emissions variable 
  pivot_longer(
    cols = starts_with("lifetime_"), 
    names_to = "emissions_type", 
    values_to = "emissions_gt"
  ) |>
  
  mutate(
    emissions_label = case_when(
      emissions_type == 'lifetime_elec_mt' ~ fig_emissions_labels[1], 
      emissions_type == 'lifetime_hard_elec_gt' ~ fig_emissions_labels[2],
      emissions_type == 'lifetime_biogenic_gt' ~ fig_emissions_labels[3], 
      emissions_type == 'lifetime_clean_grid_co2e_gt' ~ fig_emissions_labels[4]
    ), 
    
    emissions_label = factor(
      emissions_label, 
      levels = fig_emissions_labels
    ), 
    
    policy_label = factor(
      policy_label,
      levels = rev(fig_policies)
    ), 
    
    state = factor(state, levels = rev(sort(unique(as.character(state)))))
  )  %>%
  arrange(desc(state), factor(policy_label, levels = fig_policies))


eim_plot_state <- 
  ggplot() +
  
  geom_col(data = eim_data_state,
           aes(x = emissions_gt, y = policy_label, fill = emissions_label),
           position = position_stack(), 
           width = .4) +
  
  #facet_wrap(~ state, nrow = 1) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  scale_fill_manual(
    values = fig_emissions_colors,
    breaks = fig_emissions_labels,
    labels = fig_emissions_labels,
    name = NULL
  ) +
  
  labs(x = "Emissions (GtCO2e)", y = NULL, fill = "Emissions Source" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",
      linewidth = 0.2
    ),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.spacing.x = unit(0.3, "cm"),
    legend.box = "horizontal",
    
    # --- X-axis tick labels ---
    axis.text.x = element_text(
      size = 8,              # smaller text
      angle = 45,            # rotate 45 degrees
      hjust = 1,             # right-align so they don’t overlap
      vjust = 1              # vertically align nicely under ticks
    )
  )

eim_plot_state



#### FIG: DELTA LCOH PLOT ####

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

baseline_lcoh <- 
  facility_lcoh_df_ca |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    lcoh_ng = mean(lcoh, na.rm = T)
  ) |>
  ungroup() |>
  select(-tech_scenario)

delta_lcoh_data <- 
  facility_lcoh_df_ca |>
  filter(
    policy_label == "No Policy", 
    tech_scenario != 'baseline' 
  ) |>
  group_by(facility_id, tech_scenario_label) |>
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
    tech_scenario_label = factor(
      tech_scenario_label, 
      levels = tech_scenario_labels
    )
    # tech_scenario = factor(
    #   tech_scenario,
    #   levels = names(scenario_labels),
    #   labels = scenario_labels
    # )
  ) 

# Technology scenario x sector delta LCOH plot 
delta_lcoh_plot   <- 
  ggplot(delta_lcoh_data,
         aes(x = sector, y = lcoh_delta, color = tech_scenario_label)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = tech_scenario_colors, labels = tech_scenario_labels) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  
  scale_y_continuous(limits = c(-25, 75)) +
  
  labs(
    x = NULL,
    y = "Change in LCOH ($/MMBtu)",
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

delta_lcoh_plot


#### FIG: DELTA LCOH -- POLICY PLOT ####

fig_policies <- c(
  'No Policy',
  'ITC Capex -30%', 
  'ITC Capex -100%', 
  'ITC Elec -25%', 
  'Elec -50%', 
  'ITC Capex -30%, Elec -25%', 
  'PTC $10/MMBtu'
)

# make wrapped labels
fig_policies_labels <- str_wrap(fig_policies, width = 10)
names(fig_policies_labels) <- fig_policies

sector_colors <- c(
  "Chemicals"      = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper"   = "#A67C52"
)

baseline_lcoh <- 
  facility_lcoh_df_ca |>
  filter(policy_label == 'No Policy', tech_scenario == 'baseline') |>
  group_by(facility_id, tech_scenario) |>
  summarize(lcoh_ng = mean(lcoh, na.rm = TRUE), .groups = "drop") |>
  select(-tech_scenario)

delta_policy_data <- 
  facility_lcoh_df_ca |>
  filter(policy_label %in% fig_policies, tech_scenario == 'hp_boiler_ee') |>
  group_by(facility_id, policy_label) |>
  summarize(
    facility_name = first(facility_name), 
    naics_code = first(naics_code), 
    industry_clean = first(industry_clean), 
    sector = first(sector), 
    state = first(state),
    lcoh = mean(lcoh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(baseline_lcoh, by = 'facility_id') |>
  mutate(
    lcoh_delta = lcoh - lcoh_ng,
    policy_label = factor(policy_label, levels = fig_policies)
  )

delta_policy_plot <- 
  ggplot(delta_policy_data,
         aes(x = policy_label, y = lcoh_delta, color = sector)) +
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  
  scale_color_manual(values = sector_colors) +
  scale_y_continuous(limits = c(-10, 100)) +
  
  # 💡 Here’s the key: use your wrapped labels for the x-axis
  scale_x_discrete(labels = fig_policies_labels) +
  
  labs(
    x = NULL,
    y = "Change in LCOH ($/MMBtu)",
    color = "Sector"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.background = element_rect(color = "black", linewidth = 0.2),
    legend.margin = margin(2.5, 2.5, 2.5, 2.5)
  )

delta_policy_plot



#### LCOH V ELECTRICITY ####

st <- 'CA'
lcoh_func <- function(
    ## parameters
  r, 
  elec_price,
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
  heat_mmbtu,
  change_in_electricity_demand_kwh,
  
  ## policy scenarios
  capex_subsidy, 
  elec_discount){
  # time discounting formula 
  discount_sum <- sum((1 + r)^-(1:t))
  
  ## Inputting different parameters for different tech scenarios 
  case_when(
    str_detect(tech_scenario, "baseline") & scenario_rank == 'best' ~ {
      opex_ng <- (heat_mmbtu/.75) * ng_price      # energy costs
      opex_om <- ngboiler_om_high * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "baseline") & scenario_rank == 'worst' ~ {
      opex_ng <- (heat_mmbtu/.9) * ng_price       # energy costs
      opex_om <- ngboiler_om_low * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, 'eb_boiler') & scenario_rank == 'best' ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- eboiler_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "eb_boiler") & scenario_rank == 'worst' ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- eboiler_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, 'hp_boiler') & scenario_rank == 'best' ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- hthp_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "hp_boiler") & scenario_rank == 'worst' ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- hthp_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }
  )
}

# Import parameters 
param <- 
  read_csv('national_results/data/parameters_fuelrange.csv') %>%
  filter(state == st)

x_vals <- seq(0, 0.1, length.out = 200)

ng_min <- min(facility_lcoh_df_ca$lcoh[
  facility_lcoh_df_ca$tech_scenario == "baseline" &
    facility_lcoh_df_ca$scenario_rank == "best"
], na.rm = TRUE)

ng_max <- max(facility_lcoh_df_ca$lcoh[
  facility_lcoh_df_ca$tech_scenario == "baseline" &
    facility_lcoh_df_ca$scenario_rank == "best"
], na.rm = TRUE)


elec_plot_df <- 
  facility_lcoh_df_ca |>
  # Filter to no policy support, Scenario4 outcomes
  filter(tech_scenario == 'hp_boiler_ee', 
         policy_label == 'No Policy') |>
  
  # Summarize at the scenario level. 
  group_by(tech_scenario) |>
  summarize(
    #sector = min(sector), 
    capex = mean(capex, na.rm = T), 
    change_in_electricity_demand_kwh = mean(change_in_electricity_demand_kwh, na.rm = T), 
    heat_mmbtu = mean(elec_orig_process_unit_heat_demand, na.rm = T), 
    tech_scenario = min(tech_scenario, na.rm = T)
  ) |>
  ungroup() |>
  tidyr::crossing(x = x_vals) |>
  mutate(
    lcoh = lcoh_func(
      r = param$r[1], 
      elec_price = x,                      # explicitly name this
      ng_price = param$ng_price_high[1],
      t = param$t[1], 
      ngboiler_om_low = param$ngboiler_om_low[1], 
      ngboiler_om_high = param$ngboiler_om_high[1], 
      eboiler_om_low = param$eboiler_om_low[1], 
      eboiler_om_high = param$eboiler_om_high[1], 
      hthp_om_low = param$hthp_om_low[1], 
      hthp_om_high = param$hthp_om_high[1], 
      ## tech scenario + calculations 
      tech_scenario = tech_scenario,
      scenario_rank = "best",
      capex = capex,
      heat_mmbtu = heat_mmbtu,
      change_in_electricity_demand_kwh = change_in_electricity_demand_kwh,
      ## policy scenarios
      capex_subsidy = 0, 
      elec_discount = 0 
    ),
    x = x * 100
  )
  # ) |>
  # # average across best and worst case scenarios to get the sector-level outcome
  # group_by(x) |>
  # summarize(
  #   lcoh = mean(lcoh)
  # )

# Get the point at which the heat pump LCOH line intersects the NG LCOH 
ng_x_intercept <- with(elec_plot_df, approx(lcoh, x, xout = ng_max))$y

lcoh_v_elec_plot <- 
  ggplot() +
  # Main LCOH curve
  geom_line(data = elec_plot_df,
            aes(x = x, y = lcoh, color = "Heat Pump + EE", linetype = "Heat Pump + EE"),
            size = 1) +
  
  # NG Boiler reference line
  geom_hline(aes(yintercept = ng_max,
                 color = "NG Boiler (max)",
                 linetype = "NG Boiler (max)"),
             size = 0.75) +
  
  # Other vlines not in legend
  geom_vline(xintercept = param$elec_price_low * 100, color = "#FFBF00", size = 0.5) +
  geom_vline(xintercept = ng_x_intercept, color = "#FFBF00", linetype = "longdash", size = 0.75) +
  
  # Reverse x-axis
  scale_x_reverse() +
  
  # Unified legend with manual scales
  scale_color_manual(
    name = NULL,
    values = c(
      "Heat Pump + EE" = "#8FB339",
      "NG Boiler (max)" = "grey50"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Heat Pump + EE" = "solid",
      "NG Boiler (max)" = "solid"
    )
  ) +
  
  labs(
    y = "Levelized Cost of Heat ($/MMBtu)",
    x = "Cost of Electricity (¢/kWH)"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.75, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(
      color = "black",
      linewidth = 0.2
    )
  )

lcoh_v_elec_plot


#### SAVE FIGURES ####

safe_save <- function(plot_obj, width, height, dpi = 300,
                      dir = glue::glue("state_fact_sheets/outputs/state-fact-sheet-figures/CA/{format(Sys.Date(), '%Y%m%d')}")) {
  
  # Capture plot name (for file naming)
  obj_name <- deparse(substitute(plot_obj))
  
  # Make sure directory exists
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  
  # Build full file path
  file_name <- glue::glue("{dir}/{obj_name}_{format(Sys.Date(), '%Y%m%d')}.png")
  
  # --- Validation ---
  if (!exists(obj_name, envir = parent.frame())) {
    message(glue::glue("⚠️ Skipped: object '{obj_name}' not found in calling environment."))
    return(invisible(NULL))
  }
  
  if (!inherits(plot_obj, "gg")) {
    message(glue::glue("⚠️ Skipped: '{obj_name}' is not a ggplot object."))
    return(invisible(NULL))
  }
  
  # --- Save plot safely ---
  ggplot2::ggsave(filename = file_name, plot = plot_obj,
                  width = width, height = height, dpi = dpi)
  
  message(glue::glue("✅ Saved {obj_name} → {file_name}"))
  
  invisible(file_name)
}

safe_save(acc_plot, width = 10, height = 5)
safe_save(capex_plot, width = 8, height = 5)
safe_save(delta_lcoh_plot, width = 8, height = 5)
safe_save(delta_policy_plot, width = 8, height = 5)
safe_save(eim_plot_nat, width = 8, height = 5)
safe_save(eim_plot_state, width = 8, height = 5)
safe_save(emissions_plot_nat, width = 8, height = 5)
safe_save(fuels_plot, width = 8, height = 5)
safe_save(heat_plot, width = 8, height = 5)
safe_save(heat_plot_2, width = 8, height = 5)
safe_save(in_payback_plot, width = 8, height = 5)
safe_save(in_payback_plot_nopol, width = 8, height = 5)
safe_save(payback_bubble_plot, width = 8, height = 5)
safe_save(payback_capex_xy_plot, width = 8, height = 5)
safe_save(payback_opex_xy_plot, width = 8, height = 5)
safe_save(payback_xy_plot, width = 8, height = 5)
safe_save(payback_plot, width = 8, height = 5)
safe_save(payback_plot_nopol, width = 6, height = 7)
