# Integrated National Data & Figure Code #
## November 6, 2025 ##

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
        paste0("ITC Capex -", ITC * 100, "%, Elec -", rate_reduction * 100, "%"),
      rate_reduction == 0 & ITC > 0 & PTC == 0 ~
        paste0("ITC Capex -", ITC * 100, "%"),
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

r <- .065
t <- 25
discount_sum <- (1 - (1 + r)^(-t)) / r

emissions_df <-
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/total_emissions_facility_251106.csv') |>
  group_by(facility_id, tech_scenario, scenario_rank) |>
  rename(
    non_elec_mt = high_temp_mt
  ) |>
  summarize(
    # Present-day emissions figures 
    baseline_co2e_mt_25 = co2e_baseline_mt[policy_year == 2025],
    clean_grid_co2e_mt_25 = clean_grid_co2e_emission_mt[policy_year == 2025], 
    bau_grid_co2e_mt_25 = bau_grid_co2e_emission_mt[policy_year == 2025],
    low_med_mt_25 = low_med_mt[policy_year == 2025], 
    non_elec_mt_25 = non_elec_mt[policy_year == 2025], 
    biogenic_mt_25 = biogenic_mt[policy_year == 2025],
    
    # Lifetime emissions figures 
    baseline_co2e_mt = sum(co2e_baseline_mt, na.rm = T), 
    clean_grid_co2e_mt = sum(clean_grid_co2e_emission_mt, na.rm = T), 
    bau_grid_co2e_mt = sum(bau_grid_co2e_emission_mt, na.rm = T),
    low_med_mt = sum(low_med_mt, na.rm = T), 
    non_elec_mt = sum(non_elec_mt, na.rm = T), 
    biogenic_mt = sum(biogenic_mt, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    # Discounted lifetime emissions
    clean_grid_co2e_mt_dc = clean_grid_co2e_mt * discount_sum, 
    bau_grid_co2e_mt_dc = bau_grid_co2e_mt * discount_sum, 
    low_med_mt_dc = low_med_mt * discount_sum, 
    non_elec_mt_dc = non_elec_mt * discount_sum, 
    biogenic_mt_dc = biogenic_mt * discount_sum, 
    
    # Annual emissions reductions today 
    emissions_reduc_mt_25 = baseline_co2e_mt_25 - (clean_grid_co2e_mt_25 + low_med_mt_25 + non_elec_mt_25 + biogenic_mt_25), 
    
    # Lifetime emissions reductions
    emissions_reduc_mt = baseline_co2e_mt - (clean_grid_co2e_mt + low_med_mt + non_elec_mt + biogenic_mt)
  )
  
  

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
#   filter(facility_id %in% facility_lcoh_df$facility_id)

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
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
    #axis.text.x = element_text(angle = 15, hjust = 1)
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
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
    #axis.text.x = element_text(angle = 15, hjust = 1)
  )

heat_plot_2


#### FIG: CAPEX PLOT ####
fig_scenarios <-
  c("Baseline", 
    "E-boiler", 
    "E-boiler\n(EE+)", 
    "Air-Source\nHTHP", 
    "Air-Source\nHTHP (EE+)")

sector_colors <- 
  c(
    "Chemicals"     = "#09847A",
    "Food & Beverage" = "#EF5645",
    "Pulp & Paper"  = "#A67C52"
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
  labs(x = NULL, y = "Total CAPEX ($ millions)") +
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

#### FIG: PERCENT IN PAYBACK -- NO POLICY ####

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

scenario_labels <- c(
  "eb_boiler"     = "E-Boiler",
  "eb_boiler_ee"  = "E-Boiler\n(EE+)",
  "hp_boiler"     = "Air-Source\nHTHP",
  "hp_boiler_ee"  = "Air-Source\nHTHP (EE+)"
)

payback_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id) |>
  summarize(
    capex_ng = mean(capex, na.rm = T),
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup()

in_payback_data_nopol <- 
  facility_lcoh_df |>
  filter(
    policy_label == 'No Policy' 
  ) |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    capex_hp = mean(capex, na.rm = T), 
    opex_hp = mean(opex, na.rm = T)
  ) |>
  ungroup() |>
  
  left_join(payback_data_ng, by = 'facility_id') |>
  
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng, 
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_),
    
    sub_5 = if_else(payback_years <= 5, 1, 0), 
    sub_15 = if_else(payback_years <= 15 & payback_years > 5, 1, 0), 
    plus_15 = if_else(!is.na(payback_years) & payback_years > 15, 1, 0)
  ) |>
  group_by(tech_scenario, sector) |>
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
    )
  ) |>
  filter(tech_scenario != 'baseline')

in_payback_plot_nopol   <- 
  ggplot() +
  
  geom_col(data = in_payback_data_nopol,
           aes(x = tech_scenario, y = pct_in_payback, fill = sector_pct),
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
  
  scale_x_discrete(labels = scenario_labels) +
  
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
    axis.text.x = element_text(size = 7)
  )

in_payback_plot_nopol



#### FIG: NESTED PAYBACK BUBBLE PLOT ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)


payback_bubble_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id) |>
  summarize(
    capex_ng = mean(capex, na.rm = T),
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup()


payback_bubble_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler' &
      policy_label == 'No Policy' 
  ) |>
  left_join(emissions_df |>
              filter(tech_scenario == 'baseline') |>
              select(facility_id, low_med_mt_25), 
            by = c('facility_id', 'scenario_rank')
            ) |>
  group_by(facility_id) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    capex_hp = mean(capex, na.rm = T), 
    opex_hp = mean(opex, na.rm = T), 
    low_med_mt_25 = mean(low_med_mt_25, na.rm = T)
  ) |>
  ungroup() |>
  
  left_join(payback_bubble_data_ng, by = 'facility_id') |>
  
  group_by(state, industry_clean) |>
  summarize(
    sector = first(sector),
    
    opex_hp = mean(opex_hp, na.rm = T), 
    capex_hp = mean(capex_hp, na.rm = T), 
    opex_ng = mean(opex_ng, na.rm = T), 
    capex_ng = mean(capex_ng, na.rm = T), 
    low_med_mt_25 = sum(low_med_mt_25, na.rm = T) / 1000000
  ) |>
  ungroup() |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng, 
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_),
    
    statesub = paste(state, industry_clean, sep = " "), 
    statesub_label = str_wrap(statesub, width = 10) 
  ) |>
  select(state, statesub, industry_clean, sector, payback_years, elec_ghg_emissions) |>
  filter(!is.na(payback_years),
         payback_years < 21) |>
  group_by(sector) |>
  slice_min(order_by = payback_years, n = 10) |>
  ungroup()


# Collapse facility-level data to subsector level
hier_data <- 
  payback_bubble_data |>
  group_by(sector, statesub) |>
  summarize(
    state = first(state), 
    low_med_mt_25 = sum(low_med_mt_25, na.rm = TRUE),
    avg_payback = mean(payback_years, na.rm = TRUE)
  ) |>
  ungroup()

# Edge list (sector → subsector)
edges <- 
  hier_data |>
  distinct(from = sector, to = statesub)

# Vertex list (both sector + subsector)
vertices <- 
  bind_rows(
    # Sector-level nodes
    hier_data |>
      distinct(name = sector) |>
      mutate(
        level = 1,
        size = 0,
        avg_payback = NA,
        sector = name
      ),
    # Subsector-level nodes
    hier_data |>
      transmute(
        name = statesub,
        level = 2,
        size = low_med_mt_25,
        avg_payback = avg_payback,
        sector = sector, 
        state = state
      )
  )

mygraph <- graph_from_data_frame(edges, vertices = vertices)

ggraph(mygraph, layout = "circlepack", weight = size * .9) +
  geom_node_circle(
    aes(filter = !leaf, color = sector),
    fill = NA,
    linewidth = .8,
    alpha = 0.9
  ) +
  
  # --- Discrete colors for sector outlines ---
  scale_color_manual(
    #values = sector_colors,
    values = c('white', 'white', 'white'),
    guide = "none"
  ) +
  
  # --- Subsector circles (fill = payback, outline = sector) ---
  geom_node_circle(
    aes(fill = avg_payback, color = sector, filter = leaf),
    linewidth = 0.3, alpha = 0.9
  ) +
  
  # --- Payback color scale (for fill) ---
  scale_fill_gradientn(
    colours = c("#FEBC11", 
                lighten("#FEBC11", amount = .4), 
                lighten("#FEBC11", amount = .8)),  
    name = "Payback (Years)",
    na.value = "grey90"
  ) +
  
  # --- Labels ---
  geom_node_text(
    aes(label = ifelse(leaf, state, ""), 
        size = sqrt(size)),   # use bubble area (size) to scale font
    lineheight = 0.9,
    color = "black",
    check_overlap = FALSE,
    show.legend = FALSE
  ) +
  
  coord_equal() +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

ggraph(mygraph, layout = "circlepack", weight = size * 0.9) +
  
  # --- Big sector circles: white outlines only ---
  geom_node_circle(
    aes(filter = !leaf),
    fill = NA,
    color = "white",
    linewidth = 0.8,
    alpha = 1
  ) +
  
  # --- Subsector circles: gradient fill, colored border by sector ---
  geom_node_circle(
    aes(fill = avg_payback, color = sector, filter = leaf),
    linewidth = 0.4,
    alpha = 0.95
  ) +
  
  # --- Gradient for payback fill ---
  scale_fill_gradientn(
    colours = c("#C9F0D6",
                lighten("#C9F0D6", amount = 0.4), 
                "#FEBC11"
                ),
    name = "Payback (Years)",
    na.value = "grey90"
  ) +
  
  # --- Outline color for subsectors (sector-coded) ---
  scale_color_manual(
    values = sector_colors,
    guide = "none"
  ) +
  
  # --- Labels ---
  geom_node_text(
    aes(label = ifelse(leaf, state, ""), 
        size = sqrt(size)),
    lineheight = 0.9,
    color = "black",
    check_overlap = FALSE,
    show.legend = FALSE
  ) +
  
  coord_equal() +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

ggraph(mygraph, layout = "circlepack", weight = size * 0.9) +
  
  # --- Big sector circles: white outlines only ---
  geom_node_circle(
    aes(filter = !leaf),
    fill = NA,
    color = "white",
    linewidth = 0.8,
    alpha = 1
  ) +
  
  # --- Subsector circles: gradient fill, colored border by sector ---
  geom_node_circle(
    aes(fill = avg_payback, color = sector, filter = leaf),
    linewidth = 0.4,
    alpha = 0.95
  ) +
  
  # --- Gradient for payback fill ---
  scale_fill_gradientn(
    colours = c("#C9F0D6",
                lighten("#C9F0D6", amount = 0.4), 
                "#FEBC11"),
    name = "Payback (Years)",
    na.value = "grey90"
  ) +
  
  # --- Outline color for subsectors (sector-coded) ---
  scale_color_manual(
    values = sector_colors,
    guide = "none"
  ) +
  
  # --- Labels (sector-colored text) ---
  geom_node_text(
    aes(
      label = ifelse(leaf, state, ""),
      size = sqrt(size),
      color = sector               # <— map text color to sector
    ),
    lineheight = 0.9,
    check_overlap = FALSE,
    show.legend = FALSE
  ) +
  
  # --- Reuse same sector color palette for text ---
  scale_color_manual(values = sector_colors, guide = "none") +
  
  coord_equal() +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )


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
  'Capex -30%', 
  'Capex -50%'
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


payback_bubble_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id) |>
  summarize(
    capex_ng = mean(capex, na.rm = T),
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup()


payback_bubble_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee', 
    policy_label %in% fig_policies , 
    state %in% fig_states
  ) |>
  left_join(emissions_df |>
              filter(tech_scenario == 'baseline') |>
              select(facility_id, low_med_mt_25), 
            by = c('facility_id', 'scenario_rank')
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 

    capex_hp = mean(capex, na.rm = T), 
    opex_hp = mean(opex, na.rm = T), 
    low_med_mt_25 = mean(low_med_mt_25, na.rm = T)
  ) |>
  ungroup() |>
  
  left_join(payback_bubble_data_ng, by = 'facility_id') |>
  
  group_by(state, industry_clean, policy_label) |>
  summarize(
    sector = first(sector),
    
    opex_hp = mean(opex_hp, na.rm = T), 
    capex_hp = mean(capex_hp, na.rm = T), 
    opex_ng = mean(opex_ng, na.rm = T), 
    capex_ng = mean(capex_ng, na.rm = T), 
    low_med_mt_25 = sum(low_med_mt_25, na.rm = T) / 1000000
  ) |>
  ungroup() |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng, 
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_),
    
    statesub = paste(state, industry_clean, sep = " "), 
    statesub_label = str_wrap(statesub, width = 10), 
    policy_label = factor(
      policy_label, 
      levels = fig_policies
    )
  ) |>
  select(state, industry_clean, sector, policy_label, payback_years, low_med_mt_25) |>
  filter(!is.na(payback_years),
         payback_years < 50
  )

ggplot(payback_bubble_data,
       aes(x = state,
           y = payback_years,
           size = low_med_mt_25,
           fill = industry_clean))+
  geom_point(shape = 21, color = "grey30", alpha = 0.85, stroke = 0.3,
             position = position_jitter(width = 0.2, height = 0)) +
  facet_wrap(~ policy_label, nrow = 1) +
  
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


payback_bubble_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id) |>
  summarize(
    capex_ng = mean(capex, na.rm = T),
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup()


payback_bubble_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee', 
    policy_label %in% fig_policies , 
    state %in% fig_states
  ) |>
  left_join(emissions_df |>
              filter(tech_scenario == 'baseline') |>
              select(facility_id, low_med_mt_25), 
            by = c('facility_id', 'scenario_rank')
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    capex_hp = mean(capex, na.rm = T), 
    opex_hp = mean(opex, na.rm = T), 
    low_med_mt_25 = mean(low_med_mt_25, na.rm = T)
  ) |>
  ungroup() |>
  
  left_join(payback_bubble_data_ng, by = 'facility_id') |>
  
  group_by(state, industry_clean, policy_label) |>
  summarize(
    sector = first(sector),
    
    opex_hp = mean(opex_hp, na.rm = T), 
    capex_hp = mean(capex_hp, na.rm = T), 
    opex_ng = mean(opex_ng, na.rm = T), 
    capex_ng = mean(capex_ng, na.rm = T), 
    low_med_mt_25 = sum(low_med_mt_25, na.rm = T) / 1000000
  ) |>
  ungroup() |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng, 
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_),
    
    statesub = paste(state, industry_clean, sep = " "), 
    statesub_label = str_wrap(statesub, width = 10), 
    policy_label = factor(
      policy_label, 
      levels = fig_policies
    )
  ) |>
  select(state, industry_clean, sector, policy_label, payback_years, low_med_mt_25) |>
  filter(!is.na(payback_years),
         payback_years < 50
  )

ggplot(payback_bubble_data,
       aes(x = state,
           y = payback_years,
           size = low_med_mt_25,
           fill = industry_clean))+
  geom_point(shape = 21, color = "grey30", alpha = 0.85, stroke = 0.3,
             position = position_jitter(width = 0.2, height = 0)) +
  facet_wrap(~ policy_label, nrow = 1) +
  
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

payback_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id) |>
  summarize(
    capex_ng = mean(capex, na.rm = T),
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup()

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
    
    capex_hp = mean(capex, na.rm = T), 
    opex_hp = mean(opex, na.rm = T)
  ) |>
  ungroup() |>
  
  left_join(payback_data_ng, by = 'facility_id') |>
  
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng, 
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_),
    
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
    axis.text.x = element_text(size = 7)
  )

in_payback_plot

#### FIG: PAYBACK POLICY PLOT  ####

fig_policies <- 
  c(
    "ITC Capex -50%", 
    "Elec -25%", 
    "No Policy"
  )

fig_policy_colors <- c(
  "No Policy"     = '#FEBC11',
  "Elec -25%"     = "#09847A",
  "ITC Capex -50%"   = '#003660'
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
  "Total In-Scope Emissions", 
  "No Policy",
  "ITC Capex -100%", 
  "Elec -25%", 
  "Elec -50%", 
  "PTC $10/MMBtu"
)

fig_policy_colors <- c(
  "Total In-Scope Emissions" = "grey70",
  "No Policy"     = lighten('#09847A', amount = 0.45),  # lightest
  "ITC Capex -100%"   = lighten('#09847A', amount = 0.25),
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
  "ITC Capex -100%", 
  "Elec -25%", 
  "Elec -50%", 
  "PTC $10/MMBtu"
)

fig_policy_colors <- c(
  "Total In-Scope Emissions" = "grey70",
  "No Policy"     = lighten('#09847A', amount = 0.45),  # lightest
  "ITC Capex -100%"   = lighten('#09847A', amount = 0.25),
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

#### FIG: PAYBACK PLOT NO POLICY ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
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

#### SUMMARY STATISTICS ####
capex_summary <- 
  capex_data |>
  group_by(sector, tech_scenario) |>
  summarize(
    n_facilities = n(),
    capex_mean   = mean(capex_mill, na.rm = TRUE),
    capex_median = median(capex_mill, na.rm = TRUE),
    capex_q1     = quantile(capex_mill, 0.25, na.rm = TRUE),
    capex_q3     = quantile(capex_mill, 0.75, na.rm = TRUE),
    capex_sd     = sd(capex_mill, na.rm = TRUE),
    capex_min    = min(capex_mill, na.rm = TRUE),
    capex_max    = max(capex_mill, na.rm = TRUE)
  ) |>
  ungroup()

write_csv(capex_summary, 'national_results/data/summary_stats/capex_summary.csv')
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

#### FIG: PAYBACK BUBBLE X/Y ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)

fig_subsector_colors <- c(
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
  "Pulp & Paper"               = "#A67C52",
  "Toilet Paper"               = "#8DA452"
)

payback_bubble_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id) |>
  summarize(
    capex_ng = mean(capex, na.rm = T),
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup()


payback_bubble_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler' &
      policy_label == 'No Policy' 
  ) |>
  left_join(emissions_df |>
              select(facility_id, elec_ghg_emissions), by = 'facility_id') |>
  group_by(facility_id) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    capex_hp = mean(capex, na.rm = T), 
    opex_hp = mean(opex, na.rm = T), 
    elec_ghg_emissions = mean(elec_ghg_emissions, na.rm = T)
  ) |>
  ungroup() |>
  
  left_join(payback_bubble_data_ng, by = 'facility_id') |>
  
  group_by(state, industry_clean) |>
  summarize(
    sector = first(sector),
    
    opex_hp = mean(opex_hp, na.rm = T), 
    capex_hp = mean(capex_hp, na.rm = T), 
    opex_ng = mean(opex_ng, na.rm = T), 
    capex_ng = mean(capex_ng, na.rm = T), 
    elec_ghg_emissions = sum(elec_ghg_emissions, na.rm = T) / 1000000
  ) |>
  ungroup() |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng, 
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_),
    
    statesub = paste(state, industry_clean, sep = " "), 
    statesub_label = str_wrap(statesub, width = 10) 
  ) |>
  select(state, industry_clean, sector, payback_years, elec_ghg_emissions) |>
  filter(!is.na(payback_years), 
         payback_years < 50
  ) 

ggplot(payback_bubble_data,
       aes(x = state,
           y = payback_years,
           size = elec_ghg_emissions,
           fill = industry_clean)
)+
  geom_point(shape = 21, color = "grey30", alpha = 0.85, stroke = 0.3,
             position = position_jitter(width = 0.2, height = 0)) +
  scale_fill_manual(values = fig_subsector_colors, name = "Subsector") +
  scale_size_continuous(
    range = c(3, 15),
    name = "In-Scope CO2e Emissions (Mt)",
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
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

ggplot(payback_bubble_data,
       aes(x = state,
           y = payback_years,
           size = elec_ghg_emissions,
           fill = industry_clean)) +
  geom_point(shape = 21, color = "grey30", alpha = 0.85, stroke = 0.3,
             position = position_jitter(width = 0.2, height = 0)) +
  scale_fill_manual(values = fig_subsector_colors, name = "Subsector") +
  scale_size_continuous(
    range = c(3, 15),
    name = "In-Scope CO₂e Emissions (Mt)",
    labels = function(x) sprintf("%.1f Mt", x)
  ) +
  labs(x = NULL, y = "Payback Period (Years)") +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "right",
    legend.box = "horizontal",   # <-- place the two guides side-by-side
    legend.title = element_text(face = "bold", size = 9),
    legend.text  = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.box.spacing = unit(0.3, "cm")
  ) +
  guides(
    fill = guide_legend(
      title = "Subsector",
      title.position = "top",
      ncol = 1,       # <-- keep items vertical
      byrow = TRUE,
      order = 1
    ),
    size = guide_legend(
      title = "In-Scope CO2e\nEmissions (Mt)",
      title.position = "top",
      order = 2,
      override.aes = list(shape = 21, fill = "grey70", color = "grey50", alpha = 0.8)
    )
  )

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



#### FIG: PAYBACK BUBBLE PLOT ####

sector_colors <- c(
  "Chemicals"      = "#09847A",
  "Food & Beverage"= "#EF5645",
  "Pulp & Paper"   = "#A67C52"
)

payback_bubble_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == "No Policy", tech_scenario == "baseline") |>
  group_by(facility_id) |>
  summarize(
    capex_ng = mean(capex, na.rm = TRUE),
    opex_ng  = mean(opex,  na.rm = TRUE),
    .groups = "drop"
  )

payback_bubble_data <-
  facility_lcoh_df |>
  filter(tech_scenario == "hp_boiler", policy_label == "No Policy") |>
  left_join(emissions_df |> select(facility_id, elec_ghg_emissions),
            by = "facility_id") |>
  group_by(facility_id) |>
  summarize(
    industry_clean     = first(industry_clean),
    sector             = first(sector),
    state              = first(state),
    capex_hp           = mean(capex, na.rm = TRUE),
    opex_hp            = mean(opex,  na.rm = TRUE),
    elec_ghg_emissions = mean(elec_ghg_emissions, na.rm = TRUE)/1e6,
    .groups = "drop"
  ) |>
  left_join(payback_bubble_data_ng, by = "facility_id") |>
  group_by(state, industry_clean) |>
  summarize(
    sector             = first(sector),
    opex_hp            = mean(opex_hp, na.rm = TRUE),
    capex_hp           = mean(capex_hp, na.rm = TRUE),
    opex_ng            = mean(opex_ng, na.rm = TRUE),
    capex_ng           = mean(capex_ng, na.rm = TRUE),
    elec_ghg_emissions = sum(elec_ghg_emissions, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff     = capex_hp - capex_ng,
    payback_years  = if_else(annual_savings > 0, capex_diff/annual_savings, NA_real_),
    statesub       = paste(state, industry_clean, sep = " "),
    statesub_label = str_wrap(statesub, width = 10)
  ) |>
  filter(!is.na(payback_years), payback_years < 21)

make_sector_bubbles <- function(df, sector_name, scale_factor = 1.2) {
  df_sector <- df |> filter(sector == sector_name)
  
  # circle-packing layout based on radius ∝ sqrt(emissions)
  df_sector <- df_sector |>
    mutate(radius = sqrt(elec_ghg_emissions) * scale_factor)
  layout <- circleProgressiveLayout(df_sector$radius, sizetype = "radius")
  df_sector <- bind_cols(df_sector, layout)
  circle_vertices <- circleLayoutVertices(layout, npoints = 60)
  
  # bubble-size legend data (choose representative values)
  legend_data <- tibble(
    x = 1,
    y = seq(3, 1, length.out = 3),
    elec_ghg_emissions = c(max(df_sector$elec_ghg_emissions, na.rm = TRUE),
                           median(df_sector$elec_ghg_emissions, na.rm = TRUE),
                           min(df_sector$elec_ghg_emissions, na.rm = TRUE)),
    label = sprintf("%.1f Mt", c(max(df_sector$elec_ghg_emissions, na.rm = TRUE),
                                 median(df_sector$elec_ghg_emissions, na.rm = TRUE),
                                 min(df_sector$elec_ghg_emissions, na.rm = TRUE)))
  )
  
  ggplot() +
    geom_polygon(
      data = circle_vertices,
      aes(x, y, group = id,
          fill = df_sector$payback_years[id]),
      color = sector_colors[sector_name],
      linewidth = 0.4, alpha = 0.9
    ) +
    geom_text(
      data = df_sector,
      aes(x, y, label = statesub_label),
      color = "black", size = 3,
      lineheight = 0.9, check_overlap = TRUE
    ) +
    geom_point(
      data = legend_data,
      aes(x, y, size = elec_ghg_emissions),
      shape = 21, fill = "grey85", color = "grey40"
    ) +
    geom_text(
      data = legend_data,
      aes(x, y, label = label),
      hjust = -0.7, vjust = 0.5, size = 3
    ) +
    scale_fill_gradient2(
      low = "#003660", high = "#FEBC11", midpoint = 5,
      name = "Payback (Years)"
    ) +
    scale_size_continuous(
      range = c(3, 15),
      name = "CO₂e (Mt)",
      guide = "none"
    ) +
    coord_equal() +
    theme_void() +
    ggtitle(sector_name) +
    theme(
      plot.title   = element_text(face = "bold", hjust = 0.5, size = 14),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 10),
      legend.text  = element_text(size = 8)
    )
}

chem_plot <- make_sector_bubbles(payback_bubble_data, "Chemicals")
fnb_plot  <- make_sector_bubbles(payback_bubble_data, "Food & Beverage")
pnp_plot  <- make_sector_bubbles(payback_bubble_data, "Pulp & Paper")

chem_plot
fnb_plot
pnp_plot

#### FIG: PAYBACK BUBBLE PLOT — global scale ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)

# --- baseline OPEX/CAPEX ---
payback_bubble_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', tech_scenario == 'baseline') |>
  group_by(facility_id) |>
  summarize(
    capex_ng = mean(capex, na.rm = TRUE),
    opex_ng = mean(opex, na.rm = TRUE)
  ) |>
  ungroup()

# --- main data ---
payback_bubble_data <- 
  facility_lcoh_df |>
  filter(tech_scenario == 'hp_boiler', policy_label == 'No Policy') |>
  left_join(emissions_df |> select(facility_id, elec_ghg_emissions),
            by = 'facility_id') |>
  group_by(facility_id) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector),
    state = first(state),
    capex_hp = mean(capex, na.rm = TRUE),
    opex_hp = mean(opex, na.rm = TRUE),
    elec_ghg_emissions = mean(elec_ghg_emissions, na.rm = TRUE) / 1e6  # in Mt
  ) |>
  ungroup() |>
  left_join(payback_bubble_data_ng, by = 'facility_id') |>
  group_by(state, industry_clean) |>
  summarize(
    sector = first(sector),
    opex_hp = mean(opex_hp, na.rm = TRUE),
    capex_hp = mean(capex_hp, na.rm = TRUE),
    opex_ng = mean(opex_ng, na.rm = TRUE),
    capex_ng = mean(capex_ng, na.rm = TRUE),
    elec_ghg_emissions = sum(elec_ghg_emissions, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng,
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_),
    statesub = paste(state, industry_clean, sep = " "),
    statesub_label = str_wrap(statesub, width = 10)
  ) |>
  filter(!is.na(payback_years), payback_years < 21)

# --- compute global radii (area ∝ emissions) ---
# You can tweak scale_factor to adjust density
scale_factor <- 1.2
payback_bubble_data <- payback_bubble_data |>
  mutate(radius = sqrt(elec_ghg_emissions) * scale_factor)

# --- single global layout across all sectors ---
layout <- circleProgressiveLayout(payback_bubble_data$radius, sizetype = "radius")
payback_bubble_data <- bind_cols(payback_bubble_data, layout)
circle_vertices <- circleLayoutVertices(layout, npoints = 60)

# --- plot ---
payback_bubble_plot <- ggplot() +
  geom_polygon(
    data = circle_vertices,
    aes(
      x, y, group = id,
      fill = payback_bubble_data$payback_years[id],
      color = payback_bubble_data$sector[id]
    ),
    linewidth = 0.3, alpha = 0.9
  ) +
  geom_text(
    data = payback_bubble_data,
    aes(x, y, label = statesub_label),
    color = "black", size = 3, lineheight = 0.9, check_overlap = TRUE
  ) +
  scale_fill_gradient2(
    low = "#003660", high = "#FEBC11", midpoint = 5,
    name = "Payback (Years)"
  ) +
  scale_color_manual(values = sector_colors, name = "Sector") +
  coord_equal() +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  ) +
  labs(title = "Global-Scale Payback Bubble Plot")

payback_bubble_plot


#### FIG: PAYBACK BUBBLE PLOT (ARTIFIAL SPACING) ####
sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)


payback_bubble_data_ng <-
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id) |>
  summarize(
    capex_ng = mean(capex, na.rm = T),
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup()


payback_bubble_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler' &
      policy_label == 'No Policy' 
  ) |>
  left_join(emissions_df |>
              select(facility_id, elec_ghg_emissions), by = 'facility_id') |>
  group_by(facility_id) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    capex_hp = mean(capex, na.rm = T), 
    opex_hp = mean(opex, na.rm = T), 
    elec_ghg_emissions = mean(elec_ghg_emissions, na.rm = T)
  ) |>
  ungroup() |>
  
  left_join(payback_bubble_data_ng, by = 'facility_id') |>
  
  group_by(state, industry_clean) |>
  summarize(
    sector = first(sector),
    
    opex_hp = mean(opex_hp, na.rm = T), 
    capex_hp = mean(capex_hp, na.rm = T), 
    opex_ng = mean(opex_ng, na.rm = T), 
    capex_ng = mean(capex_ng, na.rm = T), 
    elec_ghg_emissions = sum(elec_ghg_emissions, na.rm = T) / 1000000
  ) |>
  ungroup() |>
  mutate(
    annual_savings = opex_ng - opex_hp,
    capex_diff = capex_hp - capex_ng, 
    payback_years = if_else(annual_savings > 0, capex_diff / annual_savings, NA_real_),
    
    statesub = paste(state, industry_clean, sep = " "), 
    statesub_label = str_wrap(statesub, width = 10) 
  ) |>
  select(state, statesub, statesub_label, industry_clean, sector, payback_years, elec_ghg_emissions) |>
  filter(!is.na(payback_years),
         payback_years < 21)

payback_bubble_data_fnb <- 
  payback_bubble_data |>
  filter(sector == 'Food & Beverage') |>
  slice_min(order_by = payback_years, n = 10) |>
  tbl_graph(nodes = _, edges = NULL) 

payback_bubble_data_pnp <- 
  payback_bubble_data |>
  filter(sector == 'Pulp & Paper') |>
  slice_min(order_by = payback_years, n = 10) |>
  tbl_graph(nodes = _, edges = NULL) 

payback_bubble_data_chem <- 
  payback_bubble_data |>
  filter(sector == 'Chemicals') |>
  slice_min(order_by = payback_years, n = 10) |>
  tbl_graph(nodes = _, edges = NULL) 

fill_lims <- 
  range(c(payback_bubble_data_fnb %N>% pull(payback_years),
          payback_bubble_data_pnp %N>% pull(payback_years),
          payback_bubble_data_chem %N>% pull(payback_years)
  ), 
  na.rm = TRUE)

fill_scale <- scale_fill_gradient2(
  limits = fill_lims, low = "#003660", high = "#FEBC11", midpoint = 5,
  name = "Payback (Years)"
)

base_plot <- function(graph, title, show_legend = TRUE) {
  ggraph(graph, layout = "circlepack", weight = elec_ghg_emissions) +
    geom_node_circle(
      aes(fill = payback_years),
      color = "grey40", linewidth = 0.3, alpha = 0.9,
      show.legend = TRUE
    ) +
    geom_node_text(
      data = function(d) d %>%
        mutate(
          label_size = rescale(sqrt(pmax(elec_ghg_emissions, 0)),
                               to = c(1.6, 4.0)),
          statesub_label = str_wrap(statesub_label, width = 10)
        ) %>%
        filter(leaf, !is.na(statesub_label), statesub_label != ""),
      aes(label = statesub_label, size = label_size),
      color = "black", lineheight = 0.9,
      hjust = 0.5, vjust = 0.5, check_overlap = TRUE,
      show.legend = FALSE
    ) +
    fill_scale +  # use the shared object
    ggtitle(title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = if (show_legend) "right" else "none"
    ) +
    coord_equal() +
    scale_size_identity()
}

# combine plots (will now merge into one legend automatically)
payback_bubble_plot <- 
  (
    base_plot(payback_bubble_data_fnb, "Food & Beverage") + 
      coord_equal(xlim = c(-1, 1), ylim = c(-1, 1)) +   # bigger
      
      base_plot(payback_bubble_data_pnp, "Pulp & Paper") +
      
      base_plot(payback_bubble_data_chem, "Chemicals") +
      coord_equal(xlim = c(-1, 1), ylim = c(-1, 1))    # bigger
  ) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right")


payback_bubble_plot





#### FIG: PERCENT IN PAYBACK -- POLICY (OLD VERSION) ####
# select policy scenarios to show 
fig_policies <- c(
  'No Policy',
  'Elec -25%', 
  'Elec -50%', 
  'PTC $10/MMBtu'
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



#### FIG: DELTA LCOH BUBBLE PLOT ####
sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
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
    "Pulp & Paper"  = "#A67C52"
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
  "Pulp & Paper" = "#A67C52"
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
