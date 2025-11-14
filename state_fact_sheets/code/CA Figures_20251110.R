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

facility_lcoh_df <- 
  facility_lcoh_df |>
  
  left_join(payback_data_ng, by = 'facility_id') |>
  
  mutate(
    annual_savings = opex_ng - opex,
    capex_diff = capex - capex_ng, 
    payback_years = case_when(
      annual_savings >= 0 ~ capex_diff / annual_savings, 
      annual_savings < 0 ~ NA_real_,
      tech_scenario == 'baseline' ~ NA_real_, 
      TRUE ~ NA_real_
    )
  ) 

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
    lifetime_baseline_co2e_mt = sum(co2e_baseline_mt, na.rm = T), 
    lifetime_clean_grid_co2e_mt = sum(clean_grid_co2e_emission_mt, na.rm = T), 
    lifetime_bau_grid_co2e_mt = sum(bau_grid_co2e_emission_mt, na.rm = T),
    lifetime_low_med_mt = sum(low_med_mt, na.rm = T), 
    lifetime_non_elec_mt = sum(non_elec_mt, na.rm = T), 
    lifetime_biogenic_mt = sum(biogenic_mt, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    facility_id = as.character(facility_id), 
    
    # Discounted lifetime emissions
    lifetime_baseline_co2e_mt_dc = lifetime_baseline_co2e_mt * discount_sum, 
    lifetime_clean_grid_co2e_mt_dc = lifetime_clean_grid_co2e_mt * discount_sum, 
    lifetime_bau_grid_co2e_mt_dc = lifetime_bau_grid_co2e_mt * discount_sum, 
    lifetime_low_med_mt_dc = lifetime_low_med_mt * discount_sum, 
    lifetime_non_elec_mt_dc = lifetime_non_elec_mt * discount_sum, 
    lifetime_biogenic_mt_dc = lifetime_biogenic_mt * discount_sum, 
    
    # Annual emissions reductions today 
    emissions_reduc_mt_25 = baseline_co2e_mt_25 - (clean_grid_co2e_mt_25 + low_med_mt_25 + non_elec_mt_25 + biogenic_mt_25), 
    
    # Lifetime emissions reductions
    lifetime_emissions_reduc_mt = lifetime_baseline_co2e_mt - (lifetime_clean_grid_co2e_mt + lifetime_low_med_mt + lifetime_non_elec_mt + lifetime_biogenic_mt), 
    
    lifetime_emissions_reduc_mt_dc = lifetime_baseline_co2e_mt_dc - (lifetime_clean_grid_co2e_mt_dc + lifetime_low_med_mt_dc + lifetime_non_elec_mt_dc + lifetime_biogenic_mt_dc), 
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

#### FIG: PERCENT IN PAYBACK -- POLICY CA ####
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
         tech_scenario == 'baseline', 
         state == 'CA') |>
  group_by(facility_id) |>
  summarize(
    capex_ng = mean(capex, na.rm = T),
    opex_ng = mean(opex, na.rm = T)
  ) |>
  ungroup()

facility_lcoh_df |>
  group_by(state) |>
  summarize(
    n_facilities = n_distinct(facility_id)
  )

in_payback_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee', 
    policy_label %in% fig_policies, 
    state == 'CA'
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


#### FIG: ABATEMENT COST CURVE CA ####

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
  "Pulp & Paper"         = "#A67C52",  # vivid green ✅
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


### IF YOU CHANGE THIS, NEED TO CHANGE FOR EMISSIONS_DF TOO *********
r <- .065
t <- 25
discount_sum <- (1 - (1 + r)^(-t)) / r

acc_data_ng <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'baseline', 
    policy_label == 'No Policy', 
    state == 'CA'
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
         policy_label == 'No Policy', 
         state == 'CA') |>
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
    lifetime_emissions_reduc_mt_dc = (sum(lifetime_emissions_reduc_mt_dc, na.rm = T)) * -1
  ) |>
  ungroup() |>
  mutate(
    lifetime_cost = lifetime_cost_hp - lifetime_cost_ng, 
    mac = (lifetime_cost / lifetime_emissions_reduc_mt_dc) 
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


