# Integrated National Data & Figure Code #
## November 13, 2025 ##

## Works with lcoh_industrialdecarb_facility_level.csv

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

#### FIG: PERCENT OF HEAT IN-SCOPE ####

fig_heat_colors <- c(
  "Low-Med Temp"  = "#09847A",  
  "Biogenic"            = "#A67C52",  
  "CHP"                 = "#4575B4", 
  "High Temperature"    = "#EF5645",
  "No Direct Replacement" = "#C9BF9D" 
)

heat_df <- 
  read_excel(glue(data_wd ,'merged_longform.xlsx')) |>
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
    )
  ) |>
  group_by(sector, scope_category) |>
  summarize(
    heat_total = sum(process_unit_heat_demand, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    pct_heat = 100 * heat_total / sum(heat_total, na.rm = TRUE), 
    scope_category = case_when(
      scope_category == 'biogenic' ~ 'Biogenic', 
      scope_category == 'chp' ~ 'CHP', 
      scope_category == 'in-scope' ~ 'Low-Med Temp', 
      scope_category == 'no direct replacement' ~ 'No Direct Replacement', 
      scope_category == 'utilities temp too high' ~ 'High Temperature'
    ), 
    scope_category = factor(
      scope_category,
      levels = rev(names(fig_heat_colors))  # reverse order for stacking only
    ), 
    sector = paste0(sector, '*')
  )


heat_plot <- 
  ggplot(heat_df, aes(x = sector, y = heat_total / 1000000, fill = scope_category)) +
  geom_col(position = "stack", width = 0.6) +
  #scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = fig_heat_colors,
    breaks = names(fig_heat_colors),
    name = "Process Category"
  ) +
  guides(fill = guide_legend(reverse = FALSE)) +
  
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

heat_plot


#### FIG: NATIONAL EMISSIONS  ####

tech_scenario_labels <- 
   c(
    "Baseline",
    "Drop-In Elec",
    "Drop-In Elec (EE+)",
    "Adv Elec",
    "Adv Elec (EE+)"
  )

fig_emissions_colors <- c(
  "Scope 1 (No Direct Replacement)" = "#C9BF9D", 
  "Scope 1 (High Temperature)"    = "#EF5645",
  "Scope 1 (CHP)"                 = "#4575B4", 
  "Scope 1 (Biogenic)"            = "#A67C52",  
  "Scope 1 (Low-Med Temp)"  = "#09847A",  
  "Scope 2" = "#FEBC11"
)

emissions_data_nat <- 
  facility_lcoh_df |>
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
    
    lifetime_clean_grid_co2e_mt = mean(lifetime_clean_grid_co2e_mt, na.rm = TRUE),
    lifetime_in_scope_mt = mean(lifetime_in_scope_mt, na.rm = TRUE),
    lifetime_chp_mt = mean(lifetime_chp_mt, na.rm = TRUE),
    lifetime_biogenic_mt = mean(lifetime_biogenic_mt, na.rm = TRUE),
    lifetime_no_direct_mt = mean(lifetime_no_direct_mt, na.rm = TRUE),
    lifetime_hightemp_mt = mean(lifetime_hightemp_mt, na.rm = TRUE)
  ) |>
  ungroup() |>
  
  # Getting sector-scenario emissions 
  group_by(sector, tech_scenario_label) %>%
  summarize(
    lifetime_clean_grid_co2e_mmt = sum(lifetime_clean_grid_co2e_mt, na.rm = TRUE)/ 1e6, 
    lifetime_in_scope_mmt = sum(lifetime_in_scope_mt, na.rm = TRUE)/ 1e6, 
    lifetime_chp_mmt = sum(lifetime_chp_mt, na.rm = TRUE)/ 1e6, 
    lifetime_biogenic_mmt = sum(lifetime_biogenic_mt, na.rm = TRUE)/ 1e6, 
    lifetime_no_direct_mmt = sum(lifetime_no_direct_mt, na.rm = TRUE)/ 1e6, 
    lifetime_hightemp_mmt = sum(lifetime_hightemp_mt, na.rm = TRUE)/ 1e6
  ) %>%
  ungroup() %>%
  # Add the sector totals

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
    
    tech_scenario_label = factor(
      tech_scenario_label, 
      levels = rev(tech_scenario_labels)
    ), 
    
    sector = paste0(sector, '*'),
    sector = factor(sector, levels = rev(sort(unique(as.character(sector)))))
  )  %>%
  arrange(desc(sector), factor(tech_scenario_label, levels = rev(tech_scenario_labels)))

emissions_plot_nat <- 
  ggplot() +
  
  geom_col(data = emissions_data_nat,
           aes(x = emissions_mmt, y = tech_scenario_label, fill = emissions_label),
           position = position_stack(), 
           width = .4) +
  
  facet_wrap(~ sector, nrow = 1) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  scale_fill_manual(
    values = fig_emissions_colors,
    breaks = names(fig_emissions_colors),
    labels = names(fig_emissions_colors),
    name = NULL
  ) +
  
  labs(x = "Cumulative Emissions (MMtCO2e)", y = NULL, fill = "Emissions Source" ) + 
  
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
    legend.spacing.x = unit(0.2, "cm"),        # moderate horizontal space between items
    legend.box = "horizontal",
    legend.margin = margin(3, 5, 3, 5, "pt"),  # small, even padding inside the legend box
    legend.box.margin = margin(1, 1, 1, 1, "pt") # minimal outer whitespace around legend
  )

emissions_plot_nat



#### FIG: EMISSIONS TREND ####

years <- c("2025","2030","2035","2040","2045","2050")
x_levels <- c("Baseline", years)

tech_levels <- c("Drop-In Elec", "Drop-In Elec (EE+)", "Adv Elec", "Adv Elec (EE+)")
tech_colors <- c(
  "Drop-In Elec"        = "#1f78b4",
  "Drop-In Elec (EE+)"  = "#a6cee3",
  "Adv Elec"            = "#33a02c",
  "Adv Elec (EE+)"      = "#b2df8a"
)

elec_trend <- 
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/total_emissions_by_facility_scope_category_251113.csv') %>%
  filter(
    facility_id %in% facility_lcoh_df$facility_id, 
    tech_scenario != 'baseline'
  ) %>%
  
  pivot_longer(
    cols = c(
      bau_co2e_kg_kwh,
      cambium95_co2e_kg_kwh,
      bau_grid_co2e_emission_mt,
      cambium95_grid_co2e_emission_mt,
    ),
    names_to = c("grid_scenario", ".value"),   # 🪄 This tells pivot_longer to create TWO value columns
    names_pattern = "(?:cambium)?(bau|95|mid)_(.*)"
  ) %>%
  mutate(
    grid_scenario = recode(grid_scenario,
                           "bau" = "BAU",
                           "95"  = "Clean Grid",
                           "mid" = "Mid Grid"
    ), 
    tech_scenario_label = case_when(
      tech_scenario == 'baseline' ~ 'Baseline', 
      tech_scenario == 'eb_boiler' ~ 'Drop-In Elec', 
      tech_scenario == 'eb_boiler_ee' ~ 'Drop-In Elec (EE+)', 
      tech_scenario == 'hp_boiler' ~ 'Adv Elec', 
      tech_scenario == 'hp_boiler_ee' ~ 'Adv Elec (EE+)'
    )
  ) %>%
  group_by(facility_id, grid_scenario, tech_scenario_label, policy_year) %>%
  summarize(
    grid_co2e_emission_mt = mean(grid_co2e_emission_mt, na.rm = TRUE),
    co2e_kg_kwh = mean(co2e_kg_kwh, na.rm = TRUE),
    change_in_electricity_demand_kwh = mean(change_in_electricity_demand_kwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  group_by(grid_scenario, tech_scenario_label, policy_year) %>%
  summarize(
    total_emissions_mt = sum(grid_co2e_emission_mt, na.rm = TRUE),
    avg_co2e_kg_kwh = weighted.mean(co2e_kg_kwh,
                                    w = change_in_electricity_demand_kwh,
                                    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(grid_scenario %in% c("Mid Grid", "Clean Grid") 
         #policy_year %in% c("2025", "2030", "2035", "2040", "2045", "2050")
  ) %>%
  mutate(
    grid_scenario = factor(grid_scenario, levels = c("Mid Grid", "Clean Grid")),
    tech_scenario_label = factor(
      tech_scenario_label,
      levels = c("Drop-In Elec", "Drop-In Elec (EE+)", "Adv Elec", "Adv Elec (EE+)")
    ),
    total_emissions_mmt = total_emissions_mt / 1e6  # convert to million metric tons
  ) %>%
  arrange(grid_scenario, tech_scenario_label)  

baseline_emiss <- 
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/total_emissions_by_facility_year_251111.csv') %>%
  filter(
    facility_id %in% facility_lcoh_df$facility_id,
    tech_scenario == "baseline"
  ) %>%
  distinct(facility_id, tech_scenario, elec_mt) %>% 
  group_by(tech_scenario) %>%
  summarize(
    total_emissions_mmt = sum(elec_mt, na.rm = TRUE) / 1e6
  ) %>%
  ungroup() %>%
  mutate(
    tech_scenario_label = 'Baseline'
  ) %>%
  select(-tech_scenario)

elec_trend_clean <-
  elec_trend |>
  filter(grid_scenario == "Clean Grid",
         policy_year %in% years) |>
  mutate(
    tech_scenario_label = factor(tech_scenario_label, levels = tech_levels),
    total_emissions_mmt = total_emissions_mt / 1e6
  )

baseline_emiss_clean <-
  baseline_emiss |>
  mutate(x_lab = "Baseline")

emissions_trend_df <-
  elec_trend_clean |>
  mutate(x_lab = policy_year)

emissions_trend_plot <-
  ggplot() +
  # Baseline (single grey bar at the left)
  geom_col(
    data = baseline_emiss_clean,
    aes(x = factor(x_lab, levels = x_levels),
        y = total_emissions_mmt),
    fill = "grey70",
    width = 0.4
  ) +
  # Emission bars by technology (side-by-side via dodge)
  geom_col(
    data = emissions_trend_df,
    aes(x = factor(x_lab, levels = x_levels),
        y = total_emissions_mmt,
        fill = tech_scenario_label),
    width = 0.3,
    color = "white",
    linewidth = 0.3,
    position = position_dodge(width = 0.35)
  ) +
  # Y axis
  scale_y_continuous(name = "In-Scope Emissions (MMtCO2e)") +
  # Fills and legend
  scale_fill_manual(values = tech_colors, name = "Technology Scenario", drop = FALSE) +
  # X axis: show Baseline + every five years, exactly in order
  scale_x_discrete(name = NULL, limits = x_levels, drop = FALSE) +
  # Theme
  theme_bw(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    
    legend.position = c(0.97, 0.97),     # ← x = right edge, y = top
    legend.justification = c("right", "top"),
    legend.box = "vertical",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",
      linewidth = 0.2
    ),
    # legend.margin = margin(4, 6, 4, 6),
    # legend.key.height = unit(0.4, "lines"),
    # legend.key.width  = unit(0.6, "lines"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank()
  )

emissions_trend_plot

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
    strip.background = element_rect(fill = "grey90", color = "white"),
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


#### TABLE: ELECTRICITY DEMAND ####

electricity_data <- 
  facility_lcoh_df |>
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

write_csv(electricity_data, 'national_results/outputs/electricity_demand_table.csv')


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

# baseline_capex <-
#   facility_lcoh_df |>
#   filter(policy_label == 'No Policy', 
#          tech_scenario == 'baseline') |>
#   select(facility_id, scenario_rank, capex_ng)

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

# Capex Plot 
capex_plot <-
  ggplot(capex_data, aes(x = tech_scenario, y = capex_delta, color = sector)) +
  geom_boxplot(
    outlier.shape = 21,
    outlier.size  = 1.8,
    outlier.stroke = 0.25,
    outlier.alpha = 0.8,
    width = 0.2,
    position = position_dodge(width = 0.4)  # widen or shrink this to adjust spacing
    
    #position = position_dodge2(width = .5, preserve = "single", reverse = TRUE)
  ) +
  scale_y_continuous(limits = c(-50, 250)) +
  scale_color_manual(
    name = "Sector*",
    values = sector_colors
  ) +
  labs(x = NULL, y = "Change in Up-Front Costs ($ millions)") +
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

#### FIG: PERCENT IN PAYBACK -- NO POLICY####
# --- Simplified sector colors ---
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

in_payback_plot_nopol <- 
  ggplot(in_payback_data_nopol,
         aes(
           x = tech_scenario_label,
           y = pct_in_payback,
           fill = sector,
           pattern = pct_group   # key aesthetic
         )) +
  
  geom_col_pattern(
    position = "stack",
    color = "grey90",
    pattern_density = 0.4,
    pattern_spacing = 0.04,
    pattern_key_scale_factor = 0.6,
    pattern_fill = 'white', 
    pattern_colour = "white",      # color of hatch lines
    pattern_size = 0.1        # thinner pattern lines (default ~0.5)
  ) +
  
  facet_wrap(~ sector, nrow = 1) +
  
  scale_pattern_manual(
    name = "Payback (Years)",
    values = c(
      "sub_5"   = "none",        # solid
      "sub_15"  = "stripe",  # crosshatch
      "plus_15" = "circle"       # dotted outline style (clean contrast)
    ),
    labels = rev(c("<5", "5–15", "15+"))
  ) +
  
  scale_fill_manual(
    values = sector_colors,
    guide = "none"
  ) +
  
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = NULL,
    y = "Percent of Facilities W/ Positive Payback"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "white"),
    legend.position = "right",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",
      linewidth = 0.2
    ),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.text.x = element_text(
      size = 7,
      angle = 45,
      hjust = 1,
      vjust = 1
    )
  )

in_payback_plot_nopol

#### FIG: DELTA NPV PLOT ####

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

# Technology scenario x sector delta npv plot 
delta_npv_plot   <- 
  ggplot(delta_npv_data,
         aes(x = sector, y = npv_delta_pct, color = tech_scenario_label)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = tech_scenario_colors, labels = tech_scenario_labels) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  
  scale_y_continuous(limits = c(-25, 500)) +
  
  labs(
    x = NULL,
    y = "Change in NPV (%)",
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

delta_npv_plot



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
    lcoh_delta_pct = 100 * (lcoh - lcoh_ng) / lcoh_ng,
    tech_scenario_label = factor(
      tech_scenario_label, 
      levels = tech_scenario_labels
    ), 
    sector = paste0(sector, '*')
  ) 

# Technology scenario x sector delta LCOH plot 
delta_lcoh_plot   <- 
  ggplot(delta_lcoh_data,
         aes(x = sector, y = lcoh_delta_pct, color = tech_scenario_label)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = tech_scenario_colors, labels = tech_scenario_labels) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  
  scale_y_continuous(limits = c(-25, 500)) +
  
  labs(
    x = NULL,
    y = "Change in LCOH (%)",
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



#### FIG: TOP PAYBACK -- NO POLICY ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)

# DO THIS SEPARATELY BECAUSE I INCLUDE *ALL* FACILITIES IN THE STATESUB FOR EMISSIONS
# BELOW, I FILTER TO FACILITIES W/ NONMISSING PAYBACKS
top_payback_data_nopol_emiss <- 
  emissions_df |>
  left_join(facility_lcoh_df |>
              distinct(facility_id, state, industry_clean), 
            by = 'facility_id') |>
  group_by(facility_id) |>
  summarize(
    state = first(state), 
    industry_clean = first(industry_clean), 
    
    in_scope_mt_25 = sum(in_scope_mt_25, na.rm = T)
  ) |>
  ungroup() |>
  filter(n() >= 2) |>   # keep only statesubs with +2 facilities
  group_by(state, industry_clean) |>
  summarize(
    in_scope_mmt_25 = sum(in_scope_mt_25, na.rm = T) / 1e6
  ) |>
  ungroup() |>
  mutate(
    statesub = paste(state, industry_clean, sep = " ")
  ) |>
  select(statesub, in_scope_mmt_25)
  

top_payback_data_nopol <- 
  facility_lcoh_df |>
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
  left_join(top_payback_data_nopol_emiss,  by = 'statesub') |>
  filter(payback_years < 30) |>
  pivot_longer(
    cols = c(payback_years, in_scope_mmt_25),
    names_to = "metric",
    values_to = "value"
  ) |>
  group_by(statesub) |>
  mutate(payback_ref = value[metric == "payback_years"]) |>
  ungroup() |>
  mutate(
    statesub = fct_reorder(statesub, payback_ref, .fun = mean, .desc = TRUE), 
    metric = fct_relevel(metric, "payback_years", "in_scope_mmt_25")
  )

top_payback_plot_nopol <- 
  ggplot(top_payback_data_nopol,
         aes(x = statesub,
             y = value,
             fill = sector) 
  ) +
  
  facet_wrap(
    ~ metric,
    nrow = 1,
    scales = "free_x",  
    labeller = as_labeller(c(
      payback_years = "Average Payback (Years)",
      in_scope_mmt_25    = "Total Emissions (MMt CO2e)"
    )), 
    strip.position = "bottom"  # move facet labels to bottom
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
    y = NULL,
    fill = "Sector*"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    strip.background   = element_blank(),
    strip.placement    = "outside",
    
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 7),   # smaller y labels
    
    legend.position    = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.text        = element_text(size = 8.5),  # smaller legend text
    legend.title       = element_text(size = 10.5), # smaller legend title
    legend.background  = element_rect(
      color = "black",
      linewidth = 0.2,
      fill = alpha("white", 0.8)
    ),
    legend.margin = margin(2, 2, 2, 2)  # tighten spacing inside box
  )

top_payback_plot_nopol


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
  filter(n() >= 2) |>
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

#### FIG: ABATEMENT COST CURVE - STATES ####

fig_state_colors <- c(
  # --- Pacific ---
  "CA" = "#00A6C2",  # teal-blue
  "OR" = "#5DA5DA",  # sky blue
  "WA" = "#1F78B4",  # deep blue
  "NV" = "#F28E2B",  # orange
  "AZ" = "#E15759",  # red-orange
  "HI" = "#76B7B2",  # soft aqua
  
  # --- Mountain West ---
  "CO" = "#4E79A7",  # steel blue
  "UT" = "#A0CBE8",  # pale blue
  "ID" = "#59A14F",  # muted olive-teal (leans blue, not green)
  "MT" = "#B6992D",  # ochre
  "WY" = "#FFBE7D",  # warm beige
  "NM" = "#E15759",  # coral
  
  # --- Great Plains ---
  "ND" = "#B07AA1",  # muted violet
  "SD" = "#9C755F",  # brown
  "NE" = "#72B7B2",  # blue-teal
  "KS" = "#EDC948",  # mustard
  "OK" = "#AF7AA1",  # lavender
  "TX" = "#2C699A",  # medium blue
  
  # --- Midwest / Great Lakes ---
  "MN" = "#68B0AB",  # turquoise
  "IA" = "#CC79A7",  # rose magenta
  "MO" = "#D37295",  # pink-violet
  "WI" = "#A0CBE8",  # pale blue
  "IL" = "#F1A340",  # amber
  "IN" = "#E08214",  # dark orange
  "MI" = "#008080",  # classic teal
  "OH" = "#EE854A",  # orange-tan
  
  # --- Mid-Atlantic ---
  "PA" = "#D73027",  # red
  "NY" = "#4575B4",  # blue
  "NJ" = "#5E4FA2",  # indigo
  "DE" = "#A6CEE3",  # light blue
  "MD" = "#80CDC1",  # light teal
  "DC" = "#999999",  # gray
  
  # --- New England ---
  "ME" = "#ABD9E9",  # sky blue
  "NH" = "#74ADD1",  # powder blue
  "VT" = "#E6A0C4",  # soft pink
  "MA" = "#8DA0CB",  # lavender-blue
  "CT" = "#9E9AC8",  # periwinkle
  "RI" = "#FDBF6F",  # soft orange
  
  # --- Southeast ---
  "VA" = "#FDAE61",  # orange
  "NC" = "#F46D43",  # coral
  "SC" = "#E6AB02",  # gold
  "GA" = "#BF812D",  # ochre brown
  "FL" = "#66C2A5",  # seafoam teal
  "AL" = "#C2A5CF",  # lilac
  "MS" = "#7B3294",  # plum
  "TN" = "#C994C7",  # pink-lavender
  "KY" = "#B2182B",  # red-brown
  "AR" = "#A6611A",  # brown-orange
  "LA" = "#1B9E77",  # dark teal (within your allowed range)
  
  # --- Appalachia / interior ---
  "WV" = "#D8B365"   # beige-tan
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
    state = first(state), 
    
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

acc_state_data <- 
  inner_join(acc_data_ng, acc_data_hp, by = 'facility_id') |>
  inner_join(emissions_df, by = 'facility_id') |>
  group_by(state) |>
  filter(n() >= 2) |>
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
    
    state = forcats::fct_reorder(state, mac, .desc = FALSE)
  ) |>
  # When the sector is too small, mac ends up looking really high. we'll drop a few 
  filter(mac < 1000)

acc_state_plot <- 
  ggplot(acc_state_data) +
  geom_rect(
    aes(xmin = x_min_mt, xmax = x_max_mt, ymin = pmin(y_min, y_max), ymax = pmax(y_min, y_max),
        fill = state),
    color = "grey60"
  ) +
  scale_fill_manual(
    name = "State (left to right)",          
    values = fig_state_colors
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

acc_state_plot


#### FIG: PERCENT IN PAYBACK -- POLICY####
# --- Simplified sector colors ---
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

in_payback_plot <- 
  ggplot(in_payback_data,
         aes(
           x = policy_label,
           y = pct_in_payback,
           fill = sector,
           pattern = pct_group   # key aesthetic
         )) +
  
  geom_col_pattern(
    position = "stack",
    color = "grey90",
    pattern_density = 0.4,
    pattern_spacing = 0.04,
    pattern_key_scale_factor = 0.6,
    pattern_fill = 'white', 
    pattern_colour = "white",      # color of hatch lines
    pattern_size = 0.1        # thinner pattern lines (default ~0.5)
  ) +
  
  facet_wrap(~ sector, nrow = 1) +
  
  scale_pattern_manual(
    name = "Payback (Years)",
    values = c(
      "sub_5"   = "none",        # solid
      "sub_15"  = "stripe",  # crosshatch
      "plus_15" = "circle"       # dotted outline style (clean contrast)
    ),
    labels = rev(c("<5", "5–15", "15+"))
  ) +
  
  scale_fill_manual(
    values = sector_colors,
    guide = "none"
  ) +
  
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = NULL,
    y = "Percent of Facilities W/ Positive Payback"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "white"),
    legend.position = "right",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",
      linewidth = 0.2
    ),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.text.x = element_text(
      size = 7,
      angle = 45,
      hjust = 1,
      vjust = 1
    )
  )

in_payback_plot


#### FIG: DELTA NPV -- POLICY PLOT ####

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

baseline_npv <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario == 'baseline') |>
  group_by(facility_id, tech_scenario) |>
  summarize(
    npv_ng = mean(npv, na.rm = T)
  ) |>
  ungroup() |>
  select(-tech_scenario)

delta_npv_policy_data <- 
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
    
    npv = mean(npv, na.rm = T)
  ) |>
  ungroup() |>
  left_join(baseline_npv, by = 'facility_id') |>
  mutate(
    npv_delta = (npv - npv_ng),
    npv_delta_pct = 100 * (npv - npv_ng) / npv_ng,
    policy_label = factor(policy_label, levels = fig_policies)
  ) 

delta_npv_policy_plot   <- 
  ggplot(delta_npv_policy_data,
         aes(x = policy_label, y = npv_delta_pct, color = sector)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = sector_colors) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  
  scale_y_continuous(limits = c(-25, 500)) +
  
  labs(
    x = NULL,
    y = "Change in NPV (%)",
    color = "Sector*"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    # legend.position = c(0.81, 0.94),
    # legend.justification = c(0, 1),
    axis.text.x = element_text(
      size = 8,              # smaller text
      angle = 45,            # rotate 45 degrees
      hjust = 1,             # right-align so they don’t overlap
      vjust = 1              # vertically align nicely under ticks
    ), 
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.background = element_rect(color = "black", linewidth = 0.2),
    legend.margin = margin(2.5, 2.5, 2.5, 2.5)           # tighten internal margins of the box
  )

delta_npv_policy_plot


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
    lcoh_delta_pct = 100 * (lcoh - lcoh_ng) / lcoh_ng,
    policy_label = factor(policy_label, levels = fig_policies)
  ) 

delta_policy_plot   <- 
  ggplot(delta_policy_data,
         aes(x = policy_label, y = lcoh_delta_pct, color = sector)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = sector_colors) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  
  scale_y_continuous(limits = c(-25, 500)) +
  
  labs(
    x = NULL,
    y = "Change in LCOH (%)",
    color = "Sector*"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    # legend.position = c(0.81, 0.94),
    # legend.justification = c(0, 1),
    axis.text.x = element_text(
      size = 8,              # smaller text
      angle = 45,            # rotate 45 degrees
      hjust = 1,             # right-align so they don’t overlap
      vjust = 1              # vertically align nicely under ticks
    ), 
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.background = element_rect(color = "black", linewidth = 0.2),
    legend.margin = margin(2.5, 2.5, 2.5, 2.5)           # tighten internal margins of the box
  )

delta_policy_plot



#### FIG: TOP PAYBACK -- POLICY  ####

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

top_payback_plot <- 
  ggplot(top_payback_data,
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

top_payback_plot



#### FIG: DELTA NPV BUBBLE X/Y -- STATE ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)

fig_states <- c('MN', 'PA', 'IL', 'CO')

fig_policies <- c(
  'No Policy', 
  'Capex -30%',
  'Capex -30%, Elec -25%', 
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
  "Soybeans"             = "#984EA3",  # purple (reused)
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

# --- Baseline NPV for reference ---
baseline_npv <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', tech_scenario == 'baseline') |>
  group_by(facility_id) |>
  summarize(npv_ng = mean(npv, na.rm = TRUE)) |>
  ungroup()

# --- Delta NPV data (same structure as payback_xy_state_data) ---
delta_npv_xy_state_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee',
    policy_label %in% fig_policies,
    state %in% fig_states
  ) |>
  left_join(baseline_npv, by = 'facility_id') |>
  left_join(
    emissions_df |>
      filter(tech_scenario == 'baseline') |>
      select(facility_id, scenario_rank, in_scope_mt_25),
    by = c('facility_id', 'scenario_rank')
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector),
    state = first(state),
    npv = mean(npv, na.rm = TRUE),
    npv_ng = mean(npv_ng, na.rm = TRUE),
    in_scope_mt_25 = mean(in_scope_mt_25, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    npv_delta_pct = 100 * (npv - npv_ng) / npv_ng
  ) |>
  filter(!is.na(npv_delta_pct)) |>
  group_by(state, industry_clean, policy_label) |>
  filter(n() >= 2) |>
  summarize(
    sector = first(sector),
    npv_delta_pct = mean(npv_delta_pct, na.rm = TRUE),
    in_scope_mt_25 = sum(in_scope_mt_25, na.rm = TRUE) / 1000000
  ) |>
  ungroup() |>
  mutate(
    statesub = paste(state, industry_clean, sep = " "),
    statesub_label = str_wrap(statesub, width = 10),
    policy_label = factor(policy_label, levels = fig_policies),
    policy_label = fct_recode(
      policy_label,
      "Capex -30%,\nElec -25%" = "Capex -30%, Elec -25%"
    )
  ) |>
  select(state, industry_clean, sector, policy_label, npv_delta_pct, in_scope_mt_25) |>
  filter(!is.na(npv_delta_pct), abs(npv_delta_pct) < 1000) |>
  arrange(desc(in_scope_mt_25))

# --- Plot (same style/layout as Payback XY figure) ---
delta_npv_xy_state_plot <- 
  ggplot(delta_npv_xy_state_data,
         aes(x = state,
             y = npv_delta_pct,
             size = in_scope_mt_25,
             fill = industry_clean)) +
  geom_point(
    shape = 21, color = "grey30", alpha = 0.85, stroke = 0.3,
    position = position_jitter(width = 0.2, height = 0)
  ) +
  facet_wrap(~ policy_label, nrow = 1) +
  scale_fill_manual(values = fig_subsector_colors, name = "Subsector") +
  scale_size_continuous(
    range = c(3, 15),
    name = "In-Scope CO2e\nEmissions",
    labels = function(x) sprintf("%.1f Mt", x)
  ) +
  labs(
    x = NULL,
    y = "Change in NPV (%)"
  ) +
  theme_bw(base_size = 13) +
  guides(
    fill = guide_legend(
      title = "Subsector",
      title.position = "top",
      keywidth = 0.7, keyheight = 0.7,
      label.theme = element_text(size = 8),
      ncol = 1,
      byrow = TRUE
    ),
    size = guide_legend(
      title.position = "top",
      label.position = "right"
    )
  ) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "white"),
    legend.position = "right",
    legend.background = element_rect(fill = alpha("white", 0.8), color = "black", linewidth = 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.text.x = element_text(
      size = 7,
      angle = 45,
      hjust = 1,
      vjust = 1
    )
  )

delta_npv_xy_state_plot


#### FIG: NATIONAL EMISSIONS "IN THE MONEY" ####
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


eim_plot_nat <- 
  ggplot() +
  
  geom_col(data = eim_data_nat,
           aes(x = emissions_mmt, y = policy_label, fill = emissions_label),
           position = position_stack(), 
           width = .4) +
  
  facet_wrap(~ sector, nrow = 1) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  scale_fill_manual(
    values = fig_emissions_colors,
    breaks = names(fig_emissions_colors),
    labels = names(fig_emissions_colors),
    name = NULL
  ) +
  
  labs(x = "Cumulative Emissions (MMtCO2e)", y = NULL, fill = "Emissions Source" ) + 
  
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
    legend.spacing.x = unit(0.2, "cm"),        # moderate horizontal space between items
    legend.box = "horizontal",
    legend.margin = margin(3, 5, 3, 5, "pt"),  # small, even padding inside the legend box
    legend.box.margin = margin(1, 1, 1, 1, "pt") # minimal outer whitespace around legend
  )

eim_plot_nat


#### FIG: STATE EMISSIONS "IN THE MONEY" ####
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

fig_states <- c(
  'IL', 'NY',  
  'WI', 'MI', 'PA',  'OR'
)

total_elec_ghg_emissions_state <- 
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
              distinct(facility_id, state), 
            by = 'facility_id') |>
  filter(state %in% fig_states) |>
  group_by(state) %>%          
  summarize(
    lifetime_clean_grid_co2e_mmt = sum(lifetime_clean_grid_co2e_mt, na.rm = TRUE)/ 1e6, 
    lifetime_in_scope_mmt = sum(lifetime_in_scope_mt, na.rm = TRUE)/ 1e6, 
    lifetime_chp_mmt = sum(lifetime_chp_mt, na.rm = TRUE)/ 1e6, 
    lifetime_biogenic_mmt = sum(lifetime_biogenic_mt, na.rm = TRUE)/ 1e6, 
    lifetime_no_direct_mmt = sum(lifetime_no_direct_mt, na.rm = TRUE)/ 1e6, 
    lifetime_hightemp_mmt = sum(lifetime_hightemp_mt, na.rm = TRUE)/ 1e6,
    policy_label = "Baseline"
  ) 

eim_data_state <- 
  facility_lcoh_df |>
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
  
  # Getting state-policy emissions in the money 
  group_by(state, policy_label) %>%
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
  bind_rows(total_elec_ghg_emissions_state) %>%
  
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
    )
  )  %>%
  arrange(factor(policy_label, levels = fig_policies))


eim_plot_state <- 
  ggplot() +
  
  geom_col(data = eim_data_state,
           aes(x = emissions_mmt, y = policy_label, fill = emissions_label),
           position = position_stack(), 
           width = .4) +
  
  facet_wrap(~ state, nrow = 1) +
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  scale_fill_manual(
    values = fig_emissions_colors,
    breaks = names(fig_emissions_colors),
    labels = names(fig_emissions_colors),
    name = NULL
  ) +
  
  labs(x = "Cumulative Emissions (MMtCO2e)", y = NULL, fill = "Emissions Source" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",
      linewidth = 0.2
    ),
    # --- X-axis tick labels ---
    axis.text.x = element_text(
      size = 8,              # smaller text
      angle = 45,            # rotate 45 degrees
      hjust = 1,             # right-align so they don’t overlap
      vjust = 1              # vertically align nicely under ticks
    ),
    
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.spacing.x = unit(0.2, "cm"),        # moderate horizontal space between items
    legend.box = "horizontal",
    legend.margin = margin(3, 5, 3, 5, "pt"),  # small, even padding inside the legend box
    legend.box.margin = margin(1, 1, 1, 1, "pt") # minimal outer whitespace around legend
  )

eim_plot_state

#### SAVE FIGURES ####

safe_save <- function(plot_obj, width, height, dpi = 300,
                      dir = glue::glue("national_results/outputs/{format(Sys.Date(), '%Y%m%d')}")) {
  
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
safe_save(acc_state_plot, width = 10, height = 5)
safe_save(capex_plot, width = 8, height = 5)
safe_save(delta_lcoh_plot, width = 8, height = 5)
safe_save(delta_npv_plot, width = 8, height = 5)
safe_save(delta_npv_policy_plot, width = 8, height = 5)
safe_save(delta_npv_xy_state_plot, width = 8, height = 5)
safe_save(delta_policy_plot, width = 8, height = 5)
safe_save(eim_plot_nat, width = 8, height = 5)
safe_save(eim_plot_state, width = 8, height = 5)
safe_save(emissions_plot_nat, width = 8, height = 5)
safe_save(emissions_trend_plot, width = 8, height = 5)
safe_save(fuels_plot, width = 8, height = 5)
safe_save(heat_plot, width = 8, height = 5)
safe_save(heat_plot_2, width = 8, height = 5)
safe_save(in_payback_plot, width = 8, height = 5)
safe_save(in_payback_plot_nopol, width = 8, height = 5)
safe_save(payback_bubble_plot, width = 8, height = 5)
safe_save(payback_capex_xy_plot, width = 8, height = 5)
safe_save(payback_opex_xy_plot, width = 8, height = 5)
safe_save(payback_xy_plot, width = 8, height = 5)
safe_save(payback_xy_state_plot, width = 8, height = 5)
safe_save(top_payback_plot, width = 8, height = 5)
safe_save(top_payback_plot_nopol, width = 8, height = 5)

#### SUMMARY STATISTICS ####
capex_summary <- 
  capex_data |>
  group_by(sector, tech_scenario) |>
  summarize(
    n_facilities = n(),
    capex_mean   = mean(capex_mill, na.rm = TRUE),
    capex_median = (capex_mill, na.rm = TRUE),
    capex_q1     = quantile(capex_mill, 0.25, na.rm = TRUE),
    capex_q3     = quantile(capex_mill, 0.75, na.rm = TRUE),
    capex_sd     = sd(capex_mill, na.rm = TRUE),
    capex_min    = min(capex_mill, na.rm = TRUE),
    capex_max    = max(capex_mill, na.rm = TRUE)
  ) |>
  ungroup()

write_csv(capex_summary, 'national_results/data/summary_stats/capex_summary.csv')
#### DATA CHECKS ####

check <- 
  facility_lcoh_df |>
  filter(state == 'DE', 
         policy_label == 'No Policy')


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

#### FIG: PAYBACK BUBBLE X/Y -- STATE ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)

fig_states <- c(
  'MN', 'PA', 'IL', 'CO'
)

fig_policies <- c(
  'No Policy', 
  'Capex -30%',
  'Capex -30%, Elec -25%', 
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

payback_xy_state_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee', 
    policy_label %in% fig_policies , 
    state %in% fig_states
  ) |>
  left_join(emissions_df |>
              filter(tech_scenario == 'baseline') |>
              select(facility_id, scenario_rank, in_scope_mt_25), 
            by = c('facility_id', 'scenario_rank')
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    payback_years = mean(payback_years, na.rm = T),
    in_scope_mt_25 = mean(in_scope_mt_25, na.rm = T)
  ) |>
  ungroup() |>
  filter(!is.na(payback_years)) |>
  group_by(state, industry_clean, policy_label) |>
  filter(n() >= 2) |>
  summarize(
    sector = first(sector),
    
    payback_years = mean(payback_years, na.rm = T), 
    in_scope_mt_25 = sum(in_scope_mt_25, na.rm = T) / 1000000
  ) |>
  ungroup() |>
  mutate(
    statesub = paste(state, industry_clean, sep = " "), 
    statesub_label = str_wrap(statesub, width = 10), 
    policy_label = factor(policy_label, levels = fig_policies),
    policy_label = fct_recode(policy_label,
                              "Capex -30%,\nElec -25%" = "Capex -30%, Elec -25%")
  ) |>
  select(state, industry_clean, sector, policy_label, payback_years, in_scope_mt_25) |>
  filter(!is.na(payback_years),
         payback_years < 50
  ) |>
  arrange(desc(in_scope_mt_25))

payback_xy_state_plot <- 
  ggplot(payback_xy_state_data,
         aes(x = state,
             y = payback_years,
             size = in_scope_mt_25,
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

payback_xy_state_plot


#### FIG: NESTED PAYBACK BUBBLE PLOT ####

sector_colors <- c(
  "Chemicals" = "#09847A",
  "Food & Beverage" = "#EF5645",
  "Pulp & Paper" = "#A67C52"
)

payback_bubble_data <- 
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee' &
      policy_label == 'No Policy' 
  ) |> 
  left_join(emissions_df |>
              filter(tech_scenario == 'baseline') |>
              select(facility_id, scenario_rank, in_scope_mt_25), 
            by = c('facility_id', 'scenario_rank')
  ) |>
  group_by(facility_id) |>
  summarize(
    sector = first(sector), 
    state = first(state), 
    industry_clean = first(industry_clean), 
    
    payback_years = mean(payback_years, na.rm = T), 
    in_scope_mt_25 = mean(in_scope_mt_25, na.rm = T)
  ) |>
  ungroup() |>
  filter(!is.na(payback_years)) |>
  group_by(state, industry_clean) |>
  filter(n() >= 2) |>   # keep only statesubs with +2 facilities
  summarize(
    sector = first(sector), 
    in_scope_mt_25 = mean(in_scope_mt_25, na.rm = T), 
    payback_years = mean(payback_years, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    statesub = paste(state, industry_clean, sep = " "), 
    statesub_label = str_wrap(statesub, width = 10) 
  ) |>
  filter(payback_years < 20) |>
  select(state, statesub, statesub_label, industry_clean, sector, payback_years, in_scope_mt_25) 


hier_data <- 
  payback_bubble_data |>
  group_by(sector, statesub, statesub_label) |>
  summarize(
    state = first(state),
    in_scope_mt_25 = sum(in_scope_mt_25, na.rm = TRUE),
    avg_payback = mean(payback_years, na.rm = TRUE)
  ) |>
  ungroup()

# Edge list (sector → subsector)
edges <- 
  hier_data |>
  distinct(from = sector, to = statesub)    # use statesub consistently as node name

# Vertex list (both sector + subsector)
vertices <- bind_rows(
  # Sector-level nodes
  hier_data |>
    distinct(name = sector) |>
    mutate(
      level = 1,
      size = 0,
      avg_payback = NA,
      sector = name,
      state = NA_character_,
      statesub = NA_character_,
      statesub_label = NA_character_
    ),
  # Subsector-level nodes
  hier_data |>
    transmute(
      name = statesub,                      # match edge "to" field
      level = 2,
      size = in_scope_mt_25,
      avg_payback = avg_payback,
      sector = sector,
      state = state,
      statesub = statesub,
      statesub_label = statesub_label
    )
) |>
  mutate(
    size = if_else(is.na(size) | size <= 0, 1e-6, size)
  )

mygraph <- graph_from_data_frame(edges, vertices = vertices)

payback_bubble_plot  <- 
  ggraph(mygraph, layout = "circlepack", weight = size) +
  
  # --- Subsector circles: gradient fill, colored border by sector ---
  geom_node_circle(
    aes(fill = avg_payback, filter = leaf),
    color = "white", 
    linewidth = 0.4,
    alpha = 0.95 
  ) +
  
  # --- Gradient for payback fill ---
  scale_fill_gradientn(
    colours = c("#09847A",
                lighten("#09847A", amount = 0.4), 
                lighten("#09847A", amount = 0.8)),
    name = "Payback\n(Years)",
    na.value = "grey90",
    limits = c(0, 20),              # force legend to cover 0–20
    breaks = seq(0, 20, by = 5)     # tick marks at 0, 5, 10, 15, 20
  ) +
  
  geom_node_text(
    aes(
      label = ifelse(leaf, state, ""),
      size = sqrt(size),
      color = sector           # map text color to sector
    ),
    lineheight = 0.9,
    check_overlap = FALSE,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = sector_colors
    #guide = "none"             # hide legend for text colors
  ) +
  
  coord_equal() +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

# ggraph(mygraph, layout = "circlepack", weight = size * 0.9) +
# 
#   # --- Big sector circles: white outlines only ---
#   geom_node_circle(
#     aes(filter = !leaf),
#     fill = NA,
#     color = "white",
#     linewidth = 0.8,
#     alpha = 1
#   ) +
# 
#   # --- Subsector circles: gradient fill, colored border by sector ---
#   geom_node_circle(
#     aes(fill = avg_payback, color = sector, filter = leaf),
#     linewidth = 0.4,
#     alpha = 0.95
#   ) +
# 
#   # --- Gradient for payback fill ---
#   scale_fill_gradientn(
#     colours = c("#C9F0D6",
#                 lighten("#C9F0D6", amount = 0.4),
#                 "#FEBC11"),
#     name = "Payback (Years)",
#     na.value = "grey90"
#   ) +
# 
#   # --- Outline color for subsectors (sector-coded) ---
#   scale_color_manual(
#     values = sector_colors,
#     guide = "none"
#   ) +
# 
#   # --- Labels (sector-colored text) ---
#   geom_node_text(
#     aes(
#       label = ifelse(leaf, state, ""),
#       size = sqrt(size),
#       color = sector               # <— map text color to sector
#     ),
#     lineheight = 0.9,
#     check_overlap = FALSE,
#     show.legend = FALSE
#   ) +
# 
#   # --- Reuse same sector color palette for text ---
#   scale_color_manual(values = sector_colors, guide = "none") +
# 
#   coord_equal() +
#   theme_void() +
#   theme(
#     legend.position = "right",
#     legend.title = element_text(face = "bold"),
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
#   )



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
  'Capex -30%',
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
  facility_lcoh_df |>
  filter(
    tech_scenario == 'hp_boiler_ee', 
    policy_label %in% fig_policies #, 
    #state %in% fig_states
  ) |>
  left_join(emissions_df |>
              filter(tech_scenario == 'baseline') |>
              select(facility_id, scenario_rank, in_scope_mt_25), 
            by = c('facility_id', 'scenario_rank')
  ) |>
  group_by(facility_id, policy_label) |>
  summarize(
    industry_clean = first(industry_clean),
    sector = first(sector), 
    state = first(state), 
    
    payback_years = mean(payback_years, na.rm = T),
    in_scope_mt_25 = mean(in_scope_mt_25, na.rm = T)
  ) |>
  ungroup() |>
  filter(!is.na(payback_years)) |>
  group_by(state, industry_clean, policy_label) |>
  filter(n() >= 2) |>
  summarize(
    sector = first(sector),
    
    payback_years = mean(payback_years, na.rm = T), 
    in_scope_mt_25 = sum(in_scope_mt_25, na.rm = T) / 1000000
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
  select(state, industry_clean, sector, policy_label, payback_years, in_scope_mt_25) |>
  filter(!is.na(payback_years),
         payback_years < 50
  )

payback_xy_plot <- 
  ggplot(payback_xy_data,
         aes(x = policy_label,
             y = payback_years,
             size = in_scope_mt_25,
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


#### FIG: EMISSIONS TREND (OLD) ####

elec_trend <- 
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/total_emissions_by_facility_year_251111.csv') %>%
  filter(
    facility_id %in% facility_lcoh_df$facility_id, 
    tech_scenario != 'baseline'
  ) %>%
  
  pivot_longer(
    cols = c(
      bau_co2e_kg_kwh,
      cambium95_co2e_kg_kwh,
      cambiummid_co2e_kg_kwh,
      bau_grid_co2e_emission_mt,
      cambium95_grid_co2e_emission_mt,
      cambiummid_grid_co2e_emission_mt
    ),
    names_to = c("grid_scenario", ".value"),   # 🪄 This tells pivot_longer to create TWO value columns
    names_pattern = "(?:cambium)?(bau|95|mid)_(.*)"
  ) %>%
  mutate(
    grid_scenario = recode(grid_scenario,
                           "bau" = "BAU",
                           "95"  = "Clean Grid",
                           "mid" = "Mid Grid"
    ), 
    tech_scenario_label = case_when(
      tech_scenario == 'baseline' ~ 'Baseline', 
      tech_scenario == 'eb_boiler' ~ 'Drop-In Elec', 
      tech_scenario == 'eb_boiler_ee' ~ 'Drop-In Elec (EE+)', 
      tech_scenario == 'hp_boiler' ~ 'Adv Elec', 
      tech_scenario == 'hp_boiler_ee' ~ 'Adv Elec (EE+)'
    )
  ) %>%
  group_by(facility_id, grid_scenario, tech_scenario_label, policy_year) %>%
  summarize(
    grid_co2e_emission_mt = mean(grid_co2e_emission_mt, na.rm = TRUE),
    co2e_kg_kwh = mean(co2e_kg_kwh, na.rm = TRUE),
    change_in_electricity_demand_kwh = mean(change_in_electricity_demand_kwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  group_by(grid_scenario, tech_scenario_label, policy_year) %>%
  summarize(
    total_emissions_mt = sum(grid_co2e_emission_mt, na.rm = TRUE),
    avg_co2e_kg_kwh = weighted.mean(co2e_kg_kwh,
                                    w = change_in_electricity_demand_kwh,
                                    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(grid_scenario %in% c("Mid Grid", "Clean Grid") 
         #policy_year %in% c("2025", "2030", "2035", "2040", "2045", "2050")
  ) %>%
  mutate(
    grid_scenario = factor(grid_scenario, levels = c("Mid Grid", "Clean Grid")),
    tech_scenario_label = factor(
      tech_scenario_label,
      levels = c("Drop-In Elec", "Drop-In Elec (EE+)", "Adv Elec", "Adv Elec (EE+)")
    ),
    total_emissions_mmt = total_emissions_mt / 1e6  # convert to million metric tons
  ) %>%
  arrange(grid_scenario, tech_scenario_label)  

baseline_emiss <- 
  read_csv('/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/national-results-data/total_emissions_by_facility_year_251111.csv') %>%
  filter(
    facility_id %in% facility_lcoh_df$facility_id,
    tech_scenario == "baseline"
  ) %>%
  distinct(facility_id, tech_scenario, elec_mt) %>% 
  group_by(tech_scenario) %>%
  summarize(
    total_emissions_mmt = sum(elec_mt, na.rm = TRUE) / 1e6
  ) %>%
  ungroup() %>%
  mutate(
    tech_scenario_label = 'Baseline'
  ) %>%
  select(-tech_scenario)

emissions_trend_df <- 
  bind_rows(elec_trend, baseline_emiss)

grid_colors <- c("BAU" = "grey40", "Mid Grid" = "#09847A", "Clean Grid" = "#66C2A5")
tech_colors <- c(
  "Drop-In Elec" = "#1f78b4",
  "Drop-In Elec (EE+)" = "#a6cee3",
  "Adv Elec" = "#33a02c",
  "Adv Elec (EE+)" = "#b2df8a"
)

# get scaling factor for the secondary axis
max_emiss <- max(emissions_trend_df$total_emissions_mt, na.rm = TRUE)
max_intensity <- max(emissions_trend_df$avg_co2e_kg_kwh, na.rm = TRUE)
scale_factor <- max_emiss / max_intensity
offsets <- c("Clean Grid" = -0.15, "Mid Grid" = 0.15)

emissions_trend_plot <- 
  
  ggplot() +
  
  # --- Baseline grey bar (left side) ---
  geom_col(
    data = baseline_emiss,
    aes(
      x = min(as.numeric(factor(elec_trend$policy_year))) - 1,  # one slot to the left
      y = total_emissions_mmt
    ),
    fill = "grey70",
    width = 0.4
  ) +
  
  # --- Emission bars (superimposed by tech) ---
  geom_col(
    data = emissions_trend_df,
    aes(
      x = as.numeric(factor(policy_year)) + offsets[grid_scenario],
      y = total_emissions_mmt,
      fill = tech_scenario_label
    ),
    position = "identity",   # <- overlap, not stack
    width = 0.3,
    color = "white",
    linewidth = 0.3
  ) +
  
  # --- Grid intensity lines ---
  geom_line(
    data = elec_trend,
    aes(
      x = as.numeric(factor(policy_year)),
      y = avg_co2e_kg_kwh * scale_factor / 1e6,
      color = grid_scenario,
      group = grid_scenario
    ),
    linewidth = 1.2
  ) +
  geom_point(
    data = elec_trend,
    aes(
      x = as.numeric(factor(policy_year)),
      y = avg_co2e_kg_kwh * scale_factor / 1e6,
      color = grid_scenario
    ),
    size = 2
  ) +
  
  # --- Axes ---
  scale_y_continuous(
    name = "In-Scope Emissions (MMtCO2e)",
    sec.axis = sec_axis(~ . * 1e6 / scale_factor, name = "Grid Intensity (kg CO2e/kWh)")
  ) +
  
  # --- Colors and fills ---
  scale_fill_manual(values = tech_colors, name = "Technology Scenario") +
  scale_color_manual(values = grid_colors, name = "Grid Scenario") +
  
  scale_x_continuous(
    breaks = c(
      min(as.numeric(factor(elec_trend$policy_year))) - 1,
      seq_along(unique(elec_trend$policy_year))
    ),
    labels = c("Baseline", unique(elec_trend$policy_year)),
    name = NULL
  ) +
  
  # --- Theme ---
  theme_bw(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",
      linewidth = 0.2
    ),
    legend.margin = margin(4, 6, 4, 6),         
    #legend.box.margin = margin(6, 0, 0, 0),     
    legend.key.height = unit(0.4, "lines"),
    legend.key.width = unit(0.6, "lines"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank()
  )

emissions_trend_plot

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

payback_capex_xy_data <- 
  facility_lcoh_df |>
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
  facility_lcoh_df |>
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
  read_excel(glue(data_wd ,'dataset_baseline.xlsx')) |>
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
    statesub = paste(state, industry_clean, sep = " ")
  ) |>
  # drop virgin islands
  filter(state != 'VI')

sg_ratio_order <- 
  sg_ratio_data |>
  filter(technology == "Air-Source HP") |>
  select(statesub, sg_ratio_order = sg_ratio)

sg_ratio_data <- 
  sg_ratio_data |>
  left_join(sg_ratio_order, by = "statesub") |>
  mutate(statesub = forcats::fct_reorder(statesub, sg_ratio_order))

scenario_colors <- c(
  "E-Boiler"     = "#1f78b4",
  "Air-Source HP"     = "#33a02c",
  "Waste HP" = "#FEBC11"
)

# Technology scenario x sector delta opex plot 
sg_ratio_plot   <- 
  ggplot(sg_ratio_data,
         aes(x = statesub, y = sg_ratio, color = technology)) +
  
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
