# National Figure Code #
## October 9, 2025 ##


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
  read_excel("LCOH modelling/output/copollutant_longform_national_wtemps_addedsectors_30sept.xlsx") %>%
  select(-1, -opex) 

# TEMPORARY: natgas capex calculation
natgas_best <- 
  tech_input_df.o %>%
  filter(tech_scenario == 'Baseline') %>%
  mutate(tech_scenario = 'BaselineBest', 
         # best case natural gas 
         capex = 4733.79*(heat_mmbtu/49132.8)^0.8325)

tech_input_df <- 
  tech_input_df.o |>
  mutate(
    tech_scenario = if_else(tech_scenario == 'Baseline', 'BaselineWorst', tech_scenario),
    # worst case natural gas
    capex = if_else(tech_scenario == 'BaselineWorst', 25761.09*(heat_mmbtu/54592)^0.8325, capex)
  ) %>%
  bind_rows(natgas_best) %>%
  mutate(#facility_name = tolower(facility_name), 
    facility_id = as.character(facility_id),
    base_emissions_co2e = elec_ghg_emissions + noelec_ghg_emissions
    
    # scenario_rank = str_extract(tech_scenario, "(Best|Worst)$"),
    # tech_scenario = str_remove(tech_scenario, "(Best|Worst)$") 
  ) %>% 
  rename(
    base_emissions_nox = `base_emissions_Nitrogen Oxides`,
    base_emissions_pm25 = `base_emissions_PM2.5 Primary (Filt + Cond)`, 
    base_emissions_so2 = `base_emissions_Sulfur Dioxide`
  )

# Pull in facility info, this file doesn't have lat and long... (maybe add in the future)
facility_info <-
  read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>%
  clean_names() %>%
  rename(naics_code = primary_naics) %>%
  rename(naics_description = naics_title) %>%
  select(facility_id, naics_code, naics_description, county_fips, subregion) %>%
  #inner_join(facility_lat_long, by = "facility_id") %>%
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
      naics_description == 'Poultry Processing' ~ 'Poultry', 
      naics_description == 'Rendering and Meat Byproduct Processing' ~ 'Rendering', 
      naics_description == 'Distilleries' ~ 'Distilleries', 
      naics_description == 'Wet Corn Milling and Starch Manufacturing' ~ 'Wet Corn Milling', 
      naics_description == 'Cheese Manufacturing' ~ 'Cheese', 
      naics_description == 'Fluid Milk Manufacturing' ~ 'Milk', 
      naics_description == 'Frozen Fruit, Juice, and Vegetable Manufacturing' ~ 'Frozen Produce',
      naics_description == 'Fruit and Vegetable Canning' ~ 'Canned Produce',
      naics_description == 'Other Snack Food Manufacturing' ~ 'Snack Foods', 
      naics_description == 'Cane Sugar Manufacturing' ~ 'Cane Sugar',
      naics_description == 'Sanitary Paper Product Manufacturing' ~ 'Toilet Paper', 
      naics_description == 'Synthetic Rubber Manufacturing' ~ 'Rubber', 
      naics_description == 'Phosphatic Fertilizer Manufacturing' ~ 'Fertilizers', 
      naics_description == 'Dried and Dehydrated Food Manufacturing' ~ 'Dried Foods',
      naics_description == 'Specialty Canning' ~ 'Specialty Canning',
      TRUE ~ 'Other Manufacturing'
    )
  )

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

# --- Merge Inputs ---
tech_combined_df <- 
  tech_input_df %>%
  left_join(facility_info, by = "facility_id") %>%
  left_join(egrid_df, by = "subregion") 

rm(egrid_df, facility_info, natgas_best, tech_input_df, tech_input_df.o)

## Graphics settings
subsector_colors <- c(
  # --- Pulp & Paper ---
  "Pulp & Paper" = "#6d7d33",
  "Toilet Paper" = "#9aa65a",
  # --- Sugar & Starch ---
  "Beet Sugar" = "#ef5645",
  "Cane Sugar" = "#ff7b54",
  "Wet Corn Milling" = "#febc11",
  # --- Alcohol & Distilling ---
  "Ethyl Alcohol" = "#2CA02C",
  "Distilleries" = "#D2691E",
  # --- Oils & Soy ---
  "Fats & Oils" = "#047c91",
  "Soybeans" = "#c9bf9d",
  # --- Meat & Rendering ---
  "Meat (non-poultry)" = "#8B0000",
  "Poultry" = "#b22222",
  "Rendering" = "#8C564B",
  # --- Dairy ---
  "Cheese" = "#FFD95B",
  "Milk" = "#FFE680",
  # --- Produce & Canning ---
  "Frozen Produce" = "#76B7B2",
  "Canned Produce" = "#98DF8A",
  "Specialty Canning" = "#B5E48C",
  "Dried Foods" = "#6A994E",
  # --- Snack & Packaged Foods ---
  "Snack Foods" = "#FFB347",
  # --- Spices & Extracts ---
  "Spices" = "#9370DB",
  # --- Fertilizers & Chemicals ---
  "Fertilizers" = "#B56576",
  "Rubber" = "#5F0F40",
  # --- Other ---
  "Other Manufacturing" = "#CCCCCC"
)

sector_colors <- c(
  "Pulp & Paper" = "#6d7d33",
  "Chemicals" = "#047c91", 
  "Food & Beverage"= "#febc11"
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
  "Scenario1"  = "E-Boiler",
  "Scenario2"  = "Air-Source HP",
  "Scenario3"  = "E-Boiler + EE",
  "Scenario4"  = "Air-Source HP + EE"
)

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
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- eboiler_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario1Worst|Scenario3Worst") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- eboiler_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario2Best|Scenario4Best") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- hthp_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario2Worst|Scenario4Worst") ~ {
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
  read_csv('state_fact_sheets/data/parameters.csv') 

# --- Create Policy Grid ---
policy_grid <- expand.grid(
  capex_subsidy = seq(0.0, 1.0, by = 0.1),
  elec_discount = seq(0.0, 1.0, by = 0.25)
)

# --- Apply Policy Grid to Tech Scenarios ---
facility_lcoh_df <- 
  tidyr::crossing(tech_combined_df, policy_grid) %>%
  left_join(param, by = 'state') |>
  mutate(
    lcoh = lcoh_func(
      param$r[1], # need to pass r & t as scalars 
      elec_price,
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
    )
  )
#mutate(industry_clean = factor(industry_clean, levels = order_levels))

ng_min <- min(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineBest'])
ng_max <- max(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineWorst'])

#### LCOH BY TECH SCENARIO ####
# Define sector colors

ng_min <- min(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineBest'], na.rm = T)
ng_max <- max(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineWorst'], na.rm = T)

# Prepare data for candlestick
lcoh_tech_sector <- 
  facility_lcoh_df %>%
  filter(
    policy_label == "No Policy",             # keep this filter if applicable
    !str_detect(tech_scenario, "Baseline")   # drop baseline scenario
  ) %>%
  mutate(
    # Remove "Best"/"Worst" suffix
    scenario_clean = str_remove(tech_scenario, "Best|Worst"),
    scenario_number = str_extract(scenario_clean, "\\d+") %>% as.integer(),
    scenario_label = factor(
      scenario_number,
      levels = 1:4,
      labels = c("E-Boiler", "Air-Source HP", "E-Boiler + EE", "Air-Source HP + EE")
    ))

# Technology scenario x sector plot 
technology_by_sector_lcoh_plot <- 
  ggplot(lcoh_tech_sector,
         aes(x = scenario_label, y = lcoh, color = sector)) +
  
  # Natural gas range display 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_min, ymax = ng_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_min, linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = ng_max, linetype = "dotted", color = "black", size = 0.5) +
  # annotate("text", x = -Inf, y = Inf, label = "LCOH range of a natural gas boiler",
  #          hjust = -0.1, vjust = 46.5, size = 3, fontface = 'italic') +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(0, 40)) +
  
  labs(
    x = NULL,
    y = "Levelized Cost of Heat ($/MMBtu)",
    color = "Sector"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.83, 0.32),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

technology_by_sector_lcoh_plot

#### LCOH BY SUBSECTOR ####
# Define sector colors

ng_min <- min(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineBest'], na.rm = T)
ng_max <- max(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineWorst'], na.rm = T)

# Technology scenario x sector plot 
subsector_lcoh_plot <- 
  ggplot(lcoh_tech_sector,
         aes(x = industry_clean, y = lcoh)) +
  
  # Natural gas range display 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_min, ymax = ng_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_min, linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = ng_max, linetype = "dotted", color = "black", size = 0.5) +
  
  geom_boxplot(outlier.shape = NA, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_fill_manual(values = subsector_colors) +
  # scale_y_continuous(limits = c(0, 40)) +
  
  labs(
    x = NULL,
    y = "Levelized Cost of Heat ($/MMBtu)",
    fill = "Subsector"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    # legend.text = element_text(size = 10),
    # legend.title = element_text(size = 11),
    # legend.background = element_rect(color = "black", linewidth = 0.2),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

subsector_lcoh_plot





#### DELTA LCOH PLOT ####

baseline_lcoh <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario %in% c('BaselineBest', 'BaselineWorst')) |>
  mutate(
    tech_scenario = str_remove(tech_scenario, "(Best|Worst)$") 
  ) |>
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
    !tech_scenario %in% c('BaselineBest', 'BaselineWorst')
  ) |>
  mutate(
    tech_scenario = str_remove(tech_scenario, "(Best|Worst)$") 
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
    lcoh_delta = (lcoh - lcoh_ng) / lcoh_ng * 100, 
    
    tech_scenario = factor(
      tech_scenario,
      levels = c('Scenario1','Scenario3', 'Scenario2', 'Scenario4'))
  ) 

delta_lcoh_plot   <- 
  ggplot(delta_lcoh_data,
         aes(x = sector, y = lcoh_delta, color = tech_scenario)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = scenario_colors, labels = scenario_labels) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  scale_y_continuous(limits = c(-100, 800), 
                     breaks = c(-100, pretty(delta_lcoh_data$lcoh_delta, n = 6))) +
  
  labs(
    x = NULL,
    y = "Δ Levelized Cost of Heat (%)",
    color = "Technology:"
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


#### DELTA LCOH -- POLICY PLOT ####

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

baseline_lcoh <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy', 
         tech_scenario %in% c('BaselineBest', 'BaselineWorst')) |>
  mutate(
    tech_scenario = str_remove(tech_scenario, "(Best|Worst)$") 
  ) |>
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
    tech_scenario %in% c('Scenario4Best', 'Scenario4Worst')
  ) |>
  mutate(
    tech_scenario = str_remove(tech_scenario, "(Best|Worst)$") 
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
    lcoh_delta = (lcoh - lcoh_ng) / lcoh_ng * 100, 
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
  scale_y_continuous(limits = c(-100, 550),
                     breaks = c(-100, pretty(delta_lcoh_data$lcoh_delta, n = 6))) +
  
  labs(
    x = NULL,
    y = "Δ Levelized Cost of Heat (%)",
    color = "Sector"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.83, 0.32),
    #legend.justification = c(0, 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

delta_policy_plot

#### LCOH V ELECTRICITY PLOT ####
x_vals <- seq(0, 0.15, length.out = 200)

elec_plot_df_mi <- 
  facility_lcoh_df |>
  # Filter to no policy support, Scenario4 outcomes, pulp & paper
  filter(str_detect(tech_scenario, "Scenario4"), 
         policy_label == 'No Policy', 
         industry_clean == 'Pulp & Paper', 
         state == 'MI') |>
  
  # Summarize at the scenario level.
  group_by(tech_scenario) |>
  summarize(
    #sector = min(sector),
    state = first(state),
    capex = mean(capex),
    change_in_electricity_demand_kwh = mean(change_in_electricity_demand_kwh),
    heat_mmbtu = mean(heat_mmbtu),
    tech_scenario = min(tech_scenario),
    capex_subsidy = mean(capex_subsidy), # will be zero
    elec_discount = mean(elec_discount), # will be zero
  ) |>
  ungroup() |>
  left_join(param, by = 'state') |>
  tidyr::crossing(x = x_vals) |>
  mutate(
    lcoh = lcoh_func(
      param$r[1], # need to pass r & t as scalars 
      # We provide x for electricity price, since we're just making a prediction
      x,
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
  rename(lcoh_mi = lcoh) |>
  pivot_wider(
    names_from = scenario_rank, 
    names_prefix = "lcoh_",
    values_from = lcoh_mi
  ) |>
  mutate(
    lcoh_Mean = (lcoh_Best + lcoh_Worst) / 2
  )

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

# Get the points at which the heat pump LCOH line intersects the NG LCOH 
ng_x_mi_max <- with(elec_plot_df_mi, approx(lcoh_Best, x, xout = ng_max_mi))$y
ng_x_mi_min <- with(elec_plot_df_mi, approx(lcoh_Worst, x, xout = ng_min_mi))$y
ng_x_mi_mean <- (ng_x_mi_max + ng_x_mi_min) / 2

lcoh_v_elec_plot_mi <- 
  ggplot(data = elec_plot_df_mi) +
  geom_ribbon(aes(x = x, ymin = lcoh_Best, ymax = lcoh_Worst),
              fill = "grey90", alpha = 0.3) +
  # Main LCOH curve
  geom_line(aes(x = x, y = lcoh_Mean, color = "Heat Pump", linetype = "Heat Pump"),
            size = 1) +
  # geom_line(aes(x = x, y = Worst, color = "Heat Pump", linetype = "Heat Pump"),
  #           size = 1) +
  
  # NG Boiler reference line
  annotate("rect", xmin = 0, xmax = 10, ymin = ng_min_mi, ymax = ng_max_mi,
           fill = "grey90", alpha = 0.3) +
  geom_segment(aes(x = 0, xend = 10,
               y = (ng_max_mi + ng_min_mi) / 2, 
               yend = (ng_max_mi + ng_min_mi) / 2,
               color = "Natural Gas Boiler",
               linetype = "Natural Gas Boiler"),
           size = 0.75) +
  
  # Current electricity price 
  # annotate("rect", xmin = param$elec_price[param$state == 'MI'] * 100, xmax = param_rmi$elec_price_high[1] * 100,
  #          ymin = -Inf, ymax = Inf,
  #          fill = "#FFBF00", alpha = 0.1) +
  geom_vline(xintercept = param$elec_price[param$state == 'MI'] * 100, 
             color =  "#FFBF00", size = 0.5) +
  
  # Intersection 
  annotate("rect", xmin = ng_x_mi_min, xmax = ng_x_mi_max,
           ymin = -Inf, ymax = Inf,
           fill = "#FFBF00", alpha = 0.1) +
  geom_vline(xintercept = ng_x_mi_mean, color = "#FFBF00", linetype = "longdash", size = 0.75) +
  
  # Reverse x-axis
  scale_x_reverse(limits = c(10, 0)) +
  
  # Unified legend with manual scales
  scale_color_manual(
    name = NULL,
    values = c(
      "Heat Pump" = "#6d7d33",
      "Natural Gas Boiler" = "grey30"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Heat Pump" = "solid",
      "Natural Gas Boiler" = "solid"
    )
  ) +
  
  scale_y_continuous(limits = c(0,21)) +
  
  labs(
    y = "Levelized Cost of Heat ($/MMBtu)",
    x = "Cost of Electricity (¢/kWH)"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.75, 0.95),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(
      color = "black",
      linewidth = 0.2
    ), 
    panel.grid = element_blank() 
  )
lcoh_v_elec_plot_mi

########### STATE POLICY FIG ARCHIVE ###############


#### SCENARIO 4 LCOH POLICY FIGURE  #####

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

scenario4_df <- 
  facility_lcoh_df %>%
  filter(str_detect(tech_scenario, "Scenario4"), 
         policy_label %in% fig_policies) %>%
  mutate(
    policy_label = factor(policy_label, levels = fig_policies)
  ) 

# Policy scenario x sector plot (S4 only) 
lcoh_policy_combined_plot <- 
  ggplot(scenario4_df,
         aes(x = policy_label, y = lcoh, color = sector)) +
  
  # NG baseline band
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_min, ymax = ng_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_min, linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = ng_max, linetype = "dotted", color = "black", size = 0.5) +
  # annotate("text", x = -Inf, y = Inf, label = "LCOH range of a natural gas boiler",
  #          hjust = -0.1, vjust = 35.5, size = 3, fontface = 'italic') +
  
  # Jittered points
  # geom_jitter(
  #   position = position_jitter(width = 0.2, height = 0),  # control spread
  #   size = 2, alpha = 0.8
  # ) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = 1, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  
  scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
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

lcoh_policy_combined_plot

#### LCOH V ELECTRICITY ####
x_vals <- seq(0, 0.1, length.out = 200)

elec_plot_df <- 
  facility_lcoh_df |>
  # Filter to no policy support, Scenario4 outcomes
  filter(str_detect(tech_scenario, "Scenario4"), 
         policy_label == 'No Policy') |>
  
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
    x = x*100
  ) |>
  # average across best and worst case scenarios to get the sector-level outcome
  group_by(x) |>
  summarize(
    lcoh = mean(lcoh)
  )

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
  geom_vline(xintercept = param$elec_price * 100, color = "#FFBF00", size = 0.5) +
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


#### CAPEX FIGURE ####

capex_data <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy') |>
  mutate(
    scenario_rank = str_extract(tech_scenario, "(Best|Worst)$"),
    tech_scenario = str_remove(tech_scenario, "(Best|Worst)$"), 
    technology = case_when(
      tech_scenario == 'Baseline' ~ 'NG Boiler', 
      tech_scenario %in% c('Scenario1', 'Scenario3') ~ 'E-Boiler', 
      tech_scenario %in% c('Scenario2', 'Scenario4') ~ 'ASHP'
    ), 
    tech_scenario = factor(tech_scenario, levels = c("NG Boiler", "E-Boiler", "ASHP")), 
    capex = capex / 1000000
  ) |>
  select(facility_id, state, industry_clean, capex, technology, tech_scenario, scenario_rank)

# Capex Plot 
capex_plot <-
  ggplot(capex_data, aes(x = industry_clean, y = capex, fill = technology)) +
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
  
  labs(x = NULL, y = "CAPEX ($ Millions)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = c(0.8, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

capex_plot

capex_data_kw <- 
  facility_lcoh_df |>
  filter(policy_label == 'No Policy') |>
  mutate(
    heat_kw = heat_mmbtu * 293.071 / 8000,
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
capex_plot_kw <-
  ggplot(capex_data_kw, aes(x = technology, y = capex_per_kw, fill = technology)) +
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

#### EMISSIONS DATA WORK & SET-UP ####
emissions_func <- 
  function(
    # From emissions dataset
    facility_emissions, # facility emissions
    grid_emissions_kg_kwh, # kg of emissions per kwh
    change_in_electricity_demand_kwh, # how much electricity demand changes under electrification
    grid_clean_pct_scenario # What % cleaner is the grid? should take a value from 0-1
  ) {
    
    # Scale the grid emissions (kg per kwh) to be more clean 
    scaled_grid.e_factor <- grid_emissions_kg_kwh * (1-grid_clean_pct_scenario)
    
    # emissions = the increase in emissions from the grid from electricity demand + non-electrifiable emissions at the facility 
    total_emissions_t = (change_in_electricity_demand_kwh * scaled_grid.e_factor) / 1000 + facility_emissions
    
    total_emissions_t
  }

# --- Create Grid Intensity Levels (from 50% to 100% cleaner) ---
grid_scenarios <- tibble(grid_clean_pct_scenario = c(0, seq(0.5, 1.0, by = 0.1)))

# --- Expand to All Grid Scenarios ---
subsector_emissions_df <- 
  # expand to all grid scenarios 
  tidyr::crossing(tech_combined_df, grid_scenarios) %>%
  # Moving to a facility-tech.scenario-grid.scenario-pollutant level dataset
  pivot_longer(
    cols = ends_with("_kg_kwh"),
    names_to = "pollutant_type",
    names_pattern = "^(.*)_kg_kwh$",  
    values_to = "grid_emissions_kg_kwh"
  ) %>%
  mutate(
    # make one emissions variable, calibrated to copollutant & scenario
    facility_emissions = case_when(
      pollutant_type == 'co2e' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_co2e, 
      pollutant_type == 'nox' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_nox, 
      pollutant_type == 'so2' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_so2, 
      pollutant_type == 'pm25' & str_detect(tech_scenario, 'Baseline') ~ base_emissions_pm25,
      
      pollutant_type == 'co2e' & str_detect(tech_scenario, 'Scenario') ~ noelec_ghg_emissions, 
      pollutant_type == 'nox' & str_detect(tech_scenario, 'Scenario') ~ nox_emissions, 
      pollutant_type == 'so2' & str_detect(tech_scenario, 'Scenario')  ~ so2_emissions, 
      pollutant_type == 'pm25' & str_detect(tech_scenario, 'Scenario') ~ pm25_emissions
    ), 
    
    total_emissions = 
      emissions_func(
        facility_emissions, # facility emissions
        grid_emissions_kg_kwh, # kg of emissions per kwh
        change_in_electricity_demand_kwh, # how much electricity demand changes under electrification
        grid_clean_pct_scenario
      ),
    
    clean_grid_scenario_label = if_else(
      grid_clean_pct_scenario == 0,
      "Current Grid Mix",
      paste0(round(grid_clean_pct_scenario * 100), "% Cleaner Grid")
    )
  ) %>%
  select(-capex, -heat_mmbtu, -contains('base'), -elec_ghg_emissions, -noelec_ghg_emissions, 
         -nox_emissions, -so2_emissions, -pm25_emissions) %>% 
  # collapse to from facility to industry(-tech scenario-grid scenario-pollutant) level  
  group_by(industry_clean, tech_scenario, clean_grid_scenario_label, pollutant_type) %>%
  summarise(
    facility_emissions = sum(facility_emissions, na.rm = TRUE),
    total_emissions = sum(total_emissions, na.rm = TRUE),
    industry_clean = min(industry_clean),
    .groups = "drop"
  ) %>%
  mutate(
    scenario_base = str_remove(tech_scenario, "Best|Worst"),
    emissions_MMt = total_emissions/1000000) %>%
  # just going with the best case for now, which also pulls BaselineBest
  filter(str_detect(tech_scenario, 'Best'))

# Order levels for the charts (highest to least emissions)
order_levels <- 
  subsector_emissions_df %>%
  filter(clean_grid_scenario_label == "Current Grid Mix" & pollutant_type == 'co2e') %>%
  group_by(industry_clean) %>%
  summarise(total_emissions = sum(total_emissions, na.rm = TRUE)) %>%
  arrange(desc(total_emissions)) %>%
  pull(industry_clean)

subsector_emissions_df <- 
  subsector_emissions_df %>%
  mutate(industry_clean = factor(industry_clean, levels = order_levels))

#### PRESENT-DAY EMISSIONS FIGURES ####
# Always order current -> more clean
clean_grid_levels <- c("Current Grid Mix", "80% Cleaner Grid", "100% Cleaner Grid")

co2e_plot <- 
  subsector_emissions_df |>
  filter(pollutant_type == 'co2e' & 
           clean_grid_scenario_label %in% clean_grid_levels) |>
  mutate(
    clean_grid_scenario_label = factor(
      clean_grid_scenario_label,
      levels = clean_grid_levels
    )
  ) |>
  ggplot(aes(x = industry_clean, y = emissions_MMt, fill = scenario_base)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  facet_wrap(~ clean_grid_scenario_label, nrow = 1) +
  labs(
    x = NULL,
    y = "GHG Emissions (MMt CO2e)",
    fill = "Tech Scenario"
  ) +
  scale_fill_manual(values = scenario_colors, labels = scenario_labels) +
  scale_y_continuous(limits = c(0, max(subsector_emissions_df$emissions_MMt) * 1.1), expand = c(0, 0)) + # dynamic limit
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

co2e_plot

so2_plot <- 
  subsector_emissions_df |>
  filter(pollutant_type == 'so2' & 
           clean_grid_scenario_label %in% clean_grid_levels) |>
  mutate(
    clean_grid_scenario_label = factor(
      clean_grid_scenario_label,
      levels = clean_grid_levels
    )
  ) |>
  ggplot(aes(x = industry_clean, y = total_emissions, fill = scenario_base)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  facet_wrap(~ clean_grid_scenario_label, nrow = 1) +
  labs(
    x = NULL,
    y = expression("SO"[2]*" Emissions (t)"),
    fill = "Tech Scenario"
  ) +
  scale_fill_manual(values = scenario_colors, labels = scenario_labels) +
  #scale_y_continuous(limits = c(0, max(emissions_df$total_emissions) * 1.1), expand = c(0, 0)) + # dynamic limit
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

so2_plot

nox_plot <- 
  subsector_emissions_df |>
  filter(pollutant_type == 'nox' & 
           clean_grid_scenario_label %in% clean_grid_levels) |>
  mutate(
    clean_grid_scenario_label = factor(
      clean_grid_scenario_label,
      levels = clean_grid_levels
    )
  ) |>
  ggplot(aes(x = industry_clean, y = total_emissions, fill = scenario_base)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  facet_wrap(~ clean_grid_scenario_label, nrow = 1) +
  labs(
    x = NULL,
    y = expression("NO"[x]*" Emissions (t)"),
    fill = "Tech Scenario"
  ) +
  scale_fill_manual(values = scenario_colors, labels = scenario_labels) +
  #scale_y_continuous(limits = c(0, max(emissions_df$total_emissions) * 1.1), expand = c(0, 0)) + # dynamic limit
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

nox_plot

pm25_plot <- 
  subsector_emissions_df |>
  filter(pollutant_type == 'pm25' & 
           clean_grid_scenario_label %in% clean_grid_levels) |>
  mutate(
    clean_grid_scenario_label = factor(
      clean_grid_scenario_label,
      levels = clean_grid_levels
    )
  ) |>
  ggplot(aes(x = industry_clean, y = total_emissions, fill = scenario_base)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  facet_wrap(~ clean_grid_scenario_label, nrow = 1) +
  labs(
    x = NULL,
    y = expression("PM"[2.5]*" Emissions (t)"),
    fill = "Tech Scenario"
  ) +
  scale_fill_manual(values = scenario_colors, labels = scenario_labels) +
  #scale_y_continuous(limits = c(0, max(emissions_df$total_emissions) * 1.1), expand = c(0, 0)) + # dynamic limit
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

pm25_plot

#### EMISSIONS V GRID INTENSITY ####
x_vals <- seq(0, 0.1, length.out = 200)

#### CUMULATIVE EMISSIONS FIGURE ####

#### STATE EMISSIONS "IN THE MONEY" ####
fig_policies <- c(
  'No Policy',
  'Capex: -100%, Elec: -0%', 
  'Capex: -0%, Elec: -25%', 
  'Capex: -0%, Elec: -50%', 
  'Capex: -100%, Elec: -50%'
)


policy_colors <- c(
  "No Policy" = "#fb9a99",
  'Capex: -100%, Elec: -0%' = , 
  'Capex: -0%, Elec: -25%' = , 
  'Capex: -0%, Elec: -50%' = , 
  'Capex: -100%, Elec: -50%' = 
)

lcoh_tech_base <- 
  facility_lcoh_df %>%
  filter(
    policy_label == "No Policy", 
    tech_scenario == 'BaselineWorst'
  ) %>%
  group_by(state, tech_scenario) %>%
  summarize(
    lcoh = mean(lcoh, na.rm = TRUE)
  ) %>%
  mutate(
    scenario_clean = "Baseline (NG)", 
    scenario_rank = str_extract(tech_scenario, "Best|Worst")
  ) %>%
  select(state, lcoh)

eim_df <- 
  facility_lcoh_df %>%
  filter(
    tech_scenario == 'Scenario4Best', 
    state == st
  ) %>%
  left_join(
    lcoh_tech_base |> 
      rename(lcoh_ng = lcoh),
    by = 'state') %>%
  mutate(
    in_money = if_else(lcoh < lcoh_ng, 1, 0), 
    sector = factor(sector, levels = c('Chemicals', 'Pulp & Paper', 'Food & Beverage'))
  ) %>%
  group_by(sector, policy_label) %>%
  summarize(
    eim = sum(elec_ghg_emissions[in_money == 1], na.rm = TRUE),
    total_elec_ghg = sum(elec_ghg_emissions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(policy_label %in% fig_policies) %>%
  mutate(
    eim_Mt = eim/1000000, 
    eim_prop = (eim/total_elec_ghg)*100,
    policy_label = factor(policy_label, levels = fig_policies),
  ) 

eim_plot <- 
  ggplot(eim_df,
         aes(x = sector, y = eim_prop, fill = policy_label)) +
  
  # add boxplot 
  geom_col(position = position_dodge(width = 0.8, preserve = "single"), width = 0.6) + 
  
  #scale_color_manual(values = subsector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  labs( x = NULL, y = "% of Electrifiable Emissions 'in the Money'", fill = "Policy" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.7, 0.95),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

eim_plot

#### SAVE PLOTS ####
ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{st}/{st}_co2e_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       co2e_plot,
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{st}/{st}_so2_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       so2_plot,
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{st}/{st}_nox_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       nox_plot,
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{st}/{st}_pm25_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       pm25_plot,
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{st}/{st}_LCOH_technology_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       technology_by_sector_lcoh_plot, 
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{st}/{st}_LCOH_policy_scenario4_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       lcoh_policy_combined_plot, 
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{st}/{st}_capex_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       capex_plot, 
       width = 8, height = 5, dpi = 300)




