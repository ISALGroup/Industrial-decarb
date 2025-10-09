### Hot spot analysis
## EMT
# 10.3.25

library(tidyverse)
library(scales)
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
library(patchwork)

setwd('/Users/eleanor/Documents/Industrial_Decarbonization/Industrial-decarb')

# ----- user inputs -----
plot_year   <- 2025
sc_to_plot  <- "Scenario1"   # change to Scenario1/2/3/4 or "Baseline" to test
out_file    <- paste0("matrix_single_", sc_to_plot, "_", plot_year, ".png")

# tech_input original 
tech_input_df.o <- read_excel("LCOH modelling/output/copollutant_longform_national_wtemps_addedsectors_30sept.xlsx") %>%
  select(-1, -opex)

natgas_best <- tech_input_df.o %>%
  filter(str_to_lower(tech_scenario) == "baseline") %>%
  mutate(tech_scenario = "BaselineBest",
         capex = 24573.01928*(heat_mmbtu/49132.8)^0.8431)

tech_input_df <- tech_input_df.o %>%
  mutate(tech_scenario = if_else(str_to_lower(tech_scenario) == "baseline", "BaselineWorst", tech_scenario),
         capex = if_else(str_to_lower(tech_scenario) == "baselineworst",
                         294796.2541*(heat_mmbtu/40944)^0.8431, capex)) %>%
  bind_rows(natgas_best) %>%
  mutate(facility_id = as.character(facility_id),
         base_emissions_co2e = elec_ghg_emissions + noelec_ghg_emissions)

# facility_info 
facility_info <-
  read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>%
  clean_names() %>%
  rename(naics_code = primary_naics) %>%
  rename(naics_description = naics_title) %>%
  select(facility_id, naics_code, naics_description, county_fips, subregion) %>%
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
      naics_description == "Frozen Fruit, Juice, and Vegetable Manufacturing" ~ 'Frozen Fruits and Vegetables'
    )
  ) #need industry_clean for all naics

# egrid + grid mix 
egrid_df <- read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 2) %>%
  janitor::clean_names() %>%
  mutate(co2e_kg_kwh = co2e * 0.000453592,
         nox_kg_kwh  = annual_n_ox * 0.000453592,
         so2_kg_kwh  = so2 * 0.000453592,
         pm25_kg_kwh = pm_2_5 * 0.000453592) %>%
  rename(subregion = e_grid_subregion_acronym) %>%
  select(subregion, co2e_kg_kwh, nox_kg_kwh, so2_kg_kwh, pm25_kg_kwh)

grid_mix_df <- read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 3) %>%
  janitor::clean_names() %>%
  mutate(current_fossil_share = coal + oil + gas + other_fossil,
         current_clean_share  = hydro + biomass + wind + solar + nuclear) %>%
  select(subregion = e_grid_subregion_acronym, current_fossil_share, current_clean_share)

# merge tech inputs -> full table across states
tech_combined_df <- tech_input_df %>%
  left_join(facility_info, by = "facility_id") %>%
  left_join(egrid_df, by = "subregion")

# read parameters 
param_df <- read_csv('state_fact_sheets/data/parameters.csv')

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
      opex_om <- eboiler_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario1Worst|Scenario3Worst") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price_high * (1-elec_discount))
      opex_om <- eboiler_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario2Best|Scenario4Best") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price_low * (1-elec_discount))
      opex_om <- hthp_om_high * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario2Worst|Scenario4Worst") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price_high * (1-elec_discount))
      opex_om <- hthp_om_low * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }
  )
}


# --- Create Policy Grid ---
policy_grid <- expand.grid(
  capex_subsidy = seq(0.0, 1.0, by = 0.1),
  elec_discount = seq(0.0, 1.0, by = 0.25)
)

# --- Apply Policy Grid to Tech Scenarios ---
facility_lcoh_df <- tidyr::crossing(tech_combined_df, policy_grid) %>%
  left_join(param_df, by = "state") %>%
  rowwise() %>%
  mutate(
    lcoh = lcoh_func(
      r = r[1],
      elec_price_low = elec_price_low[1],
      elec_price_high = elec_price_high[1],
      ng_price = ng_price[1],
      t = t[1],
      ngboiler_om_low = ngboiler_om_low[1],
      ngboiler_om_high = ngboiler_om_high[1],
      eboiler_om_low = eboiler_om_low[1],
      eboiler_om_high = eboiler_om_high[1],
      hthp_om_low = hthp_om_low[1],
      hthp_om_high = hthp_om_high[1],
      tech_scenario = tech_scenario,
      capex = capex,
      heat_mmbtu = heat_mmbtu,
      change_in_electricity_demand_kwh = change_in_electricity_demand_kwh,
      capex_subsidy = capex_subsidy,
      elec_discount = elec_discount
    ),
    policy_label = paste0("Capex: -", capex_subsidy * 100, "%, Elec: -", elec_discount * 100, "%"),
    policy_label = if_else(policy_label == "Capex: -0%, Elec: -0%", "No Policy", policy_label)
  ) %>%
  ungroup()


# ---- build grid_yearly_scaled (decarbonization targets) and facility_yearly same as your earlier code ----
policy_targets_global <- c('2030' = 0.50, '2050' = 1.00)
start_year <- 2025; end_year <- 2050; years_vec <- seq(start_year, end_year); allow_degradation <- FALSE

interp_targets_to_years <- function(targets_named, years) {
  yrs_target <- as.integer(names(targets_named)); vals <- as.numeric(targets_named)
  vals <- ifelse(vals > 1, vals/100, vals)
  tibble(year = years, target_clean = stats::approx(x = yrs_target, y = vals, xout = years, rule = 2)$y)
}

targets_tbl <- grid_mix_df %>%
  select(subregion, current_clean_share) %>%
  rowwise() %>%
  mutate(tmp = list(interp_targets_to_years(policy_targets_global, years_vec))) %>%
  tidyr::unnest(cols = c(tmp)) %>%
  ungroup() %>%
  mutate(target_clean = as.numeric(target_clean),
         target_clean = if_else(!allow_degradation & target_clean < current_clean_share, current_clean_share, target_clean)) %>%
  select(subregion, year, target_clean)

grid_yearly_scaled <- egrid_df %>%
  left_join(grid_mix_df, by = "subregion") %>%
  left_join(targets_tbl, by = "subregion") %>%
  mutate(
    target_fossil_share = 1 - target_clean,
    scaling_factor = if_else(current_fossil_share > 0, target_fossil_share / current_fossil_share, 0),
    scaled_co2e_kg_kwh_year = co2e_kg_kwh * scaling_factor
  ) %>%
  select(subregion, year, current_fossil_share, current_clean_share, target_clean, scaling_factor, scaled_co2e_kg_kwh_year)

facility_emissions_yearly <- tech_combined_df %>%
  left_join(grid_yearly_scaled, by = "subregion") %>%
  mutate(
    is_baseline = stringr::str_detect(tolower(tech_scenario), "baseline"),
    annual_emissions_co2e_emissions_mmt = if_else(
      is_baseline,
      elec_ghg_emissions + noelec_ghg_emissions,
      (change_in_electricity_demand_kwh * scaled_co2e_kg_kwh_year) / 1000 + noelec_ghg_emissions
    )
  ) %>%
  select(facility_id, facility_name, state, subregion, sector, naics_code, naics_description,
         tech_scenario, year, current_fossil_share, current_clean_share, target_clean, scaling_factor, scaled_co2e_kg_kwh_year, annual_emissions_co2e_emissions_mmt)

# average Best/Worst into facility_yearly_avg
facility_emissions_yearly_avg <- facility_emissions_yearly %>%
  mutate(tech_scenario = str_remove(tolower(tech_scenario), regex("(best|worst)$", ignore_case = TRUE))) %>%
  group_by(facility_id, facility_name, state, subregion, sector, naics_code, naics_description,
           tech_scenario, year) %>%
  summarise(annual_emissions_co2e_avg_tonnes = mean(annual_emissions_co2e_emissions_mmt, na.rm = TRUE), .groups = "drop")


# -----------------------------
# 1) Build summary tables 
# -----------------------------
# normalize tech name
normalize_tech <- function(x) {
  x <- str_remove(tolower(x), regex("(best|worst)$", ignore_case = TRUE))
  str_to_title(str_trim(x))
}

# lcoh_summary: per-facility, per-tech, per-policy
lcoh_summary <- facility_lcoh_df %>%
  mutate(tech_clean = normalize_tech(tech_scenario)) %>%
  group_by(facility_id, tech_clean, state, naics_description, policy_label) %>%
  summarise(lcoh_mean = mean(lcoh, na.rm = TRUE), .groups = "drop")

# baseline LCOH must be baseline rows for same policy_label
baseline_lcoh <- lcoh_summary %>%
  filter(str_to_lower(tech_clean) == "baseline") %>%
  select(facility_id, policy_label, lcoh_baseline = lcoh_mean)

# lcoh_delta: join baseline by facility_id & policy_label
lcoh_delta <- lcoh_summary %>%
  left_join(baseline_lcoh, by = c("facility_id", "policy_label")) %>%
  mutate(lcoh_delta = lcoh_mean - lcoh_baseline) %>%
  select(facility_id, state, tech_clean, policy_label, naics_description, lcoh_delta)

# facility emissions for a chosen year 
facility_emissions_year <- facility_emissions_yearly_avg %>%
  mutate(tech_clean = normalize_tech(tech_scenario)) %>%
  left_join(select(tech_combined_df, facility_id) %>% distinct(), by = "facility_id") %>%
  filter(year == 2025) %>%                         # default year; we'll use this in aggregation
  rename(annual_emissions_co2e_emissions_mmt = annual_emissions_co2e_avg_tonnes) %>%
  select(facility_id, state, tech_clean, naics_description, annual_emissions_co2e_emissions_mmt)

# joined: LCOH delta matched to emissions by facility × tech × state
joined_full <- lcoh_delta %>%
  select(-naics_description) %>% 
  left_join(facility_emissions_year, by = c("facility_id", "state", "tech_clean"))

# Aggregate once: state × naics_description × tech_clean × policy_label
agg_full <- joined_full %>%
  filter(!is.na(naics_description)) %>%   # drop unknown industries 
  group_by(state, naics_description, tech_clean, policy_label) %>%
  summarise(
    sum_emissions_co2e_emissions_mmt = sum(annual_emissions_co2e_emissions_mmt, na.rm = TRUE),
    mean_lcoh_delta = ifelse(sum_emissions_co2e_emissions_mmt > 0,
                             sum(lcoh_delta * coalesce(annual_emissions_co2e_emissions_mmt, 0), na.rm = TRUE) / sum_emissions_co2e_emissions_mmt,
                             NA_real_),
    n_facilities = n_distinct(facility_id),
    .groups = "drop"
  ) %>%
  ungroup()

### Analysis tables ####

# select policy scenarios to show 
fig_policies <- c(
  'No Policy',
  'Capex: -50%, Elec: -0%', 
  'Capex: -100%, Elec: -0%', 
  #'Capex: -0%, Elec: -25%', 
  'Capex: -0%, Elec: -50%', 
  'Capex: -50%, Elec: -50%'#, 
  #'Capex: -100%, Elec: -50%'
)

naics_complete <- c("Wet Corn Milling and Starch Manufacturing", "Ethyl Alcohol Manufacturing","Phosphatic Fertilizer Manufacturing",
                    "Animal (except Poultry) Slaughtering","Cheese Manufacturing", "Rendering and Meat Byproduct Processing", "Breweries",
                    "Synthetic Rubber Manufacturing", "Sanitary Paper Product Manufacturing","Fluid Milk Manufacturing","Other Snack Food Manufacturing")

states_of_interest <- c("CA", "IL", "NY", "MD", "MI", "MN", "MA", "WI", "CO", "PA", "OR")

agg_full_filtered <- agg_full %>% 
  filter(policy_label %in% fig_policies) %>% 
  filter(naics_description %in% naics_complete) %>% 
  filter(state %in% states_of_interest)

# --- Identify favorable (negative LCOH delta) cases ---
neg_lcoh_summary <- agg_full_filtered %>%
  filter(!is.na(mean_lcoh_delta), mean_lcoh_delta < 0) %>%
  arrange(state, policy_label, mean_lcoh_delta)

# --- Summarize by state and policy: total emissions where LCOH < 0 ---
neg_by_state_policy_naics_tech <- neg_lcoh_summary %>%
  group_by(state, policy_label, tech_clean, naics_description) %>%
  summarise(
    total_emissions = sum(sum_emissions_co2e_emissions_mmt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_emissions))


# --- Summarize by naics, policy, and tech scenario across all states ---
neg_by_policy_naics_tech <- neg_lcoh_summary %>%
  group_by(naics_description, tech_clean, policy_label) %>%
  summarise(
    total_emissions = sum(sum_emissions_co2e_emissions_mmt, na.rm = TRUE),
    n_states = n_distinct(state),
    .groups = "drop"
  ) %>%
  arrange(desc(total_emissions))

# Conversion constants
KWH_PER_MMBTU <- 293.071  # 1 MMBtu = 293.071 kWh
MMBTU_PER_MCF  <- 1.037   # 1 thousand cubic feet NG = 1.037 MMBtu

# Compute comparable $/MMBtu for each energy type
price_comparison <- param_df %>%
  mutate(
    elec_price_low_mmbtu  = elec_price_low  * KWH_PER_MMBTU,  # $/kWh × kWh/MMBtu
    elec_price_high_mmbtu = elec_price_high * KWH_PER_MMBTU,
    ng_price_mmbtu        = ng_price / MMBTU_PER_MCF,         # $/MCF ÷ MMBtu/MCF
    elec_to_ng_ratio_low  = elec_price_low_mmbtu  / ng_price_mmbtu,
    elec_to_ng_ratio_high = elec_price_high_mmbtu / ng_price_mmbtu
  ) %>%
  select(
    state,
    ng_price,
    ng_price_mmbtu,
    elec_price_low,
    elec_price_high,
    elec_price_low_mmbtu,
    elec_price_high_mmbtu,
    elec_to_ng_ratio_low,
    elec_to_ng_ratio_high
  ) %>%
  arrange(desc(elec_to_ng_ratio_low))

### Graph Matrix ###

# fill full matrix (so missing combos show as blanks)
all_states <- sort(unique(agg_full$state))
all_industries <- sort(unique(agg_full$naics_description))

agg_full <- agg_full %>%
  tidyr::complete(state = all_states, naics_description = all_industries,
                  tech_clean = unique(agg_full$tech_clean),
                  policy_label = unique(agg_full$policy_label),
                  fill = list(sum_emissions_co2e_emissions_mmt = 0, mean_lcoh_delta = NA_real_, n_facilities = 0))

# -----------------------------
# 2) Plotting 
# -----------------------------

# ---------- User inputs ----------
tech_choices_input   <- c("Scenario4") 
policy_choices_input <- c("No Policy", "Capex: -50%, Elec: -0%","Capex: -0%, Elec: -50%", "Capex: -50%, Elec: -50%")  

# output folder & date string
out_dir <- "state_fact_sheets/outputs/matrix_plots"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
date_str <- format(Sys.Date(), "%y%m%d")   # e.g. "251003"

# ---------- compute global state order (once) ----------
# Use all tech/policy combos requested, so order is consistent across plots
state_order_global <- joined_full %>%
  filter(tech_clean %in% tech_choices, policy_label %in% policy_choices) %>%
  group_by(state) %>%
  summarise(total = sum(coalesce(annual_emissions_tonnes, 0), na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  pull(state)


# ---------- helper: normalize input (allow comma-separated single string) ----------
parse_input <- function(x) {
  if(length(x) == 1 && grepl(",", x)) {
    out <- str_split(x, ",")[[1]] %>% str_trim()
  } else {
    out <- as.character(x)
  }
  out[out != ""]
}

tech_choices   <- parse_input(tech_choices_input)
policy_choices <- parse_input(policy_choices_input)

# ---------- helper: build the points-only plot (p3) ----------
build_p3 <- function(tech_choice, policy_choice, plot_year = 2025, max_point_size = 8) {
  df_plot <- joined_full %>%
    filter(tech_clean == tech_choice, policy_label == policy_choice) %>%
    group_by(state, naics_description) %>%
    summarise(
      sum_emissions_tonnes = sum(coalesce(annual_emissions_tonnes, 0), na.rm = TRUE),
      mean_lcoh = mean(lcoh_delta, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(sum_emissions_tonnes > 0, !is.na(mean_lcoh))
  
  if (nrow(df_plot) == 0) return(NULL)
  
  industry_order <- df_plot %>% group_by(naics_description) %>% summarise(total = sum(sum_emissions_tonnes)) %>% arrange(desc(total)) %>% pull(naics_description)
  state_order    <- df_plot %>% group_by(state) %>% summarise(total = sum(sum_emissions_tonnes)) %>% arrange(desc(total)) %>% pull(state)
  
  df_plot <- df_plot %>%
    mutate(
      naics_description = factor(naics_description, levels = industry_order),
      state = factor(state, levels = rev(state_order))
    )
  
  lim <- max(abs(df_plot$mean_lcoh), na.rm = TRUE)
  lim <- ifelse(is.finite(lim) & lim > 0, lim, 1)
  
  p <- ggplot(df_plot, aes(x = naics_description, y = state)) +
    geom_point(aes(size = sum_emissions_tonnes, color = mean_lcoh), alpha = 0.9) +
    scale_size_area(name = "Annual CO₂e (t)", max_size = max_point_size, labels = scales::comma) +
    scale_color_gradient2(
      name = "Mean Δ LCOH\n(USD/MMBtu)",
      midpoint = 0, low = "#3288bd", mid = "#ffffbf", high = "#d53e4f",
      limits = c(-lim, lim), oob = scales::squish, na.value = "grey50"
    ) +
    labs(title = paste0(tech_choice, " — ", policy_choice),
         subtitle = paste0("Color = mean Δ LCOH vs NG baseline (state × industry). Points sized by annual CO₂e. Year: ", plot_year),
         x = "Industry", y = "State") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right")
  
  p
}



# ---------- loop requested combos and save ----------
combos_requested <- tidyr::crossing(tech_clean = tech_choices, policy_label = policy_choices)

for (i in seq_len(nrow(combos_requested))) {
  tech_choice  <- combos_requested$tech_clean[i]
  policy_choice <- combos_requested$policy_label[i]
  message("Rendering: ", tech_choice, " | ", policy_choice)
  
  p <- tryCatch(build_p3(tech_choice, policy_choice), error = function(e) { message("  build error: ", e$message); NULL })
  if (is.null(p)) {
    message("  -> No data for this combo; skipping.")
    next
  }
  
  raw_name <- paste0(tech_choice, " x ", policy_choice, " matrix ", date_str, ".png")
  safe_name <- raw_name %>% gsub("[^A-Za-z0-9 _.-]", "_", .) %>% str_replace_all(" ", "_")
  out_path <- file.path(out_dir, safe_name)
  
  ggplot2::ggsave(out_path, p, width = 15, height = 10, dpi = 300)
  message("  -> Saved: ", out_path)
}




#### state emissions figures ####
# modeled naics
modeled_naics <- c(311221, 325193, 311313, 322120, 311224,
                   325110, 325311, 312140, 311611, 311225,
                   325180, 325194, 322130, 322110, 311421,
                   325211, 311513, 311514, 311314, 311942,
                   311613, 312120, 325120, 311423, 325312, 325212,
                   311411, 311615, 322291, 311511, 311422, 311919, 311230)

target_naics <- read_excel("state_fact_sheets/data/raw/target_NAICS.xlsx") %>%
  clean_names() %>%
  filter(naics_code %in% modeled_naics) %>% 
  mutate(naics_code = as.character(naics_code))

target_year <- 2023

# pull in ghgrp data
ghgrp_raw_df <- read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>%
  clean_names() %>% 
  filter(year == target_year) %>% 
  mutate(naics_code = as.character(primary_naics)) %>%
  filter(str_starts(naics_code, "31") | str_starts(naics_code, "32") | str_starts(naics_code, "33")) %>%
  filter(!str_starts(naics_code, "324")) %>%
  filter(!is.na(co2e_emission)) %>% 
  mutate(
    sector = case_when(
      str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food & Beverage",
      str_starts(naics_code, "325") ~ "Chemicals",
      str_starts(naics_code, "322") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    )
  ) %>%
  left_join(target_naics, by = "naics_code") %>%
  mutate(
    description = if_else(is.na(description), "Other Manufacturing", description)
    #,
    #naics_code = if_else(sector == "Other Manufacturing", NA, naics_code)
  ) %>% 
  mutate(
    # Sector labels already assigned earlier — now correct ambiguous "Other Manufacturing" descriptions
    description = case_when(
      description == "Other Manufacturing" & str_starts(naics_code, "311") ~ "Other Food & Beverage Manufacturing",
      description == "Other Manufacturing" & str_starts(naics_code, "312") ~ "Other Food & Beverage Manufacturing",
      description == "Other Manufacturing" & str_starts(naics_code, "322") ~ "Other Pulp & Paper Manufacturing",
      description == "Other Manufacturing" & str_starts(naics_code, "325") ~ "Other Chemicals Manufacturing",
      TRUE ~ description
    )
  ) 


ghgrp_filtered_df <- ghgrp_raw_df %>% 
  filter(state %in% states_of_interest) %>% 
  rename(naics_description = description) %>% 
  mutate(co2e_emissions_mmt = co2e_emission/10^6)
  
library(tidyverse)
library(lubridate)

# ---------- user options ----------
plot_year <- 2023
out_dir <- "state_fact_sheets/outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
date_str <- format(Sys.Date(), "%y%m%d")
top_n_categories <- 12   # how many NAICS to show individually

# ---------- aggregate & collapse small NAICS ----------
plot_df <- ghgrp_filtered_df %>%
  group_by(state, naics_description) %>%
  summarise(total_co2e = sum(co2e_emissions_mmt, na.rm = TRUE), .groups = "drop")

top_cats <- plot_df %>%
  group_by(naics_description) %>%
  summarise(national_total = sum(total_co2e, na.rm = TRUE)) %>%
  arrange(desc(national_total)) %>%
  slice_head(n = top_n_categories) %>%
  pull(naics_description)

plot_df2 <- plot_df %>%
  mutate(naics_desc_trim = if_else(naics_description %in% top_cats, naics_description, "Other in-scope sectors")) %>%
  group_by(state, naics_desc_trim) %>%
  summarise(total_co2e = sum(total_co2e, na.rm = TRUE), .groups = "drop")

# order states by total emissions
state_order <- plot_df2 %>%
  group_by(state) %>%
  summarise(state_total = sum(total_co2e, na.rm = TRUE)) %>%
  arrange(desc(state_total)) %>%
  pull(state)

plot_df2 <- plot_df2 %>%
  mutate(state = factor(state, levels = state_order))

# ---------- sector lookup and palette seeds (your requested colors) ----------
naics_sector_lookup <- ghgrp_filtered_df %>%
  distinct(naics_description, sector) %>%
  filter(!is.na(naics_description))

# build unique description list and ensure "Other..." sector labels
unique_descs <- plot_df2 %>%
  distinct(naics_desc_trim) %>%
  rename(naics_description = naics_desc_trim) %>%
  left_join(naics_sector_lookup, by = "naics_description") %>%
  mutate(
    sector = case_when(
      naics_description == "Other in-scope sectors" ~ "Other in-scope",
      is.na(sector) & str_detect(naics_description, "Other") ~ "Other Manufacturing",
      TRUE ~ sector
    )
  )

# palette seeds (use exactly what you supplied)
make_shades <- function(n, light, dark = NULL) {
  if (n <= 1) return(ifelse(is.null(dark), light, dark))
  if (is.null(dark)) return(rep(light, n))
  colorRampPalette(c(light, dark))(n)
}

sector_palette_seeds <- list(
  `Food & Beverage`      = c("#ef5645", "#c43424"),
  `Chemicals`            = c("#c2e4e6", "#047c91"),
  `Pulp & Paper`         = c("#c7d3a1", "#6d7d33"),
  `Other Manufacturing`  = c("#f1eeea"),
  `Other in-scope`       = c("#febc11")
)

# ---------- create fill color mapping: naics_description -> color ----------
color_map <- unique_descs %>%
  arrange(sector, naics_description) %>%
  group_by(sector) %>%
  summarise(descs = list(naics_description), .groups = "drop") %>%
  mutate(
    n = map_int(descs, length),
    shades = map2(sector, n, ~ {
      seeds <- sector_palette_seeds[[.x]]
      if (is.null(seeds)) return(rep("#f1eeea", .y))
      if (length(seeds) == 1) return(make_shades(.y, seeds[1], NULL))
      make_shades(.y, seeds[1], seeds[2])
    }),
    mapping = map2(descs, shades, ~ tibble(naics_description = .x, color = .y))
  ) %>%
  pull(mapping) %>%
  bind_rows()

fill_colors <- set_names(color_map$color, color_map$naics_description)

# ---------- prepare plotting df and ensure colors exist ----------
plot_df2_for_plot <- plot_df2 %>%
  rename(naics_description = naics_desc_trim) %>%
  left_join(color_map %>% select(naics_description, color), by = "naics_description") %>%
  mutate(color = coalesce(color, "#808080"))

# build legend order from unique_descs (keeps sector order)
legend_order <- unique_descs %>% arrange(sector, naics_description) %>% pull(naics_description)

# put "Other in-scope sectors" then "Other Manufacturing" at the END,
# so the final last level is "Other Manufacturing" (topmost stack)
final_levels <- setdiff(legend_order, c("Other Manufacturing", "Other in-scope sectors"))
final_levels <- c(final_levels, "Other in-scope sectors", "Other Manufacturing")

# apply factor ordering and reorder fill_colors to match
plot_df2_for_plot <- plot_df2_for_plot %>%
  mutate(naics_description = factor(as.character(naics_description), levels = final_levels),
         state = factor(state, levels = state_order))

# reorder fill_colors, assign grey for any missing keys
fill_colors <- fill_colors[final_levels]
missing_cols <- setdiff(final_levels, names(fill_colors))
if (length(missing_cols) > 0) fill_colors[missing_cols] <- "#808080"
fill_colors <- fill_colors[final_levels]

# ---------- compute labels: % NOT Other Manufacturing per state ----------
label_df <- plot_df2_for_plot %>%
  group_by(state) %>%
  summarise(
    total = sum(total_co2e, na.rm = TRUE),
    other_manuf = sum(total_co2e[naics_description == "Other Manufacturing"], na.rm = TRUE),
    pct_not_other = 100 * (1 - other_manuf / total)
  ) %>%
  mutate(label = paste0(round(pct_not_other, 1), "%"), y_pos = total * 1.02) # slight offset above bar

# ---------- plot ----------
p_stacked <- ggplot(plot_df2_for_plot, aes(x = state, y = total_co2e, fill = naics_description)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = fill_colors, name = "Industry (NAICS)") +
  scale_y_continuous(labels = scales::comma) +
  geom_text(data = label_df, aes(x = state, y = y_pos, label = label),
            inherit.aes = FALSE, vjust = 0, size = 8, fontface = "bold", color = "black") +
  labs(title = paste0("Manufacturing CO2e Emissions by NAICS (","GHGRP"," ", plot_year, ")"),
       subtitle = "Labels represent the percentage of modeled manufacturing emissions.",
       x = "State", y = expression("CO"[2]*"e emissions (MMT)")) +
  theme_minimal(base_size = 24) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())

p_stacked

# save
out_file <- file.path(out_dir, paste0("ghgrp_stacked_by_naics_colbysector_", date_str, ".png"))
ggsave(out_file, p_stacked, width = 18, height = 10, dpi = 300)


