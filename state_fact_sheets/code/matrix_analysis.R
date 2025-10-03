### Hot spot analysis
## EMT
# 10.3.25

library(tidyverse)
library(scales)
library(readxl)
# ----- user inputs -----
plot_year   <- 2025
sc_to_plot  <- "Scenario1"   # change to Scenario1/2/3/4 or "Baseline" to test
out_file    <- paste0("matrix_single_", sc_to_plot, "_", plot_year, ".png")

# ----- REBUILD (use the full, unfiltered files) -----
# tech_input original (don't filter by state)
tech_input_df.o <- read_excel("LCOH modelling/output/copollutant_longform_national_wtemps_addedsectors_30sept.xlsx") %>%
  select(-1, -opex)

# create BaselineBest rows (same approach as you used before)
natgas_best <- tech_input_df.o %>%
  filter(str_to_lower(tech_scenario) == "baseline") %>%
  mutate(tech_scenario = "BaselineBest",
         capex = 4733.79 * (heat_mmbtu / 49132.8)^0.8325)

tech_input_df <- tech_input_df.o %>%
  mutate(tech_scenario = if_else(str_to_lower(tech_scenario) == "baseline", "BaselineWorst", tech_scenario),
         capex = if_else(str_to_lower(tech_scenario) == "baselineworst",
                         25761.09 * (heat_mmbtu / 54592)^0.8325, capex)) %>%
  bind_rows(natgas_best) %>%
  mutate(facility_id = as.character(facility_id),
         base_emissions_co2e = elec_ghg_emissions + noelec_ghg_emissions)

# facility_info (no state filter)
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

# egrid + grid mix (as before)
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

# policy grid (same as before)
policy_grid <- expand.grid(capex_subsidy = seq(0,1,by=0.1), elec_discount = seq(0,1,by=0.25))

#### LCOH DATA WORK & SET-UP ####
lcoh_func <- function(
    ## parameters
  r, 
  elec_price,
  ng_price,
  t, # for now, assume same lifetime across equipment, but we can change this by technology later 
  # nat gas boiler assumptions
  #t_ngboiler,
  ngboiler_om_best, 
  ngboiler_om_worst, 
  # e-boiler assumptions
  #t_eboiler, 
  eboiler_om_best, 
  eboiler_om_worst, 
  # hp assumptions
  #t_hthp, 
  hthp_om_best, 
  hthp_om_worst, 
  
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
      opex_om <- ngboiler_om_worst * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "BaselineBest") ~ {
      opex_ng <- (heat_mmbtu/.9) * ng_price       # energy costs
      opex_om <- ngboiler_om_best * capex    # o&m costs
      
      numerator   <- capex + ((opex_om + opex_ng) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario1Best|Scenario3Best") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- eboiler_om_worst * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario1Worst|Scenario3Worst") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- eboiler_om_best * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario2Best|Scenario4Best") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- hthp_om_worst * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }, 
    str_detect(tech_scenario, "Scenario2Worst|Scenario4Worst") ~ {
      capex_adj <- capex * (1 - capex_subsidy)
      opex_elec <- change_in_electricity_demand_kwh * (elec_price * (1-elec_discount))
      opex_om <- hthp_om_best * capex
      
      numerator   <- capex_adj + ((opex_om + opex_elec) * discount_sum)
      denominator <- heat_mmbtu * discount_sum
      
      numerator / denominator
    }
  )
}



facility_lcoh_df <- tidyr::crossing(tech_combined_df, policy_grid) %>%
  left_join(param_df, by = "state") %>%
  # call lcoh_func rowwise (the function you provided earlier)
  rowwise() %>%
  mutate(
    lcoh = lcoh_func(
      r = r[1], elec_price = elec_price[1], ng_price = ng_price[1], t = t[1],
      ngboiler_om_best = ngboiler_om_best[1], ngboiler_om_worst = ngboiler_om_worst[1],
      eboiler_om_best = eboiler_om_best[1], eboiler_om_worst = eboiler_om_worst[1],
      hthp_om_best = hthp_om_best[1], hthp_om_worst = hthp_om_worst[1],
      tech_scenario = tech_scenario, capex = capex, heat_mmbtu = heat_mmbtu,
      change_in_electricity_demand_kwh = change_in_electricity_demand_kwh,
      capex_subsidy = capex_subsidy, elec_discount = elec_discount
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

# facility_yearly uses tech_combined_df joined to grid_yearly_scaled
facility_yearly <- tech_combined_df %>%
  left_join(grid_yearly_scaled, by = "subregion") %>%
  mutate(
    is_baseline = stringr::str_detect(tolower(tech_scenario), "baseline"),
    annual_emissions_co2e_tonnes = if_else(
      is_baseline,
      elec_ghg_emissions + noelec_ghg_emissions,
      (change_in_electricity_demand_kwh * scaled_co2e_kg_kwh_year) / 1000 + noelec_ghg_emissions
    )
  ) %>%
  select(facility_id, facility_name, state, subregion, sector, naics_code, naics_description,
         tech_scenario, year, annual_emissions_co2e_tonnes)

# average Best/Worst into facility_yearly_avg
facility_yearly_avg <- facility_yearly %>%
  mutate(tech_scenario = str_remove(tolower(tech_scenario), regex("(best|worst)$", ignore_case = TRUE))) %>%
  group_by(facility_id, facility_name, state, subregion, sector, naics_code, naics_description,
           tech_scenario, year) %>%
  summarise(annual_emissions_avg_tonnes = mean(annual_emissions_co2e_tonnes, na.rm = TRUE), .groups = "drop")


library(scales)

# -----------------------------
# 1) Build summary tables once
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
facility_emissions_year <- facility_yearly_avg %>%
  mutate(tech_clean = normalize_tech(tech_scenario)) %>%
  # facility_yearly_avg should include industry_clean; if not, join to facility_info to get it:
  left_join(select(tech_combined_df, facility_id) %>% distinct(), by = "facility_id") %>%
  filter(year == 2025) %>%                         # default year; we'll use this in aggregation
  rename(annual_emissions_tonnes = annual_emissions_avg_tonnes) %>%
  select(facility_id, state, tech_clean, annual_emissions_tonnes)

# joined: LCOH delta matched to emissions by facility × tech × state
joined_full <- lcoh_delta %>%
  left_join(facility_emissions_year, by = c("facility_id", "state", "tech_clean"))

# Aggregate once: state × naics_description × tech_clean × policy_label
agg_full <- joined_full %>%
  filter(!is.na(naics_description)) %>%   # drop unknown industries if you want (or keep with NA label)
  group_by(state, naics_description, tech_clean, policy_label) %>%
  summarise(
    sum_emissions_tonnes = sum(annual_emissions_tonnes, na.rm = TRUE),
    mean_lcoh_delta = ifelse(sum_emissions_tonnes > 0,
                             sum(lcoh_delta * coalesce(annual_emissions_tonnes, 0), na.rm = TRUE) / sum_emissions_tonnes,
                             NA_real_),
    n_facilities = n_distinct(facility_id),
    .groups = "drop"
  ) %>%
  ungroup()

# fill full matrix (so missing combos show as blanks)
all_states <- sort(unique(agg_full$state))
all_industries <- sort(unique(agg_full$naics_description))

agg_full <- agg_full %>%
  tidyr::complete(state = all_states, naics_description = all_industries,
                  tech_clean = unique(agg_full$tech_clean),
                  policy_label = unique(agg_full$policy_label),
                  fill = list(sum_emissions_tonnes = 0, mean_lcoh_delta = NA_real_, n_facilities = 0))

# -----------------------------
# 2) Plotting 
# -----------------------------
library(tidyverse)
library(scales)

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






