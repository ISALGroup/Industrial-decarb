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

# ---------------------------
# Price comparison (convert to $/MMBtu)
# ---------------------------

# Conversion constants
KWH_PER_MMBTU <- 293.071
MMBTU_PER_MCF <- 1.037

price_comparison <- param_df %>%
  mutate(
    elec_price_low_mmbtu  = elec_price_low * KWH_PER_MMBTU,
    elec_price_high_mmbtu = elec_price_high * KWH_PER_MMBTU,
    ng_price_mmbtu        = ng_price / MMBTU_PER_MCF,
    elec_to_ng_ratio_low  = elec_price_low_mmbtu / ng_price_mmbtu,
    elec_to_ng_ratio_high = elec_price_high_mmbtu / ng_price_mmbtu
  ) %>%
  select(state, elec_to_ng_ratio_low, elec_to_ng_ratio_high) %>%
  arrange(desc(elec_to_ng_ratio_low))


# -----------------------------
# Build summary tables 
# -----------------------------
# normalize tech name
normalize_tech <- function(x) {
  x <- str_remove(tolower(x), regex("(best|worst)$", ignore_case = TRUE))
  str_to_title(str_trim(x))
}

# facility_lcoh_tech_avg: per-facility, per-tech, per-policy, averaged over best/worst case scenarios
facility_lcoh_tech_avg <- facility_lcoh_df %>%
  mutate(tech_clean = normalize_tech(tech_scenario)) %>%
  group_by(facility_id, tech_clean, state, naics_description, policy_label, heat_mmbtu) %>%
  summarise(lcoh_avg = mean(lcoh, na.rm = TRUE), .groups = "drop")

# baseline LCOH must be baseline rows for same policy_label
baseline_lcoh <- facility_lcoh_tech_avg %>%
  filter(str_to_lower(tech_clean) == "baseline") %>%
  select(facility_id, policy_label, lcoh_baseline = lcoh_avg)

lcoh_elec_scenarios <- facility_lcoh_tech_avg %>%
  filter(str_to_lower(tech_clean) != "baseline") %>% 
  rename(lcoh_tech = lcoh_avg)
 
lcoh_delta <- lcoh_elec_scenarios %>% 
  left_join(baseline_lcoh, by=c("facility_id", "policy_label"))

# sum by state x sector x tech scenario x policy label
lcoh_state_sector_summary <- lcoh_delta %>% 
  group_by(state, naics_description, tech_clean, policy_label) %>% 
  summarise(
    total_weight_mmbtu = sum(heat_mmbtu, na.rm = TRUE),
    
    # weighted baseline mean: sum(baseline * heat)/sum(heat)
    lcoh_baseline_mmbtu_weighted_mean = if (total_weight > 0) {
      sum(lcoh_baseline * heat_mmbtu, na.rm = TRUE) / total_weight
    } else {
      NA_real_
    },
    
    # weighted tech mean
    lcoh_tech_mmbtu_weighted_mean = if (total_weight > 0) {
      sum(lcoh_tech * heat_mmbtu, na.rm = TRUE) / total_weight
    } else {
      NA_real_
    },
    
    # weighted delta (equivalent to baseline_mean - tech_mean)
    lcoh_delta_weighted_mean = if (total_weight > 0) {
      sum((lcoh_tech - lcoh_baseline) * heat_mmbtu, na.rm = TRUE) / total_weight
    } else {
      NA_real_
    },
    
    # extras for diagnostics
    lcoh_baseline_mean_unweighted = mean(lcoh_baseline, na.rm = TRUE),
    lcoh_tech_mean_unweighted     = mean(lcoh_tech, na.rm = TRUE),
    n_facilities = n_distinct(facility_id),
    .groups = "drop"
  )

# facility emissions for a chosen year 
facility_emissions_year <- facility_emissions_yearly_avg %>%
  mutate(tech_clean = normalize_tech(tech_scenario)) %>%
  left_join(select(tech_combined_df, facility_id) %>% distinct(), by = "facility_id") %>%
  filter(year == 2025) %>%                         # default year; we'll use this in aggregation
  rename(annual_emissions_co2e_emissions_mmt = annual_emissions_co2e_avg_tonnes) %>%
  select(facility_id, state, tech_clean, naics_description, annual_emissions_co2e_emissions_mmt)

###### MATRIX FIGURES ########

# ---------------------------
# USER INPUT (change these)
# ---------------------------
tech_choice    <- "Scenario4"
policy_choice  <- "No Policy"

fig_policies <- c(
  'No Policy',
  'Capex: -50%, Elec: -0%',
  'Capex: -100%, Elec: -0%',
  'Capex: -0%, Elec: -50%',
  'Capex: -50%, Elec: -50%'
)

naics_complete <- c(
  "Wet Corn Milling and Starch Manufacturing",
  "Ethyl Alcohol Manufacturing",
  "Phosphatic Fertilizer Manufacturing",
  "Animal (except Poultry) Slaughtering",
  "Cheese Manufacturing",
  "Rendering and Meat Byproduct Processing",
  "Breweries",
  "Synthetic Rubber Manufacturing",
  "Sanitary Paper Product Manufacturing",
  "Fluid Milk Manufacturing",
  "Other Snack Food Manufacturing"
)

states_of_interest <- c("CA", "IL", "NY", "MD", "MI", "MN", "MA", "WI", "CO", "PA", "OR")

out_dir <- "state_fact_sheets/outputs/matrix_plots"
#dir_create(out_dir, recurse = TRUE)


# ---------------------------
# Join LCOH delta to facility emissions
# ---------------------------
joined_full <- lcoh_delta %>%
  select(-naics_description) %>%
  left_join(facility_emissions_year, by = c("facility_id", "state", "tech_clean")) %>% 
  filter(state %in% states_of_interest,
         naics_description %in% naics_complete)

# ---------------------------
# Aggregate: state × naics_description × tech_clean × policy_label
# ---------------------------

agg_full_2 <- facility_emissions_year %>% 
  group_by(state, naics_description, tech_clean) %>%
  summarise(
    sum_emissions_co2e_emissions_mmt = sum(annual_emissions_co2e_emissions_mmt, na.rm = TRUE),
    n_facilities = n_distinct(facility_id),
    .groups = "drop"
  ) %>%
  ungroup() %>% 
  left_join(lcoh_state_sector_summary, by = c("state", "tech_clean", "naics_description"))

write.csv(agg_full_2, "state_fact_sheets/data/modified/lcoh_emissions_sum_by_state_sector.csv")

agg_full <- joined_full %>%
  filter(!is.na(naics_description)) %>%  # drop unknown industries
  group_by(state, naics_description, tech_clean, policy_label) %>%
  summarise(
    sum_emissions_co2e_emissions_mmt = sum(annual_emissions_co2e_emissions_mmt, na.rm = TRUE),
    mean_lcoh_delta = ifelse(
      sum_emissions_co2e_emissions_mmt > 0,
      sum(lcoh_delta * coalesce(annual_emissions_co2e_emissions_mmt, 0), na.rm = TRUE) /
        sum_emissions_co2e_emissions_mmt,
      NA_real_
    ),
    n_facilities = n_distinct(facility_id),
    .groups = "drop"
  ) %>%
  ungroup()

# ---------------------------
# Filter the aggregate for plotting / summaries
# ---------------------------
agg_full_filtered <- agg_full %>%
  filter(policy_label %in% fig_policies,
         naics_description %in% naics_complete,
         state %in% states_of_interest)

# ---------------------------
# Summary CSV (state × policy × tech × naics)
# ---------------------------
agg_summary <- agg_full_filtered %>%
  group_by(state, policy_label, tech_clean, naics_description, mean_lcoh_delta) %>%
  summarise(total_emissions = sum(sum_emissions_co2e_emissions_mmt, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_emissions)) %>%
  filter(tech_clean != "Baseline") %>%
  left_join(price_comparison, by = "state")

summary_csv_path <- "state_fact_sheets/data/modified/delta_lcoh_by_state_policy_naics_tech.csv"
xfun::dir_create(path_dir(summary_csv_path), recurse = TRUE)
write.csv(agg_summary, summary_csv_path, row.names = FALSE)

# ---------------------------
# Prepare subset of joined_full for plotting (restrict to chosen states/industries)
# ---------------------------
joined_full_plot <- joined_full %>%
  filter(state %in% states_of_interest, naics_description %in% naics_complete)

# Keep an ordered list of states by total emissions (global)
state_order_global <- joined_full_plot %>%
  group_by(state) %>%
  summarise(total_state_em = sum(coalesce(annual_emissions_co2e_emissions_mmt, 0), na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_state_em)) %>%
  pull(state)

# ---------------------------
# Bubble (point) matrix: state × industry colored by mean ΔLCOH, sized by emissions
# ---------------------------
df_plot <- joined_full %>%
  filter(tech_clean == tech_choice, policy_label == policy_choice) %>%
  group_by(state, naics_description) %>%
  summarise(
    sum_emissions = sum(coalesce(annual_emissions_co2e_emissions_mmt, 0), na.rm = TRUE),
    mean_lcoh = mean(coalesce(lcoh_delta, NA_real_), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(sum_emissions > 0, !is.na(mean_lcoh))

if (nrow(df_plot) == 0) stop(glue::glue("No data for tech = '{tech_choice}', policy = '{policy_choice}'"))

# symmetric color limit
lim_bubble <- max(abs(df_plot$mean_lcoh), na.rm = TRUE)
lim_bubble <- ifelse(is.finite(lim_bubble) & lim_bubble > 0, lim_bubble, 1)
max_point_size <- 28

df_plot <- df_plot %>%
  mutate(
    state = fct_reorder(state, sum_emissions, .fun = max, .desc = TRUE),
    naics_description = fct_reorder(naics_description, sum_emissions, .fun = max, .desc = TRUE)
  )

p_bubble <- ggplot(df_plot, aes(x = state, y = naics_description)) +
  geom_point(aes(size = sum_emissions, fill = mean_lcoh),
             shape = 21, color = "black", stroke = 0.25, alpha = 0.95) +
  scale_size_area(name = "Annual CO₂e (t)", max_size = max_point_size, labels = comma) +
  scale_x_discrete(expand = expansion(add = c(0.7, 0.7))) +
  scale_y_discrete(expand = expansion(add = c(0.35, 0.35))) +
  scale_fill_gradient2(
    name = "Mean Δ LCOH\n(USD/MMBtu)",
    midpoint = 0,
    low = "#2b83ba", mid = "#ffffff", high = "#d7191c",
    limits = c(-lim_bubble, lim_bubble), oob = scales::squish, na.value = "grey80"
  ) +
  labs(
    title = paste0(tech_choice, " — ", policy_choice),
    subtitle = "Points sized by annual CO₂e; color = mean Δ LCOH (state × industry). Year: 2025",
    x = "State", y = "Industry (NAICS description)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(8, 12, 20, 8), "pt")
  ) +
  coord_cartesian(clip = "off") +
  scale_y_discrete(limits = rev(levels(fct_inorder(df_plot$naics_description))))

# Save bubble plot
date_str <- format(Sys.Date(), "%y%m%d")
raw_name <- paste0(tech_choice, " x ", policy_choice, " bubble ", date_str, ".png")
safe_name <- gsub("[^A-Za-z0-9 _.-]", "_", raw_name) %>% str_replace_all(" ", "_")
save_path_bubble <- file.path(out_dir, safe_name)
#ggsave(save_path_bubble, p_bubble, width = 16, height = 8, dpi = 300)
#message("Saved bubble plot: ", save_path_bubble)

# ---------------------------
# Tiled matrix: use agg_full_filtered
# ---------------------------
policy_choice  <- c(#'No Policy',  
                    #'Capex: -50%, Elec: -0%' ,
                    #'Capex: -100%, Elec: -0%',
                    #'Capex: -0%, Elec: -50%'#,
                    'Capex: -50%, Elec: -50%'
                    )

df_mat <- agg_full %>%
  filter(tech_clean == tech_choice, policy_label == policy_choice) %>%
  select(state, naics_description, mean_lcoh_delta) %>%
  distinct()

if (nrow(df_mat) == 0) stop(glue::glue("No aggregate data for tech = '{tech_choice}', policy = '{policy_choice}'"))

# compute sensible orders
state_order <- df_mat %>%
  group_by(state) %>%
  summarise(n_present = sum(!is.na(mean_lcoh_delta)), .groups = "drop") %>%
  arrange(desc(n_present)) %>%
  pull(state)

industry_order <- df_mat %>%
  group_by(naics_description) %>%
  summarise(total_present = sum(!is.na(mean_lcoh_delta)), .groups = "drop") %>%
  arrange(desc(total_present)) %>%
  pull(naics_description)

df_mat <- df_mat %>%
  mutate(
    state = factor(state, levels = state_order),
    naics_description = factor(naics_description, levels = rev(industry_order)) # reverse so first is at top
  )

# symmetric color limits
#lim_tile <- max(abs(df_mat$mean_lcoh_delta), na.rm = TRUE)
#lim_tile <- ifelse(is.finite(lim_tile) & lim_tile > 0, lim_tile, 1)

p_tile <- ggplot(df_mat, aes(x = state, y = naics_description, fill = mean_lcoh_delta)) +
  geom_tile(color = "grey80", size = 0.25, na.rm = FALSE) +
  scale_fill_gradient2(
    midpoint = 0,
    low = "#003660",
    mid = "#fff",
    high = "#ef5645",
    limits = c(-30, 30), #hard coding so that each policy scenario has the same scale
    oob = scales::squish,
    #na.value = "#dcd6cc",
    name = "Δ LCOH\n(USD/MMBtu)"
  ) +
  labs(
    title = paste0(tech_choice, " — ", policy_choice),
    subtitle = "Δ LCOH vs NG baseline (mean across facilities in state × subsector)",
    x = "State", y = "Subsector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  scale_x_discrete(expand = expansion(add = c(0.2, 0.2))) +
  scale_y_discrete(expand = expansion(add = c(0, 0))) 

p_tile

# Save tiled plot
raw_name_tile <- paste0(tech_choice, " x ", policy_choice, " tile ", date_str, ".png")
safe_name_tile <- gsub("[^A-Za-z0-9 _.-]", "_", raw_name_tile) %>% str_replace_all(" ", "_")
save_path_tile <- file.path(out_dir, safe_name_tile)
ggsave(save_path_tile, p_tile, width = 12, height = 6, dpi = 300, scale=1.2)
message("Saved tiled plot: ", save_path_tile)



