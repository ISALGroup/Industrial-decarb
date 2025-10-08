# V1. 9/23
# Summing cumulative emissions by tech scenario and sector
# Build out a grid projection based on state targets
# does not include copollutant logic


#### SET-UP ####
# Load Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(tidylog)
library(stringr)
library(janitor)
library(glue)
library(patchwork)

#### SET STATE ####
# Set state :) 
state <- "WI"

# Tech scenario input 
tech_input_df <- 
  read_excel(glue("LCOH modelling/output/copollutant_longform_{state}.xlsx")) %>%
  mutate(facility_name = tolower(facility_name), 
         base_emissions_co2e = elec_ghg_emissions + noelec_ghg_emissions) %>% 
  rename(
    base_emissions_nox = `base_emissions_Nitrogen Oxides`,
    base_emissions_pm25 = `base_emissions_PM2.5 Primary (Filt + Cond)`, 
    base_emissions_so2 = `base_emissions_Sulfur Dioxide`
  ) %>%
  select(-1, -opex, -fuel_reduction, -match_type, -match_score) 

# TEMPORARY: natgas capex calculation
natgas_best <- 
  tech_input_df %>%
  filter(tech_scenario == 'Baseline') %>%
  mutate(tech_scenario = 'BaselineBest', 
         # best case natural gas 
         capex = 4733.79*(heat_mmbtu/49132.8)^0.8325)

tech_input_df <- 
  tech_input_df %>%
  mutate(
    tech_scenario = if_else(tech_scenario == 'Baseline', 'BaselineWorst', tech_scenario),
    # worst case natural gas
    capex = if_else(tech_scenario == 'BaselineWorst', 25761.09*(heat_mmbtu/40944)^0.8325, capex)
  ) %>%
  bind_rows(natgas_best)


# Pull in lat and long from rlps file
facility_lat_long <-
  read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>%
  select(facility_id, latitude, longitude) %>%
  distinct(facility_id, .keep_all = TRUE)

# Pull in facility info, this file doesn't have lat and long... (maybe add in the future)
facility_info <-
  read_excel("state_fact_sheets/data/raw/Facility_and_Unit_Emissions_Database_2023_v3.xlsx", sheet = 2) %>%
  clean_names() %>%
  rename(naics_code = primary_naics) %>%
  rename(naics_description = naics_title) %>%
  select(facility_id, naics_code, naics_description, county_fips, subregion) %>%
  inner_join(facility_lat_long, by = "facility_id") %>%
  mutate(
    sector = case_when(
      str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food & Beverage",
      str_starts(naics_code, "325") ~ "Chemicals",
      str_starts(naics_code, "322") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    )
  )


# --- Merge Inputs ---
tech_combined_df <- 
  tech_input_df %>%
  left_join(facility_info, by = "facility_id") 
# DROPPING IF ONLY ONE FACILITY IN A SECTOR
#group_by(naics_code) %>%  
#filter(n_distinct(facility_id) > 1) %>%      
#ungroup()


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

# Resource mix 
grid_mix_df <- read_excel("state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 3) %>%
  clean_names() %>%
  mutate(
    current_fossil_share = coal + oil + gas + other_fossil,
    current_clean_share = hydro + biomass + wind + solar + nuclear #removed geothermal for now due to E, close to 0
  ) %>%
  select(subregion = e_grid_subregion_acronym, current_fossil_share, current_clean_share)

#### YEAR-BY-YEAR GRID PROJECTION & ANNUAL EMISSIONS (updated) ####

# User inputs
policy_targets_global <- c('2030' = 0.50, '2050' = 1.00)  # e.g., 60% by 2030, 100% by 2050
start_year <- 2025
end_year   <- 2050
years_vec  <- seq(start_year, end_year)
allow_degradation <- FALSE  # if FALSE, any target below current clean share will be capped to current

# Helper: interpolate named target vector to full years (fixed signature)
interp_targets_to_years <- function(targets_named, years) {
  yrs_target <- as.integer(names(targets_named))
  vals <- as.numeric(targets_named)
  vals <- ifelse(vals > 1, vals / 100, vals) # accept 0-100 or 0-1
  tibble(year = years, target_clean = stats::approx(x = yrs_target, y = vals, xout = years, rule = 2)$y)
}

# Build targets table per subregion (global targets applied to all subregions)
targets_tbl <- grid_mix_df %>%
  select(subregion, current_clean_share) %>%
  rowwise() %>%
  mutate(tmp = list(interp_targets_to_years(policy_targets_global, years_vec))) %>%
  tidyr::unnest(cols = c(tmp)) %>%
  ungroup() %>%
  # enforce no degradation if requested
  mutate(
    target_clean = as.numeric(target_clean),
    target_clean = if_else(!allow_degradation & target_clean < current_clean_share, current_clean_share, target_clean),
    target_clean = pmin(1, pmax(0, target_clean))
  ) %>%
  select(subregion, year, target_clean)

# Build year-by-year scaled grid intensities (kg/kWh)
grid_yearly_scaled <- egrid_df %>%
  left_join(grid_mix_df, by = "subregion") %>%       # provides current_fossil_share/current_clean_share
  left_join(targets_tbl, by = "subregion") %>%       # adds year + target_clean
  mutate(
    current_fossil_share = pmin(1, pmax(0, as.numeric(current_fossil_share))),
    target_clean = pmin(1, pmax(0, as.numeric(target_clean))),
    target_fossil_share = pmax(0, 1 - target_clean),
    scaling_factor = if_else(current_fossil_share > 0, target_fossil_share / current_fossil_share, 0),
    co2e_kg_kwh_year = co2e_kg_kwh * scaling_factor,
    nox_kg_kwh_year  = nox_kg_kwh  * scaling_factor,
    so2_kg_kwh_year  = so2_kg_kwh  * scaling_factor,
    pm25_kg_kwh_year = pm25_kg_kwh * scaling_factor
  ) %>%
  select(subregion, year, current_fossil_share, current_clean_share, target_clean, scaling_factor,
         co2e_kg_kwh_year, nox_kg_kwh_year, so2_kg_kwh_year, pm25_kg_kwh_year)

# Optional flag & approach for copollutants:
# to approximate electrifiable fraction of copollutants by the same fraction as CO2e,
# set apply_copollutants_by_ratio <- TRUE. Until confirm proportions, keep FALSE.
apply_copollutants_by_ratio <- FALSE

# Join year intensities to tech_combined_df to produce facility × year rows (CO2e logic only)
facility_yearly <- tech_combined_df %>%
  # expand facilities across years by joining subregion-year grid table
  left_join(grid_yearly_scaled, by = "subregion") %>%
  # compute annual CO2e emissions 
  mutate(
    is_baseline = stringr::str_detect(tolower(tech_scenario), "baseline"),
    # Baseline: elec_ghg_emissions + noelec_ghg_emissions
    # Other scenarios: electrifiable emissions computed from change_in_electricity_demand_kwh * grid intensity (kg/kWh -> /1000 to t)
    annual_emissions_co2e_tonnes = if_else(
      is_baseline,
      elec_ghg_emissions + noelec_ghg_emissions,
      (change_in_electricity_demand_kwh * co2e_kg_kwh_year) / 1000 + noelec_ghg_emissions
    )
  ) %>%
  # If desired, compute copollutants with same electrifiable fraction as CO2e (OPTIONAL)
  # NOTE: only do this if apply_copollutants_by_ratio == TRUE and you understand the assumption
  { if (apply_copollutants_by_ratio) {
    . %>% mutate(
      # calculate electrifiable fraction of CO2e at facility level 
      electrifiable_frac_co2e = elec_ghg_emissions / (elec_ghg_emissions + noelec_ghg_emissions),
      # Example for NOx: total base NOx (if present in tech_input_df) would be split similarly; adjust names to your actual columns
      # annual_emissions_nox_kg = (change_in_electricity_demand_kwh * nox_kg_kwh_year) * 1 + (noelectric_nox_kg) # placeholder
      # Add similar lines for so2, pm25 if you have base totals and want to apportion
    )
  } else { . }
  } %>%
  select(facility_id, facility_name, state, county_fips, latitude, longitude,
         subregion, sector, naics_code, naics_description, tech_scenario,
         year, current_fossil_share, target_clean, scaling_factor,
         co2e_kg_kwh_year, annual_emissions_co2e_tonnes)

# Average Best/Worst for each facility × year × tech_scenario 
facility_yearly_avg <- facility_yearly %>%
  mutate(
    tech_scenario = str_remove(tolower(tech_scenario), regex("(best|worst)$", ignore_case = TRUE))
  ) %>%
  group_by(facility_id, facility_name, state, subregion, sector, naics_code, naics_description,
           tech_scenario, year) %>%
  summarise(
    annual_emissions_avg_tonnes = mean(annual_emissions_co2e_tonnes, na.rm = TRUE),
    co2e_kg_kwh_year = mean(co2e_kg_kwh_year, na.rm = TRUE),
    .groups = "drop"
  )

# Cumulative CO2e (2030-2050) by sector & tech_scenario for clean-grid trajectory (already embedded in facility_yearly_avg)
cumulative_co2e_by_scenario_sector <- facility_yearly_avg %>%
  filter(year >= 2030, year <= 2050) %>%
  group_by(sector, tech_scenario) %>%
  summarise(
    cum_tco2e = sum(annual_emissions_avg_tonnes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cum_MtCO2e = cum_tco2e / 1e6,
         grid_scenario = "100% Clean")

# --- BAU: construct BAU emission series using 2025 grid intensity for all years 2030-2050 ---
# capture 2025 intensities by subregion
bau_grid_2025 <- grid_yearly_scaled %>%
  filter(year == 2025) %>%
  select(subregion, bau_co2e_kg_kwh_2025 = co2e_kg_kwh_year)

# for each facility × tech scenario (including Best/Worst), compute annual emissions for each year 2030-2050 using 2025 intensity
facility_yearly_bau <- tech_combined_df %>%
  # join the 2025 intensity into the facilities
  left_join(bau_grid_2025, by = "subregion") %>%
  # expand across years 2030:2050
  tidyr::crossing(year = seq(2030, 2050)) %>%
  mutate(
    is_baseline = stringr::str_detect(tolower(tech_scenario), "baseline"),
    annual_emissions_co2e_tonnes_bau = if_else(
      is_baseline,
      elec_ghg_emissions + noelec_ghg_emissions, # baseline unaffected by BAU grid assumption (explicit baseline value)
      (change_in_electricity_demand_kwh * bau_co2e_kg_kwh_2025) / 1000 + noelec_ghg_emissions
    )
  ) %>%
  # average best/worst like above
  mutate(tech_scenario = str_remove(tolower(tech_scenario), regex("(best|worst)$", ignore_case = TRUE))) %>%
  group_by(facility_id, facility_name, state, subregion, sector, naics_code, naics_description,
           tech_scenario, year) %>%
  summarise(
    annual_emissions_avg_tonnes_bau = mean(annual_emissions_co2e_tonnes_bau, na.rm = TRUE),
    .groups = "drop"
  )

# sum 2030-2050
bau_cum_co2e <- facility_yearly_bau %>%
  group_by(sector, tech_scenario) %>%
  summarise(
    cum_tco2e = sum(annual_emissions_avg_tonnes_bau, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cum_MtCO2e = cum_tco2e / 1e6,
         grid_scenario = "BAU")

# Combine for plotting
plot_df <- bind_rows(bau_cum_co2e, cumulative_co2e_by_scenario_sector) 
  

# ---- (plotting code unchanged, just reuse your existing mapping objects) ----

# Set up fill and label mappings (unchanged)
scenario_fill <- c(
  "baseline" = "#fb9a99",
  "scenario1" = "#1f78b4",
  "scenario3" = "#a6cee3",
  "scenario2" = "#33a02c",
  "scenario4" = "#b2df8a"
)

scenario_labels <- c(
  "baseline" = "Baseline",
  "scenario1" = "1: E-Boiler",
  "scenario3" = "2: E-Boiler + EE",
  "scenario2" = "3: ASHP",
  "scenario4" = "4: ASHP + EE"
)

# ---- Order sectors manually ----
sector_order <- c("Pulp & Paper", "Chemicals", "Food & Beverage")

plot_df <- plot_df %>%
  mutate(
    sector = factor(sector, levels = sector_order),
    tech_scenario = factor(tech_scenario, levels = names(scenario_fill))
  ) 



# ---- Get shared y-axis max ----
y_max <- max(plot_df$cum_MtCO2e, na.rm = TRUE)

# ---- BAU plot ----
p_bau <- plot_df %>%
  filter(grid_scenario == "BAU") %>%
  ggplot2::ggplot(aes(x = sector, y = cum_MtCO2e, fill = tech_scenario)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
  ggplot2::labs(
    title = "Business-As-Usual Grid Scenario",
    x = NULL,
    y = "Cumulative MMt CO2e (2030-2050)",
    fill = "Tech scenario"
  ) +
  ggplot2::scale_fill_manual(values = scenario_fill, labels = scenario_labels) +
  ggplot2::scale_y_continuous(limits = c(0, y_max)) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom")

# ---- Clean Grid plot ----
p_clean <- plot_df %>%
  filter(grid_scenario == "100% Clean") %>%
  ggplot2::ggplot(aes(x = sector, y = cum_MtCO2e, fill = tech_scenario)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
  ggplot2::labs(
    title = "100% Clean Grid Trajectory Scenario",
    x = NULL,
    y = NULL,
    fill = "Tech scenario"
  ) +
  ggplot2::scale_fill_manual(values = scenario_fill, labels = scenario_labels) +
  ggplot2::scale_y_continuous(limits = c(0, y_max)) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom")

# ---- Combine side-by-side with shared legend ----
combined_plot <- p_bau + p_clean + patchwork::plot_layout(guides = "collect") & 
  ggplot2::theme(legend.position = "bottom")

print(combined_plot)
ggsave("state_fact_sheets/outputs/wi_cumulative_emissions_250923.png", width = 8, height = 5, dpi = 300)
