### EMT
### Version 1
### Oct. 24, 2025

### Emissions code
### Calculating: criteria pollutants, emissions under different grid scenarios, health impacts 
### Also making emissions figures for the national report
### So help me god 


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


### Set working directory ###
setwd('/Users/eleanor/Documents/Industrial_Decarbonization')

### Read in results and supporting data ###
# unit-level results data, no tech_scenario
unit_input_raw <- read_excel("Industrial-decarb/state_fact_sheets/data/merged_longform.xlsx") %>% 
  clean_names()

# facility-level results w/ tech_scenario LOCAL FILE BC TOO LARGE FOR GIT
facility_input_raw <- read_csv("lcoh_industrialdecarb_facility_level.csv") %>% 
  clean_names()

# AP-42 emissions factors
ap42_ef_table <- read_csv("Industrial-decarb/state_fact_sheets/data/ap42_ef_lookup_with_explanations.csv") 

# eGRID data
egrid_df <-
  read_excel("Industrial-decarb/state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 2) %>%
  clean_names() %>%
  mutate(bau_co2e_kg_kwh = co2e * 0.000453592) %>%
  mutate(bau_ch4_kg_kwh = ch4 * 0.000453592) %>%
  mutate(bau_n2o_kg_kwh = n2o * 0.000453592) %>%
  mutate(bau_nox_kg_kwh = annual_n_ox * 0.000453592) %>%
  mutate(bau_so2_kg_kwh = so2 * 0.000453592) %>%
  mutate(bau_pm25_kg_kwh = pm_2_5 * 0.000453592) %>%
  rename(subregion = e_grid_subregion_acronym) %>%
  select(subregion, bau_co2e_kg_kwh, bau_nox_kg_kwh, bau_so2_kg_kwh, bau_pm25_kg_kwh)

# Resource mix 
grid_mix_df <- read_excel("Industrial-decarb/state_fact_sheets/data/raw/egrid_summary_tables_rev2.xlsx", sheet = 3) %>%
  clean_names() %>%
  mutate(
    current_fossil_share = coal + oil + gas + other_fossil,
    current_clean_share = hydro + biomass + wind + solar + nuclear #removed geothermal for now due to E, close to 0
  ) %>%
  select(subregion = e_grid_subregion_acronym, current_fossil_share, current_clean_share)

######### Merge inputs ###########

# remove cost data, not needed for emissions calculations
facility_level_emissions_df <- facility_input_raw %>% 
  filter(itc == 0,
         ptc == 0,
         rate_reduction == 0
  ) %>% 
  select(-itc, -ptc, -rate_reduction, -discounted_elec_price, -non_elec_capex, -non_elec_yearly_opm_costs,
         -non_elec_fuel_costs, -elec_capex, -elec_yearly_opm_costs, -elec_fuel_costs, -lcoh, -electricity_costs, 
         -opex, -fuel_costs, -capex, -yearly_opm_costs) %>% 
  # merge w egrid data
  left_join(grid_mix_df, by = "subregion") %>% 
  left_join(egrid_df, by = "subregion")

facility_input_simple <- facility_level_emissions_df%>% 
  select(facility_id, tech_scenario, base_emissions)

### make sure AP42 unit and fuel categories match our data

# a normalization function
norm <- function(x){
  x %>%
    tolower() %>%
    str_replace_all("\\s+", " ") %>%  # collapse whitespace
    str_trim() %>%
    str_replace_all("[\\.,()\\-/&']", "") %>% # remove common punctuation
    str_replace_all("no2|no.2|no 2", "no2") %>%
    str_replace_all("fuel oil|distillate", "fuel_oil") %>%
    str_replace_all("diesel", "fuel_oil") %>%
    str_replace_all("natural gas|ng|fuel gas", "natural_gas") %>%
    str_replace_all("propane|lpg", "propane") %>%
    str_replace_all("wood|biomass|wood residue", "biomass") %>%
    str_replace_all("\\s+", "_")
}

# normalize the fuel type and unit type so they can be merged
unit_input_norm <- unit_input_raw %>%
  mutate(
    unit_norm = norm(unit_type),
    fuel_norm = norm(if_else(!is.na(fuel_type), as.character(fuel_type), ""))  # adjust 'fuel' col name if different
  )

ap42_ef_table_norm <- ap42_ef_table %>%
  mutate(
    unit_norm = norm(unit_matched),
    fuel_norm = norm(if_else(!is.na(fuel_matched), as.character(fuel_matched), ""))
  )

# merge AP42 with unit data
unit_emissions_df <- unit_input_norm %>% 
  left_join(ap42_ef_table_norm, by = c("fuel_norm", "unit_norm"))


###### ONSITE EMISSIONS CALCULATIONS ######
onsite_facility_emissions <- unit_emissions_df %>% 
  filter(electrified_option == "not_electrifiable") %>% 
  mutate(unit_so2_emissions_kg = so2_kg_mmbtu * process_unit_heat_demand,
         unit_pm25_emissions_kg = pm25_kg_mmbtu * process_unit_heat_demand,
         unit_nox_emissions_kg = nox_kg_mmbtu * process_unit_heat_demand) %>% 
  rename(unit_co2e_emissions_mt = unit_ghg_emissions) %>% 
  group_by(facility_id, is_biogenic) %>% 
  summarize(
    onsite_facility_so2_emissions_kg = sum(unit_so2_emissions_kg, na.rm = TRUE),
    onsite_facility_pm25_emissions_kg = sum(unit_pm25_emissions_kg, na.rm = TRUE),
    onsite_facility_nox_emissions_kg = sum(unit_nox_emissions_kg, na.rm = TRUE),
    onsite_facility_co2e_emissions_mt = sum(unit_co2e_emissions_mt, na.rm = TRUE) 
  )

  
##### GRID EMISSIONS CALCULATIONS #######
# build out linear grid decarbonization table
years <- 2030:2050
start_year <- 2030
start_share <- 0.50
end_year <- 2050
end_share <- 1.00

# create national traj with explicit 2025 row (ces = NA placeholder)
national_traj <- tibble(policy_year = c(2025, years)) %>%
  mutate(ces = case_when(
    policy_year == 2025 ~ NA_real_,   # placeholder to be filled with facility baseline on join
    policy_year <= start_year ~ start_share,
    policy_year >= end_year   ~ end_share,
    TRUE ~ start_share + (end_share - start_share) * ((policy_year - start_year) / (end_year - start_year))
  ))


clean_grid_facility_emissions <- facility_level_emissions_df %>%
  # fill 2025 ces with the facility baseline:
  distinct(primary_naics, naics_title, facility_id, reporting_year, facility_name, state, sector,
           subregion, county_fips, tech_scenario, scenario_rank,
           current_clean_share, bau_co2e_kg_kwh, bau_nox_kg_kwh, bau_so2_kg_kwh, bau_pm25_kg_kwh,
           change_in_electricity_demand_kwh) %>%
  tidyr::crossing(national_traj) %>%
  mutate(
    # set 2025 = BAU baseline clean share to preserve bau_EF in that year
    ces = if_else(policy_year == 2025, coalesce(current_clean_share, 0), ces)
  ) %>% 
  # scale the emissions factors based on the 
  mutate(scaled_co2e_kg_kwh = bau_co2e_kg_kwh * ((1-ces)/(1-current_clean_share)),
         scaled_so2_kg_kwh = bau_so2_kg_kwh * ((1-ces)/(1-current_clean_share)),
         scaled_nox_kg_kwh = bau_nox_kg_kwh * ((1-ces)/(1-current_clean_share)),
         scaled_pm25_kg_kwh = bau_pm25_kg_kwh * ((1-ces)/(1-current_clean_share))) %>% 
  mutate(clean_grid_co2e_emissions_mt = scaled_co2e_kg_kwh * change_in_electricity_demand_kwh/1000,
         clean_grid_so2_emissions_kg = scaled_so2_kg_kwh * change_in_electricity_demand_kwh,
         clean_grid_nox_emissions_kg = scaled_nox_kg_kwh * change_in_electricity_demand_kwh,
         clean_grid_pm25_emissions_kg = scaled_pm25_kg_kwh * change_in_electricity_demand_kwh
         ) %>% 
  select(primary_naics, facility_id, reporting_year, facility_name, state, county_fips, subregion, naics_title, sector, tech_scenario,
         scenario_rank, current_clean_share, ces, policy_year, scaled_co2e_kg_kwh, scaled_so2_kg_kwh, scaled_nox_kg_kwh, scaled_pm25_kg_kwh, 
         clean_grid_co2e_emissions_mt, clean_grid_so2_emissions_kg, clean_grid_nox_emissions_kg, clean_grid_pm25_emissions_kg)

bau_grid_facility_emissions <- facility_level_emissions_df %>% 
  crossing(national_traj) %>% 
  select(-ces) %>% 
  mutate(bau_grid_co2e_emissions_mt = bau_co2e_kg_kwh * change_in_electricity_demand_kwh/1000,
         bau_grid_so2_emissions_kg = bau_so2_kg_kwh * change_in_electricity_demand_kwh,
         bau_grid_nox_emissions_kg = bau_nox_kg_kwh * change_in_electricity_demand_kwh,
         bau_grid_pm25_emissions_kg = bau_pm25_kg_kwh * change_in_electricity_demand_kwh
  ) %>% 
  select(primary_naics, facility_id, reporting_year, facility_name, state, county_fips, subregion, naics_title, sector, tech_scenario,
         scenario_rank, change_in_electricity_demand_kwh, non_elec_orig_unit_ghg_emissions, elec_unit_ghg_emissions, base_emissions, current_clean_share, policy_year, bau_co2e_kg_kwh, bau_so2_kg_kwh, bau_nox_kg_kwh, bau_pm25_kg_kwh, 
         bau_grid_co2e_emissions_mt, bau_grid_so2_emissions_kg, bau_grid_nox_emissions_kg, bau_grid_pm25_emissions_kg)


# baseline scenario
baseline_facility_emission <- facility_level_emissions_df %>% 
  crossing(national_traj) %>% 
  filter(tech_scenario == "baseline") %>% 
  select(facility_id,scenario_rank, baseline_emissions = base_emissions, policy_year)

# bau grid electrification scenario
bau_grid_combined_total_emissions <- bau_grid_facility_emissions %>% 
  left_join(onsite_facility_emissions, by="facility_id") %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)),
         is_biogenic = replace_na(is_biogenic, FALSE),
         total_co2e_emissions_mt = (onsite_facility_co2e_emissions_mt / 1000) + bau_grid_co2e_emissions_mt, 
         total_so2_emissions_kg = onsite_facility_so2_emissions_kg  + bau_grid_so2_emissions_kg,
         total_nox_emissions_kg = onsite_facility_nox_emissions_kg  + bau_grid_nox_emissions_kg,
         total_pm25_emissions_kg = onsite_facility_pm25_emissions_kg + bau_grid_pm25_emissions_kg) %>% 
  #filter(tech_scenario != "baseline") %>% 
  left_join(baseline_facility_emission, by = c("facility_id", "scenario_rank", "policy_year"))

bau_grid_total_simple <- bau_grid_combined_total_emissions %>% 
  filter(
         policy_year == 2025,
         scenario_rank == 'best') %>% 
  mutate(diff_base_totalco2e = baseline_emissions - total_co2e_emissions_mt) %>% 
  select(facility_id, primary_naics, state, tech_scenario, baseline_emissions, total_co2e_emissions_mt, diff_base_totalco2e)
  
         
# electrified technology, clean grid
clean_grid_combined_total_emissions <- clean_grid_facility_emissions %>% 
  left_join(onsite_facility_emissions, by="facility_id") %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)),
         is_biogenic = replace_na(is_biogenic, FALSE),
         total_co2e_emissions_mt = onsite_facility_co2e_emissions_mt + clean_grid_co2e_emissions_mt, 
         total_so2_emissions_kg = onsite_facility_so2_emissions_kg  + clean_grid_so2_emissions_kg,
         total_nox_emissions_kg = onsite_facility_nox_emissions_kg  + clean_grid_nox_emissions_kg,
         total_pm25_emissions_kg = onsite_facility_pm25_emissions_kg + clean_grid_pm25_emissions_kg) %>% 
  filter(tech_scenario != "baseline") %>% 
  left_join(baseline_facility_emission, by = c("facility_id", "scenario_rank", "policy_year"))


##### FIGURES ######

### BAU Grid Technology comparison figure

avg_fac_bau_tech_df <- bau_grid_combined_total_emissions %>% 
  filter(policy_year == 2025) %>% 
  group_by(facility_id, tech_scenario) %>% 
  summarise(
    bau_grid_co2e_emissions_mt = mean(bau_grid_co2e_emissions_mt, na.rm = TRUE),
    onsite_facility_co2e_emissions_mt = mean(onsite_facility_co2e_emissions_mt, na.rm = TRUE), #come back to this bc getting too many 0s, even under baseline which makes no sense
    is_biogenic = any(is_biogenic, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  filter(tech_scenario != "baseline")

bau_tech_summary <- avg_fac_bau_tech_df %>% 
  group_by(tech_scenario) %>% 
  summarise(
    biogenic_mt = sum(onsite_facility_co2e_emissions_mt * as.numeric(is_biogenic), na.rm = TRUE),
    onsite_nonbiogenic_mt = sum(onsite_facility_co2e_emissions_mt * as.numeric(!is_biogenic), na.rm = TRUE),
    grid_mt = sum(bau_grid_co2e_emissions_mt, na.rm = TRUE),
    .groups = "drop"
  )

# --- 2) Pull in Baseline total from baseline_facility_emission (avg ranks per facility, then sum) ---
baseline_total_mt <- baseline_facility_emission %>%
  filter(policy_year == 2025 | is.na(policy_year)) %>%  # prefer 2025 if present
  group_by(facility_id) %>%
  summarise(baseline_mt = mean(baseline_emissions, na.rm = TRUE), .groups = "drop") %>%
  summarise(total_baseline_mt = sum(baseline_mt, na.rm = TRUE)) %>%
  pull(total_baseline_mt)

# --- 3) Build a long df for plotting: stack tech scenarios + single Baseline bar ---
tech_long <- bau_tech_summary %>%
  pivot_longer(
    cols = c(biogenic_mt, onsite_nonbiogenic_mt, grid_mt),
    names_to = "category",
    values_to = "emissions_mt"
  ) %>%
  mutate(
    category = factor(category,
                      levels = c("biogenic_mt", "onsite_nonbiogenic_mt", "grid_mt"),
                      labels = c("Onsite Biogenic (out of scope)",
                                 "Onsite Non-biogenic (out of scope)",
                                 "Grid emissions")),
    tech_label = tech_scenario
  ) %>%
  select(tech_label, category, emissions_mt)

baseline_long <- tibble(
  tech_label = "Baseline",
  category   = "Baseline total",
  emissions_mt = baseline_total_mt
)

plot_long <- bind_rows(baseline_long, tech_long) %>%
  mutate(
    tech_label = factor(tech_label, levels = c("Baseline", sort(unique(tech_long$tech_label)))),
    emissions_mmton = emissions_mt / 1e6
  )

# --- 4) Plot (Baseline is a single color; techs are stacked) ---
fill_colors <- c("Onsite Biogenic (out of scope)" = "#09847a",
                 "Onsite Non-biogenic (out of scope)" = "#c2e4e6",
                 "Grid emissions" = "#ef5645",
                 "Baseline total" = "#dae6e6")


emissions_tech_plot <- ggplot(plot_long, aes(x = tech_label, y = emissions_mmton, fill = category)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = fill_colors) +
  labs(
    x = "Technology Scenario",
    y = "CO₂e (Million Metric Tons)",
    fill = NULL,
    #title = "CO₂e by Technology Scenario (BAU Grid) with Baseline"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = comma)


#### clean grid figure 

# filter for hp_ee scenario
df_scen <- clean_grid_combined_total_emissions %>% 
  filter(tech_scenario == "hp_boiler_ee")

# average across best/worst scenarios
avg_fac_year <- df_scen %>%
  group_by(facility_id, policy_year) %>%
  summarise(
    clean_grid_co2e_emissions_mt = mean(clean_grid_co2e_emissions_mt, na.rm = TRUE),
    onsite_facility_co2e_emissions_mt = mean(onsite_facility_co2e_emissions_mt, na.rm = TRUE),
    is_biogenic = any(is_biogenic, na.rm = TRUE),
    .groups = "drop"
  )

agg_hp <- avg_fac_year %>%
  group_by(policy_year) %>%
  summarise(
    biogenic_mt = sum(onsite_facility_co2e_emissions_mt * as.numeric(is_biogenic), na.rm = TRUE),
    onsite_nonbiogenic_mt = sum(onsite_facility_co2e_emissions_mt * as.numeric(!is_biogenic), na.rm = TRUE),
    grid_mt = sum(clean_grid_co2e_emissions_mt, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Compute a single baseline total (avoid double-counting across ranks by averaging per facility first)
baseline_total_mt <- baseline_facility_emission %>%
  # we expect baseline_facility_emission to have facility_id, scenario_rank, baseline_emissions, policy_year
  # take the 2025 baseline if multiple years exist, else just average across ranks then sum
  filter(policy_year == 2025 | is.na(policy_year)) %>%   # keep 2025 rows if present
  group_by(facility_id) %>%
  summarise(baseline_mt = mean(baseline_emissions, na.rm = TRUE), .groups = "drop") %>%
  summarise(total_baseline_mt = sum(baseline_mt, na.rm = TRUE)) %>%
  pull(total_baseline_mt)

# 3) Build a long dataframe for the years (stacked categories) and a one-row baseline bar
plot_years_long <- agg_hp %>%
  pivot_longer(cols = c(biogenic_mt, onsite_nonbiogenic_mt, grid_mt),
               names_to = "category", values_to = "co2e_mt") %>%
  mutate(category = factor(category,
                           levels = c("biogenic_mt", "onsite_nonbiogenic_mt", "grid_mt"),
                           labels = c("Onsite Biogenic (out of scope)", "Onsite Non-biogenic (out of scope)", "Grid emissions")),
         label = as.character(policy_year))

baseline_row <- tibble(
  label = "Baseline",
  category = "Baseline total",
  co2e_mt = baseline_total_mt
)

# Combine: baseline first, then years
combined_long <- bind_rows(
  # baseline (single category)
  baseline_row,
  # years' stacked categories (keep label column from pivot)
  plot_years_long %>% select(label, category, co2e_mt)
) %>%
  # ensure x-order: baseline left, then ascending years
  mutate(label = factor(label, levels = c("Baseline", sort(unique(as.character(agg_hp$policy_year))))),
         co2e_mmton = co2e_mt / 1e6)

# 4) Plot — use custom colors: 3 for stacks + 1 for baseline
colors <- c("Onsite Biogenic (out of scope)" = "#09847a",
            "Onsite Non-biogenic (out of scope)" = "#c2e4e6",
            "Grid emissions" = "#ef5645",
            "Baseline total" = "#dae6e6")

clean_grid_emissions_plot <- ggplot(combined_long, aes(x = label, y = co2e_mmton, fill = category)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = colors) +
  labs(x = "", y = "CO₂e (Million Metric Tons)", fill = NULL#,
       #title = "CO₂e Emissions under Clean Grid Scenario",
       #subtitle = "Electrification Scenario: Heat Pump with Energy Efficiency") 
       ) +
  theme_bw(base_size = 13, base_family = "avenir") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

####### CHECKS ###########

sum_by_subsector_baseline_ghgrp <- facility_level_emissions_df %>% 
  filter(tech_scenario == 'baseline') %>% 
  group_by(primary_naics, facility_id) %>% 
  summarise(avg_baseline_co2e_emissions = mean(base_emissions, na.rm = TRUE)) %>% 
  group_by(primary_naics) %>% 
  summarise(baseline_co2e_emissions = sum(avg_baseline_co2e_emissions, na.rm = TRUE))


total_baseline_ghgrp <- sum_by_subsector_baseline_ghgrp %>% 
  summarise(total_baseline_ghgrp_mt = sum(baseline_co2e_emissions, na.rm = TRUE)) %>%
  pull(total_baseline_ghgrp_mt)

