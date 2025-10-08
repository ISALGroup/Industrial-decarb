# Wisconsin Data Work & Figures #
## September 17, 2025

#### SET-UP ####
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

# Tech scenario input 
tech_input_df <- 
  read_excel('LCOH modelling/output/longform_WI.xlsx') %>%
  mutate(facility_name = tolower(facility_name), 
         base_emissions_co2e = elec_ghg_emissions + noelec_ghg_emissions) %>% 
  # rename(
  #   base_emissions_nox = `base_emissions_Nitrogen Oxides`,
  #   base_emissions_pm25 = `base_emissions_PM2.5 Primary (Filt + Cond)`, 
  #   base_emissions_so2 = `base_emissions_Sulfur Dioxide`, 
  #   so2_emissions = sox_emissions
  # ) %>%
  select(-1, -opex) 

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

## how many facilities are in each group? 
# tech_combined_df |>
#   group_by(sector) |>
#   summarise(n_facilities = n_distinct(facility_id))

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

# Import parameters 
param <- 
  read_csv('state_fact_sheets/data/parameters.csv') %>%
  filter(state == 'WI')

# --- Create Policy Grid ---
policy_grid <- expand.grid(
  capex_subsidy = seq(0.0, 1.0, by = 0.1),
  elec_discount = seq(0.0, 1.0, by = 0.25)
)

# --- Apply Policy Grid to Tech Scenarios ---
facility_lcoh_df <- 
  tidyr::crossing(tech_combined_df, policy_grid) %>%
  mutate(
    lcoh = lcoh_func(
      param$r, 
      param$elec_price,
      param$ng_price,
      param$t, 
      param$ngboiler_om_best, 
      param$ngboiler_om_worst, 
      param$eboiler_om_best, 
      param$eboiler_om_worst, 
      param$hthp_om_best, 
      param$hthp_om_worst, 
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
      naics_description == 'Rendering and Meat Byproduct Processing' ~ 'Rendering' 
    )) #|>
  #mutate(industry_clean = factor(industry_clean, levels = order_levels))


ng_min <- min(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineBest'])
ng_max <- max(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineWorst'])

sector_colors <- c(
  "Pulp & Paper" = "#6d7d33",
  "Ethanol+" = "#febc11", 
  "Food & Beverage" = "#8B0000"
)

#### SCENARIO 4 LCOH FIGURE  #####

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
    policy_label = factor(policy_label, levels = fig_policies),
    sector = if_else(sector == 'Chemicals', "Ethanol+", sector)
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
  scale_y_continuous(limits = c(5, 21)) +
  
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

#### LCOH v. ELECTRICITY ####

x_vals <- seq(0, 0.1, length.out = 200)

elec_plot_df <- 
  facility_lcoh_df |>
  # Filter to no policy support, Scenario4 outcomes
  filter(str_detect(tech_scenario, "Scenario4"), 
         policy_label == 'No Policy') |>
  
  # Summarize at the sector-scenario level. 
  group_by(#sector, 
           tech_scenario) |>
  summarize(
    sector = min(sector), 
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
      param$ngboiler_om_best, 
      param$ngboiler_om_worst, 
      param$eboiler_om_best, 
      param$eboiler_om_worst, 
      param$hthp_om_best, 
      param$hthp_om_worst, 
      ## tech scenario + calculations 
      tech_scenario,
      capex,
      heat_mmbtu,
      change_in_electricity_demand_kwh,
      ## policy scenarios
      capex_subsidy, 
      elec_discount 
      ), 
    sector = if_else(sector == 'Chemicals', "Ethanol+", sector), 
    x = x*100
    ) |>
  # average across best and worst case scenarios to get the sector-level outcome
  group_by(sector, x) |>
  summarize(
    lcoh = mean(lcoh)
  )
  
lcoh_v_elec_plot <- 
  ggplot(elec_plot_df, aes(x = x, y = lcoh, color = sector)) +
  geom_line(size = 1) +
  
  # NG baseline 
  geom_hline(yintercept = ng_max, 
             linetype = "longdash", 
             color = "grey50", 
             size = 0.5) +
  # annotate("text",
  #          label = "LCOH of a natural gas boiler (max)",
  #          x = .01, y = ng_max + .5,
  #          size = 3,
  #          fontface = 'italic') +
  
  geom_vline(xintercept = param$elec_price,
             linetype = "solid",
             color = "grey50",
             size = 0.5) +
  # annotate("text",
  #          label = "WI Price",
  #          x = param$elec_price + .0035, y = 5,
  #          size = 3,
  #          fontface = 'italic') +

  geom_vline(xintercept = 0.041,
             linetype = "dotted",
             color = sector_colors[1],
             size = 0.75) +
  # annotate("text",
  #          label = "Cost Parity Price (Pulp & Paper)",
  #          x = 0.0405 + .012, y = 5,
  #          size = 3,
  #          fontface = 'italic') +
  
  geom_vline(xintercept = 0.039,
             linetype = "dotted",
             color = sector_colors[2],
             size = 0.75) +
  
  geom_vline(xintercept = 0.034,
             linetype = "dotted",
             color = sector_colors[3],
             size = 0.75) +

  scale_color_manual(values = sector_colors) +
  scale_x_reverse() +
  
  labs(
    y = "Levelized Cost of Heat ($/MMBtu)",
    x = "Cost of Electricity ($/kWH)",
    color = "Sector") +
  
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

ggsave("state_fact_sheets/outputs/state-fact-sheet-figures/WI/WI_LCOHvElec.png",
       lcoh_v_elec_plot,
       width = 8, height = 5, dpi = 300)

#### LCOH V ELEC V2 ####

lcoh_v_elec_plot_v2 <- 
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
  geom_vline(xintercept = 0.0405 * 100, color = "#FFBF00", linetype = "longdash", size = 0.75) +
  
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

ggsave("state_fact_sheets/outputs/state-fact-sheet-figures/WI/WI_LCOHvElec_v2.png",
       lcoh_v_elec_plot_v2,
       width = 8, height = 5, dpi = 300)





#### EMISSIONS FIGURE ####

emissions_func <- 
  function(
    # From emissions dataset
    facility_emissions, # facility emissions
    grid_emissions_kg_kwh, # kg of emissions per kwh
    change_in_electricity_demand_kwh, # how much electricity demand changes under electrification
    
    # User input
    grid_clean_pct_scenario # What % cleaner is the grid? should take a value from 0-1
  ) {
    
    # Scale the grid emissions (kg per kwh) to be more clean (or the same, depending on what the user selects). 
    scaled_grid.e_factor <- grid_emissions_kg_kwh * (1-grid_clean_pct_scenario)
    
    
    # emissions = the increase in emissions from the grid from electricity demand + non-electrifiable emissions at the facility 
    total_emissions_t = (change_in_electricity_demand_kwh * scaled_grid.e_factor) / 1000 + facility_emissions
    
    total_emissions_t
  }

#### WISCONSIN EMISSIONS IN MONEY ####
fig_policies <- c(
  'No Policy',
  'Capex: -100%, Elec: -0%', 
  'Capex: -0%, Elec: -25%', 
  'Capex: -0%, Elec: -50%', 
  'Capex: -100%, Elec: -50%'
)

lcoh_tech_base <- 
  facility_lcoh_df %>%
  filter(
    policy_label == "No Policy", 
    tech_scenario %in% c('BaselineWorst', 'BaselineBest')
  ) %>%
  group_by(state, tech_scenario) %>%
  summarize(
    lcoh = mean(lcoh, na.rm = TRUE)
  ) %>%
  mutate(
    scenario_clean = "Baseline (NG)", 
    scenario_rank = str_extract(tech_scenario, "Best|Worst")
  ) %>%
  rename(
    industry_ordered = scenario_clean
  ) 

WI_eim_df <- 
  facility_lcoh_df %>%
  filter(
    !tech_scenario %in% c('BaselineWorst', 'BaselineBest'), 
    state == 'WI'
  ) %>%
  left_join(
    lcoh_tech_base |> 
      filter(scenario_rank == 'Worst') |>
      rename(lcoh_ng = lcoh) |>
      select(state, lcoh_ng), 
    by = 'state') %>%
  mutate(
    in_money = if_else(lcoh < lcoh_ng, 1, 0), 
    sector = if_else(sector == 'Chemicals', "Ethanol+", sector), 
    sector = factor(sector, levels = c('Ethanol+', 'Pulp & Paper', 'Food & Beverage'))
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

WI_eim_plot <- 
  ggplot(WI_eim_df,
         aes(x = sector, y = eim_prop, fill = policy_label)) +
  
  # add boxplot 
  geom_col(position = position_dodge(width = 0.8, preserve = "single"), width = 0.6) + 
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  labs( x = NULL, y = "% of Electrifiable Emissions 'in the Money'", fill = "Policy" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.7, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

