# State Figures #
## September 15, 2025
## Takes output from lcoh_emissions_policy_scenarios, makes figures

# Version notes: 
## 1.4: using updated copollutant information 


#### SET STATE  ####
state <- "MN"

#### SET-UP ####
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(patchwork)
library(stringr)
library(glue)
library(tidylog)

# pull in the state emissions file & create ordered factor
state_emissions_df <- 
  read_excel(glue("state_fact_sheets/data/modified/state-data/{state}/{state}_emissions_copollutant_test.xlsx")) %>%
  #read_excel(glue("state_fact_sheets/data/modified/state-data/{state}/{state}_emissions_{format(Sys.Date(), '%Y%m%d')}.xlsx")) %>%
  mutate(
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
    )
  ) 

order_levels <- 
  state_emissions_df %>%
  filter(clean_grid_scenario_label == "Current Grid Mix" & tech_scenario == 'BaselineBest' & pollutant_type == 'co2e') %>%
  group_by(industry_clean) %>%
  summarise(total_emissions = sum(total_emissions, na.rm = TRUE)) %>%
  arrange(desc(total_emissions)) %>%
  pull(industry_clean)

state_emissions_df <- 
  state_emissions_df %>%
  mutate(industry_clean = factor(industry_clean, levels = order_levels))
  
# pull in facility LCOH file
facility_lcoh_df <- 
  read_excel(glue("state_fact_sheets/data/modified/state-data/{state}/{state}_lcoh_20250910.xlsx")) %>%
  #read_excel(glue("state_fact_sheets/data/modified/state-data/{state}/{state}_lcoh_{format(Sys.Date(), '%Y%m%d')}.xlsx")) %>%
  mutate(
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
    )) %>%
  filter(!industry_clean %in% c('Pulp & Paper', 'Ethyl Alcohol')) %>%
  mutate(industry_clean = factor(industry_clean, levels = order_levels))

#### EMISSIONS FIGURE SET-UP ####

# Define scenarios and colors
scenario_colors <- c(
  "Baseline"   = "#fb9a99",
  "Scenario1"  = "#1f78b4",
  "Scenario2"  = "#33a02c",
  "Scenario3"  = "#a6cee3",
  "Scenario4"  = "#b2df8a"
)

scenario_labels <- c(
  "Baseline"   = "Baseline",
  "Scenario1"  = "1: E-Boiler",
  "Scenario2"  = "2: ASHP",
  "Scenario3"  = "3: E-Boiler + EE",
  "Scenario4"  = "4: ASHP + EE"
)

# Always order current -> more clean
clean_grid_levels <- c("Current Grid Mix", "50% Cleaner Grid", "100% Cleaner Grid")

# Filter, average best/worst, convert to MtCO₂e
emissions_df <- 
  state_emissions_df %>%
  # Which grid scenarios to highlight? 
  filter(clean_grid_scenario_label %in% clean_grid_levels) %>%
  mutate(
    scenario_base = str_remove(tech_scenario, "Best|Worst"),
    clean_grid_scenario_label = factor(
      clean_grid_scenario_label,
      levels = clean_grid_levels
    )) %>%
  # just going with the best case for now, which also pulls BaselineBest
  filter(str_detect(tech_scenario, 'Best')) %>%
  group_by(industry_clean, clean_grid_scenario_label, scenario_base, pollutant_type) %>%
  summarise(
    total_emissions = sum(total_emissions)) %>%
  ungroup() %>%
  mutate(emissions_Mt = total_emissions/1000000) # for CO2e

#### EMISSIONS FIGURES ####
co2e_plot <- 
  emissions_df |>
  filter(pollutant_type == 'co2e') |>
  ggplot(aes(x = industry_clean, y = emissions_Mt, fill = scenario_base)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  facet_wrap(~ clean_grid_scenario_label, nrow = 1) +
  labs(
    x = NULL,
    y = "GHG Emissions (Mt CO2e)",
    fill = "Tech Scenario"
  ) +
  scale_fill_manual(values = scenario_colors, labels = scenario_labels) +
  scale_y_continuous(limits = c(0, max(emissions_df$emissions_Mt) * 1.1), expand = c(0, 0)) + # dynamic limit
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

so2_plot <- 
  emissions_df |>
  filter(pollutant_type == 'so2') |>
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

nox_plot <- 
  emissions_df |>
  filter(pollutant_type == 'nox') |>
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

pm25_plot <- 
  emissions_df |>
  filter(pollutant_type == 'pm25') |>
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


co2e_plot
so2_plot
nox_plot
pm25_plot

# --------- LCOH by Technology Scenario Figure --------------
# Define sector colors
sector_colors <- c(
  "Pulp & Paper" = "#6d7d33",
  "Beet Sugar" = "#ef5645",
  "Ethyl Alcohol" = "#2CA02C", 
  "Fats & Oils" = "#047c91", 
  "Soybeans" = "#c9bf9d", 
  "Spices" = "#9370DB", 
  "Meat (non-poultry)" = "#8B0000", 
  "Distilleries" = "#D2691E", 
  "Rendering" = "#8C564B",  
  "Wet Corn Milling"    = "#febc11"   
)

ng_min <- min(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineBest'])
ng_max <- max(facility_lcoh_df$lcoh[facility_lcoh_df$tech_scenario == 'BaselineWorst'])

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
  aes(x = scenario_label, y = lcoh, color = industry_clean)) +
  
  # Natural gas range display 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_min, ymax = ng_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_min, linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = ng_max, linetype = "dotted", color = "black", size = 0.5) +
  # annotate("text", x = -Inf, y = Inf, label = "LCOH range of a natural gas boiler",
  #          hjust = -0.1, vjust = 46.5, size = 3, fontface = 'italic') +
  
  # add boxplot 
  geom_boxplot(outlier.shape = NA, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = sector_colors) +
  scale_y_continuous(limits = c(5, 30)) +
  
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

# --------- SCENARIO 4 LCOH POLICY FIGURE -------------
# configure policy models
capex_levels <- c(0.3, 0.5, 1)
elec_levels <- c(0.25, 0.5)

policy_grid <- 
  crossing(
  capex_subsidy = capex_levels,
  elec_discount = elec_levels
) 

scenario4_policy_df <- 
  facility_lcoh_df %>%
  inner_join(policy_grid, by = c("capex_subsidy", "elec_discount")) 

scenario4_no_policy_df <- 
  facility_lcoh_df %>% 
  filter(policy_label == "No Policy")

scenario4_df <- 
  rbind(scenario4_no_policy_df, scenario4_policy_df) %>% 
  filter(str_detect(tech_scenario, "Scenario4")) %>%
  mutate(
    policy_label = factor(policy_label, levels = c(
      "No Policy",
      "Capex: -30%, Elec: -25%",
      "Capex: -50%, Elec: -25%",
      "Capex: -100%, Elec: -25%",
      "Capex: -30%, Elec: -50%",
      "Capex: -50%, Elec: -50%",
      "Capex: -100%, Elec: -50%"
    )),
  )

# Policy scenario x sector plot (S4 only) 
lcoh_policy_combined_plot <- 
  ggplot(scenario4_df,
  aes(x = policy_label, y = lcoh, color = industry_clean)) +
  
  # NG baseline band
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_min, ymax = ng_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_min, linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = ng_max, linetype = "dotted", color = "black", size = 0.5) +
  # annotate("text", x = -Inf, y = Inf, label = "LCOH range of a natural gas boiler",
  #          hjust = -0.1, vjust = 35.5, size = 3, fontface = 'italic') +
  
  # Boxplots
  geom_boxplot(outlier.shape = NA, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = sector_colors) +
  scale_y_continuous(limits = c(5, 30)) +
  
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

tech_colors <- c(
  "ASHP"      = "#CC79A7", 
  "E-Boiler"  = "#56B4E9",
  "NG Boiler" = "#4D4D4D"  
)

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

# --------- SAVE PLOTS ----------
ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{state}/{state}_co2e_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       co2e_plot,
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{state}/{state}_so2_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       so2_plot,
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{state}/{state}_nox_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       nox_plot,
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{state}/{state}_pm25_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       pm25_plot,
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{state}/{state}_LCOH_technology_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       technology_by_sector_lcoh_plot, 
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{state}/{state}_LCOH_policy_scenario4_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       lcoh_policy_combined_plot, 
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{state}/{state}_capex_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       capex_plot, 
       width = 8, height = 5, dpi = 300)



