# August 6. 2025
# EMT
# Figures for state memos

# pulling code from other files to get all 4 figures in 1 file
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(patchwork)
library(stringr)

# Set working directory
#setwd("~/Documents/Industrial_Decarbonization/Industrial-decarb")

# ------ Pull in LCOH Policy and Emissions Results Data -------
# pull in facility level file
facility_level_df <- read_excel("state_fact_sheets/data/modified/state-data/MN/250815_facility_lcoh_results_mn.xlsx") 

# pull in the state emissions file
state_emissions_df <- read_excel("state_fact_sheets/data/modified/state-data/MN/250815_state_emissions_results_mn_v3.xlsx") 
#state_lcoh_df <- read_excel("state_fact_sheets/data/modified/state-data/MN/250812_state_lcoh_results_mn.xlsx") didn't use

# --------- EMISSIONS FIGURE -----------

# configure state clean electricity targets
clean_targets <- c("Current Grid Mix", 0.8, 1)

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

# Filter, average best/worst, convert to MtCO₂e
emissions_df <- state_emissions_df %>%
  filter(clean_grid_scenario_label %in% c("Current Grid Mix", "80% Clean Grid", "100% Clean Grid")) %>%
  mutate(
    scenario_base = str_remove(tech_scenario, "Best|Worst"),
    clean_grid_scenario_label = factor(
      clean_grid_scenario_label,
      levels = c("Current Grid Mix", "80% Clean Grid", "100% Clean Grid")
    ), 
    industry_clean = case_when(
      naics_description == 'Beet Sugar Manufacturing' ~ 'Beet Sugar', 
      naics_description == 'Ethyl Alcohol Manufacturing' ~ 'Ethyl Alcohol', 
      naics_description == 'Fats and Oils Refining and Blending' ~ 'Fats & Oils', 
      naics_description == 'Paper Mills' ~ 'Pulp & Paper', 
      naics_description == 'Paperboard Mills' ~ 'Pulp & Paper',
      naics_description == 'Soybean and Other Oilseed Processing' ~ 'Soybeans'
    ), 
    industry_clean = factor(industry_clean,
                            levels = c("Pulp & Paper", "Ethyl Alcohol", "Beet Sugar", "Soybeans", "Fats & Oils"))
  ) %>%
  # just going with the best case for now 
  filter(str_detect(tech_scenario, 'Best')|tech_scenario == 'Baseline') %>%
  group_by(industry_clean, clean_grid_scenario_label, scenario_base) %>%
  summarise(emissions_Mt = sum(emissions_total_t_co2e)/1000000) 

emissions_plot <- ggplot(emissions_df, aes(x = industry_clean, y = emissions_Mt, fill = scenario_base)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  facet_wrap(~ clean_grid_scenario_label, nrow = 1) +
  labs(
    x = NULL,
    y = "Emissions (MtCO2e)",
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

emissions_plot

# ------------ LCOH by Technology Scenario Figure --------------
# Define sector colors
sector_colors <- c(
  "Pulp & Paper" = "#6d7d33",
  "Beet Sugar" = "#ef5645",
  "Ethyl Alcohol" = "#febc11", 
  "Fats & Oils" = "#047c91", 
  "Soybeans" = "#c9bf9d"
)

ng_min <- 7
ng_max <- 8.5

# Prepare data for candlestick
lcoh_tech_sector <- facility_level_df %>%
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
    ),
    industry_clean = case_when(
      naics_description == 'Beet Sugar Manufacturing' ~ 'Beet Sugar', 
      naics_description == 'Ethyl Alcohol Manufacturing' ~ 'Ethyl Alcohol', 
      naics_description == 'Fats and Oils Refining and Blending' ~ 'Fats & Oils', 
      naics_description == 'Paper Mills' ~ 'Pulp & Paper',
      naics_description == 'Paperboard Mills' ~ 'Pulp & Paper',
      naics_description == 'Soybean and Other Oilseed Processing' ~ 'Soybeans'
    ), 
    industry_clean = factor(industry_clean,
                            levels = c("Ethyl Alcohol", "Pulp & Paper", "Beet Sugar", "Soybeans", "Fats & Oils"))
  ) 

# Technology scenario x sector plot 
technology_by_sector_lcoh_plot <- ggplot(
  lcoh_tech_sector,
  aes(x = scenario_label, y = lcoh, color = industry_clean)) +
  
  # Natural gas range display 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_min, ymax = ng_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_min, linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = ng_max, linetype = "dotted", color = "black", size = 0.5) +
  annotate("text", x = -Inf, y = Inf, label = "LCOH range of a natural gas boiler",
           hjust = -0.1, vjust = 46.5, size = 3, fontface = 'italic') +
  
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

# ----------- SCENARIO 4 LCOH POLICY MODELING -------------
# configure policy models
capex_levels <- c(0.3, 0.5, 1)
elec_levels <- c(0.25, 0.5)

policy_grid <- crossing(
  capex_subsidy = capex_levels,
  elec_discount = elec_levels
) 

scenario4_policy_df <- facility_level_df %>%
  inner_join(policy_grid, by = c("capex_subsidy", "elec_discount")) 

scenario4_no_policy_df <- facility_level_df %>% 
  filter(policy_label == "No Policy")

scenario4_df <- rbind(scenario4_no_policy_df, scenario4_policy_df) %>% 
  filter(str_detect(tech_scenario, "Scenario4")) %>%
  mutate(
    industry_clean = case_when(
      naics_description == 'Beet Sugar Manufacturing' ~ 'Beet Sugar', 
      naics_description == 'Ethyl Alcohol Manufacturing' ~ 'Ethyl Alcohol', 
      naics_description == 'Fats and Oils Refining and Blending' ~ 'Fats & Oils', 
      naics_description == 'Paper Mills' ~ 'Pulp & Paper', 
      naics_description == 'Paperboard Mills' ~ 'Pulp & Paper',
      naics_description == 'Soybean and Other Oilseed Processing' ~ 'Soybeans'
    ), 
    industry_clean = factor(industry_clean,
                            levels = c("Ethyl Alcohol", "Pulp & Paper", "Beet Sugar", "Soybeans", "Fats & Oils")),
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
lcoh_policy_combined_plot <- ggplot(
  scenario4_df,
  aes(x = policy_label, y = lcoh, color = industry_clean)) +
  
  # NG baseline band
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_min, ymax = ng_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_min, linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = ng_max, linetype = "dotted", color = "black", size = 0.5) +
  annotate("text", x = -Inf, y = Inf, label = "LCOH range of a natural gas boiler",
           hjust = -0.1, vjust = 35.5, size = 3, fontface = 'italic') +
  
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
    legend.position = c(0.8, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

lcoh_policy_combined_plot


# ---------- SAVE PLOTS ----------
ggsave("state_fact_sheets/outputs/state-fact-sheet-figures/MN/mn_emissions_plot_v2.png",
       emissions_plot, 
       width = 8, height = 5, dpi = 300)

ggsave("state_fact_sheets/outputs/state-fact-sheet-figures/MN/mn_LCOH_technology_plot_v2.png",
       technology_by_sector_lcoh_plot, 
       width = 8, height = 5, dpi = 300)

ggsave("state_fact_sheets/outputs/state-fact-sheet-figures/MN/mn_LCOH_policy_scenario4_plot_v2.png",
       lcoh_policy_combined_plot, 
       width = 8, height = 5, dpi = 300)





