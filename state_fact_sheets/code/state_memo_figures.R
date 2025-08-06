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
setwd("~/Documents/Industrial_Decarbonization/Industrial-decarb")

# ------ Pull in LCOH Policy and Emissions Results Data -------
# pull in the state file
results_df <- read_excel("state_fact_sheets/data/modified/250806_state_level_results.xlsx") 

# configure state clean electricity targets
clean_targets <- c("Current Mix", 0.8, 1)
# --------- EMISSIONS FIGURE -----------

emissions_df <- results_df %>% 
  filter(grid_clean_pct_scenario %in% clean_targets) %>% 
  select(naics_code, naics_description, sector, tech_scenario, 
         clean_grid_scenario_label, emissions_total_mt_co2e)
# Define fill colors and labels
scenario_fill <- c(
  "baseline" = "#fb9a99",
  "scenario1" = "#1f78b4",
  "scenario2" = "#33a02c",
  "scenario3" = "#a6cee3",
  "scenario4" = "#b2df8a"
)

scenario_labels <- c(
  "baseline" = "Baseline",
  "scenario1" = "1: E-Boiler",
  "scenario2" = "2: ASHP",
  "scenario3" = "3: E-Boiler + EE",
  "scenario4" = "4: ASHP + EE"
)

# Prepare summarized emissions with min and max
emissions_summary <- emissions_df %>%
  mutate(
    scenario = case_when(
      tech_scenario == "Baseline" ~ "baseline",
      grepl("^scenario_1", tech_scenario) ~ "scenario1",
      grepl("^scenario_2", tech_scenario) ~ "scenario2",
      grepl("^scenario_3", tech_scenario) ~ "scenario3",
      grepl("^scenario_4", tech_scenario) ~ "scenario4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(sector, scenario, clean_grid_scenario_label) %>%
  summarise(
    point = mean(emissions_total_mt_co2e, na.rm = TRUE),
    low = min(emissions_total_mt_co2e, na.rm = TRUE),
    high = max(emissions_total_mt_co2e, na.rm = TRUE),
    .groups = "drop"
  )

# Split by grid scenario
emissions_list <- split(emissions_summary, emissions_summary$clean_grid_scenario_label)

# Get shared y-axis limit across all plots
ymax <- max(sapply(emissions_list, function(df) max(df$point, na.rm = TRUE)))

# Create one plot per grid scenario
plots <- lapply(seq_along(emissions_list), function(i) {
  df <- emissions_list[[i]]
  title <- unique(df$clean_grid_scenario_label)
  
  y_lab <- if (i == 1) "Emissions (MtCO2e)" else NULL
  
  ggplot(df, aes(x = sector, y = point, fill = scenario)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.6) +
    labs(
      x = NULL,
      y = y_lab,
      title = title
    ) +
    scale_fill_manual(values = scenario_fill, labels = scenario_labels, name = "Scenario") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, ymax)) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = if (title == "100% Clean Grid") c(0.3, 0.95) else "none",
      legend.justification = c(0, 1),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.key.size = unit(0.4, "cm"),
      legend.background = element_rect(color = "black", linewidth = 0.2),
      plot.title = element_text(hjust = 0.5, size = 13),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
    )
})

# Use patchwork to combine all plots
library(patchwork)
wrap_plots(plots, ncol = 2)



# Combine selected plots (e.g., first 3)
combined_plot <- wrap_plots(plots[1:3])
combined_plot

# ------------ LCOH by Technology Scenario Figure --------------
# Define sector colors
sector_colors <- c(
  "Paper Mills" = "#6d7d33",
  "Beet Sugar" = "#047c91",
  "Ethyl Alcohol" = "#febc11"
)

ng_min <- 7.5
ng_max <- 8

# Filter to no policy, non-baseline tech scenarios
lcoh_tech_candle <- results_df %>%
  filter(policy_label == "No Policy", !str_detect(tech_scenario, "Baseline")) %>%
  mutate(
    scenario_number = str_extract(tech_scenario, "scenario_\\d") %>% str_replace("scenario_", "") %>% as.integer(),
    scenario_label = factor(scenario_number, levels = 1:4,
                            labels = c("E-Boiler", "E-Boiler + EE", "Air-Source HP", "Air-Source HP + EE")),
    industry_clean = str_replace(naics_description, " Manufacturing", "")
  ) %>%
  group_by(industry_clean, scenario_label) %>%
  summarise(
    LCOH_min = min(lcoh, na.rm = TRUE),
    LCOH_max = max(lcoh, na.rm = TRUE),
    .groups = "drop"
  )


# Candlestick plot
technology_by_sector_lcoh_plot <- ggplot(lcoh_tech_candle, aes(x = scenario_label, ymin = LCOH_min, ymax = LCOH_max, color = industry_clean)) +
  
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ng_min, ymax = ng_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_min, linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = ng_max, linetype = "dotted", color = "black", size = 0.5) +
  annotate("text", x = -Inf, y = Inf, label = "LCOH range of a natural gas boiler",
           hjust = -0.1, vjust = 46.5, size = 3, fontface = 'italic') +
  
  geom_linerange(aes(group = industry_clean), linewidth = 4,
                 position = position_dodge(width = 0.4)) +
  
  scale_color_manual(values = sector_colors) +
  scale_y_continuous(limits = c(5, 30)) +
  
  labs(
    x = NULL,
    y = "Levelized Cost of Heat ($/MMBtu)",
    color = "Sector"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.8, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

technology_by_sector_lcoh_plot

# ----------- SCENARIO 4 LCOH POLICY MODELING -------------
# configure policy models
capex_levels <- c(0, 0.5, 1)
elec_levels <- c(0, 0.2, 0.7)

policy_grid <- crossing(
  capex_subsidy = capex_levels,
  elec_discount = elec_levels
) 

results_filtered <- results_df %>%
  inner_join(policy_grid, by = c("capex_subsidy", "elec_discount")) %>%
  mutate(policy_label = factor(policy_label, levels = unique(policy_label)))

# Filter for Scenario 4 (Air-Source HP + EE)
scenario4_df <- results_filtered %>%
  filter(str_detect(tech_scenario, "scenario_4")) %>%
  mutate(
    industry_clean = str_replace(naics_description, " Manufacturing", "")
  )

# Create the plot
lcoh_policy_combined_plot <- ggplot(scenario4_df, aes(x = policy_label, y = lcoh, fill = industry_clean)) +
  
  # NG baseline band
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = ng_min, ymax = ng_max,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = ng_min,
             linetype = "dotted",
             color = "black",
             size = 0.5) +
  geom_hline(yintercept = ng_max,
             linetype = "dotted",
             color = "black",
             size = 0.5) +
  
  # Boxplots
  geom_boxplot(outlier.shape = NA, width = 0.3, position = position_dodge(width = 0.5)) +
  
  scale_fill_manual(values = sector_colors) +
  
  labs(
    y = "Levelized Cost of Heat ($/MMBtu)",
    x = NULL,
    fill = "Sector"
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
# this isn't quite right yet but logging off for now....



