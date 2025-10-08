
#### MN Emissions Figures ####
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

# EMT update to streamline emissions source data

# Set working directory
setwd("~/Documents/Industrial_Decarbonization/Industrial-decarb")

#### Data Wrangling ####
# Pull in results data set
#facility_results_df <- read_excel("state_fact_sheets/data/modified/250801_facility_level_results.xlsx")
state_results_df<- read_excel("state_fact_sheets/data/modified/250804_state_level_results.xlsx")

#### Clean and prepare emissions data ####
# Identify baseline using 'Current Mix'
#### Clean and prepare emissions data ####

emissions_clean <- state_results_df %>%
  filter(
    grid_clean_pct_scenario %in% c("0.8", "1") |
      clean_grid_scenario_label == "Current Grid Mix"
  ) %>%
  mutate(grid_clean_pct_num = suppressWarnings(as.numeric(grid_clean_pct_scenario))) %>%
  select(naics_code, naics_description, tech_scenario, policy_label, 
         grid_clean_pct_scenario, grid_clean_pct_num, clean_grid_scenario_label, 
         grid_emissions_kg_co2, grid_emissions_kg_so2, grid_emissions_kg_nox, grid_emissions_kg_pm25)
  group_by(naics_description, tech_scenario, grid_clean_pct_scenario) %>%
  summarise(
    total_co2_emissions = sum(grid_emissions_kg_co2, na.rm = TRUE) / 1e6,
    .groups = "drop") 



# Set up fill and label mappings
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

#### Generate plot for each grid condition ####
plots <- lapply(emissions_list, function(df) {
  title <- unique(df$grid_mix)
  
  ggplot(df, aes(x = sector, y = point, fill = scenario)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.6) +
    geom_errorbar(
      data = df %>% filter(!is.na(low)),
      aes(ymin = low, ymax = high),
      width = 0.2,
      position = position_dodge(width = 0.8)
    ) +
    labs(
      x = NULL,
      y = ifelse(title == "Current Grid", "Emissions (MtCO2e)", NULL),
      title = title
    ) +
    scale_fill_manual(values = scenario_fill, labels = scenario_labels, name = "Scenario") +
    scale_y_continuous(limits = c(0, 4.5), expand = c(0, 0)) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = ifelse(title == "100% Clean Grid (2040)", c(0.3, 0.95), "none"),
      legend.justification = c(0, 1),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.key.size = unit(0.4, "cm"),
      legend.background = element_rect(color = "black", linewidth = 0.2),
      plot.title = element_text(hjust = 0.5, size = 13),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
    )
})

# Combine plots with patchwork
library(patchwork)
combined_plot <- plots[[1]] + plots[[2]] + plots[[3]]
combined_plot


#### Combine ####

(p_base | p_80 | p_100)

ggsave("state_fact_sheets/outputs/mn_emissions_combo.png", width = 8, height = 5, dpi = 300)

