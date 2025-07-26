library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(stringr)
library(grid)
library(janitor)
library(tidyverse)
library(ggbeeswarm)

##### ORGANIZE EXCEL MODEL OUTPUT #######
# Load data
lcoh_raw_df <- read_excel("state_fact_sheets/data/raw/MN_lcoh_facility.xlsx")

# Reshape to long format
lcoh_long <- lcoh_raw_df %>%
  pivot_longer(
    cols = starts_with("scenario"),
    names_to = "scenario",
    values_to = "LCOH"
  ) %>%
  mutate(
    case = case_when(
      str_detect(scenario, "best") ~ "best",
      str_detect(scenario, "worst") ~ "worst"
    ),
    scenario_clean = str_remove(scenario, "_best|_worst"),
    
    # Extract policy labels
    policy_label = case_when(
      str_detect(scenario_clean, "30_cost_share") ~ "30% Capex\nShare",
      str_detect(scenario_clean, "50_cost_share") ~ "50% Capex\nShare",
      str_detect(scenario_clean, "100_cost_share") ~ "100% Capex\nShare",
      str_detect(scenario_clean, "low_rate") ~ "Low Elec.\nRate",
      str_detect(scenario_clean, "100_low") ~ "100% Capex + \nLow Rate",
      str_detect(scenario_clean, "no_policy") ~ "No Policy",
      str_detect(scenario_clean, "natural_gas") ~ "Baseline"
    ),
    
    # Extract scenario labels
    scenario_label = case_when(
      str_detect(scenario_clean, "scenario_1") ~ "Scenario 1",
      str_detect(scenario_clean, "scenario_2") ~ "Scenario 2",
      str_detect(scenario_clean, "scenario_3") ~ "Scenario 3",
      str_detect(scenario_clean, "scenario_4") ~ "Scenario 4",
      str_detect(scenario_clean, "natural_gas") ~ "Baseline"
    )
  )


# Create a single label combining tech and policy for easier reading
lcoh_long <- lcoh_long %>%
  mutate(scenario_policy_label = ifelse(
    scenario_label == "Baseline", 
    "Baseline", 
    paste(scenario_label, "-", policy_label)
  ))

# Filter to best and worst average
lcoh_avg <- lcoh_long %>%
  filter(scenario_label != "Baseline") %>%
  group_by(plant_name, industry, scenario_label, policy_label) %>%
  summarise(LCOH_avg = mean(LCOH), .groups = "drop") %>%
  mutate(scenario_policy_label = paste(scenario_label, "-", policy_label))

# Get natural gas baseline by plant
ng_baseline <- lcoh_long %>%
  filter(scenario_label == "Baseline") %>%
  group_by(plant_name, industry) %>%
  summarise(NG_LCOH = mean(LCOH), .groups = "drop")

# Define sector colors
sector_colors <- c(
  "Paper Mills" = "#6d7d33",
  "Beet Sugar" = "#047c91",
  "Ethyl Alcohol" = "#febc11"
)


#### FIGURE: TECHNOLOGIES BY SECTOR (no policy) ####
# Create lcoh_candle from lcoh_long
lcoh_tech_candle <- lcoh_long %>%
  filter(policy_label == "No Policy", scenario_label != "Baseline") %>%
  mutate(scenario_label = factor(scenario_label, 
                                 levels = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"),
                                 labels = c("E-Boiler", "E-Boiler + EE", "Air-Source HP", "Air-Source HP + EE"))) %>%
  group_by(industry, scenario_label, case) %>%
  summarise(LCOH = mean(LCOH), .groups = "drop") %>%
  pivot_wider(
    names_from = case,
    values_from = LCOH,
    names_prefix = "LCOH_"
  ) %>%
  rename(
    LCOH_min = LCOH_best,
    LCOH_max = LCOH_worst
  ) %>% 
  mutate(industry_clean = str_replace(industry, " Manufacturing", ""))

ng_band <- lcoh_long %>%
  filter(scenario_label == "Baseline") %>%
  summarise(
    ng_min = min(LCOH),
    ng_max = max(LCOH)
  )

# Create the candlestick plot
technology_by_sector_lcoh_plot <- ggplot(lcoh_tech_candle, aes(x = scenario_label, ymin = LCOH_min, ymax = LCOH_max, color = industry_clean)) +

  # Add grey band for natural gas baseline
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = ng_band$ng_min, ymax = ng_band$ng_max,
           fill = "grey90", alpha = 0.3) +
  
  geom_hline(yintercept = ng_band$ng_min, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  geom_hline(yintercept = ng_band$ng_max, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  
  annotate("text",
           x = -Inf, y = Inf, 
           label = "LCOH range of a natural gas boiler",
           hjust = -0.1, vjust = 46.5,
           size = 3,
           fontface = 'italic') +
  
  scale_y_continuous(limits = c(5, 30))  +
  
  geom_linerange(
    aes(group = industry),
    linewidth = 4,
    position = position_dodge(width = 0.4)
  ) +
  
  scale_color_manual(values = sector_colors) +
  
  labs(
    #title = "LCOH by Sector and Technology Scenario",
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
    legend.background = element_rect(
      color = "black",   
      linewidth = 0.2    
    )
  )

print(technology_by_sector_lcoh_plot)

# Save
ggsave("state_fact_sheets/outputs/mn_sector_technology_lcoh3.png", technology_by_sector_lcoh_plot, width = 8, height = 5, dpi = 300)


#### FIGURE: TECHNOLOGIES BY SECTOR W/ POLICY ####
# Create lcoh_candle from lcoh_long
lcoh_tech_candle <- lcoh_long %>%
  filter(scenario_label == "Scenario 4") %>%
  pivot_wider(
    names_from = case,
    values_from = LCOH,
    names_prefix = "LCOH_"
  ) %>%
  rename(
    LCOH_min = LCOH_best,
    LCOH_max = LCOH_worst
  ) %>% 
  group_by(industry, policy_label) %>%
  summarise(LCOH_min = min(LCOH_min, na.rm = TRUE),
            LCOH_max = max(LCOH_max, na.rm = TRUE), 
            .groups = "drop") %>%
  mutate(industry_clean = str_replace(industry, " Manufacturing", ""))

ng_band <- lcoh_long %>%
  filter(scenario_label == "Baseline") %>%
  summarise(
    ng_min = min(LCOH),
    ng_max = max(LCOH)
  )

policy_order <- c("No Policy", "30% Capex\nShare", "50% Capex\nShare", "100% Capex\nShare", "Low Elec.\nRate", "100% Capex + \nLow Rate")

# Create the candlestick plot
sector_policy_lcoh_plot <- ggplot(lcoh_tech_candle, aes(x = factor(policy_label, levels = policy_order), ymin = LCOH_min, ymax = LCOH_max, color = industry_clean)) +
  
  # Add grey band for natural gas baseline
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = ng_band$ng_min, ymax = ng_band$ng_max,
           fill = "grey90", alpha = 0.3) +
  
  geom_hline(yintercept = ng_band$ng_min, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  geom_hline(yintercept = ng_band$ng_max, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  
  annotate("text",
           x = -Inf, y = Inf, 
           label = "LCOH range of a natural gas boiler",
           hjust = -0.1, vjust = 45,
           size = 3,
           fontface = 'italic') +
  
  scale_y_continuous(limits = c(5, 30))  +
  
  geom_linerange(
    aes(group = industry),
    linewidth = 4,
    position = position_dodge(width = 0.4)
  ) +
  
  scale_color_manual(values = sector_colors) +
  
  labs(
    #title = "LCOH by Sector and Technology Scenario",
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
    legend.background = element_rect(
      color = "black",   
      linewidth = 0.2    
    )
  )

print(sector_policy_lcoh_plot)

# Save
#ggsave("state_fact_sheets/outputs/mn_sector_policy_lcoh5.png", sector_policy_lcoh_plot, width = 8, height = 5, dpi = 300)


#### FIGURE: LCOH Policy Modeling Scenario 4 ONLY #####

# Filter to Scenario 4 (electrification scenario of interest)
scenario4_avg <- lcoh_long %>%
  filter(scenario_label == "Scenario 4") %>%
  group_by(plant_name, industry, policy_label) %>%
  summarise(LCOH_avg = mean(LCOH), .groups = "drop") %>%
  mutate(industry_clean = str_replace(industry, " Manufacturing", ""))

# Calculate average natural gas LCOH across all facilities
ng_avg <- lcoh_long %>%
  filter(scenario_label == "Baseline") %>%
  summarise(NG_LCOH_avg = mean(LCOH)) %>%
  pull(NG_LCOH_avg)

# Define the policy order
policy_order <- c("No Policy", "30% Capex\nShare", "50% Capex\nShare", "100% Capex\nShare", "Low Elec.\nRate", "100% Capex + \nLow Rate")

# Plot
policy_lcoh_plot <- ggplot(scenario4_avg, aes(x = factor(policy_label, levels = policy_order), 
                          y = LCOH_avg, color = industry_clean)) +
  
  # Natural gas baseline line
  geom_hline(yintercept = ng_avg, linetype = "dashed", color = "grey30", linewidth = 1) +
  
  annotation_custom(
    grob = textGrob(label = "Avg. NG\nLCOH", hjust = 0,
                    gp = gpar(col = "grey30", fontsize = 9, fontface = "italic")),
    ymin = ng_avg,      # Vertical position of the textGrob
    ymax = ng_avg,
    xmin = 5.65,         # Note: The grobs are positioned outside the plot area
    xmax = 5.65) + 
  coord_cartesian(clip = "off") +
  
  # Points with jitter to spread facilities slightly for visibility
  geom_jitter(width = 0.2, size = 3, alpha = 0.8) +
  
  scale_y_continuous(limits = c(5, 30))  +
  
  scale_color_manual(values = sector_colors) +
  
  labs(#title = "Facility-Level LCOH for Scenario 4 Across Policy Scenarios",
       x = NULL,
       y = "Levelized Cost of Heat ($/MMBtu)",
       color = "Sector") +
  
  theme_bw(base_size = 14) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c("bottom"),
    legend.justification = c(1, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11), 
    legend.background = element_rect(
      color = "black",   
      linewidth = 0.2    
    ), 
    plot.margin = margin(t = 10, r = 45, b = 10, l = 10)
  )

print(policy_lcoh_plot)
# Save
# ggsave("state_fact_sheets/outputs/mn_sector_policy_lcoh3.png", policy_lcoh_plot, width = 8, height = 5, dpi = 300)


#### PARITY ANALYSIS ####
# Join and check parity
lcoh_compare <- lcoh_avg %>%
  left_join(ng_baseline, by = c("plant_name", "industry")) %>%
  mutate(at_parity = LCOH_avg <= NG_LCOH)

# Collapse into one row per plant with scenarios listed
parity_summary <- lcoh_compare %>%
  filter(at_parity == TRUE) %>%
  group_by(plant_name) %>%
  summarise(
    `Scenarios Reaching Cost Parity` = paste(scenario_policy_label, collapse = ", "),
    .groups = "drop"
  )

# Add facilities that don't reach parity
all_plants <- lcoh_long %>%
  distinct(plant_name)

scenarios_by_plant <- all_plants %>%
  left_join(parity_summary, by = "plant_name") %>%
  mutate(`Scenarios Reaching Cost Parity` = ifelse(
    is.na(`Scenarios Reaching Cost Parity`), "None", `Scenarios Reaching Cost Parity`)
  )

parity_summary <- lcoh_compare %>%
  filter(at_parity == TRUE) %>%
  mutate(
    scenario_policy = paste(scenario_label, "-", policy_label)
  ) %>%
  group_by(industry, scenario_policy) %>%
  summarise(facility_count = n_distinct(plant_name), .groups = "drop") %>%
  pivot_wider(
    names_from = scenario_policy,
    values_from = facility_count,
    values_fill = 0
  )


# writexl::write_xlsx(
#   list(
#     "Facility Parity Detail" = scenarios_by_plant,
#     "Parity Summary" = parity_summary
#   ),
#   path = "state_fact_sheets/data/modified/lcoh_policy_results.xlsx"
# )
