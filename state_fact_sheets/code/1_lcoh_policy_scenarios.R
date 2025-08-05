library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(stringr)
library(grid)
library(janitor)
library(tidyverse)
library(here)
#library(ggtext)

# set working directory
#setwd("~/Documents/Industrial_Decarbonization/Industrial-decarb")

# Pull in tech scenarios results
tech_input_raw_df <- read_excel("state_fact_sheets/data/raw/lcoh_policy_modeling_input.xlsx")

# Parameters
r <- 0.065
t <- 15
elec_price <- 0.092  # $/kWh
discount_factors <- (1 + r) ^ -(0:(t - 1))
discount_sum <- sum(discount_factors)  # This is used to discount future OPEX and heat

# Base opex and LCOH
tech_input_df <- tech_input_raw_df %>%
  mutate(
    opex = change_in_elec_demand_kwh * elec_price,
    lcoh_base = (capex / discount_sum + opex) / heat_mmbtu
  )

# Percent reductions from 0% to 100% in 10% steps
#capex_reductions <- seq(0, 1, by = 0.1)

# Define policy options
capex_subsidies <- c(0, 0.5, 1)
elec_price_reductions <- c(0, 0.25, 0.5)

# Create policy grid
policy_grid <- expand.grid(
  capex_subsidy = capex_subsidies,
  elec_discount = elec_price_reductions
)

# Expand tech_input_df × policy_grid (cross join)
policy_scenarios_df <- tidyr::crossing(tech_input_df, policy_grid) %>%
  mutate(
    capex_adj = capex - (capex * capex_subsidy),
    elec_price_adj = elec_price * (1 - elec_discount),
    opex_adj = change_in_elec_demand_kwh * elec_price_adj,
    lcoh = (capex_adj / discount_sum + opex_adj) / heat_mmbtu,
    
    policy_label = paste0("Capex: -", capex_subsidy * 100, "%, Elec: -", elec_discount * 100, "%"), 
    policy_label = 
      if_else(policy_label == "Capex: -0%, Elec: -0%", "No Policy", policy_label), 
    policy_label = factor(policy_label, levels = c(
        "No Policy",
        "Capex: -50%, Elec: -0%",
        "Capex: -100%, Elec: -0%",
        "Capex: -0%, Elec: -25%",
        "Capex: -50%, Elec: -25%",
        "Capex: -100%, Elec: -25%",
        "Capex: -0%, Elec: -50%",
        "Capex: -50%, Elec: -50%",
        "Capex: -100%, Elec: -50%"
      )),
    
    naics_description = 
      case_when(
        naics_description == "Beet Sugar Manufacturing" ~ "Beet Sugar", 
        naics_description == "Ethyl Alcohol Manufacturing" ~ "Ethyl Alcohol",
        TRUE ~ naics_description
      ), 
    naics_description = factor(
      naics_description,
      levels = c("Paper Mills", "Beet Sugar", "Ethyl Alcohol")
    ))
  

# # Create base/no-policy scenario
# no_policy_df <- tech_input_df %>%
#   mutate(
#     capex_subsidy = 1,
#     elec_discount = 0,
#     capex_adj = capex,
#     elec_price_adj = 0.092,
#     opex_adj = opex,
#     lcoh = lcoh_base,
#     policy_label = "No Policy"
#   )

# Combine all into one dataframe
# combined_lcoh_df <- bind_rows(no_policy_df, policy_scenarios_df)  %>%
#   mutate(policy_label = factor(policy_label, levels = c(
#     "No Policy",
#     "Capex: 70%, Elec: -20%",
#     "Capex: 70%, Elec: -30%",
#     "Capex: 70%, Elec: -50%",
#     "Capex: 50%, Elec: -20%",
#     "Capex: 50%, Elec: -30%",
#     "Capex: 50%, Elec: -50%",
#     "Capex: 0%, Elec: -20%",
#     "Capex: 0%, Elec: -30%",
#     "Capex: 0%, Elec: -50%"
#   )))  


#. filter to scenario 4
scenario4_df <- 
  policy_scenarios_df |>
  filter(str_detect(tech_scenario, "scenario_4"))

# PLOTTING #revenge
# Define your custom sector colors
sector_colors <- c(
  "Paper Mills" = "#6d7d33",
  "Beet Sugar" = "#047c91",
  "Ethyl Alcohol" = "#febc11"
)

label_df <- data.frame(
  policy_label = levels(scenario4_df$policy_label)[2],  # pick the factor level you want
  lcoh = 7.75,
  label = "LCOH of a natural gas boiler"
)

lcoh_policy_combined_plot <- 
  ggplot(scenario4_df, aes(x = policy_label, y = lcoh, fill = naics_description)) +
  
  # NG parity band
  # Add grey band for natural gas baseline
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 6.96333, ymax = 8.490105,
           fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = 6.96333, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  geom_hline(yintercept = 8.490105, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  # annotate("text",
  #          label = "LCOH of a natural gas boiler",
  #          x = "No Policy", y = 7.75,
  #          # hjust = -0.1, vjust = 50,
  #          size = 3,
  #          fontface = 'italic') +
  
  # Boxplot
  geom_boxplot(outlier.shape = NA, width = 0.3, position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = sector_colors) +
  labs(
    y = "LCOH ($/MMBtu)",
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
    legend.background = element_rect(
      color = "black",   
      linewidth = 0.2    
    )
  )

print(lcoh_policy_combined_plot)

#ggsave("state_fact_sheets/outputs/mn_combined_policy_lcoh2.png", lcoh_policy_combined_plot, width = 8, height = 6, dpi = 300)