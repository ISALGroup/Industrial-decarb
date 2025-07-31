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
library(ggtext)


# Pull in tech scenarios results
setwd("~/Documents/Industrial_Decarbonization/Industrial-decarb")

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

# Policy scenario combinations
capex_cost_shares <- c(0.7, 0.5, 0)     # fraction paid by facility
elec_price_reductions <- c(0.2, 0.3, 0.5)  # percent reductions

# Define policy options
capex_cost_shares <- c(0.3, 0.5, 1)
elec_price_reductions <- c(0.2, 0.3, 0.5)

# Create policy grid
policy_grid <- expand.grid(
  capex_share = capex_cost_shares,
  elec_discount = elec_price_reductions
)

# Expand tech_input_df × policy_grid (cross join)
policy_scenarios_df <- tidyr::crossing(tech_input_df, policy_grid) %>%
  mutate(
    capex_adj = capex * capex_share,
    elec_price_adj = elec_price * (1 - elec_discount),
    opex_adj = change_in_elec_demand_kwh * elec_price_adj,
    lcoh = (capex_adj / discount_sum + opex_adj) / heat_mmbtu,
    policy_label = paste0("Capex: ", capex_share * 100, "%, Elec: -", elec_discount * 100, "%")
  )

# Create base/no-policy scenario
no_policy_df <- tech_input_df %>%
  mutate(
    capex_share = 1,
    elec_discount = 0,
    capex_adj = capex,
    elec_price_adj = 0.092,
    opex_adj = opex,
    lcoh = lcoh_base,
    policy_label = "No Policy"
  )

# Combine all into one dataframe
combined_lcoh_df <- bind_rows(no_policy_df, policy_scenarios_df)  %>%
  mutate(policy_label = factor(policy_label, levels = c(
    "No Policy",
    "Capex: 70%, Elec: -20%",
    "Capex: 70%, Elec: -30%",
    "Capex: 70%, Elec: -50%",
    "Capex: 50%, Elec: -20%",
    "Capex: 50%, Elec: -30%",
    "Capex: 50%, Elec: -50%",
    "Capex: 0%, Elec: -20%",
    "Capex: 0%, Elec: -30%",
    "Capex: 0%, Elec: -50%"
  ))) %>% 
  mutate(naics_description = factor(
    naics_description,
    levels = c("Paper Mills", "Beet Sugar Manufacturing", "Ethyl Alcohol Manufacturing")
  ))


# PLOTTING #revenge
# Define your custom sector colors
sector_colors <- c(
  "Paper Mills" = "#6d7d33",
  "Beet Sugar Manufacturing" = "#047c91",
  "Ethyl Alcohol Manufacturing" = "#febc11"
)

lcoh_policy_combined_plot <- ggplot(scenario4_df, aes(x = policy_label, y = lcoh, fill = naics_description)) +
  # NG parity band
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 7, ymax = 8.5, fill = "grey80", alpha = 0.5) +
  # Boxplot
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = sector_colors) +
  labs(
    title = "Effect of Combined Policy Support on HP + Efficiency Upgrades LCOH",
    x = "Policy Scenario",
    y = "LCOH ($/MMBtu)",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


# save
ggsave("state_fact_sheets/outputs/mn_combined_policy_lcoh.png", lcoh_policy_combined_plot, width = 8, height = 6, dpi = 300)
