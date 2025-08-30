# LCOH_wpriceprojections_figs

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

# pull in the state emissions file & create ordered factor
state_emissions_df <- 
  read_excel(glue("state_fact_sheets/data/modified/state-data/{state}/state_emissions_results_{state}_20250819.xlsx")) %>%
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
      naics_description == 'Distilleries' ~ 'Distilleries'
    )
  )

order_levels <- 
  state_emissions_df %>%
  filter(clean_grid_scenario_label == "Current Grid Mix" & tech_scenario == 'BaselineBest') %>%
  group_by(industry_clean) %>%
  summarise(total_emissions = sum(baseline_co2e_emissions, na.rm = TRUE)) %>%
  arrange(desc(total_emissions)) %>%
  pull(industry_clean)

# pull in facility level file
facility_lcoh_df <- 
  read_excel(glue("state_fact_sheets/data/modified/state-data/{state}/facility_lcoh_proj.price_{state}_{format(Sys.Date(), '%Y%m%d')}.xlsx")) %>%
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
      naics_description == 'Distilleries' ~ 'Distilleries'
    ), 
    industry_clean = factor(industry_clean, levels = order_levels)) 

#state_lcoh_df <- read_excel("state_fact_sheets/data/modified/state-data/MN/250812_state_lcoh_results_mn.xlsx") didn't use

# --------- LCOH by Technology Scenario Figure --------------
# Define sector colors
sector_colors <- c(
  "Pulp & Paper" = "#6d7d33",
  "Beet Sugar" = "#ef5645",
  "Ethyl Alcohol" = "#febc11", 
  "Fats & Oils" = "#047c91", 
  "Soybeans" = "#c9bf9d", 
  "Spices" = "#9370DB", 
  "Meat (non-poultry)" = "#8B0000", 
  "Distilleries" = "#D2691E"
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
  scale_y_continuous(limits = c(4, 30)) +
  
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
  scale_y_continuous(limits = c(4, 30)) +
  
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


# --------- SAVE PLOTS ----------

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{state}/{state}_LCOH.proj.price_technology_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       technology_by_sector_lcoh_plot, 
       width = 8, height = 5, dpi = 300)

ggsave(glue("state_fact_sheets/outputs/state-fact-sheet-figures/{state}/{state}_LCOH.proj.price_policy_scenario4_plot_{format(Sys.Date(), '%Y%m%d')}.png"),
       lcoh_policy_combined_plot, 
       width = 8, height = 5, dpi = 300)





