# S2 Presentation Combined Figures #
## September 22, 2025


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
library(forcats)

# pull in the state emissions file & create ordered factor
state_emissions_df.o <- 
  read_excel(glue("state_fact_sheets/data/modified/state-data/combined/combined_emissions_{format(Sys.Date(), '%Y%m%d')}.xlsx")) %>%
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

# order_levels <- 
#   state_emissions_df.o %>%
#   filter(clean_grid_scenario_label == "Current Grid Mix" & tech_scenario == 'BaselineBest' & pollutant_type == 'co2e') %>%
#   group_by(state, industry_clean) %>%
#   summarise(total_emissions = sum(total_emissions, na.rm = TRUE), .groups = "drop") %>%
#   group_by(state) %>%
#   mutate(industry_ordered = fct_reorder(industry_clean, total_emissions, .desc = TRUE)) %>%
#   select(state, industry_clean, industry_ordered)

order_levels <- 
  state_emissions_df.o %>%
  filter(clean_grid_scenario_label == "Current Grid Mix" & tech_scenario == 'BaselineBest' & pollutant_type == 'co2e') %>%
  group_by(industry_clean) %>%
  summarise(total_emissions = sum(total_emissions, na.rm = TRUE), .groups = "drop") %>%
  mutate(industry_ordered = fct_reorder(industry_clean, total_emissions, .desc = TRUE)) %>%
  select(industry_clean, industry_ordered)

# Collapse to industry clean, making pulp & paper one thing 
state_emissions_df <- 
  state_emissions_df.o %>%
  left_join(order_levels, by = c("industry_clean")) %>%
  group_by(state, industry_ordered, clean_grid_scenario_label, tech_scenario, pollutant_type) %>%
  summarize(
    facility_emissions = sum(facility_emissions, na.rm = TRUE),
    total_emissions = sum(total_emissions, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  ungroup()

# pull in facility LCOH file
facility_lcoh_df <- 
  read_excel(glue("state_fact_sheets/data/modified/state-data/Combined/combined_lcoh_{format(Sys.Date(), '%Y%m%d')}.xlsx")) %>%
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
  left_join(order_levels, by = c("industry_clean"))

#### EMISSIONS BY STATE ####

sector_colors <- c(
  "Pulp & Paper" = "#6d7d33",
  "Beet Sugar" = "#ef5645",
  "Ethyl Alcohol" = "#047c91", 
  "Fats & Oils" = "#2CA02C", 
  "Soybeans" = "#c9bf9d", 
  "Spices" = "#9370DB", 
  "Meat (non-poultry)" = "#8B0000", 
  "Distilleries" = "#D2691E", 
  "Rendering" = "#8C564B",  
  "Wet Corn Milling"    = "#febc11", 
  "Baseline (NG)" = "darkgrey"
)

# Filter, average best/worst, convert to MtCO₂e
base_emissions_df <- 
  state_emissions_df %>%
  # Let's just show the base emissions 
  filter(clean_grid_scenario_label == "Current Grid Mix", 
         tech_scenario == 'BaselineBest', 
         pollutant_type == 'co2e') %>%
  group_by(state) %>%
  #slice_max(order_by = total_emissions, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(emissions_Mt = total_emissions/1000000) # for CO2e

#### BASE EMISSIONS FIGURE ####
co2e_by_state_plot <- 
  base_emissions_df |> 
  ggplot(aes(x = state, y = emissions_Mt, fill = industry_ordered)) + 
  geom_col(position = position_dodge(width = 0.8, preserve = "single"), width = 0.6) + 
  #facet_wrap(~ clean_grid_scenario_label, nrow = 1) + 
  labs( x = NULL, y = "GHG Emissions (Mt CO2e)", fill = "Subsector" ) + 
  scale_fill_manual(values = sector_colors) + 
  scale_y_continuous(limits = c(0, max(base_emissions_df$emissions_Mt) * 1.1), expand = c(0, 0)) + 
  # dynamic limit 
  theme_bw(base_size = 14) + 
  theme( 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 11), 
    legend.background = element_rect(color = "black", linewidth = 0.2) 
    )


#### EMISSIONS ACROSS SCENARIOS, ACROSS STATES #### 
# Filter, average best/worst, convert to MtCO₂e
emissions_df <- 
  state_emissions_df %>%
  # Which grid scenarios to highlight? 
  filter(clean_grid_scenario_label == "Current Grid Mix", 
         pollutant_type == 'co2e') %>%  
  mutate(
    scenario_base = str_remove(tech_scenario, "Best|Worst")) %>%
  # just going with the best case for now, which also pulls BaselineBest
  filter(str_detect(tech_scenario, 'Best')) %>%
  group_by(state, industry_ordered, scenario_base) %>%
  summarise(
    total_emissions = sum(total_emissions)) %>%
  ungroup() %>%
  mutate(emissions_Mt = total_emissions/1000000) 

top_sectors <- 
  emissions_df %>%
  group_by(state, industry_ordered) %>%
  summarise(sector_total = sum(emissions_Mt), .groups = "drop") %>%
  group_by(state) %>%
  slice_max(order_by = sector_total, n = 3, with_ties = FALSE) %>%
  ungroup()

# Step 3: Keep only those in emissions_df
emissions_df <- 
  emissions_df %>%
  semi_join(top_sectors, by = c("state", "industry_ordered"))

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

emissions_scenario_plot <- 
  emissions_df |>
  ggplot(aes(x = industry_ordered, y = emissions_Mt, fill = scenario_base)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  facet_wrap(~ state, nrow = 1, scales = "free_x", drop = TRUE) +
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

#### LCOH BY SUBSECTOR ####
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

lcoh_state_sector <- 
  facility_lcoh_df %>%
  filter(
    policy_label == "No Policy", 
    tech_scenario %in% c('Scenario4Worst', 'Scenario4Best')
  ) %>%
  group_by(state, industry_ordered, tech_scenario) %>%
  summarize(
    lcoh = mean(lcoh, na.rm = TRUE)
  ) %>%
  mutate(
    #scenario_clean = str_remove(tech_scenario, "Best|Worst"), 
    scenario_rank = str_extract(tech_scenario, "Best|Worst")
  ) %>%
  bind_rows(lcoh_tech_base) %>%
  select(-tech_scenario)

state_lcoh_plot <- 
  ggplot(lcoh_state_sector,
         aes(x = state, y = lcoh, color = industry_ordered)) +
  
  # add boxplot 
  geom_boxplot(outlier.shape = NA, width = 0.3,
               position = position_dodge(width = 0.5)) +
  
  scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 30)) +
  labs(
    x = NULL,
    y = "Levelized Cost of Heat ($/MMBtu)",
    color = "Subsector"
  ) +
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.83, 0.32),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )
  
#### EMISSIONS IN THE MONEY ####

# select policy scenarios to show 
fig_policies <- c(
  'No Policy',
  'Capex: -100%, Elec: -0%', 
  'Capex: -0%, Elec: -25%', 
  'Capex: -0%, Elec: -50%', 
  'Capex: -100%, Elec: -50%'
)

# Emissions in Money df 
eim_df <- 
  facility_lcoh_df %>%
  filter(
    !tech_scenario %in% c('BaselineWorst', 'BaselineBest')
  ) %>%
  left_join(
    lcoh_tech_base |> 
      filter(scenario_rank == 'Worst') |>
      rename(lcoh_ng = lcoh) |>
      select(state, lcoh_ng), 
    by = 'state') %>%
  mutate(
    in_money = if_else(lcoh < lcoh_ng, 1, 0)
  ) %>%
  group_by(state, policy_label) %>%
  summarize(
    eim = sum(elec_ghg_emissions[in_money == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(policy_label %in% fig_policies) %>%
  mutate(
    eim_Mt = eim/1000000, 
    policy_label = factor(policy_label, levels = fig_policies),
  ) 

eim_plot <- 
  ggplot(eim_df,
         aes(x = state, y = eim_Mt, fill = policy_label)) +
  
  # add boxplot 
  geom_col(position = position_dodge(width = 0.8, preserve = "single"), width = 0.6) + 
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  labs( x = NULL, y = "GHG Emissions (Mt CO2e)", fill = "Policy" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.7, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

eim_plot

#### MICHIGAN EMISSIONS IN MONEY ####
MI_eim_df <- 
  facility_lcoh_df %>%
  filter(
    !tech_scenario %in% c('BaselineWorst', 'BaselineBest'), 
    state == 'MI'
  ) %>%
  left_join(
    lcoh_tech_base |> 
      filter(scenario_rank == 'Worst') |>
      rename(lcoh_ng = lcoh) |>
      select(state, lcoh_ng), 
    by = 'state') %>%
  mutate(
    in_money = if_else(lcoh < lcoh_ng, 1, 0)
  ) %>%
  group_by(industry_ordered, policy_label) %>%
  summarize(
    eim = sum(elec_ghg_emissions[in_money == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(policy_label %in% fig_policies) %>%
  mutate(
    eim_Mt = eim/1000000, 
    policy_label = factor(policy_label, levels = fig_policies),
  ) 

MI_eim_plot <- 
  ggplot(MI_eim_df,
         aes(x = industry_ordered, y = eim_Mt, fill = policy_label)) +
  
  # add boxplot 
  geom_col(position = position_dodge(width = 0.8, preserve = "single"), width = 0.6) + 
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  labs( x = NULL, y = "GHG Emissions (Mt CO2e)", fill = "Policy" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.7, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )


#### MICHIGAN EMISSIONS IN MONEY ####
MI_eim_df <- 
  facility_lcoh_df %>%
  filter(
    !tech_scenario %in% c('BaselineWorst', 'BaselineBest'), 
    state == 'MI'
  ) %>%
  left_join(
    lcoh_tech_base |> 
      filter(scenario_rank == 'Worst') |>
      rename(lcoh_ng = lcoh) |>
      select(state, lcoh_ng), 
    by = 'state') %>%
  mutate(
    in_money = if_else(lcoh < lcoh_ng, 1, 0)
  ) %>%
  group_by(industry_ordered, policy_label) %>%
  summarize(
    eim = sum(elec_ghg_emissions[in_money == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(policy_label %in% fig_policies) %>%
  mutate(
    eim_Mt = eim/1000000, 
    policy_label = factor(policy_label, levels = fig_policies),
  ) 

MI_eim_plot <- 
  ggplot(MI_eim_df,
         aes(x = industry_ordered, y = eim_Mt, fill = policy_label)) +
  
  # add boxplot 
  geom_col(position = position_dodge(width = 0.8, preserve = "single"), width = 0.6) + 
  
  #scale_color_manual(values = sector_colors) +
  #scale_y_continuous(limits = c(5, 21)) +
  
  labs( x = NULL, y = "GHG Emissions (Mt CO2e)", fill = "Policy" ) + 
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.7, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )


#### EXPORT FIGS ####

ggsave("state_fact_sheets/outputs/state-fact-sheet-figures/Combined/co2e_by_state_092225.png",
       co2e_by_state_plot,
       width = 8, height = 5, dpi = 300)

ggsave("state_fact_sheets/outputs/state-fact-sheet-figures/Combined/emisssions_scenario_092225.png",
       emissions_scenario_plot,
       width = 8, height = 5, dpi = 300)

ggsave("state_fact_sheets/outputs/state-fact-sheet-figures/Combined/state_lcoh_092225.png",
       state_lcoh_plot,
       width = 8, height = 5, dpi = 300)

ggsave("state_fact_sheets/outputs/state-fact-sheet-figures/Combined/eim_092225.png",
       eim_plot,
       width = 8, height = 5, dpi = 300)

ggsave("state_fact_sheets/outputs/state-fact-sheet-figures/Combined/MI_eim_092225.png",
       MI_eim_plot,
       width = 8, height = 5, dpi = 300)






