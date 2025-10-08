
#### MN Emissions Figures ####
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

#### Data Wrangling ####
emissions.o <- read_excel("state_fact_sheets/data/raw/MN_sector_emissions_scenarios.xlsx", sheet = "Baseline")
emissions.80.o <- read_excel("state_fact_sheets/data/raw/MN_sector_emissions_scenarios.xlsx", sheet = "80pct")
emissions.100.o <- read_excel("state_fact_sheets/data/raw/MN_sector_emissions_scenarios.xlsx", sheet = "100pct")

## Baseline
emissions <- 
  emissions.o |>
  rename(
    baseline = `Baseline emissions`, 
    scenario1 = `Electric boiler (worse case)`, 
    scenario2_high = `Ambient heat pump (worse case)`, 
    scenario2_low = `Ambient heat pump (best case)`, 
    scenario3 = `Energy efficiency + Electric boilers (worse case)`, 
    scenario4_high = `Energy efficiency + Ambient heat pump (worse case)`, 
    scenario4_low = `Energy efficiency + Ambient heat pump (best case)`
  ) |>
  mutate(
    baseline = baseline / 1000000, 
    scenario1 = scenario1 / 1000000, 
    scenario2_low = scenario2_low / 1000000, 
    scenario2_high = scenario2_high / 1000000, 
    scenario3 = scenario3 / 1000000, 
    scenario4_low = scenario4_low / 1000000, 
    scenario4_high = scenario4_high / 1000000
  ) |>
  select(sector, baseline, scenario1, scenario2_high, scenario2_low, scenario3, scenario4_high, scenario4_low) |>
  pivot_longer(
    cols = c(baseline, scenario1, scenario2_low, scenario2_high, scenario3, scenario4_low, scenario4_high), 
    names_to = "scenario", 
    values_to = "emissions"
  ) |> 
  mutate(
    bound = case_when(
      str_detect(scenario, "_low") ~ "low",
      str_detect(scenario, "_high") ~ "high",
      TRUE ~ "point"
    ), 
    scenario = str_remove(scenario, "_low|_high")
  ) |>
  pivot_wider(
    names_from = "bound", 
    values_from = "emissions"
  ) |>
  mutate(
    point = case_when(
      scenario %in% c("scenario2", "scenario4") ~ ((low+high)/2), 
      TRUE ~ point
    )
  )

## 80% clean grid 
emissions.80 <- 
  emissions.80.o |>
  rename(
    baseline = `Baseline emissions`, 
    scenario1 = `Electric boiler (worse case)`, 
    scenario2_high = `Ambient heat pump (worse case)`, 
    scenario2_low = `Ambient heat pump (best case)`, 
    scenario3 = `Energy efficiency + Electric boilers (worse case)`, 
    scenario4_high = `Energy efficiency + Ambient heat pump (worse case)`, 
    scenario4_low = `Energy efficiency + Ambient heat pump (best case)`
  ) |>
  mutate(
    baseline = baseline / 1000000, 
    scenario1 = scenario1 / 1000000, 
    scenario2_low = scenario2_low / 1000000, 
    scenario2_high = scenario2_high / 1000000, 
    scenario3 = scenario3 / 1000000, 
    scenario4_low = scenario4_low / 1000000, 
    scenario4_high = scenario4_high / 1000000
  ) |>
  select(sector, baseline, scenario1, scenario2_high, scenario2_low, scenario3, scenario4_high, scenario4_low) |>
  pivot_longer(
    cols = c(baseline, scenario1, scenario2_low, scenario2_high, scenario3, scenario4_low, scenario4_high), 
    names_to = "scenario", 
    values_to = "emissions"
  ) |> 
  mutate(
    bound = case_when(
      str_detect(scenario, "_low") ~ "low",
      str_detect(scenario, "_high") ~ "high",
      TRUE ~ "point"
    ), 
    scenario = str_remove(scenario, "_low|_high")
  ) |>
  pivot_wider(
    names_from = "bound", 
    values_from = "emissions"
  ) |>
  mutate(
    point = case_when(
      scenario %in% c("scenario2", "scenario4") ~ ((low+high)/2), 
      TRUE ~ point
    )
  )

## 100% Clean 
emissions.100 <- 
  emissions.100.o |>
  rename(
    baseline = `Baseline emissions`, 
    scenario1 = `Electric boiler (worse case)`, 
    scenario2_high = `Ambient heat pump (worse case)`, 
    scenario2_low = `Ambient heat pump (best case)`, 
    scenario3 = `Energy efficiency + Electric boilers (worse case)`, 
    scenario4_high = `Energy efficiency + Ambient heat pump (worse case)`, 
    scenario4_low = `Energy efficiency + Ambient heat pump (best case)`
  ) |>
  mutate(
    baseline = baseline / 1000000, 
    scenario1 = scenario1 / 1000000, 
    scenario2_low = scenario2_low / 1000000, 
    scenario2_high = scenario2_high / 1000000, 
    scenario3 = scenario3 / 1000000, 
    scenario4_low = scenario4_low / 1000000, 
    scenario4_high = scenario4_high / 1000000
  ) |>
  select(sector, baseline, scenario1, scenario2_high, scenario2_low, scenario3, scenario4_high, scenario4_low) |>
  pivot_longer(
    cols = c(baseline, scenario1, scenario2_low, scenario2_high, scenario3, scenario4_low, scenario4_high), 
    names_to = "scenario", 
    values_to = "emissions"
  ) |> 
  mutate(
    bound = case_when(
      str_detect(scenario, "_low") ~ "low",
      str_detect(scenario, "_high") ~ "high",
      TRUE ~ "point"
    ), 
    scenario = str_remove(scenario, "_low|_high")
  ) |>
  pivot_wider(
    names_from = "bound", 
    values_from = "emissions"
  ) |>
  mutate(
    point = case_when(
      scenario %in% c("scenario2", "scenario4") ~ ((low+high)/2), 
      TRUE ~ point
    )
  )

#### Emissions By Sector & Scenario -- Baseline EF ####
p_base <- 
  ggplot(emissions, aes(x=sector, y=point, fill = scenario)) +
  geom_col(
    position = position_dodge(width = 0.8),  
    width = 0.6                              
  ) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = NULL,
    y = "Emissions (MtCO2e)",
    title = "Current Grid",
    fill = "Scenario"
  ) +
  scale_y_continuous(limits = c(0, 4.5), expand = c(0, 0)) +
  scale_fill_manual(
    name = "Scenario",
    values = c(
      "baseline" = "#fb9a99",
      "scenario1" = "#1f78b4",
      "scenario2" = "#33a02c",
      "scenario3" = "#a6cee3",
      "scenario4" = "#b2df8a"
    ),
    labels = c(
      "baseline" = "Baseline",
      "scenario1" = "1: E-Boiler",
      "scenario2" = "2: ASHP",
      "scenario3" = "3: E-Boiler + EE",
      "scenario4" = "4: ASHP + EE"
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 13),      
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),  
  )

# ggsave("state_fact_sheets/outputs/mn_emissions_baseline.png", width = 8, height = 5, dpi = 300)

#### Emissions By Sector & Scenario -- 80% Clean ####

p_80 <- 
  ggplot(emissions.80, aes(x=sector, y=point, fill = scenario)) +
  geom_col(
    position = position_dodge(width = 0.8),  
    width = 0.6                              
  ) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "80% Clean Grid (2030)"
  ) +
  scale_y_continuous(limits = c(0, 4.5), expand = c(0, 0)) +
  scale_fill_manual(
    name = "Scenario",
    values = c(
      "baseline" = "#fb9a99",
      "scenario1" = "#1f78b4",
      "scenario2" = "#33a02c",
      "scenario3" = "#a6cee3",
      "scenario4" = "#b2df8a"
    ),
    labels = c(
      "baseline" = "Baseline",
      "scenario1" = "1: E-Boiler",
      "scenario2" = "2: Air-Source HP",
      "scenario3" = "3: E-Boiler + Energy Eff.",
      "scenario4" = "4: Air-Source HP + Energy Eff."
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = 13),      
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)  
    )

#ggsave("state_fact_sheets/outputs/mn_emissions_80pct.png", width = 8, height = 5, dpi = 300)

#### Emissions By Sector & Scenario -- 100% Clean ####
p_100 <- 
  ggplot(emissions.100, aes(x=sector, y=point, fill = scenario)) +
  geom_col(
    position = position_dodge(width = 0.8),  
    width = 0.6                              
  ) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "100% Clean Grid (2040)"
  ) +
  scale_y_continuous(limits = c(0, 4.5), expand = c(0, 0)) +
  scale_fill_manual(
    name = "Scenario",
    values = c(
      "baseline" = "#fb9a99",
      "scenario1" = "#1f78b4",
      "scenario2" = "#33a02c",
      "scenario3" = "#a6cee3",
      "scenario4" = "#b2df8a"
    ),
    labels = c(
      "baseline" = "Baseline",
      "scenario1" = "1: E-Boiler",
      "scenario2" = "2: ASHP",
      "scenario3" = "3: E-Boiler + EE",
      "scenario4" = "4: ASHP + EE"
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),      
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),  
    legend.position = c(0.3, 0.95),                        
    legend.justification = c(0, 1),
    legend.text = element_text(size = 8),                   
    legend.title = element_text(size = 9),                  
    legend.key.size = unit(0.4, "cm"),                      
    legend.background = element_rect(color = "black", linewidth = 0.2)
  )

#ggsave("state_fact_sheets/outputs/mn_emissions_100pct.png", width = 8, height = 5, dpi = 300)


#### Combine ####

(p_base | p_80 | p_100)

ggsave("state_fact_sheets/outputs/mn_emissions_combo.png", width = 8, height = 5, dpi = 300)

