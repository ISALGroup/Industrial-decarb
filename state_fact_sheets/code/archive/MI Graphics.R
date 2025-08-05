#### Michigan Graphics #### 

setwd("/Users/nmariano/Library/CloudStorage/Dropbox/1 Projects/MI Graphics/")

library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)

#### Emissions By Sector & Scenario -- Baseline EF ####
emissions.o <- read_excel("data/MI_sector_emissions_scenarios.xlsx", sheet = "Baseline")

# Data wrangling
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

# Make plot
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
      "scenario2" = "2: HTHP",
      "scenario3" = "3: E-Boiler + Energy Eff.",
      "scenario4" = "4: HTHP + Energy Eff."
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )

#ggsave("outputs/emissions_baseline.png", width = 6.75, height = 4, units = "in", dpi = 300, device='png')

#### Emissions By Sector & Scenario -- 80% Clean ####
emissions.80.o <- read_excel("data/MI_sector_emissions_scenarios.xlsx", sheet = "80pct")

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
    y = "Emissions (MtCO2e)",
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
      "scenario2" = "2: HTHP",
      "scenario3" = "3: E-Boiler + Energy Eff.",
      "scenario4" = "4: HTHP + Energy Eff."
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )

# ggsave("outputs/emissions_80.png", width = 6.75, height = 4, unit = "in", dpi = 300)

#### Emissions By Sector & Scenario -- 100% Clean ####
emissions.100.o <- read_excel("data/MI_sector_emissions_scenarios.xlsx", sheet = "100pct")

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
    y = "Emissions (MtCO2e)",
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
      "scenario2" = "2: HTHP",
      "scenario3" = "3: E-Boiler + Energy Eff.",
      "scenario4" = "4: HTHP + Energy Eff."
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )

#ggsave("outputs/emissions_100.png", width = 6.75, height = 4, unit = "in", dpi = 300)


#### Investments By Sector Graph ####
investments.o <- read_excel("data/MI_sector_investments.xlsx")

investments <- 
  investments.o |>
  rename(
    eboiler_high = `Electric boilers (Best-case) investment`, 
    eboiler_low = `Electric boilers (Worst-case) investment`, 
    hthp_high = `Heat pumps (Best-case) investment`, 
    hthp_low = `Heat pumps (Worst-case) investment`
  ) |>
  select(sector, eboiler_low, eboiler_high, hthp_low, hthp_high) |>
  mutate(
    eboiler_high = eboiler_high / 1000000, 
    eboiler_low = eboiler_low / 1000000, 
    hthp_high = hthp_high / 1000000, 
    hthp_low = hthp_low / 1000000
  ) |>
  pivot_longer(
    cols = c(eboiler_low, eboiler_high, hthp_low, hthp_high), 
    names_to = "technology", 
    values_to = "capex_usd"
  ) |> 
  mutate(
    bound = case_when(
      str_detect(technology, "_low") ~ "low",
      str_detect(technology, "_high") ~ "high",
    ), 
    technology = str_remove(technology, "_low|_high")
  ) |>
  pivot_wider(
    names_from = "bound", 
    values_from = "capex_usd"
  ) 
  
ggplot(investments, aes(x = sector, ymin = low, ymax = high, color = technology)) +
  geom_linerange(
    position = position_dodge(width = 0.45),
    linewidth = 10  # this sets bar "width"
  ) +
  labs(x = NULL, y = "CAPEX ($ millions)", color = "Technology") +
  scale_color_manual(
   values = c(
     "eboiler" = "#1f78b4",
     "hthp" = "#33a02c"
   ),
   labels = c(
     "eboiler" = "E-boiler",
     "hthp" = "HTHP"
   )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )

#ggsave("outputs/investments.png", width = 6.75, height = 4, unit = "in", dpi = 300)

#### LCOH By Sector Graph ####
lcoh.o <- read_excel("data/MI_sector_LCOH.xlsx")

lcoh <- 
  lcoh.o |>
  pivot_longer(
    cols = c(`Paper Mills`, `Beet Sugar`, `Ethyl Alcohol`, `Spice + Salt`, Overall), 
    names_to = "sector", 
    values_to = "lcoh"
  ) |> 
  pivot_wider(
    names_from = 'bound', 
    values_from = 'lcoh'
  ) |>
  filter(sector != 'Overall')

ggplot(lcoh, aes(x = sector, ymin = min, ymax = max, color = scenario)) +
  geom_linerange(
    position = position_dodge(width = .3),
    linewidth = 2.5  # this sets bar "width"
  ) +
  labs(x = NULL, y = "LCOH ($/mmBtu)", color = "Technology") +
  scale_color_manual(
    name = "Scenario",
    values = c(
      "scenario1" = "#1f78b4",
      "scenario2" = "#33a02c",
      "scenario3" = "#a6cee3",
      "scenario4" = "#b2df8a"
    ),
    labels = c(
      "scenario1" = "1: E-Boiler",
      "scenario2" = "2: HTHP",
      "scenario3" = "3: E-Boiler + Energy Eff.",
      "scenario4" = "4: HTHP + Energy Eff."
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

#ggsave("outputs/lcoh.png", width = 6.75, height = 4, unit = "in", dpi = 300)


