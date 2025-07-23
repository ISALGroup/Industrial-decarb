#### MN Emissions Figures ####

emissions.o <- read_excel("state_fact_sheets/data/raw/MN_sector_emissions_scenarios.xlsx", sheet = "Baseline")
emissions.80.o <- read_excel("state_fact_sheets/data/raw/MN_sector_emissions_scenarios.xlsx", sheet = "80pct")
emissions.100.o <- read_excel("state_fact_sheets/data/raw/MN_sector_emissions_scenarios.xlsx", sheet = "100pct")


#### Emissions By Sector & Scenario -- Baseline EF ####
# Data wrangling
emissions <- 
  emissions.o |>
  rename(
    baseline = `Baseline emissions`, 
    scenario1 = `Electric boiler (worse case)`, 
    scenario2_high = `Ambient heat pump (worse case)`, 
    scenario2_low = `Ambient heat pump (best case)`, 
    scenario3 = `Energy efficiency + Electric boilers (worse case)`, 
    scenario4_high = `Energy efficiency + Waste heat pump (worse case)`, 
    scenario4_low = `Energy efficiency + Waste heat pump (best case)`
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
      "scenario2" = "2: Air-Source HP",
      "scenario3" = "3: E-Boiler + Energy Eff.",
      "scenario4" = "4: Waste HP + Energy Eff."
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.7, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10), 
    legend.background = element_rect(
      color = "black",   
      linewidth = 0.2    
    )
  )

ggsave("state_fact_sheets/outputs/mn_emissions_baseline.png", width = 8, height = 5, dpi = 300)

#### Emissions By Sector & Scenario -- 80% Clean ####

emissions.80 <- 
  emissions.80.o |>
  rename(
    baseline = `Baseline emissions`, 
    scenario1 = `Electric boiler (worse case)`, 
    scenario2_high = `Ambient heat pump (worse case)`, 
    scenario2_low = `Ambient heat pump (best case)`, 
    scenario3 = `Energy efficiency + Electric boilers (worse case)`, 
    scenario4_high = `Energy efficiency + Waste heat pump (worse case)`, 
    scenario4_low = `Energy efficiency + Waste heat pump (best case)`
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
      "scenario2" = "2: Air-Source HP",
      "scenario3" = "3: E-Boiler + Energy Eff.",
      "scenario4" = "4: Waste HP + Energy Eff."
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.7, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10), 
    legend.background = element_rect(
      color = "black",   
      linewidth = 0.2    
    )
  )

ggsave("state_fact_sheets/outputs/mn_emissions_80pct.png", width = 8, height = 5, dpi = 300)

#### Emissions By Sector & Scenario -- 100% Clean ####

emissions.100 <- 
  emissions.100.o |>
  rename(
    baseline = `Baseline emissions`, 
    scenario1 = `Electric boiler (worse case)`, 
    scenario2_high = `Ambient heat pump (worse case)`, 
    scenario2_low = `Ambient heat pump (best case)`, 
    scenario3 = `Energy efficiency + Electric boilers (worse case)`, 
    scenario4_high = `Energy efficiency + Waste heat pump (worse case)`, 
    scenario4_low = `Energy efficiency + Waste heat pump (best case)`
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
      "scenario2" = "2: Air-Source HP",
      "scenario3" = "3: E-Boiler + Energy Eff.",
      "scenario4" = "4: Waste HP + Energy Eff."
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.7, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10), 
    legend.background = element_rect(
      color = "black",   
      linewidth = 0.2    
    )
  )

ggsave("state_fact_sheets/outputs/mn_emissions_100pct.png", width = 8, height = 5, dpi = 300)

