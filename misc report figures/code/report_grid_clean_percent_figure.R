# this code imports electricity generation AEO data from EIA to generate a stacked-area percentage chart for the US grid through 2050 
# OBQ
# Sept 25. 2025

require(plyr)
require(tidyverse)

install.packages(c("scales", "lubridate"))
library(scales)
library(lubridate)
library(readr)
library(ggplot2)

electricity <- read_csv("misc report figures/data/Electricity.csv", skip = 4)
renewables <- read_csv("misc report figures/data/Renewable_Energy_All_Sectors_Electricity_Generation_(Case_Reference_case_Region_United_States).csv", skip = 4)

#clean and long
elec_long <- electricity %>%
  select(
    Year,
    Coal = matches("Coal.*BkWh"),
    Petroleum = matches("Petroleum.*BkWh"),
    NaturalGas = matches("Natural Gas.*BkWh"),
    Nuclear = matches("Nuclear.*BkWh"),
    RenewablesTotal = matches("Renewable Sources.*BkWh"),
    Other = matches("Other.*BkWh"),
    TotalGen = matches("^Total Electricity Generation:.*BkWh$")
  ) %>%
  mutate(across(-Year, as.numeric))

#clean and long
renew_long <- renewables %>%
  transmute(
    Year,
    Wind = `Wind BkWh`,
    Solar = `Solar BkWh`,
    Biomass = `Wood and Other Biomass BkWh` + `Municipal Waste BkWh`,
    Geothermal = `Geothermal BkWh`,
    Hydro = `Hydropower BkWh`
  )

#join
combined <- elec_long %>%
  left_join(renew_long, by = "Year")

#long
fuel_long <- combined %>%
  mutate(
    Total_BkWh = TotalGen 
  ) %>%
  select(Year, Total_BkWh, Coal, Petroleum, NaturalGas, Nuclear, Wind, Solar, Hydro, Biomass, Geothermal, Other) %>%
  pivot_longer(
    cols = -c(Year, Total_BkWh),
    names_to = "Fuel",
    values_to = "BkWh"
  ) %>%
  mutate(
    pct = BkWh / Total_BkWh
  )

#order
fuel_order <- c("Coal", "Petroleum", "NaturalGas", "Nuclear",
                "Hydro", "Wind", "Solar", "Biomass", "Geothermal", "Other")
fuel_long$Fuel <- factor(fuel_long$Fuel, levels = fuel_order)

fuel_long_clean <- fuel_long %>%
  filter(!is.na(BkWh), !is.na(pct)) %>% 
  mutate(BkWh = if_else(Fuel == "Other" & BkWh < 0, 0, BkWh),
         pct = if_else(Fuel == "Other" & pct < 0, 0, pct))

renewables <- c("Hydro", "Wind", "Solar", "Biomass", "Geothermal")
renewables_nuclear_df <- fuel_long_clean %>%
  mutate(renew_nuclear = Fuel %in% c(renewables, "Nuclear")) %>%
  group_by(Year, renew_nuclear) %>%
  summarise(total_share = sum(pct), .groups = "drop") %>%
  filter(renew_nuclear) %>%
  select(Year, total_share)

fuel_colors <- c(
  "Coal"       = "antiquewhite2",
  "Petroleum"  = "antiquewhite3",
  "NaturalGas" = "antiquewhite4",
  "Nuclear"    = "#09847A",
  "Hydro"      = "#0BA89A",
  "Wind"       = "#9CBEBE",
  "Solar"      = "#C2E4E6",
  "Biomass"    = "#6D7D33",
  "Geothermal" = "#DCE1E5",
  "Other"      = "#DCD6CC"
)

p <- ggplot(fuel_long_clean, aes(x = Year, y = pct, fill = Fuel)) +
  geom_area(position = "stack", color = NA) +
  geom_line(
    data = renewables_nuclear_df,
    aes(x = Year, y = total_share),
    inherit.aes = FALSE,
    color = "#FEBC11", linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 2046, y = 0.77,               
    label = "% CLEAN",
    color = "#FEBC11", fontface = "bold",
    family = "Avenir",
    hjust = 0
  ) +
  scale_fill_manual(values = fuel_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2024, 2050, 2)) +
  labs(
    title = "Projected U.S. Electricity Generation Mix",
    subtitle = "EIA Reference Case, AEO 2025",
    x = "Year",
    y = "Share of grid mix (%)",
    fill = "Fuel"
  ) +
  theme_minimal(base_family = "Avenir") +
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),   
    axis.ticks       = element_line(),    
    legend.position  = "right"
  )

print(p)

ggsave("misc report figures/output/grid_clean_percent.png", 
       width = 8,
       height = 6,
       units = "in") 

