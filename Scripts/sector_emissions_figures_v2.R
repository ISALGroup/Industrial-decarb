##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 2
# November 5, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create the following figures for National Policy Roadmap:
#                 1) Industrial Sector’s Share of U.S. Energy-Related CO2 Emissions
#                 2) Share of target subsector emissions covered by the ghgrp facilities
#           
#
# Notes:    See EIA's "Model Documentation Report: Industrial Demand Module of the National 
#           Energy Modeling System" for EIA category to NAICS code classification
#               https://www.eia.gov/outlooks/aeo/nems/documentation/industrial/pdf/IDM_AEO2025.pdf
#
#           v2: change data sources for charts; 
#             1) use MECS instead of EIA for subsector level bar chart
#             2) use EIA data that separates out electric power sector
# 
# Inputs:   
#         
#
# Outputs:   
#
##################################################################

# Clean the environment
rm(list=ls())

# Load libraries
library(here)
library(janitor) 
library(readxl)
library(writexl)
library(tidyverse)
library(openxlsx)
library(readr)
library(ggplot2)
library(patchwork)
library(grid)
library(scales)


###### load data #####
setwd("/Users/Ben L/Documents/UCSB Bren/Research/2035 Initiative Industrial Decarbonization/Industrial-decarb")

# EIA Data
eia_co2_sector_emissions = read_excel("misc report figures/data/EIA/2024_Figure_1_energy_related_emissions_by_sector.xlsx", 
                                      skip = 2, sheet="Figure 1") |>
  rename(sector = "...1") |>
  rename(emissions_2024 = "2024") |>
  select(sector, emissions_2024) |>
  filter(sector != "total") |>
  mutate(
    sector = case_when(
      sector=="residential" ~ "Residential",
      sector=="commercial" ~ "Commercial",
      sector=="transportation" ~ "Transportation",
      sector=="industrial" ~ "Industrial",
      sector=="electric power" ~ "Electric Power"
    ) 
  ) |>
  mutate(
    fraction = emissions_2024 / sum(emissions_2024),        # fraction of total
    label = paste0(sector, " (", round(fraction * 100, 0), "%)") # label for chart
  ) |>
  mutate(sort_order =  case_when(
    sector %in% c("Industrial") ~ 2, 
    TRUE ~ 1                           
  )) |>
  arrange(sort_order, -fraction) |>
  mutate(sector_f = factor(sector, levels = unique(sector))) |>
  mutate(label_vjust = ifelse(sector_f == "Commercial", 0.9, 0.5)) |>
  mutate(
    label_angle = case_when(
      sector == "Commercial" ~ -40,   # Tilt slightly downward
      sector == "Residential" ~ -55,   # Tilt slightly upward
      TRUE ~ 0                       # All other labels stay horizontal
    )
  )

# MECS data
mecs = read_excel("misc report figures/data/EIA/MECS_onsite_emissions_subsector.xlsx") |>
  select(-naics_codes) |>
  filter(subsector!="All Manufacturing") |>
  mutate(subsector = case_when(
    str_detect(subsector, "Non-Manufacturing") ~ 
      paste("Non-Manufacturing Industrial:\n agriculture, mining, and construction"), 
    TRUE ~ subsector                                 # everything else -> Other
  )) |>
  mutate(sort_order =  case_when(
    subsector %in% c("Other Manufacturing") ~ 2, 
    str_detect(subsector, "Non-Manufacturing") ~ 2,
    TRUE ~ 1                           
  )) |>
  arrange(sort_order, -share_industrial_emissions) |>
  mutate(subsector_f = factor(subsector, levels = unique(subsector))) 




########## Charts ##########

pie = ggplot(eia_co2_sector_emissions, aes(x = "", y = emissions_2024, fill = sector_f, 
                                           angle = label_angle)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = pi/4) +
  geom_text(aes(label = label, vjust = label_vjust),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 2.8) +
  theme_void() +
  scale_fill_manual(values = c("Industrial" = "#FEBC11", "Commercial" = "#9CBEBE", 
                               "Residential" = "#9CBEBE", "Transportation" = "#9CBEBE",
                               "Electric Power" = "#9CBEBE")) +
  theme(legend.position = "none")

bar = ggplot(mecs, aes(x = "Percent Industrial Emissions", y = onsite_emissions_MMT_co2e, 
                       fill = subsector_f)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(
    aes(label = scales::percent(share_industrial_emissions, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "End Use"
  ) +
  theme_void() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual( name = "Subsector",
                     values = c(
                       "Chemicals" = "#09847A",
                       "Food & Beverage" = "#EF5645",
                       "Pulp & Paper" = "#6D7D33",
                       "Other Manufacturing" = "gray80",
                       "Non-Manufacturing Industrial:\n agriculture, mining, and construction" = "darkgrey"
                     ))

combined = pie + bar + plot_layout(widths = c(5, 1)) + 
  plot_annotation(
    #title = "Energy-Related CO2 emissions, 2024", 
    #subtitle = "million metric tons",
    theme = theme(
      text = element_text(family = "Avenir"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),
    )
  )
combined
grid.lines(x = unit(c(0.432, 0.55), "npc"), y = unit(c(0.71, 0.93), "npc"), gp = gpar(col = "black", lwd = 2, lty=2))
grid.lines(x = unit(c(0.466, 0.55), "npc"), y = unit(c(0.365, 0.065), "npc"), gp = gpar(col = "black", lwd = 2, lty=2))



ghgrp_plot = ggplot(ghgrp_emissions_2023, aes(x = subsector, y = emissions, fill = source)) +
  geom_bar(stat = "identity", color = "white", width = 0.7, position = "fill") +
  geom_text(aes(label = label), position = position_fill(vjust = 0.5), color = "white", size = 3.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("ghgrp_share" = "#003660", "other_share" = "#DCD6CC"),
    labels = c("GHGRP Facilities CO2 Emissions", "Other Facilities CO2 Emissions")
  ) +
  labs(
    x = "Sector",
    y = "Share of Emissions",
    fill = "",
    title = "Share of sector Emissions Covered by GHGRP"
  ) +
  theme_bw() + 
  theme(text = element_text(family = "Avenir"))
ghgrp_plot
ggsave("misc report figures/output/ghgrp_emissions_share_by_sector.png", plot = ghgrp_plot, 
       width = 6, height = 4, dpi = 300)









### Verify in-text numbers
ghgrp_2023_facilities = read_excel("Industrial_Decarb_Database/Databases/Facility_and_Unit_Emissions_Database_2023_v4.xlsx", 
                                   sheet="descr_info") |>
  filter(year==2023) |>
  filter(primary_naics %in% c(311221, 325193, 311313, 322120, 311224,
                              325110, 325311, 312140, 311611, 311225,
                              325180, 325194, 322130, 322110, 311421,
                              325211, 311513, 311514, 311314, 311942,
                              311613, 312120, 325120, 311423, 325312, 325212,
                              311411, 311615, 322291, 311511, 311422, 311919, 311230))
length(unique(ghgrp_2023_facilities$facility_id)) #1026 facilities

ghgrp_2023 = read_excel("Industrial_Decarb_Database/Databases/Facility_and_Unit_Emissions_Database_v4.xlsx", 
                        sheet="facility_emissions") |>
  filter(reporting_year==2023) |>
  filter(primary_naics %in% c(311221, 325193, 311313, 322120, 311224,
                              325110, 325311, 312140, 311611, 311225,
                              325180, 325194, 322130, 322110, 311421,
                              325211, 311513, 311514, 311314, 311942,
                              311613, 312120, 325120, 311423, 325312, 325212,
                              311411, 311615, 322291, 311511, 311422, 311919, 311230)) |>
  mutate(
    co2e_total = coalesce(carbon_dioxide_subpart_c, 0)  +
      coalesce(methane_subpart_c, 0) * 28 +
      coalesce(nitrous_oxide_subpart_c, 0) * 265
  )


ghgrp_2023_unit = read_excel("Industrial_Decarb_Database/Databases/Facility_and_Unit_Emissions_Database_v4.xlsx", 
                        sheet="unit_emissions") |>
  filter(reporting_year==2023 & subpart %in% c("C", "AA")) |>
  filter(primary_naics %in% c(311221, 325193, 311313, 322120, 311224,
                              325110, 325311, 312140, 311611, 311225,
                              325180, 325194, 322130, 322110, 311421,
                              325211, 311513, 311514, 311314, 311942,
                              311613, 312120, 325120, 311423, 325312, 325212,
                              311411, 311615, 322291, 311511, 311422, 311919, 311230)) |>
  filter(ghg_name %in% c("Nitrous Oxide", "Methane", "Carbon Dioxide Biogenic", 
                         "Methane Spent Liquor", "Nitrous Oxide Spent Liquor",
                         "Carbon Dioxide Biogenic (Spent Liquor)", "Carbon Dioxide Non-Biogenic")) |>
  mutate(
    co2e = case_when(
      str_detect(ghg_name, "Carbon Dioxide") ~ coalesce(ghg_quantity, 0),
      str_detect(ghg_name, "Methane") ~ coalesce(ghg_quantity, 0) * 28,
      str_detect(ghg_name, "Nitrous Oxide") ~ coalesce(ghg_quantity, 0) *  265
    )
  )

# total_co2e_subpart_c = ghgrp_2023 |>
#   summarize(total_co2e_subpart_c = sum(co2e_total, na.rm = TRUE)) |> #139 MMT co2e
#   mutate(total_co2e_subpart_c = total_co2e_subpart_c / 1000000) |>
#   mutate(id = 1)

total_co2e_subpart_c = ghgrp_2023_unit |>
  summarize(total_co2e_subpart_c = sum(co2e, na.rm = TRUE)) |> #120 MMT co2e
  mutate(total_co2e_subpart_c = total_co2e_subpart_c / 1000000) |>
  mutate(id = 1)

total_co2_eia_sectors = eia_co2_industrial_emissions |> 
  filter(subsector %in% c("Chemicals", "Food & Beverage", "Pulp & Paper")) |>
  summarise(emissions_relevant_subsectors = sum(eia_MMmtco2_2023, na.rm = TRUE)) |>
  mutate(id = 1) |>
  left_join(y = total_co2e_subpart_c, by = c("id")) |>
  mutate(ghgrp_share = total_co2e_subpart_c/emissions_relevant_subsectors)  #35% based on facility
                                                                            #30% based on unit



