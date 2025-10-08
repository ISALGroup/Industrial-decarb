# August 1. 2025
# EMT
# Filters GHGRP rlps data for a specific state and naics codes
# Creates emissions donut figure for state fact sheets


library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(janitor)
library(tidyr)

setwd("~/Documents/Industrial_Decarbonization/Industrial-decarb")
# ---------- CONFIG ----------
target_state <- "MN"
target_year <- 2023
states_of_interest <- c("IL", "MI", "MD", "MN", "PA", "CO", "OR")

sector_colors <- c(
  "Food & Beverage" = "#047C91",  # 311, 312
  "Chemicals" = "#FEBC11",        # 325
  "Pulp & Paper" = "#6D7D33",     # 322
  "Other Manufacturing" = "#DCE1E5"
)

# ---------- LOAD DATA ----------
ghgrp_raw_df <- read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>%
  clean_names() %>% 
  filter(year == target_year) %>% 
  mutate(naics_code = as.character(primary_naics)) %>%
  filter(str_starts(naics_code, "31") | str_starts(naics_code, "32") | str_starts(naics_code, "33")) %>%
  filter(!str_starts(naics_code, "324")) %>%
  filter(!is.na(co2e_emission)) 

target_naics <- read_excel("state_fact_sheets/data/raw/target_NAICS.xlsx") %>%
  clean_names() %>%
  mutate(naics_code = as.character(naics_code))

# --------- MODEL PROGRESS BY STATE OF INTEREST -------------

ghgrp_with_status <- ghgrp_raw_df %>%
  left_join(target_naics, by = "naics_code") %>%
  mutate(
    in_target = !is.na(status)
  ) %>% 
  filter(state %in% states_of_interest)

state_summary <- ghgrp_with_status %>%
  group_by(state) %>%
  summarise(
    total_emissions = sum(co2e_emission, na.rm = TRUE),
    target_emissions = sum(co2e_emission[in_target], na.rm = TRUE),
    done_emissions = sum(co2e_emission[status == "Done"], na.rm = TRUE),
    ip_or_done_emissions = sum(co2e_emission[in_target & status %in% c("Done", "In Progress")], na.rm = TRUE)
  ) %>%
  mutate(
    pct_target_of_total = round(100 * target_emissions / total_emissions, 1),
    pct_done_of_target = round(100 * done_emissions / target_emissions, 1),
    pct_ip_or_done_of_target = round(100 * ip_or_done_emissions / target_emissions, 1),
  ) %>%
  select(state, pct_target_of_total, pct_done_of_target, pct_ip_or_done_of_target) %>% 
  filter(state %in% states_of_interest)


in_progress_contributions <- ghgrp_with_status %>%
  filter(status == "In Progress") %>%
  group_by(state, naics_code, description) %>%
  summarise(in_progress_emissions = sum(co2e_emission), .groups = "drop") %>%
  left_join(
    ghgrp_with_status %>%
      filter(in_target) %>%
      group_by(state) %>%
      summarise(target_total = sum(co2e_emission), .groups = "drop"),
    by = "state"
  ) %>%
  mutate(
    pct_of_target = round(100 * in_progress_emissions / target_total, 1)
  ) %>%
  select(state, naics_code, description, pct_of_target) %>% 
  arrange(desc(pct_of_target))

# ---------- MAP SECTORS BY SELECTED STATE ----------

state_ghgrp_df <- ghgrp_raw_df %>%
  filter(state == target_state) %>%
  select(naics_code, co2e_emission, facility_id, facility_name)

mapped_ghgrp_df <- state_ghgrp_df %>%
  mutate(
    sector = case_when(
      str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food & Beverage",
      str_starts(naics_code, "325") ~ "Chemicals",
      str_starts(naics_code, "322") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    )
  ) %>%
  left_join(target_naics, by = "naics_code") %>%
  mutate(
    description = if_else(is.na(description), "Other Manufacturing", description),
    naics_code = if_else(sector == "Other Manufacturing", NA, naics_code)
  ) %>% 
  mutate(
    # Sector labels already assigned earlier — now correct ambiguous "Other Manufacturing" descriptions
    description = case_when(
      description == "Other Manufacturing" & str_starts(naics_code, "311") ~ "Other Food & Beverage Manufacturing",
      description == "Other Manufacturing" & str_starts(naics_code, "312") ~ "Other Food & Beverage Manufacturing",
      description == "Other Manufacturing" & str_starts(naics_code, "322") ~ "Other Pulp & Paper Manufacturing",
      description == "Other Manufacturing" & str_starts(naics_code, "325") ~ "Other Chemicals Manufacturing",
      TRUE ~ description
    )
  )

# ---------- CREATE OUTPUT TABLE ----------
# Force sector order so "Other Manufacturing" is last
ordered_sectors <- c("Food & Beverage", "Chemicals", "Pulp & Paper", "Other Manufacturing")

output_table <- mapped_ghgrp_df %>%
  group_by(sector, description, naics_code) %>%
  summarise(co2e_total = sum(co2e_emission), .groups = "drop") %>%
  mutate(percent_of_total = round(100 * co2e_total / sum(co2e_total), 2)) %>%
  arrange(desc(sector)) %>% 
  mutate(sector = factor(sector, levels = ordered_sectors))

# ---------- AGGREGATE FOR DONUT BY NAICS ----------
donut_data <- output_table %>%
  group_by(sector, description) %>%
  summarise(emission = sum(co2e_total), .groups = "drop") %>%
  mutate(
    pct = emission / sum(emission) * 100
  ) %>%
  arrange(factor(sector, levels = names(sector_colors)), desc(emission)) %>%
  mutate(slice_order = row_number())


# ---------- DONUT CHART ----------
donut_chart <- ggplot(donut_data, aes(x = 2, y = emission, fill = sector)) +
  geom_bar(
    stat = "identity",
    width = 1,
    color = "white",
    size = 0.3,
    aes(group = reorder(description, slice_order))
  ) +
  coord_polar(theta = "y", start = pi/2) +
  scale_fill_manual(values = sector_colors, name = "Sector") +
  theme_void() +
  xlim(0.5, 2.5) +
  ggtitle(paste(target_state, "Manufacturing CO2 Emissions by NAICS (", target_year, ")", sep = "")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold", margin = margin(t = 10, b = 20)),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(5, 5, 5, 5)
  )

print(donut_chart)


# ---------- OUTPUTS ----------
writexl::write_xlsx(output_table, "state_fact_sheets/data/modified/mn_emissions_summary_250805.xlsx")
writexl::write_xlsx(naics_coverage_by_state, "state_fact_sheets/data/modified/percent_emissions_models_finished_.xlsx")
writexl::write_xlsx(output_table2, "state_fact_sheets/data/modified/mi_emissions_finished_naics_summary_250730_.xlsx")
ggsave("state_fact_sheets/outputs/mn_donut_chart_250805.png", donut_chart, width = 8, height = 6, dpi = 300)


# ------- TOOLING AROUND --------




