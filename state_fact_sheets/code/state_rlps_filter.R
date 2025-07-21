library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(janitor)
library(tidyr)
library(showtext)

# Load NAICS descriptions
target_naics_df <- read_excel("state_fact_sheets/data/raw/target_NAICS.xlsx") %>%
  clean_names() %>%
  mutate(naics_code = as.character(naics_code))

# Load and filter GHGRP data
ghgrp_df_raw <- read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") 

ghgrp_state_raw <- ghgrp_df_raw %>%
  filter(state == "IL", year == 2023) %>% 
  rename(naics_code = primary_naics)

ghgrp_state_mod <- ghgrp_state_raw %>% 
  left_join(target_naics_df, by = "naics_code") %>%
  filter(!is.na(co2e_emission)) %>% 
  filter(str_sub(naics_code, 1, 2) %in% c("31", "32", "33") & !str_starts(naics_code, "324")) %>% 
  mutate(sector = case_when(
    str_starts(naics_code, "322") ~ "Pulp and Paper",
    str_starts(naics_code, "325") ~ "Chemicals",
    str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food and Beverage",
    TRUE ~ "Other"
  )) %>% 
  mutate(description = if_else(is.na(description), "Other Manufacturing", description)) %>% 
  mutate(description = if_else(naics_code == "325613", "Other Chemicals Manufacturing", description)) %>% 
  select(sector, naics_code, description, facility_id, facility_name, co2e_emission)

output_table <- ghgrp_state_mod %>%
  mutate(
    # Collapse Other Manufacturing into one category
    description = ifelse(sector == "Other", "Other Manufacturing", description),
    naics_code = ifelse(sector == "Other", NA, naics_code)
  ) %>%
  group_by(sector, description, naics_code) %>%
  summarise(co2e_total = sum(co2e_emission, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    percent_of_total = round(100 * co2e_total / sum(co2e_total), 2)
  ) %>%
  arrange(desc(sector)) %>%
  select(naics_code, description, co2e_total, percent_of_total, sector)

#### Emissions just in our scope ####
facility_scope_emissions_df <- ghgrp_state_raw %>% 
  inner_join(target_naics_df, by = "naics_code") %>% 
  filter(!is.na(co2e_emission)) %>%
  filter(str_sub(naics_code, 1, 2) %in% c("31", "32", "33") & !str_starts(naics_code, "324")) %>% 
  mutate(sector = case_when(
    str_starts(naics_code, "322") ~ "Pulp and Paper",
    str_starts(naics_code, "325") ~ "Chemicals",
    str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food and Beverage",
    TRUE ~ "Other"
  )) %>% 
  mutate(description = if_else(is.na(description), "Other Manufacturing", description)) %>% 
  mutate(description = if_else(naics_code == "325613", "Other Chemicals Manufacturing", description)) %>% 
  select(sector, naics_code, description, facility_id, facility_name, co2e_emission)

sector_scope_emissions_df <- facility_scope_emissions_df %>% 
  mutate(
    # Collapse Other Manufacturing into one category
    description = ifelse(sector == "Other", "Other Manufacturing", description),
    naics_code = ifelse(sector == "Other", NA, naics_code)
  ) %>%
  group_by(sector, description, naics_code) %>%
  summarise(co2e_total = sum(co2e_emission, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    percent_of_total_scope = round(100 * co2e_total / sum(co2e_total), 2)
  ) %>%
  arrange(desc(sector)) %>%
  select(naics_code, description, co2e_total, percent_of_total_scope, sector)

# Write to Excel
writexl::write_xlsx(output_table, "state_fact_sheets/data/modified/il_naics_emissions_percentages_2023.xlsx")
writexl::write_xlsx(ghgrp_state_mod, "state_fact_sheets/data/modified/il_rlps_ghgrp_naics_2023.xlsx")
writexl::write_xlsx(
  list(
    "In Scope Facility Emissions" = facility_scope_emissions_df,
    "In Scope Sector Emissions" = sector_scope_emissions_df
  ),
  path = "state_fact_sheets/data/modified/IL_in_scope_emissions.xlsx"
)
