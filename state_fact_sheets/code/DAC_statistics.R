library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(stringr)
library(grid)
library(janitor)
library(here)

##### Prep data #####

### Read in EJ indicator data
ej_indicators <- read_excel("state_fact_sheets/data/raw/facility_id_w_EJ_indicators_v1.xlsx") %>% 
  clean_names()

### match facility to naics code 
facility_info <- read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>% 
  clean_names() %>% 
  select(facility_id, co2e_emission, primary_naics)

ej_indicators_naics <- left_join(ej_indicators, facility_info, by = "facility_id") %>% 
  rename(naics = primary_naics)

### filter for state and naics
target_state <- "MN"
target_sectors <- c("311", "325", "322", "312")
target_naics <- c("311313","325193","322120")

state_ej_indicators <- ej_indicators_naics %>% 
  filter(state == target_state) 

target_sector_ej_indicators <- state_ej_indicators %>% 
  filter(str_sub(naics, 1, 3) %in% target_sectors)

##### DAC statistics #####

### All industrial sector

## % of facility IDs in DAC

# Ensure unique facility-level rows for count
facility_summary <- state_ej_indicators %>%
  distinct(facility_id, identified_as_disadvantaged)

# % of facility IDs in disadvantaged areas
pct_facilities_disadvantaged <- facility_summary %>%
  summarize(
    total = n(),
    disadvantaged = sum(identified_as_disadvantaged)
  ) %>%
  mutate(percent = disadvantaged / total * 100)

# % of CO2e emissions in DAC
pct_emissions_disadvantaged <- state_ej_indicators %>%
  group_by(identified_as_disadvantaged) %>%
  summarize(total_emissions = sum(co2e_emission, na.rm = TRUE)) %>%
  mutate(percent = total_emissions / sum(total_emissions) * 100)

### Just target sectors
# Ensure unique facility-level rows for count
target_sector_facility_summary <- target_sector_ej_indicators %>%
  distinct(facility_id, identified_as_disadvantaged)

# % of facility IDs in disadvantaged areas
target_sector_pct_facilities_disadvantaged <- target_sector_facility_summary %>%
  summarize(
    total = n(),
    disadvantaged = sum(identified_as_disadvantaged)
  ) %>%
  mutate(percent = disadvantaged / total * 100)

# % of CO2e emissions in DAC
target_sector_pct_emissions_disadvantaged <- target_sector_ej_indicators %>%
  group_by(identified_as_disadvantaged) %>%
  summarize(total_emissions = sum(co2e_emission, na.rm = TRUE)) %>%
  mutate(percent = total_emissions / sum(total_emissions) * 100)


