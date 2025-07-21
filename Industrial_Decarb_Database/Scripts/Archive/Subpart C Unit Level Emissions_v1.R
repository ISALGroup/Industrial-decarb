##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# February 10, 2024               
# Ben Ladabaum                  
#                               
# Description:  Create datast with unit level emissions and fuel level information
#
# Notes:  
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


### Functions ###
# test whether set of variables are unique identifiers
is_unique_id <- function(data, vars) {
  n_unique <- nrow(unique(data[vars]))
  n_total <- nrow(data)
  n_unique == n_total
}

# convert variables from string to numeric
convert_to_numeric <- function(data, columns) {
  data[columns] <- lapply(data[columns], function(col) as.numeric(as.character(col)))
  return(data)
}


### Load Data ###

# Steam Monthly Inputs
steam_monthly_inputs = read_excel(here("Data/Subpart C", "Eqc2_monthly_inputs_steam.xlsx")) 
is_unique_id(steam_monthly_inputs, c("facility_id", "reporting_year", "unit_name",
                                     "month", "fuel_type"))

facilities_data = read_excel(here("Data", "rlps_ghg_emitter_facilities.xlsx")) |>
  select(facility_id, primary_naics, year) |>
  rename(reporting_year = year) 
is_unique_id(facilities_data, c("facility_id", "reporting_year"))

# biogenic emissions
biogenic_emissions = read_excel(here("Data/Subpart C", "Configuration_level.xlsx")) |>
  distinct() |>
  select(facility_id, reporting_year, unit_name, tier123_biogenic_co2_emissions) |>
  convert_to_numeric("tier123_biogenic_co2_emissions")
is_unique_id(biogenic_emissions, c("facility_id", "reporting_year", "unit_name"))

fuel_level_data = read_excel(here("Data/Subpart C", "Fuel_level_information.xlsx")) |>
  convert_to_numeric(c("tier1_co2_combustion_emissions", "tier2_co2_combustion_emissions", 
                       "tier3_co2_combustion_emissions", "tier1_ch4_emissions_co2e",
                       "tier2_ch4_emissions_co2e", "tier3_ch4_emissions_co2e", 
                       "tier4_ch4_emissions_co2e", "tier1_ch4_combustion_emissions",
                       "tier2_ch4_combustion_emissions", "tier3_ch4_combustion_emissions",
                       "t4ch4combustionemissions", "tier1_n2o_emissions_co2e",
                       "tier2_n2o_emissions_co2e", "tier3_n2o_emissions_co2e",
                       "tier4_n2o_emissions_co2e", "tier1_n2o_combustion_emissions",
                       "tier2_n2o_combustion_emissions", "tier3_n2o_combustion_emissions",
                       "t4n2ocombustionemissions", "tier4_fuel_quantity", 
                       "tier1_fuel_quantity", "tier2_eq_c2a_fuel_qty",
                       "tier3_eq_c3_fuel_qty", "tier3_eq_c4_fuel_qty",
                       "tier3_eq_c5_fuel_qty"))

### Combine Data into one dataset ###
fuel_level_data_w_naics = left_join(x = fuel_level_data, facilities_data, by=c("facility_id", "reporting_year")) |>
  mutate(total_co2_emissions = rowSums(cbind(tier1_co2_combustion_emissions, tier2_co2_combustion_emissions, 
                                             tier3_co2_combustion_emissions), na.rm = TRUE)) |>
  mutate(total_ch4_emissions = rowSums(cbind(tier1_ch4_combustion_emissions, tier2_ch4_combustion_emissions, 
                                             tier3_ch4_combustion_emissions, t4ch4combustionemissions), na.rm = TRUE)) |>
  mutate(total_ch4_emissions_co2e = rowSums(cbind(tier1_ch4_emissions_co2e, tier2_ch4_emissions_co2e, 
                                                  tier3_ch4_emissions_co2e, tier4_ch4_emissions_co2e), na.rm = TRUE)) |>
  mutate(total_n2o_emissions = rowSums(cbind(tier1_n2o_combustion_emissions, tier2_n2o_combustion_emissions, 
                                             tier3_n2o_combustion_emissions, t4n2ocombustionemissions), na.rm = TRUE)) |>
  mutate(total_n2o_emissions_co2e = rowSums(cbind(tier1_n2o_emissions_co2e, tier2_n2o_emissions_co2e, 
                                                  tier3_n2o_emissions_co2e, tier4_n2o_emissions_co2e), na.rm = TRUE)) |>
  mutate(total_fuel_quantity = rowSums(cbind(tier1_fuel_quantity, tier2_eq_c2a_fuel_qty, 
                                             tier3_eq_c3_fuel_qty, tier3_eq_c4_fuel_qty,
                                             tier3_eq_c5_fuel_qty, tier4_fuel_quantity), na.rm = TRUE))


emissions_by_unit <- fuel_level_data_w_naics |> 
  group_by(facility_id, reporting_year, unit_name, unit_type, fuel_type, primary_naics) |>
  summarise(
    total_co2_emissions = sum(total_co2_emissions, na.rm = TRUE),
    total_ch4_emissions = sum(total_ch4_emissions, na.rm = TRUE),
    total_ch4_emissions_co2e = sum(total_ch4_emissions_co2e, na.rm = TRUE),
    total_n2o_emissions = sum(total_n2o_emissions, na.rm = TRUE),
    total_n2o_emissions_co2e = sum(total_n2o_emissions_co2e, na.rm = TRUE),
    total_fuel_quantity = sum(total_fuel_quantity, na.rm = TRUE)
  ) |>
  ungroup() |>
  left_join(y = biogenic_emissions, 
            by=c("facility_id", "reporting_year", "unit_name")) |>
  mutate(biogenic_co2_emissions = ifelse(fuel_type %in% (c("Wood and Wood Residuals (dry basis)",
                                                           "Wood and Wood Residuals",
                                                           "Biogas (Captured methane)",
                                                           "Other Biomass Gases",
                                                           "Agricultural Byproducts",
                                                           "Vegetable Oil", 
                                                           "Solid Byproducts", 
                                                           "Landfill Gas")), 
                                         total_co2_emissions, 0)) |>
  mutate(nonbiogenic_co2_emissions = ifelse(is.na(total_co2_emissions), 0, total_co2_emissions) - 
           ifelse(is.na(biogenic_co2_emissions), 0, biogenic_co2_emissions)) |>
  pivot_longer(cols = c("total_co2_emissions", "total_ch4_emissions", "total_ch4_emissions_co2e", 
                        "total_n2o_emissions", "total_n2o_emissions_co2e", "biogenic_co2_emissions",
                        "nonbiogenic_co2_emissions", "tier123_biogenic_co2_emissions"), 
               names_to = "ghg_gas_name",
               values_to = "ghg_quantity") |>
  mutate(
    ghg_gas_name = case_when(
      ghg_gas_name == "total_co2_emissions" ~ "Carbon Dioxide Total",
      ghg_gas_name == "total_ch4_emissions" ~ "Methane",
      ghg_gas_name == "total_ch4_emissions_co2e" ~ "Methane (Co2 eq)",
      ghg_gas_name == "total_n2o_emissions" ~ "Nitrous Oxide",
      ghg_gas_name == "total_n2o_emissions_co2e" ~ "Nitrous Oxide (Co2 eq)",
      ghg_gas_name == "biogenic_co2_emissions" ~ "Carbon Dioxide Biogenic",
      ghg_gas_name == "nonbiogenic_co2_emissions" ~ "Carbon Dioxide Non-Biogenic",
      ghg_gas_name == "tier123_biogenic_co2_emissions" ~ "Co2 Biogenic (facility level from configuration data)"
    )
  )|>
  #filter(ghg_gas_name!="Co2 Biogenic (facility level from configuration data)")|>
  arrange(primary_naics, facility_id, reporting_year, unit_name, fuel_type, ghg_gas_name)|>
  select(primary_naics, facility_id, reporting_year, unit_type, unit_name, fuel_type, ghg_gas_name, ghg_quantity,
         total_fuel_quantity) |>
  mutate(capacity = "") |>
  mutate(capacity_utiliziation = "") |>
  mutate(vintage = "") |>
  mutate(floor_space = "") |>
  mutate(temperature = "") |>
  mutate(steam_generation_est = "") |>
  mutate(electricity_generation_est = "") |>
  mutate(prime_mover_type = "") |>
  mutate(hrsg_bypass = "") |>
  rename(ghg_quantity_subpart_c = ghg_quantity)


write_csv(emissions_by_unit, path = here("Output", "subpart_c_emissions_and_fuel_by_unit_v1.csv"))

