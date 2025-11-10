##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 3
# March 3, 2025               
# Ben Ladabaum                  
#                               
# Description:  Create dataset with unit level emissions and fuel level information
#
# Notes:    _v2: restructure dataset so that there is an additional column called "subpart". 
#                Since subpart AA and subpart C have different units, instead of merging, we will append.
#           _v3: update methodology for biogenic emissions: If unit has only one fuel type, use 
#                configuration level dataset for biogenic emissions. If there is more than one 
#                fuel type, use fuel level data
#           _v4: update biogenic emissions methodology: 1) count more fuels as biogenic (e.g., rendered 
#                 animal fat); 2) include tier 4 and p75 emissions from configuration level data
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

setwd("/Users/Ben L/Library/CloudStorage/Box-Box/Industrial Plant Raw Data/Industrial-Decarb-Database")


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

# facilities
facilities_data = read_excel("Data/rlps_ghg_emitter_facilities.xlsx") |>
  select(facility_id, primary_naics, year) |>
  rename(reporting_year = year) 
is_unique_id(facilities_data, c("facility_id", "reporting_year"))

# fuel level
fuel_level_data = read_excel("Data/Subpart C/Fuel_level_information.xlsx", col_types = "text") |>
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
                       "tier3_eq_c5_fuel_qty",
                       "facility_id",
                       "reporting_year"))

# Determine which facility-unit-fuel type combinations have more than one fuel type
num_fuels = fuel_level_data |>
  select(facility_id, reporting_year, unit_name, fuel_type) |>
  distinct() |>
  group_by(facility_id, reporting_year, unit_name) |>
  summarise(
    num_fuels = n()
  )

# tier 4 emissions
tier4_emissions = read_excel("Data/Subpart C/Configuration_level.xlsx") |>
  distinct() |>
  select(facility_id, reporting_year, unit_name, unit_type, part_75_co2_emissions_method,
         tier_4_biogenic_co2_emissions, tier4_nonbiogenic_co2emission) |>
  convert_to_numeric(c("part_75_co2_emissions_method", "tier_4_biogenic_co2_emissions", 
                       "tier4_nonbiogenic_co2emission")) |>
  left_join(facilities_data, by=c("facility_id", "reporting_year")) |>
  filter(!is.na(tier_4_biogenic_co2_emissions) | !is.na(tier4_nonbiogenic_co2emission)
         | !is.na(part_75_co2_emissions_method)) |>
  mutate(tier4_p75_nonbiogenic_co2emission =
           rowSums(across(c(tier4_nonbiogenic_co2emission, part_75_co2_emissions_method)), na.rm = TRUE)) |>
  left_join(y = num_fuels, 
            by=c("facility_id", "reporting_year", "unit_name")) 
is_unique_id(tier4_emissions, c("facility_id", "reporting_year", "unit_name"))


# biogenic emissions
biogenic_emissions = read_excel("Data/Subpart C/Configuration_level.xlsx") |>
  distinct() |>
  select(facility_id, reporting_year, unit_name, tier123_biogenic_co2_emissions) |>
  convert_to_numeric("tier123_biogenic_co2_emissions")
is_unique_id(biogenic_emissions, c("facility_id", "reporting_year", "unit_name"))




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
  group_by(facility_id, reporting_year, unit_name, unit_type, fuel_type, 
           fuel_type_blend, primary_naics) |>
  summarise(
    total_co2_emissions = sum(total_co2_emissions, na.rm = TRUE),
    total_ch4_emissions = sum(total_ch4_emissions, na.rm = TRUE),
    total_ch4_emissions_co2e = sum(total_ch4_emissions_co2e, na.rm = TRUE),
    total_n2o_emissions = sum(total_n2o_emissions, na.rm = TRUE),
    total_n2o_emissions_co2e = sum(total_n2o_emissions_co2e, na.rm = TRUE),
    total_fuel_quantity = sum(total_fuel_quantity, na.rm = TRUE)
  ) |>
  ungroup() |>
  left_join(y = num_fuels, 
            by=c("facility_id", "reporting_year", "unit_name")) |>
  left_join(y = biogenic_emissions, 
            by=c("facility_id", "reporting_year", "unit_name")) |>
  bind_rows(tier4_emissions) |>
  mutate(biogenic_co2_emissions = ifelse(fuel_type %in% (c("Wood and Wood Residuals (dry basis)",
                                                           "Wood and Wood Residuals",
                                                           "Biogas (Captured methane)",
                                                           "Other Biomass Gases",
                                                           "Agricultural Byproducts",
                                                           "Vegetable Oil", 
                                                           "Solid Byproducts", 
                                                           "Landfill Gas",
                                                           "Biodiesel",
                                                           "Biodiesel (100%)",
                                                           "Rendered Animal Fat")) | 
                                           fuel_type_blend %in% c("Biogenic Process Derived Fuel",
                                                                  "Biogeninc Process Derived Fuel (PDF)",
                                                                  "Biogenic Process Derived Fuel (PDF)",
                                                                  "Biogenic Process Derived Fuel (Glidfuel)",
                                                                  "Biomass",
                                                                  "Wood, wood oil, and wood pellets"
                                           ), 
                                         total_co2_emissions, 0)) |>
  mutate(biogenic_co2_emissions = ifelse(num_fuels==1, tier123_biogenic_co2_emissions, biogenic_co2_emissions)) |>
  mutate(nonbiogenic_co2_emissions = ifelse(is.na(total_co2_emissions), 0, total_co2_emissions) - 
           ifelse(is.na(biogenic_co2_emissions), 0, biogenic_co2_emissions)) |>
  mutate(biogenic_co2_emissions = ifelse(!is.na(tier_4_biogenic_co2_emissions), 
                                         tier_4_biogenic_co2_emissions, biogenic_co2_emissions)) |>
  mutate(nonbiogenic_co2_emissions = ifelse(!is.na(tier4_p75_nonbiogenic_co2emission), 
                                            tier4_p75_nonbiogenic_co2emission, nonbiogenic_co2_emissions)) |>
  select(-tier_4_biogenic_co2_emissions, -tier4_nonbiogenic_co2emission, 
         - tier123_biogenic_co2_emissions, -tier4_p75_nonbiogenic_co2emission) |>
  pivot_longer(cols = c("total_co2_emissions", "total_ch4_emissions", "total_ch4_emissions_co2e", 
                        "total_n2o_emissions", "total_n2o_emissions_co2e", "biogenic_co2_emissions",
                        "nonbiogenic_co2_emissions"), 
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
      ghg_gas_name == "nonbiogenic_co2_emissions" ~ "Carbon Dioxide Non-Biogenic"
    )
  )|>
  arrange(primary_naics, facility_id, reporting_year, unit_name, fuel_type, ghg_gas_name) |>
  group_by(primary_naics, facility_id, reporting_year, unit_type, unit_name, fuel_type, ghg_gas_name,
           total_fuel_quantity) |>
  summarise(ghg_quantity = sum(ghg_quantity, na.rm = TRUE)) |>
  ungroup() |>
  mutate(subpart = "C") |>
  mutate(capacity = "") |>
  mutate(capacity_utiliziation = "") |>
  mutate(vintage = "") |>
  mutate(floor_space = "") |>
  mutate(temperature = "") |>
  mutate(steam_generation_est = "") |>
  mutate(electricity_generation_est = "") |>
  mutate(prime_mover_type = "") |>
  mutate(hrsg_bypass = "")

# have to save locally on my computer because too big for github and box is no longer working
write_csv(emissions_by_unit, "/Users/Ben L/Documents/UCSB Bren/Research/2035 Initiative Industrial Decarbonization/Plant and Unit Level Dataset/Output/subpart_c_emissions_and_fuel_by_unit_v4.csv")

