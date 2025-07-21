##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 1
# February 26, 2024               
# Ben Ladabaum                  
#                               
# Description:  Consistency checks for emissions data
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

setwd("/Users/Ben L/Library/CloudStorage/Box-Box/Industrial Plant Raw Data/Industrial-Decarb-Database")

### Functions ###
# test whether set of variables are unique identifiers
is_unique_id <- function(data, vars) {
  n_unique <- nrow(unique(data[vars]))
  n_total <- nrow(data)
  n_unique == n_total
}

### Load data ###

# facility level data
facility_level_data = read_excel("Output/subpart_c_emissions_by_facility_v2.xlsx") |>
  select(facility_id, reporting_year, primary_naics, carbon_dioxide_subpart_c,
         biogenic_co2_subpart_c, methane_subpart_c, nitrous_oxide_subpart_c) |>
  pivot_longer(cols = c("carbon_dioxide_subpart_c", "biogenic_co2_subpart_c", 
                        "methane_subpart_c", "nitrous_oxide_subpart_c"), 
               names_to = "ghg_gas_name",
               values_to = "ghg_quantity") |>
  filter(!is.na(ghg_quantity)) |>
  mutate(
    ghg_gas_name = case_when(
      ghg_gas_name == "carbon_dioxide_subpart_c" ~ "Carbon Dioxide",
      ghg_gas_name == "methane_subpart_c" ~ "Methane",
      ghg_gas_name == "biogenic_co2_subpart_c" ~ "Biogenic Carbon dioxide",
      ghg_gas_name == "nitrous_oxide_subpart_c" ~ "Nitrous Oxide"
    )
  )|>
  rename(ghg_quantity_facility_level = ghg_quantity)

# unit level data
unit_level = read.csv("Output/subpart_c_emissions_and_fuel_by_unit_v3.csv") |>
  group_by(facility_id, reporting_year, primary_naics, ghg_gas_name) |>
  summarise(
    ghg_quantity = sum(ghg_quantity, na.rm = TRUE)
  ) |>
  filter(ghg_quantity!=0) |>
  filter(ghg_gas_name %in% c("Carbon Dioxide Non-Biogenic", "Methane", 
                             "Nitrous Oxide", "Carbon Dioxide Biogenic")) |>
  mutate(
    ghg_gas_name = case_when(
      ghg_gas_name == "Carbon Dioxide Non-Biogenic" ~ "Carbon Dioxide",
      ghg_gas_name == "Methane" ~ "Methane",
      ghg_gas_name == "Nitrous Oxide" ~ "Nitrous Oxide",
      ghg_gas_name == "Carbon Dioxide Biogenic" ~ "Biogenic Carbon dioxide"
    )
  )|>
  mutate(primary_naics = as.character(primary_naics))|>
  rename(ghg_quantity_unit_level = ghg_quantity)


###################### Consistency Checks ##################

### Do sum of unit level emissions equal facility level emissions? ###
facility_emissions_comparison = full_join(x = facility_level_data, y = unit_level,
                                          by = c("facility_id", "reporting_year", 
                                                 "primary_naics", "ghg_gas_name")) |>
  mutate(difference = ghg_quantity_facility_level - ghg_quantity_unit_level)





### Are biogenic emissions consistent across data sets? ###

# # fuel level data
# biogenic_emissions_fuel_data = fuel_level_data |>
#   mutate(total_co2_emissions = rowSums(cbind(tier1_co2_combustion_emissions, tier2_co2_combustion_emissions, 
#                                              tier3_co2_combustion_emissions), na.rm = TRUE)) |>
#   mutate(biogenic_co2_emissions = ifelse(fuel_type %in% (c("Wood and Wood Residuals (dry basis)",
#                                                            "Wood and Wood Residuals",
#                                                            "Biogas (Captured methane)",
#                                                            "Other Biomass Gases",
#                                                            "Agricultural Byproducts",
#                                                            "Vegetable Oil", 
#                                                            "Solid Byproducts", 
#                                                            "Landfill Gas")), 
#                                          total_co2_emissions, 0)) |>
#   group_by(facility_id, reporting_year) |>
#   summarise(
#     biogenic_co2_emissions = sum(biogenic_co2_emissions, na.rm = TRUE),
#   ) |>
#   ungroup() |>
#   filter(biogenic_co2_emissions!=0) |>
#   rename(bio_co2_emissions_fuel_data = biogenic_co2_emissions)
# 
# # configuration level data
# biogenic_emissions_config_level = biogenic_emissions |>
#   group_by(facility_id, reporting_year) |>
#   summarise(
#     bio_co2_emissions_conf_level = sum(bio_co2_emissions_conf_level)
#   ) |> 
#   ungroup() |>
#   filter(bio_co2_emissions_conf_level!=0)
# 
# # subpart level data
# biogenic_emissions_subpart_data = subpart_level_data |>
#   filter(ghg_gas_name == "Biogenic Carbon dioxide") |>
#   select(facility_id, ghg_quantity_subpart_data, reporting_year) |>
#   rename(bio_co2_emissions_subpart_data = ghg_quantity_subpart_data)
# 
# # join together
# biogenic_comparison = left_join(x = biogenic_emissions_subpart_data, 
#                                 y = biogenic_emissions_config_level,
#                                 by = c("facility_id", "reporting_year")) |>
#   left_join(y = biogenic_emissions_fuel_data,
#             by = c("facility_id", "reporting_year")) |>
#   left_join(y = facilities_data,
#             by = c("facility_id", "reporting_year")) |>
#   select(primary_naics, facility_id, reporting_year, 
#          bio_co2_emissions_subpart_data, bio_co2_emissions_conf_level, bio_co2_emissions_fuel_data) |>
#   arrange(primary_naics, facility_id, reporting_year)




### Export ###
emissions_consistency_checks <- createWorkbook()

addWorksheet(emissions_consistency_checks, "facility_emissions")
writeData(emissions_consistency_checks, "facility_emissions", facility_emissions_comparison)

# addWorksheet(emissions_consistency_checks, "biogenic_emissions")
# writeData(emissions_consistency_checks, "biogenic_emissions", biogenic_comparison)

saveWorkbook(emissions_consistency_checks, here("Output", "emissions_consistency_checks.xlsx"), 
             overwrite = TRUE)


