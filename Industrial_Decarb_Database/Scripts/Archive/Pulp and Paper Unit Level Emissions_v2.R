##################################################################
#                               
# Project 2035: Industrial Decarb
# 
# Version: 2
# February 24, 2024               
# Ben Ladabaum                  
#                               
# Description:  Create datast with unit-level process emissions for pulp and paper
#
# Notes:  _v2: append subpart AA emissions instead of merging. Subpart C and subpart AA 
#              have different unit names.
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

# Facilities data
facilities_data = read_excel(here("Data", "rlps_ghg_emitter_facilities.xlsx")) |>
  rename(reporting_year = year) |>
  select(facility_id, reporting_year, primary_naics, secondary_naics)
is_unique_id(facilities_data, c("facility_id", "reporting_year"))

#spent liquor
spent_liquor = read_excel(here("Data/Subpart AA", "aa_spent_liquor_information.xlsx")) |>
  convert_to_numeric(c("biomass_ch4_emissions_factor", "biomass_n2o_emissions_factor"))
is_unique_id(spent_liquor, c("facility_id", "reporting_year", "unit_name"))

# fuel data
fossil_fuel_data = read_excel(here("Data/Subpart AA", "aa_fossil_fuel_information.xlsx")) 
is_unique_id(fossil_fuel_data, c("facility_id", "reporting_year", "unit_name", "fuel_type"))


### Process Emissions ###
spent_liquor_emissions = spent_liquor |>
  rename(biogenic_spent_liquor_co2_emissions = spent_liquor_co2_emissions) |>
  select(facility_id, reporting_year, starts_with("spent_liquor"), biogenic_spent_liquor_co2_emissions, 
         unit_name, unit_type)

emissions_by_unit = left_join(x = fossil_fuel_data, 
                                     facilities_data, by=c("facility_id", "reporting_year"))|>
  filter(primary_naics %in% c("322110", "322120", "322130")) |>
  left_join(y = spent_liquor_emissions, by = c("facility_id", "reporting_year", "unit_name")) |>
  mutate(fossil_fuel_co2_emissions = rowSums(cbind(tier_1_co2_emissions, tier_2_co2_emissions, 
                                                   tier_3_co2_emissions), na.rm = TRUE)) |>
  mutate(fossil_fuel_ch4_emissions = rowSums(cbind(tier_1_ch4_emissions, tier_2_ch4_emissions, 
                                                   tier_3_ch4_emissions), na.rm = TRUE)) |>
  mutate(fossil_fuel_ch4_emissions_co2e = rowSums(cbind(tier_1_ch4_emissions_co2e, tier_2_ch4_emissions_co2e, 
                                                        tier_3_ch4_emissions_co2e), na.rm = TRUE)) |>
  mutate(fossil_fuel_n2o_emissions = rowSums(cbind(tier_1_n2o_emissions, tier_2_n2o_emissions, 
                                                   tier_3_n2o_emissions), na.rm = TRUE)) |>
  mutate(fossil_fuel_n2o_emissions_co2e = rowSums(cbind(tier_1_n2o_emissions_co2e, tier_2_n2o_emissions_co2e, 
                                                        tier_3_n2o_emissions_co2e), na.rm = TRUE)) |>
  select(primary_naics, facility_id, reporting_year, unit_name, unit_type, fuel_type, 
         fossil_fuel_ch4_emissions, fossil_fuel_ch4_emissions_co2e, 
         fossil_fuel_co2_emissions, biogenic_spent_liquor_co2_emissions, 
         fossil_fuel_n2o_emissions, fossil_fuel_n2o_emissions_co2e, starts_with("spent_liquor")) |>
  pivot_longer(cols = c("fossil_fuel_ch4_emissions", "fossil_fuel_ch4_emissions_co2e", 
                        "fossil_fuel_co2_emissions", "biogenic_spent_liquor_co2_emissions", 
                        "fossil_fuel_n2o_emissions", "fossil_fuel_n2o_emissions_co2e",
                        "spent_liquor_ch4_emissions", "spent_liquor_n2o_emissions"), 
               names_to = "ghg_gas_name",
               values_to = "ghg_quantity")|>
  mutate(
    ghg_gas_name = case_when(
      ghg_gas_name == "fossil_fuel_co2_emissions" ~ "Carbon Dioxide Non-Biogenic",
      ghg_gas_name == "fossil_fuel_ch4_emissions" ~ "Methane",
      ghg_gas_name == "fossil_fuel_ch4_emissions_co2e" ~ "Methane (Co2 eq)",
      ghg_gas_name == "fossil_fuel_n2o_emissions" ~ "Nitrous Oxide",
      ghg_gas_name == "fossil_fuel_n2o_emissions_co2e" ~ "Nitrous Oxide (Co2 eq)",
      ghg_gas_name == "biogenic_spent_liquor_co2_emissions" ~ "Carbon Dioxide Biogenic (Spent Liquor)",
      ghg_gas_name == "spent_liquor_ch4_emissions" ~ "Methane Spent Liquor",
      ghg_gas_name == "spent_liquor_n2o_emissions" ~ "Nitrous Oxide Spent Liquor"
    )
  ) |>
  mutate(subpart = "AA") |>
  group_by(primary_naics, facility_id, reporting_year, unit_name, unit_type, ghg_gas_name, subpart) |>
  summarise(
    ghg_quantity = sum(ghg_quantity, na.rm = TRUE)
  ) |>
  ungroup()

is_unique_id(emissions_by_unit, c("facility_id", "reporting_year", "unit_name", 
                          "ghg_gas_name"))


### Combine with combustion emissions ###

# load subpart c emissions dataset
subpart_c = read_csv(here("Output", "subpart_c_emissions_and_fuel_by_unit_v2.csv")) |>
  mutate(primary_naics = as.character(primary_naics)) |>
  filter(primary_naics %in% c("322110", "322120", "322130"))
is_unique_id(subpart_c, c("facility_id", "reporting_year", "unit_name", 
                          "fuel_type", "ghg_gas_name"))


# Append subpart c and subpart aa
pulp_paper_emissions_by_unit = subpart_c |>
  bind_rows(emissions_by_unit) |>
  arrange(facility_id, reporting_year, unit_name, 
            fuel_type, ghg_gas_name)


# test = pulp_paper_emissions_by_unit |>
#   filter(facility_id=="1004705" & reporting_year==2023 & is.na(ghg_quantity_subpart_aa))
# 
# test2 = pulp_paper_emissions_by_unit |>
#   filter(facility_id=="1004705" & reporting_year==2023 & ! is.na(ghg_quantity_subpart_aa))


### export ###
pulp_paper_emissions_by_unit_v2 <- createWorkbook()

addWorksheet(pulp_paper_emissions_by_unit_v2, "unit_emissions")
writeData(pulp_paper_emissions_by_unit_v2, "unit_emissions", pulp_paper_emissions_by_unit)

saveWorkbook(pulp_paper_emissions_by_unit_v2, here("Output", "pulp_paper_emissions_by_unit_v2.xlsx"), 
             overwrite = TRUE)
