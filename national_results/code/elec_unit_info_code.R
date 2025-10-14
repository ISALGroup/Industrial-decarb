#### Working with the capex formulas 

elec_unit_data_2 <- read_csv("national_results/data/elec_unit_data_2.csv")

elec_unit_data <- 
  elec_unit_data_2 |>
  mutate(
    capex_formula = str_remove(capex_formula, 'C = '), 
    capex_formula = case_when(
      combustion_unit_category == 'generic' ~ str_replace(capex_formula, 'S', '(process_unit_heat_demand / efficiency)'), 
      combustion_unit_category == 'specific (incl)' ~ str_replace(capex_formula, 'S', 'process_unit_heat_demand'),
      TRUE ~ capex_formula
    ),
    
    efficiency = str_remove(efficiency, '%'), 
    efficiency = as.numeric(efficiency) / 100
  )

write_csv(elec_unit_data, 'elec_unit_info.csv')
