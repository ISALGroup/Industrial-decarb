# -*- coding: utf-8 -*-
"""
Created on Tue Jul 29 13:04:30 2025

@author: Antoine
"""

import pandas as pd
import models_results as mr
import numpy as np
import os
os.chdir('C:/Users/Antoine/Desktop/LCOH modelling')
# with open('input.txt', 'r') as file:
#     lines = file.readlines()
# print(lines)


unit_emissions = pd.read_excel('data/Facility_and_Unit_Emissions_Database_2023_v3.xlsx', sheet_name= 'unit_emissions')

facility_info = pd.read_excel('data/Facility_and_Unit_Emissions_Database_2023_v3.xlsx', sheet_name= 'descr_info')

fuel_emission_factors = pd.read_excel('data/emission_factors.xlsx') #in kgCOeq. per MMBTU

fuel_prices = pd.read_excel('data/fuel_prices.xlsx') #in $ per MMBTU

naics_temps = pd.read_excel('data/facility_temps.xlsx', sheet_name= 'NAICS')

facility_temps = pd.read_excel('data/facility_temps.xlsx', sheet_name= 'facility')

unit_emissions2 = pd.merge(unit_emissions, facility_info.filter(['facility_id', 'facility_name', 'state', 'naics_title']), on= 'facility_id')
unit_emissions2.to_excel('output/merged.xlsx') 
soi_naics_codes = [322120, 325193, 311313]

states = ['MN']

year = 2023

non_electrifiable_units = ['K (Kiln)']
non_electrifiable_fuels = ['Wood and Wood Residuals (dry basis)']

def electrifiable_condition(row):
    if row['unit_type'] in non_electrifiable_units or row['fuel_type'] in non_electrifiable_fuels:
        return 0
    else:
        return row['fuel_energy']


unit_emissions_year = unit_emissions2[unit_emissions2['reporting_year'] == year]
unit_emissions_year_soi = unit_emissions_year[unit_emissions_year['primary_naics'].isin(soi_naics_codes)]
facility_info_states = facility_info[facility_info['state'].isin(states)]
facilities_states = facility_info_states['facility_id'].unique()
facilities_states_list = facilities_states.tolist()

unit_emissions_year_states = unit_emissions_year_soi[unit_emissions_year_soi['facility_id'].isin(facilities_states_list)]

grouped_emissions_year_states = unit_emissions_year_states.groupby(['facility_id', 'fuel_type', 'ghg_name', 'unit_type', 'unit_name'])['ghg_quantity'].sum().reset_index()

ghg_gases_list = ['Carbon Dioxide Non-Biogenic', 'Methane (Co2 eq)', 'Nitrous Oxide (Co2 eq)', 'Carbon Dioxide Biogenic']

filtered_emissions1 = grouped_emissions_year_states[grouped_emissions_year_states['ghg_quantity'] != 0]
filtered_emissions2 = filtered_emissions1[filtered_emissions1['ghg_name'].isin(ghg_gases_list)]

ghg_gases_list2 = ['Carbon Dioxide Non-Biogenic', 'Methane (Co2 eq)', 'Nitrous Oxide (Co2 eq)']
only_biogenic = filtered_emissions2[filtered_emissions2['ghg_name'] == 'Carbon Dioxide Biogenic']
co2eq_filtered_dataset = filtered_emissions2[filtered_emissions2['ghg_name'].isin(ghg_gases_list2)]
summed_emissions = co2eq_filtered_dataset.groupby(['facility_id', 'fuel_type'])['ghg_quantity'].sum().reset_index()
concated = pd.concat([only_biogenic, summed_emissions])

added_price = pd.merge(concated, fuel_prices, on = 'fuel_type')
added_ef = pd.merge(added_price, fuel_emission_factors, on = 'fuel_type')
fuel_energy_added = added_ef
fuel_energy_added['fuel_energy'] = 1000*fuel_energy_added['ghg_quantity']/fuel_energy_added['emission_factor'] #in MMBTU

electrified_amount = fuel_energy_added
electrified_amount['electrifiable_fuel_energy'] = electrified_amount.apply(electrifiable_condition, axis= 1)

electrified_amount['electrifiable_emissions'] = electrified_amount['electrifiable_fuel_energy'] * fuel_energy_added['emission_factor'] / 1000.
electrified_amount['fuel_cost'] = electrified_amount['fuel_energy'] * electrified_amount['fuel_price']

electrified_amount['electrifiable_fuel_cost'] = electrified_amount['electrifiable_fuel_energy'] * electrified_amount['fuel_price']

#### Baseline scenario

boiler_efficiency = 0.8
operating_hours = 8000
mmbtu_to_kwh = 293.07107
electrified_amount['electrifiable_process_heat_equivalent'] = electrified_amount['electrifiable_fuel_energy']*boiler_efficiency


electrified_amount['electrifiable_process_heat_kwh'] = electrified_amount['electrifiable_process_heat_equivalent'] * mmbtu_to_kwh

electrified_amount['equipment_sizing'] = electrified_amount['electrifiable_process_heat_kwh']/operating_hours

### Scenario 1

e_boiler_low = 17 #$/kWth
e_boiler_high = 60 #$/kWth


scenario1 = electrified_amount
scenario1['eboiler_low_capex'] = scenario1['equipment_sizing'] * e_boiler_low
scenario1['eboiler_high_capex'] = scenario1['equipment_sizing'] * e_boiler_high

grid_price = 0.092 #$/kWh
grid_ef = 0.341235757 #kgCO2/kWh

scenario1['eboiler_opex'] = scenario1['electrifiable_process_heat_kwh']*grid_price #$
scenario1['eboiler_emissions'] = scenario1['electrifiable_process_heat_kwh']*grid_ef/1000. #tCO2eq.




### LCOH parameters

discount_rate = 0.065
equipment_lifetime = 15

def LCOH(capex, opex, discount, thermal_energy,lifetime, EC = 0, CC = 0):
    numerator = capex
    denominator = 0
    for i in range(lifetime):
        numerator += (opex + EC + CC)/((1 + discount)**(i + 1))
        denominator += ((thermal_energy)/((1 + discount)**(i + 1)))
    LCOH = numerator/denominator
    return LCOH

#print(LCOH(10000000, 46000000, 0.065, 1700000, 15))

def low_row_lcoh(row):
    if row['electrifiable_process_heat_equivalent'] == 0:
        return None
    else:
        
        return LCOH(row['eboiler_low_capex'], row['eboiler_opex'], discount_rate, row['electrifiable_process_heat_equivalent'], equipment_lifetime)


scenario1['scenario1_LCOH_low'] = scenario1.apply(low_row_lcoh, axis= 1)

def high_row_lcoh(row):
    if row['electrifiable_process_heat_equivalent'] == 0:
        return None
    else:
        
        return LCOH(row['eboiler_high_capex'], row['eboiler_opex'], discount_rate, row['electrifiable_process_heat_equivalent'], equipment_lifetime)

scenario1['scenario1_LCOH_high'] = scenario1.apply(high_row_lcoh, axis= 1)








facility_info_sub = facility_info[['facility_name', 'state', 'primary_naics', 'facility_id']]
add_in_naics = pd.merge(scenario1, facility_info_sub, on= 'facility_id')

add_temp_naics = pd.merge(add_in_naics, naics_temps, on='primary_naics', how= 'left')

add_temp_facility= pd.merge(add_temp_naics, facility_temps, on='facility_id', how= 'left')

add_temp_facility['supply_temperature'] = add_temp_facility['temperature_x'].combine_first(add_temp_facility['temperature_y'])

#facility_aggreg = add_temp_facility.groupby(['facility_id', 'primary_naics', 'facility_name', 'state', 'supply_temperature'])['fuel_energy'].sum().reset_index()
facility_aggreg = add_temp_facility.groupby(['facility_id', 'primary_naics', 'facility_name', 'state', 'supply_temperature'], as_index=False).agg(
    agg_fuel_energy = ('fuel_energy', np.sum),
    agg_ghg_emissions = ('ghg_quantity', np.sum),
    agg_electrifiable_emissions = ('electrifiable_emissions', np.sum),
    agg_electrifiable_energy = ('electrifiable_fuel_energy', np.sum),
    agg_fuel_cost = ('fuel_cost', np.sum),
    agg_electrifiable_fuel_cost = ('electrifiable_fuel_cost', np.sum),
    agg_electrifiable_process_heat = ('electrifiable_process_heat_equivalent', np.sum),
    agg_electrifiable_process_heat_kwh = ('electrifiable_process_heat_kwh', np.sum),
    agg_equipment_sizing = ('equipment_sizing', np.sum)
    )
# add_temp_facility.to_excel('output.xlsx')
# facility_aggreg.to_excel('facility_aggreg.xlsx')
def agg_low_row_lcoh(row):
    if row['agg_electrifiable_process_heat'] == 0:
        return None
    else:
        
        return LCOH(row['eboiler_low_capex'], row['eboiler_opex'], discount_rate, row['agg_electrifiable_process_heat'], equipment_lifetime)



def agg_high_row_lcoh(row):
    if row['agg_electrifiable_process_heat'] == 0:
        return None
    else:
        
        return LCOH(row['eboiler_high_capex'], row['eboiler_opex'], discount_rate, row['agg_electrifiable_process_heat'], equipment_lifetime)



facility_aggreg['eboiler_low_capex'] = facility_aggreg['agg_equipment_sizing'] * e_boiler_low
facility_aggreg['eboiler_high_capex'] = facility_aggreg['agg_equipment_sizing'] * e_boiler_high
facility_aggreg['eboiler_opex'] = facility_aggreg['agg_electrifiable_process_heat_kwh']*grid_price
facility_aggreg['eboiler_emissions'] = facility_aggreg['agg_electrifiable_process_heat_kwh']*grid_ef/1000.
facility_aggreg['scenario1_LCOH_low'] = facility_aggreg.apply(agg_low_row_lcoh, axis= 1)

facility_aggreg['scenario1_LCOH_high'] = facility_aggreg.apply(agg_high_row_lcoh, axis= 1)

column_names = facility_aggreg.columns
print(column_names)
#facility_aggreg.to_excel('facility_aggreg.xlsx')

### Scenario 2 
print(facility_aggreg['supply_temperature'])
ambient_average_t = 10
abs_zero = 273.15
low_dt = 5
high_dt = 10
carnot_eff = 0.5

heat_pump_low_capex = 300
heat_pump_high_capex = 1000

def cop(source_t, supply_t, dt, carnot_eff, abs_zero = 237.15):
    carnot_cop = (supply_t + abs_zero)/(supply_t - source_t + dt)
    effective_cop = carnot_eff * carnot_cop
    return effective_cop

def scenario2_cop_high(row):
    t_supply = row['supply_temperature']
    cop_result = cop(ambient_average_t, t_supply, low_dt, carnot_eff)
    return cop_result

def scenario2_cop_low(row):
    t_supply = row['supply_temperature']
    cop_result = cop(ambient_average_t, t_supply, high_dt, carnot_eff)
    return cop_result

facility_aggreg['ambient_cop_low'] = facility_aggreg.apply(scenario2_cop_low, axis = 1)

facility_aggreg['ambient_cop_high'] = facility_aggreg.apply(scenario2_cop_high, axis = 1)

facility_aggreg['scenario2_high_electricity_consumption'] = facility_aggreg['agg_electrifiable_process_heat_kwh']/facility_aggreg['ambient_cop_low']

facility_aggreg['scenario2_high_opex'] = facility_aggreg['scenario2_high_electricity_consumption'] * grid_price

facility_aggreg['scenario2_high_emissions'] = facility_aggreg['agg_ghg_emissions'] - facility_aggreg['agg_electrifiable_emissions'] +  facility_aggreg['scenario2_high_electricity_consumption'] * grid_ef/1000.


facility_aggreg['scenario2_low_electricity_consumption'] = facility_aggreg['agg_electrifiable_process_heat_kwh']/facility_aggreg['ambient_cop_high']

facility_aggreg['scenario2_low_opex'] = facility_aggreg['scenario2_low_electricity_consumption'] * grid_price

facility_aggreg['scenario2_low_emissions'] = facility_aggreg['agg_ghg_emissions'] - facility_aggreg['agg_electrifiable_emissions'] +  facility_aggreg['scenario2_low_electricity_consumption'] * grid_ef/1000.


facility_aggreg['scenario2_low_capex'] = facility_aggreg['agg_equipment_sizing'] * heat_pump_low_capex

facility_aggreg['scenario2_high_capex'] = facility_aggreg['agg_equipment_sizing'] * heat_pump_high_capex

def scen2_low_row_lcoh(row):
    if row['agg_electrifiable_process_heat'] == 0:
        return None
    else:
        
        return LCOH(row['scenario2_low_capex'], row['scenario2_low_opex'], discount_rate, row['agg_electrifiable_process_heat'], equipment_lifetime)



def scen2_high_row_lcoh(row):
    if row['agg_electrifiable_process_heat'] == 0:
        return None
    else:
        
        return LCOH(row['scenario2_high_capex'], row['scenario2_high_opex'], discount_rate, row['agg_electrifiable_process_heat'], equipment_lifetime)


facility_aggreg['scenario2_high_LCOH'] = facility_aggreg.apply(scen2_high_row_lcoh, axis=1)

facility_aggreg['scenario2_low_LCOH'] = facility_aggreg.apply(scen2_low_row_lcoh, axis=1)


### Scenario 3

energy_savings = 0.1
capex_per_mmbtu = 0

facility_aggreg['savings_fuel_energy'] = facility_aggreg['agg_fuel_energy']*(1-energy_savings)
facility_aggreg['savings_electrifiable_fuel_energy'] = facility_aggreg['agg_electrifiable_energy']*(1-energy_savings)
facility_aggreg['savings_ghg_quantity'] = facility_aggreg['agg_ghg_emissions']*(1-energy_savings)
facility_aggreg['savings_electrifiable_ghg_quantity'] = facility_aggreg['agg_electrifiable_emissions']*(1-energy_savings)
facility_aggreg['savings_fuel_cost'] = facility_aggreg['agg_fuel_cost']*(1-energy_savings)
facility_aggreg['savings_electrifiable_fuel_cost'] = facility_aggreg['agg_electrifiable_fuel_cost']*(1-energy_savings)
facility_aggreg['savings_electrifiable_process_heat'] = facility_aggreg['agg_electrifiable_process_heat']*(1-energy_savings) 
facility_aggreg['savings_electrifiable_process_heat_kwh'] = facility_aggreg['agg_electrifiable_process_heat_kwh']*(1-energy_savings) 
facility_aggreg['savings_equipment_sizing'] = facility_aggreg['agg_equipment_sizing']*(1-energy_savings) 
facility_aggreg['savings_eboiler_low_capex'] = facility_aggreg['savings_equipment_sizing'] * e_boiler_low
facility_aggreg['savings_eboiler_opex'] = facility_aggreg['savings_electrifiable_process_heat_kwh'] * grid_price
facility_aggreg['savings_eboiler_high_capex'] = facility_aggreg['savings_equipment_sizing'] * e_boiler_high

def savings_low_row_lcoh(row):
    if row['savings_electrifiable_process_heat'] == 0:
        return None
    else:
        
        return LCOH(row['savings_eboiler_low_capex'], row['savings_eboiler_opex'], discount_rate, row['agg_electrifiable_process_heat'], equipment_lifetime)



def savings_high_row_lcoh(row):
    if row['savings_electrifiable_process_heat'] == 0:
        return None
    else:
        
        return LCOH(row['savings_eboiler_high_capex'], row['savings_eboiler_opex'], discount_rate, row['agg_electrifiable_process_heat'], equipment_lifetime)


facility_aggreg['savings_electrifiable_ghg_quantity'] = facility_aggreg['savings_electrifiable_process_heat_kwh']*grid_ef/1000.
facility_aggreg['scenario3_emissions'] = facility_aggreg['savings_ghg_quantity'] - facility_aggreg['savings_electrifiable_ghg_quantity'] + facility_aggreg['savings_electrifiable_ghg_quantity']

facility_aggreg['scenario3_LCOH_low'] = facility_aggreg.apply(savings_low_row_lcoh, axis= 1)

facility_aggreg['scenario3_LCOH_high'] = facility_aggreg.apply(savings_high_row_lcoh, axis= 1)


### Scenario 4

facility_aggreg['scenario4_high_electricity_consumption'] = facility_aggreg['savings_electrifiable_process_heat_kwh']/facility_aggreg['ambient_cop_low']

facility_aggreg['scenario4_high_opex'] = facility_aggreg['scenario4_high_electricity_consumption'] * grid_price

facility_aggreg['scenario4_high_emissions'] = facility_aggreg['scenario4_high_electricity_consumption'] * grid_ef/1000.


facility_aggreg['scenario4_low_electricity_consumption'] = facility_aggreg['savings_electrifiable_process_heat_kwh']/facility_aggreg['ambient_cop_high']

facility_aggreg['scenario4_low_opex'] = facility_aggreg['scenario4_low_electricity_consumption'] * grid_price

facility_aggreg['scenario4_low_emissions'] = facility_aggreg['scenario4_low_electricity_consumption'] * grid_ef/1000.


facility_aggreg['scenario4_low_capex'] = facility_aggreg['savings_equipment_sizing'] * heat_pump_low_capex

facility_aggreg['scenario4_high_capex'] = facility_aggreg['savings_equipment_sizing'] * heat_pump_high_capex

def scen4_low_row_lcoh(row):
    if row['agg_electrifiable_process_heat'] == 0:
        return None
    else:
        
        return LCOH(row['scenario4_low_capex'], row['scenario4_low_opex'], discount_rate, row['agg_electrifiable_process_heat'], equipment_lifetime)



def scen4_high_row_lcoh(row):
    if row['agg_electrifiable_process_heat'] == 0:
        return None
    else:
        
        return LCOH(row['scenario4_high_capex'], row['scenario4_high_opex'], discount_rate, row['agg_electrifiable_process_heat'], equipment_lifetime)


facility_aggreg['scenario4_high_LCOH'] = facility_aggreg.apply(scen4_high_row_lcoh, axis=1)

facility_aggreg['scenario4_low_LCOH'] = facility_aggreg.apply(scen4_low_row_lcoh, axis=1)



new_df = facility_aggreg
new_df['baseline_capex'] = 0
new_df['baseline_change_in_electricity_demand_kwh'] = 0
new_df['baseline_LCOH'] = None
new_df.rename(columns = {'agg_ghg_emissions' : 'baseline_ghg_emissions', 'agg_fuel_cost' : 'baseline_opex',
                         'eboiler_low_capex': 'scenario1best_capex', 'eboiler_high_capex' : 'scenario1worst_capex',
                         'scenario1_LCOH_low' : 'scenario1best_LCOH', 'scenario1_LCOH_high' : 'scenario1worst_LCOH',
                         'agg_electrifiable_process_heat_kwh' : 'scenario1worst_change_in_electricity_demand_kwh',
                         'scenario2_high_electricity_consumption' : 'scenario2worst_change_in_electricity_demand_kwh',
                         'scenario2_high_opex' : 'scenario2worst_opex', 'scenario2_high_emissions': 'scenario2worst_ghg_emissions',
                         'scenario2_low_electricity_consumption' : 'scenario2best_change_in_electricity_demand_kwh',
                         'scenario2_low_opex' : 'scenario2best_opex', 'scenario2_low_emissions': 'scenario2best_ghg_emissions'
                         
                         
                         }, inplace = True )


new_df['scenario1best_opex'] = new_df['eboiler_opex']
new_df['scenario1worst_opex'] = new_df['eboiler_opex']
new_df['baseline_heat_mmbtu'] = new_df['agg_electrifiable_process_heat']
new_df['scenario1best_heat_mmbtu'] = new_df['agg_electrifiable_process_heat']
new_df['scenario1worst_heat_mmbtu'] = new_df['agg_electrifiable_process_heat']
new_df['scenario2best_heat_mmbtu'] = new_df['agg_electrifiable_process_heat']
new_df['scenario2worst_heat_mmbtu'] = new_df['agg_electrifiable_process_heat']
new_df['scenario3best_heat_mmbtu'] = new_df['agg_electrifiable_process_heat']
new_df['scenario3worst_heat_mmbtu'] = new_df['agg_electrifiable_process_heat']
new_df['scenario4best_heat_mmbtu'] = new_df['agg_electrifiable_process_heat']
new_df['scenario4worst_heat_mmbtu'] = new_df['agg_electrifiable_process_heat']

new_df['baseline_noelec_ghg_emissions'] = new_df['baseline_ghg_emissions'] - new_df['agg_electrifiable_emissions']
new_df['baseline_elec_ghg_emissions'] = new_df['agg_electrifiable_emissions'] 

new_df['scenario1best_noelec_ghg_emissions'] = new_df['baseline_ghg_emissions'] - new_df['agg_electrifiable_emissions'] 
new_df['scenario1worst_noelec_ghg_emissions'] = new_df['baseline_ghg_emissions'] - new_df['agg_electrifiable_emissions']
new_df['scenario1best_elec_ghg_emissions'] = new_df['agg_electrifiable_emissions'] 
new_df['scenario1worst_elec_ghg_emissions'] = new_df['agg_electrifiable_emissions']

new_df['scenario2best_noelec_ghg_emissions'] = new_df['baseline_ghg_emissions'] - new_df['agg_electrifiable_emissions'] 
new_df['scenario2worst_noelec_ghg_emissions'] = new_df['baseline_ghg_emissions'] - new_df['agg_electrifiable_emissions']
new_df['scenario2best_elec_ghg_emissions'] = new_df['agg_electrifiable_emissions'] 
new_df['scenario2worst_elec_ghg_emissions'] = new_df['agg_electrifiable_emissions']

new_df['scenario3best_noelec_ghg_emissions'] = new_df['baseline_ghg_emissions'] - new_df['agg_electrifiable_emissions'] 
new_df['scenario3worst_noelec_ghg_emissions'] = new_df['baseline_ghg_emissions'] - new_df['agg_electrifiable_emissions']
new_df['scenario3best_elec_ghg_emissions'] = new_df['agg_electrifiable_emissions'] 
new_df['scenario3worst_elec_ghg_emissions'] = new_df['agg_electrifiable_emissions']

new_df['scenario4best_noelec_ghg_emissions'] = new_df['baseline_ghg_emissions'] - new_df['agg_electrifiable_emissions'] 
new_df['scenario4worst_noelec_ghg_emissions'] = new_df['baseline_ghg_emissions'] - new_df['agg_electrifiable_emissions']
new_df['scenario4best_elec_ghg_emissions'] = new_df['agg_electrifiable_emissions'] 
new_df['scenario4worst_elec_ghg_emissions'] = new_df['agg_electrifiable_emissions']


new_df['scenario1best_change_in_electricity_demand_kwh'] = new_df['scenario1worst_change_in_electricity_demand_kwh']

new_df['scenario2best_capex'] = new_df['scenario2_low_capex']
new_df['scenario2worst_capex'] = new_df['scenario2_high_capex']

new_df['scenario2best_LCOH'] = new_df['scenario2_low_LCOH']
new_df['scenario2worst_LCOH'] = new_df['scenario2_high_LCOH']



new_df['scenario3best_opex'] = new_df['savings_eboiler_opex'] 
new_df['scenario3worst_opex'] = new_df['savings_eboiler_opex'] 

new_df['scenario3best_ghg_emissions'] = new_df['scenario3_emissions']
new_df['scenario3worst_ghg_emissions'] = new_df['scenario3_emissions']

new_df['scenario3best_change_in_electricity_demand_kwh'] = new_df['savings_electrifiable_process_heat_kwh']
new_df['scenario3worst_change_in_electricity_demand_kwh'] = new_df['savings_electrifiable_process_heat_kwh']

new_df['scenario3best_capex'] = new_df['savings_eboiler_low_capex']
new_df['scenario3worst_capex'] = new_df['savings_eboiler_high_capex']

new_df['scenario3best_LCOH'] = new_df['scenario3_LCOH_low']
new_df['scenario3worst_LCOH'] = new_df['scenario3_LCOH_high']




new_df['scenario4best_opex'] = new_df['scenario4_low_opex']
new_df['scenario4worst_opex'] = new_df['scenario4_high_opex']

new_df['scenario4best_ghg_emissions'] = new_df['scenario4_low_emissions']
new_df['scenario4worst_ghg_emissions'] = new_df['scenario4_high_emissions']

new_df['scenario4best_change_in_electricity_demand_kwh'] = new_df['scenario4_low_electricity_consumption']
new_df['scenario4worst_change_in_electricity_demand_kwh'] = new_df['scenario4_high_electricity_consumption']

new_df['scenario4best_capex'] = new_df['scenario4_low_capex']
new_df['scenario4worst_capex'] = new_df['scenario4_high_capex']

new_df['scenario4best_LCOH'] = new_df['scenario4_low_LCOH']
new_df['scenario4worst_LCOH'] = new_df['scenario4_high_LCOH']




selected_columns = ['facility_id', 'primary_naics', 'facility_name', 'state', 'baseline_capex', 'baseline_change_in_electricity_demand_kwh', 'scenario1best_change_in_electricity_demand_kwh',
                    'baseline_opex', 'scenario1best_capex', 'scenario1worst_capex','scenario1worst_change_in_electricity_demand_kwh', 'scenario2worst_change_in_electricity_demand_kwh', 'scenario2worst_opex',
                    'scenario2best_change_in_electricity_demand_kwh', 'scenario2best_opex',
                    'scenario2best_capex', 'scenario2worst_capex', 'scenario1best_opex', 'scenario1best_heat_mmbtu', 'scenario1worst_heat_mmbtu','scenario2best_heat_mmbtu', 'scenario2worst_heat_mmbtu',
                    'scenario1worst_opex', 'scenario3best_opex', 'scenario3worst_opex',
                    'scenario3best_change_in_electricity_demand_kwh', 'scenario3worst_change_in_electricity_demand_kwh',
                    'scenario3best_capex', 'scenario3worst_capex', 'scenario4best_opex', 'scenario4worst_opex', 
                    'scenario4best_change_in_electricity_demand_kwh', 'scenario4worst_change_in_electricity_demand_kwh',
                    'scenario3best_heat_mmbtu', 'scenario3worst_heat_mmbtu','scenario4best_heat_mmbtu', 'scenario4worst_heat_mmbtu',
                    'scenario4best_capex', 'scenario4worst_capex', 'baseline_heat_mmbtu', 'baseline_noelec_ghg_emissions', 'baseline_elec_ghg_emissions',
                    'scenario1best_noelec_ghg_emissions', 'scenario1worst_elec_ghg_emissions', 'scenario2best_noelec_ghg_emissions', 'scenario2worst_elec_ghg_emissions',
                    'scenario3best_noelec_ghg_emissions', 'scenario3worst_elec_ghg_emissions', 'scenario4best_noelec_ghg_emissions', 'scenario4worst_elec_ghg_emissions',
                    'scenario1best_elec_ghg_emissions', 'scenario1worst_noelec_ghg_emissions', 'scenario2best_elec_ghg_emissions', 'scenario2worst_noelec_ghg_emissions',
                    'scenario3best_elec_ghg_emissions', 'scenario3worst_noelec_ghg_emissions', 'scenario4best_elec_ghg_emissions', 'scenario4worst_noelec_ghg_emissions'
                    ]

long_form_df = new_df[selected_columns].copy()

columns_of_interest = ['facility_id', 'facility_name', 'state']
scenarios = {
    'Baseline': [col for col in long_form_df.columns if col.startswith('baseline_')],
    'Scenario1Best': [col for col in long_form_df.columns if col.startswith('scenario1best_')],
    'Scenario1Worst': [col for col in long_form_df.columns if col.startswith('scenario1worst_')],
    'Scenario2Best': [col for col in long_form_df.columns if col.startswith('scenario2best_')],
    'Scenario2Worst': [col for col in long_form_df.columns if col.startswith('scenario2worst_')],
    'Scenario3Best': [col for col in long_form_df.columns if col.startswith('scenario3best_')],
    'Scenario3Worst': [col for col in long_form_df.columns if col.startswith('scenario3worst_')],
    'Scenario4Best': [col for col in long_form_df.columns if col.startswith('scenario4best_')],
    'Scenario4Worst': [col for col in long_form_df.columns if col.startswith('scenario4worst_')],
}

df_scenarios = {scenario: long_form_df[columns_of_interest].copy() for scenario in scenarios}

# Ajouter les valeurs des colonnes spécifiques à chaque scénario
for scenario, cols in scenarios.items():
    for col in cols:
        variable = col.split('_', 1)[1]  # Extraire le nom de la variable
        df_scenarios[scenario][variable] = long_form_df[col]
# Ajouter une colonne 'Scenario' à chaque DataFrame pour identifier le scénario
for scenario in df_scenarios:
    df_scenarios[scenario]['tech_scenario'] = scenario

# Combiner tous les DataFrames en un seul
df_combined = pd.concat(df_scenarios, ignore_index=True)

# Sélectionner et réorganiser les colonnes finales
final_columns = ['facility_id', 'facility_name', 'state', 'tech_scenario', 'capex', 'opex', 'change_in_electricity_demand_kwh', 'heat_mmbtu', 'elec_ghg_emissions', 'noelec_ghg_emissions']
df_final = df_combined[final_columns]


df_final.to_excel('output/longform.xlsx')



facility_aggreg.to_excel('output/facility_aggreg.xlsx')
