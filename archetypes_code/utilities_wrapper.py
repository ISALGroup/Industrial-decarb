# -*- coding: utf-8 -*-
"""
Created on Mon May 12 14:13:28 2025

@author: Antoine
"""
import pandas as pd
import utilities_base
import importlib
import utilities_test_archetype
import default_boiler
import boiler_class
from archetypes_base import *

archetype = 'utilities_test_archetype'
#Scenarios are: 'all_default' (will take baseline values for all of USA), 'default'
# (will take the scenarios loaded by default), or custom scenarios
scenario = 'all_default'
archetype_model = importlib.import_module(archetype)
grid = 'default'
state = 'USA'
fuels_cost = 'default'
boilers = 'default'


#Electricity prices: https://www.eia.gov/electricity/data.php#sales
eia_elec_costs = pd.read_excel('HS861 2010-.xlsx', sheet_name= 'Total Electric Industry')
# Cost in cents/kWh
avg_cost_elec_usa2023 = 16
# Cost in $/kWh
avg_cost_elec_usa2023_unit = 0.16
#NG prices: https://www.eia.gov/dnav/ng/ng_pri_sum_a_EPG0_PIN_DMcf_a.htm
eia_ng_costs = pd.read_excel('NG_PRI_SUM_A_EPG0_PIN_DMCF_A.xls', sheet_name= 'Data 1')
#Cost in $/Mcf
eia_cost_ng_usa2024 = 3.93
#Cost in $/kJ (1 Mcf = 1 093 000 kJ: https://www.eia.gov/energyexplained/units-and-calculators/energy-conversion-calculators.php)
eia_cost_ng_usa2024_unit = 3.93/1093000.
#Coal prices: https://www.eia.gov/coal/data.php - Average delivered prices to end use sector by Census division and state
eia_coal_costs = pd.read_excel('table34.xlsx')
# Cost in dollars per short ton
avg_cost_coal_US = 83.39
# Cost in dollar per kJ (1 short ton = 19 856 151 kJ https://www.eia.gov/energyexplained/units-and-calculators/energy-conversion-calculators.php)
avg_cost_coal_US_unit = 83.39/19856151.
#Electricity assumed to be in kWh, all fuel in kJ & LHV
#Grid emission factors: eGrid from EPA https://www.epa.gov/egrid/summary-data
grid_ef = pd.read_excel('egrid2023_summary_tables_rev1.xlsx', sheet_name= 'Table 1')
#Grid EF in lbs/MWh
usa_grid_ef2023 = {'CO2' : 771.5, 'CH4' : 0.057, 'N2O' : 0.008, 'NOx' : 0.5, 'SO2' : 0.361}
#Grid EF in kg/kWh
def grid_converter(efs):
    converted_grid = {}
    for ef in efs:
        converted_grid[ef] = efs[ef] * 0.45359237/1000.
    return converted_grid
usa_grid_ef2023_unit = grid_converter(usa_grid_ef2023)

flows, units = archetype_model.main(archetype_model.allflows , archetype_model.processunits)

if scenario == 'all_default':
    heat_demand = calc_heat_demand(flows, units)
    boiler = default_boiler.Default_Boiler
    boiler.req_duty = heat_demand
    grid_price = avg_cost_elec_usa2023_unit
    utilities_base.utilities_main(flows, units, grid_price, grid_price_sell = grid_price, grid_ef = usa_grid_ef2023_unit, boilers = [boiler], air_compressors = [], fuels_price = {'CH4': eia_cost_ng_usa2024_unit, 'Coal' : avg_cost_coal_US_unit})


if scenario == 'default':
    if boilers == 'default':
        heat_demand = calc_heat_demand(flows, units)
        boiler = default_boiler.Default_Boiler
        boiler.req_duty = heat_demand
 #   grid_price = 

