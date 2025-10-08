'''
Name: Aidan ONeil
Date: 7/29/2025 (last edit: 7/29/2025)

'''
import pandas as pd
import numpy as np
import csv
import inspect
from archetypes_base import *

allflows = []
processunits = []

flow_dataframe = pd.DataFrame(columns=['name', 'components', 'flow_type',
                                       'temperature', 'pressure',
                                       'composition', 'origin', 'destination',
                                       'mass_flow_rate','elec_flow_rate',
                                       'heat_flow_rate','combustion_energy_content'])

## Global variables

ambient_t = 20
products_amount = 1000
C_pw = 4.21
Hvap = 2260


################################################UNITS##########################################
# Unit 1: Raw Byproduct Reciever
Unit1 = Unit('Reciever')
Unit1.expected_flows_in= ['Electricity (Reciever)', 'Byproduct Feed']
Unit1.expected_flows_out = ['Cows feet', 'Whole Byproducts']
Unit1.coefficients = {'Electricity (kw/kg)': 0.005, 'Feet wt%': 0.02}

def Byproduct_reciever_func(feed_flow, coeff):
    feed_in = feed_flow.attributes['mass_flow_rate']
    feet_out = feed_in * coeff['Feet wt%']
    feed_out = feed_in - feet_out
    electricity_in = coeff['Electricity (kw/kg)'] * feed_in
    print('Unit 1')
    return[{'name' : 'Cows Feet', 'components' : ['Waste','Oil'], 'composition':  [.35,.65], 'mass_flow_rate' : feet_out,
            'flow_type': 'Process stream', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : True},
           {'name' : 'Electricity (Reciever)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Whole Byproducts', 'components' : ['Meat'], 'composition':  [1], 'mass_flow_rate' : feed_out,
            'flow_type': 'Process stream', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : True}]
Unit1.calculations = {'Byproduct Feed': Byproduct_reciever_func}
FlowA = Flow('Byproduct Feed',['Meat'],'input', ambient_t, 1, [1], None , None, products_amount, np.nan, 0)
FlowA.set_calc_flow()
allflows.append(FlowA)

# Unit 2: Neatsfoot Oil Processing - Need a Unit and Steam temp
Unit2 = Unit('Oil Processing')
Unit2.expected_flows_in = ['Cows Feet', 'Electricity (Oil Processor)', 'Steam (Oil Processor)'] 
Unit2.expected_flows_out = ['Neatsfoot Oil', 'Waste (Oil Processor)', 'Codensate (Oil Processor)']
Unit2.coefficients = {'Electricity (kw/kg)': 0.015, 'Steam Demand (kJ/kg)': 300., 'Unit Temp': 100,
                      'Steam Temp': 110}

def Oil_processing_func(feet_flow, coeff):
    feet_in = feet_flow.attributes['mass_flow_rate']
    oil_out = (feet_flow.attributes['composition'][feet_flow.attributes['components'].index('Oil')]) * feet_in
    waste_out = feet_in - oil_out
    Q_steam = coeff['Steam Demand (kJ/kg)'] * feet_in
    m_steam = Q_steam / Hvap
    Q_out_oil = (oil_out / feet_in) * Q_steam
    Q_waste = Q_steam - Q_out_oil
    electricity_in = feet_in * coeff['Electricity (kw/kg)']
    print('Unit 2')
    return[{'name' : 'Electricity (Oil Processor)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Neatsfoot Oil', 'components' : ['Oil'], 'composition':  [1], 'mass_flow_rate' : oil_out,
            'flow_type': 'Product', 'heat_flow_rate': Q_out_oil ,'In or out' : 'Out', 'Set calc' : False},
           {'name' : 'Waste (Oil Processor)', 'components' : ['Waste'], 'composition':  [1], 'mass_flow_rate' : waste_out,
            'flow_type': 'Waste', 'heat_flow_rate': Q_waste ,'In or out' : 'Out', 'Set calc' : False, 'temperature': coeff['Unit Temp']},
           {'name' : 'Steam (Oil Processor)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'Temperature': coeff['Steam Temp'], 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Oil Processor)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]
Unit2.calculations = {'Cows Feet': Oil_processing_func}

# Unit 3: Reprocessing and Grinding
Unit3 = Unit('Reprocessor')
Unit3.expected_flows_in = ['Whole Byproducts', 'Electricity (Reprocessor)']
Unit3.expected_flows_out = ['Ground Slurry', 'Waste (Reprocessor)']
Unit3.coefficients = {'Electricity (kw/kg)': 0.02, 'Waste wt%': 0.03, 'Unit Temp': ambient_t}

def Reprocessor_func(meat_flow, coeff):
    meat_in = meat_flow.attributes['mass_flow_rate']
    electricity_in = meat_in * coeff['Electricity (kw/kg)']
    waste_out = meat_in * coeff['Waste wt%']
    slurry_out = meat_in - waste_out
    print('Unit 3')
    return[{'name' : 'Electricity (Reprocessor)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Ground Slurry', 'components' : ['Slurry'], 'composition':  [1], 'mass_flow_rate' : slurry_out,
            'flow_type': 'Process stream', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : True},
           {'name' : 'Waste (Reprocessor)', 'components' : ['Waste'], 'composition':  [1], 'mass_flow_rate' : waste_out,
            'flow_type': 'Waste', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : False, 'temperature': coeff['Unit Temp']}]
Unit3.calculations = {'Whole Byproducts': Reprocessor_func} 
    
# Unit 4: Cooking and Rendering
Unit4 = Unit('Cooking and Rendering')
Unit4.expected_flows_in = ['Fuel (Cooking/Rendering)', 'Electricity (Cooking/Rendering)', 'Ground Slurry']
Unit4.expected_flows_out = ['Waste (Cooking/Rendering)', 'Rendered Meat']
Unit4.coefficients = {'Fuel Needed (kJ/kg)': 1500., 'Electricity (kw/kg)': 0.01, 'Waste Ratio': .15, 'loses': .05,
                      'Fuel HHV': 5200, 'Unit Temp': 100}

def Cooking_rendering_func(meat_flow, coeff):
    meat_in = meat_flow.attributes['mass_flow_rate']
    electricity_in = coeff['Electricity (kw/kg)'] * meat_in
    waste_out = meat_in * coeff['Waste Ratio']
    meat_out = meat_in - waste_out
    Q_fuel = coeff['Fuel Needed (kJ/kg)'] * meat_in
    m_fuel = Q_fuel / coeff['Fuel HHV']
    Q_out = Q_fuel * (1-coeff['loses'])
    Q_waste = Q_fuel - Q_out
    print('Unit 4')
    return[{'name' : 'Electricity (Cooking/Rendering)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Rendered Meat', 'components' : ['Meat'], 'composition':  [1], 'mass_flow_rate' : meat_out,
            'flow_type': 'Process stream', 'heat_flow_rate': Q_out ,'In or out' : 'Out', 'Set calc' : True},
           {'name' : 'Waste (Cooking/Rendering)', 'components' : ['Waste'], 'composition':  [1], 'mass_flow_rate' : m_fuel+waste_out,
            'flow_type': 'Waste', 'heat_flow_rate': Q_waste ,'In or out' : 'Out', 'Set calc' : False, 'temperature': coeff['Unit Temp']},
           {'name' : 'Fuel (Cooking/Rendering)', 'components' : 'Fuel', 'mass_flow_rate' : m_fuel,
             'flow_type': 'Fuel', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_fuel}]
Unit4.calculations = {'Ground Slurry': Cooking_rendering_func}
           
# Unit 5: Pressing
Unit5 = Unit('Pressing')
Unit5.expected_flows_in = ['Electricity (Pressing)', 'Rendered Meat']
Unit5.expected_flows_out = ['Pressed Cake', 'Pressed Liquor']
Unit5.coefficients = {'Electricity (kw/kg)': 0.015, 'Liquor Split': .35, 'Heat Loss': .50}

def Pressing_func(meat_flow, coeff):
    meat_in = meat_flow.attributes['mass_flow_rate']
    Q_in = meat_flow.attributes['heat_flow_rate']
    liquor_out = meat_in * coeff['Liquor Split']
    solids_out = meat_in - liquor_out
    electricity_in = meat_in * coeff['Electricity (kw/kg)']
    Q_loss = Q_in * coeff['Heat Loss']
    Q_avail = Q_in - Q_loss
    Q_liquor = Q_avail * coeff['Liquor Split']
    Q_solids = Q_avail - Q_liquor
    print('Unit 5')
    return[{'name' : 'Electricity (Pressing)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Pressed Liquor', 'components' : ['Liquor'], 'composition':  [1], 'mass_flow_rate' : liquor_out,
            'flow_type': 'Process stream', 'heat_flow_rate': Q_liquor ,'In or out' : 'Out', 'Set calc' : True},
           {'name' : 'Pressed Cake', 'components' : ['Cake', 'Water'], 'composition':  [.9, .1], 'mass_flow_rate' : solids_out,
            'flow_type': 'Process stream', 'heat_flow_rate': Q_solids,'In or out' : 'Out', 'Set calc' : True},
           {'Heat loss': Q_loss}]
Unit5.calculations = {'Rendered Meat': Pressing_func} 
  
# Unit 6: Protein Meal Dryer
Unit6 = Unit('Protein Meal Dryer')
Unit6.expected_flows_in = ['Pressed Cake', 'Steam (Dryer)', 'Electricity (Dryer)']
Unit6.expected_flows_out = ['Condensate (Dryer)', 'Steam Out (Dryer)', 'Bone Meal']
Unit6.coefficients = {'Electricity (kw/kg)': 0.025, 'C_pmeat': 3.44, 'Unit Temp': 160, 'loses': .10, 'Steam temp': 170}

def Meal_dryer_func(meat_flow, coeff):
    meat_in = meat_flow.attributes['mass_flow_rate']
    Q_in = meat_flow.attributes['heat_flow_rate']
    water_out = (meat_flow.attributes['composition'][meat_flow.attributes['components'].index('Water')]) * meat_in
    Q_water_evap = (Hvap + (C_pw * 80)) * water_out
    meat_out = meat_in - water_out 
    Q_meat = meat_out * coeff['C_pmeat'] * (coeff['Unit Temp'] - ambient_t)
    Q_steam = (Q_meat + Q_water_evap - Q_in) / (1- coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss = Q_steam * coeff['loses']
    electricity_in = meat_in * coeff['Electricity (kw/kg)']
    print('Unit 6')
    return[{'name' : 'Electricity (Dryer)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Steam (Dryer)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'Temperature': coeff['Steam temp'], 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Dryer)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Bone Meal', 'components' : ['Bone Meal'], 'composition':  [1], 'mass_flow_rate' : meat_out,
            'flow_type': 'Process stream', 'heat_flow_rate': Q_meat ,'In or out' : 'Out', 'Set calc' : True},
           {'name' : 'Steam Out (Dryer)', 'components' : ['Water'], 'composition':  [1], 'mass_flow_rate' : water_out,
            'flow_type': 'Steam', 'heat_flow_rate': Q_water_evap ,'In or out' : 'Out', 'Set calc' : False, 'temperature': coeff['Unit Temp']},
           {'Heat loss': Q_loss}]
Unit6.calculations = {'Pressed Cake':  Meal_dryer_func}          

# Unit 7: Meal Grinding and Packaging
Unit7 = Unit('Meal Grinder')
Unit7.expected_flows_in = ['Bone Meal', 'Electricity (Grinder)']
Unit7.expected_flows_out = ['Product Bone Meal']
Unit7.coefficients = {'Electricity (kw/kg)': 0.015}

def Meal_grinder_func(meal_flow, coeff):
    meal_in = meal_flow.attributes['mass_flow_rate']
    Q_loss = meal_flow.attributes['heat_flow_rate']
    electricity_in = meal_in * coeff['Electricity (kw/kg)']
    print('Unit 7')
    
    return[{'name' : 'Electricity (Grinder)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Product Bone Meal', 'components' : ['Bone Meal'], 'composition':  [1], 'mass_flow_rate' : meal_in,
            'flow_type': 'Product', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : False},
           {'Heat loss': Q_loss}]
Unit7.calculations = {'Bone Meal': Meal_grinder_func}

# Unit 8: Centrifuge
Unit8 = Unit('Centrifuge')
Unit8.expected_flows_in = ['Pressed Liquor', 'Electricity (Centrifuge)']
Unit8.expected_flows_out = ['Waste (Centrifuge)', 'Tallow', 'High Quality Fats', 'Low Quality Fats']
Unit8.coefficients = {'Electricity (kw/kg)': 0.025, 'Waste percent': .10, 'Tallow percent': .60, 'High Quality Fats percent': .15}

def Centrifuge_func(liquor_flow, coeff):
    liquor_in = liquor_flow.attributes['mass_flow_rate']
    Q_loss = liquor_flow.attributes['heat_flow_rate']
    electricity_in = liquor_in * coeff['Electricity (kw/kg)']
    tallow_out = coeff['Tallow percent'] * liquor_in
    hq_fats_out = coeff['High Quality Fats percent'] * liquor_in
    waste_out = coeff['Waste percent'] * liquor_in
    lq_fats_out = liquor_in - tallow_out - hq_fats_out - waste_out
    print('Unit 8')
    return[{'name' : 'Electricity (Centrifuge)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Tallow', 'components' : ['Tallow'], 'composition':  [1], 'mass_flow_rate' : tallow_out,
            'flow_type': 'Product', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : False},
           {'name' : 'Waste (Centrifuge)', 'components' : ['Waste'], 'composition':  [1], 'mass_flow_rate' : waste_out,
            'flow_type': 'Waste', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : False},
           {'name' : 'High Quality Fats', 'components' : ['Fat'], 'composition':  [1], 'mass_flow_rate' : hq_fats_out,
            'flow_type': 'Process stream', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : True},
           {'name' : 'Low Quality Fats', 'components' : ['Fat'], 'composition':  [1], 'mass_flow_rate' : lq_fats_out,
            'flow_type': 'Process stream', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : True},
           {'Heat loss': Q_loss}]
Unit8.calculations = {'Pressed Liquor': Centrifuge_func}

# Unit 9: Lard Refining
Unit9 = Unit('Lard Refiner')
Unit9.expected_flows_in = ['High Quality Fats', 'Electricity (Lard Refining)', 'Steam (Lard Refining)']
Unit9.expected_flows_out = ['Refined Lard', 'Condensate (Lard Refining)', 'Waste (Lard Refining)'] 
Unit9.coefficients = {'Electricity (kw/kg)': 0.015, 'Steam temp': 110, 'Steam Demand (kj/kg)': 500, 'Waste percent':0.05}

def Lard_refinding_func(fat_flow, coeff):
    fat_in = fat_flow.attributes['mass_flow_rate']
    waste_out = fat_in * coeff['Waste percent']
    fat_out = fat_in - waste_out
    electricity_in = fat_in * coeff['Electricity (kw/kg)']
    Q_steam = fat_in * coeff['Steam Demand (kj/kg)']
    m_steam = Q_steam / Hvap
    print('Unit 9')
    return[{'name' : 'Electricity (Lard Refining)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Steam (Lard Refining)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'Temperature': coeff['Steam temp'], 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Lard Refining)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Waste (Lard Refining)', 'components' : ['Waste'], 'composition':  [1], 'mass_flow_rate' : waste_out,
            'flow_type': 'Waste', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : False},
           {'name' : 'Refined Lard', 'components' : ['Fat'], 'composition':  [1], 'mass_flow_rate' : fat_out,
            'flow_type': 'Product', 'heat_flow_rate': Q_steam ,'In or out' : 'Out', 'Set calc' : False}]
Unit9.calculations = {'High Quality Fats': Lard_refinding_func}
           
# Unit 10: Grease Treatment
Unit10 = Unit('Grease Treatment')
Unit10.expected_flows_in = ['Low Quality Fats', 'Electricity (Grease Treatment)', 'Steam (Grease Treatment)']
Unit10.expected_flows_out = ['Filtered Grease', 'Condensate (Grease Treatment)', 'Waste (Grease Treatment)'] 
Unit10.coefficients = {'Electricity (kw/kg)': 0.01, 'Steam temp': 110, 'Steam Demand (kj/kg)': 200, 'Waste percent':0.1}

def Grease_refinding_func(fat_flow, coeff):
    fat_in = fat_flow.attributes['mass_flow_rate']
    waste_out = fat_in * coeff['Waste percent']
    fat_out = fat_in - waste_out
    electricity_in = fat_in * coeff['Electricity (kw/kg)']
    Q_steam = fat_in * coeff['Steam Demand (kj/kg)']
    m_steam = Q_steam / Hvap
    print('Unit 10')
    return[{'name' : 'Electricity (Grease Treatment)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Steam (Grease Treatment)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'Temperature': coeff['Steam temp'], 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Grease Treatment)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Waste (Grease Treatment)', 'components' : ['Waste'], 'composition':  [1], 'mass_flow_rate' : waste_out,
            'flow_type': 'Waste', 'heat_flow_rate': 0 ,'In or out' : 'Out', 'Set calc' : False},
           {'name' : 'Filtered Grease', 'components' : ['Fat'], 'composition':  [1], 'mass_flow_rate' : fat_out,
            'flow_type': 'Product', 'heat_flow_rate': Q_steam ,'In or out' : 'Out', 'Set calc' : False}]
Unit10.calculations = {'Low Quality Fats': Grease_refinding_func}

###########################################################################################################################
processunits = [Unit1, Unit2, Unit3, Unit4, Unit5, Unit6, Unit7, Unit8,
                Unit9, Unit10]

main(allflows, processunits)

for unit in processunits:
    unit.check_heat_balance(allflows)
    unit.check_mass_balance(allflows)

for flow in allflows:
    if flow.attributes['flow_type'] == 'Product':
        print(flow)

#utilities_recap('heat_intensity_soybean', allflows, processunits)


