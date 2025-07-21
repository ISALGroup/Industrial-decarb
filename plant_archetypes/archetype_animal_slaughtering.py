# Questions for Liam:
'''
Do we have any way of distinquishing if pork or beef from the GHGRP database? Is there a seperate database we could include?

I was confused by some of the flows in your diagram. We should go back through it to make sure it all makes sense

General Comments: the splits are all wrong I just tried to guess so maybe an industrial average makes sense or I will leave it up to you!
'''

# -*- coding: utf-8 -*-
"""
Created on Tuesday April 28th 11:13:45 am 2025

@author: Aidan ONeil
"""

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

# Global Variables
ambient_t = 20
Hvap = 2260
C_pw = 4.186
C_pair = 1.000

##########################################UNITS#############################
# Unit 1: Stunning - Is there any coefficients you think should be here, maybe it is electricity for stunning or a co2 flow?
Unit1 = Unit('Stunner')
Unit1.expected_flows_in = ['Live Animal']
Unit1.expected_flows_out = ['Stunned Animal']

Unit1.coefficients = {}

def Stunning_func(animal_flow, coeff):
    animal_in = animal_flow.attributes['mass_flow_rate']
    print('Stunner')
    return[{'name' : 'Stunned Animal', 'components' : ['Animal'], 'composition' :[1], 'mass_flow_rate' : animal_in,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0}]

Unit1.calculations = {'Live Animal': Stunning_func}
FlowA = Flow(name = 'Live Animal', components = ['Animal'], composition = [1], flow_type = 'input', mass_flow_rate = 100000)
FlowA.set_calc_flow()
allflows.append(FlowA)

# Unit 2; Exsanguination - Done!
Unit2 = Unit('Exsanguiation')
Unit2.expected_flows_in = ['Stunned Animal']
Unit2.expected_flows_out = ['Blood', 'Carcass']

Unit2.coefficients = {'Blood wt%': (42.5/1000)}

def Exsanguination_func(animal_flow, coeff):
    animal_in = animal_flow.attributes['mass_flow_rate']
    blood_in = coeff['Blood wt%'] * animal_in
    animal_out = animal_in - blood_in
    print('Exsanguination')
    return[{'name' : 'Blood', 'components' : ['Blood'], 'composition' :[1], 'mass_flow_rate' : blood_in,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           {'name' : 'Carcass', 'components' : ['Carcass'], 'composition' :[1], 'mass_flow_rate' : animal_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0}]

Unit2.calculations = {'Stunned Animal': Exsanguination_func}

# Unit 3: Blood Processing - how much water and electricity are coming in? Is all the water leaving as wastewater or is it interacting with the blood
Unit3 = Unit('Blood Processer')
Unit3.expected_flows_in = ['Blood', 'Water (Blood Processer)', 'Electricity (Blood Processer)']
Unit3.expected_flows_out = ['Waste Water (Blood Processer)', 'Blood to Dryer']

Unit3.coefficients = {'Water to Blood Ratio': 0.75, 'Electricty (kw/kg)': 0.15}

def Blood_processer_func(blood_flow, coeff):
    blood_in = blood_flow.attributes['mass_flow_rate']
    electricity_in = blood_in * coeff['Electricty (kw/kg)']
    water_in = blood_in * coeff['Water to Blood Ratio']
    print('Blood Processor')
    return[{'name' : 'Electricity (Blood Processer)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Blood to Dryer', 'components' : ['Blood'], 'composition' :[1], 'mass_flow_rate' : blood_in,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           {'name' : 'Water (Blood Processer)', 'components' : ['Water'], 'composition' :[1], 'mass_flow_rate' : water_in,
             'flow_type': 'Water', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Waster Water (Blood Processer)', 'components' : ['Water'], 'composition' :[1], 'mass_flow_rate' : water_in,
             'flow_type': 'Waste Water', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit3.calculations = {'Blood': Blood_processer_func}

# Unit 4: Blood Dryer - Update the Unit Temp and Steam Temp and the inlet and outlet water wt% and electricity usage and C_p of the feed
Unit4 = Unit('Blood Dryer')
Unit4.expected_flows_in = ['Blood to Dryer', 'Steam (Blood Dryer)', 'Electricity (Blood Dryer)']
Unit4.expected_flows_out = ['Condensate (Blood Dryer)', 'Waste Water (Blood Dryer)', 'Feed Suppliment']

Unit4.coefficients = {'Inlet Water wt%': .70, 'Outlet Water wt%': .10, 'Unit Temp': 80, 'Steam Temp': 110,
                      'C_pfeed': 3.6, 'loses': .10, 'Electricity (kw/kg)': 0.25}

def Blood_dryer_func(blood_flow, coeff):
    blood_in = blood_flow.attributes['mass_flow_rate']
    water_in = blood_in * coeff['Inlet Water wt%']
    feed_out = (blood_in - water_in) / (1- coeff['Outlet Water wt%'])
    water_evap = blood_in - feed_out
    electricity_in = feed_out * coeff['Electricity (kw/kg)']
    Q_in = blood_flow.attributes['heat_flow_rate']
    Q_waterevap = (water_evap * C_pw * (100 - ambient_t)) + (water_evap * Hvap)
    Q_out = feed_out * coeff['C_pfeed'] * (coeff['Unit Temp'] - ambient_t)
    Q_steam = (Q_waterevap + Q_out - Q_in) / (1- coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss = Q_steam * coeff['loses']
    print('Blood Dryer')
    return[{'name' : 'Steam (Blood Dryer)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Steam', 'Temperature': coeff['Steam Temp'], 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Blood Dryer)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Condensate', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss},
           {'name' : 'Waste Water (Blood Dryer)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': water_evap,
             'flow_type': 'Waste Water', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_waterevap},
           {'name' : 'Electricity (Blood Dryer)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Feed Suppliment', 'components' : ['Feed'], 'composition' :[1], 'mass_flow_rate' : feed_out,
             'flow_type': 'Product', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_out}]

Unit4.calculations = {'Blood to Dryer': Blood_dryer_func}

# Unit 5: Cleaner - Update C_pbeef and steam temp
Unit5 = Unit('Cleaner')
Unit5.expected_flows_in = ['Carcass', 'Hot Water (Cleaner)']
Unit5.expected_flows_out = ['Cleaned Carcass', 'Waste Water (Cleaner)']

Unit5.coefficients = {'Unit Temp': 65, 'C_pbeef': 3.54, 'loses': 0.05, 'Steam Temp': 80}

def Cleaner_func(carcass_flow, coeff):
    carcass_in = carcass_flow.attributes['mass_flow_rate']
    Q_in = carcass_flow.attributes['heat_flow_rate']
    Q_out = carcass_in * coeff['C_pbeef'] * (coeff['Unit Temp'] - ambient_t)
    Q_hotwater = (Q_out - Q_in) / (1- coeff['loses'])
    m_hotwater = Q_hotwater / (C_pw * (coeff['Steam Temp'] - ambient_t))
    Q_loss = Q_hotwater * coeff['loses']
    print('Cleaner')
    return[{'name' : 'Hot Water (Cleaner)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_hotwater,
             'flow_type': 'Steam', 'Temperature': coeff['Steam Temp'], 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_hotwater},
           {'name' : 'Waste Water (Cleaner)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_hotwater,
             'flow_type': 'Wastewater', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss},
           {'name' : 'Cleaned Carcass', 'components' : ['Meat'], 'composition' :[1], 'mass_flow_rate' : carcass_in,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_out}]

Unit5.calculations = {'Carcass': Cleaner_func}

# Unit 6: Evisceration - Update steam temp
Unit6 = Unit('Evisceration')
Unit6.expected_flows_in = ['Hot Water (Evisceration)', 'Cleaned Carcass', 'Electricity (Evisceration)']
Unit6.expected_flows_out = ['Waste Water (Evisceration)', 'Edible Organs', 'Viscera', 'Meat']

Unit6.coefficients = {'Organs wt%': (125/1000), 'Viscera Split': 0.50, 'Unit Temp': 75, 'Electricity (kw/kg)': .021,
                      'C_pbeef': 3.54, 'loses': 0.10, 'Steam Temp': 82}

def Evisceration_func(cleaned_carcass_flow, coeff):
    carcass_in = cleaned_carcass_flow.attributes['mass_flow_rate']
    organs_in = carcass_in * coeff['Organs wt%']
    carcass_out = carcass_in - organs_in
    viscera_out = organs_in * coeff['Viscera Split']
    edible_organs_out = organs_in - viscera_out
    electricity_in = coeff['Electricity (kw/kg)'] * carcass_in
    Q_in = cleaned_carcass_flow.attributes['heat_flow_rate']
    Q_out = carcass_in * coeff['C_pbeef'] * (coeff['Unit Temp'] - ambient_t)
    Q_hotwater = (Q_out - Q_in) / (1-coeff['loses'])
    m_hotwater = (Q_hotwater / (C_pw * (coeff['Steam Temp'] - ambient_t)))
    Q_viscera = Q_out * coeff['Organs wt%'] * coeff['Viscera Split']
    Q_carcass = Q_out * (1- coeff['Organs wt%'])
    Q_edible = Q_out - Q_carcass - Q_viscera
    Q_loss = Q_hotwater * coeff['loses']
    print('Evisceration')

    return[{'name' : 'Hot Water (Evisceration)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_hotwater,
             'flow_type': 'Steam', 'Temperature': coeff['Steam Temp'], 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_hotwater},
           {'name' : 'Waste Water (Evisceration)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_hotwater,
             'flow_type': 'Wastewater', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss},
           {'name' : 'Meat', 'components' : ['Meat'], 'composition' :[1], 'mass_flow_rate' : carcass_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_carcass},
           {'name' : 'Viscera', 'components' : ['Viscera'], 'composition' :[1], 'mass_flow_rate' : viscera_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_viscera},
           {'name' : 'Edible Organs', 'components' : ['Organs'], 'composition' :[1], 'mass_flow_rate' : edible_organs_out,
             'flow_type': 'Product', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_edible},
           {'name' : 'Electricity (Evisceration)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit6.calculations = {'Cleaned Carcass': Evisceration_func}

# Unit 7: Visceria Processing - what is the C_p of viscera
Unit7 = Unit('Visceria Proceser')
Unit7.expected_flows_in = ['Viscera', 'Hot Water (Processer)']
Unit7.expected_flows_out = ['Tripe', 'Waste Water (Processer)']

Unit7.coefficients = {'Waste': (20/100), 'Unit Temp': 80, 'Steam Temp': 90, 'loses': 0.10, 'C_pviscera': 3.5}

def Viscera_processing_func(viscera_flow, coeff):
    viscera_in = viscera_flow.attributes['mass_flow_rate']
    waste_out = viscera_in * coeff['Waste']
    Q_in = viscera_flow.attributes['heat_flow_rate']
    Q_out = viscera_in * coeff['C_pviscera'] * (coeff['Unit Temp'] - ambient_t)
    Q_hotwater = (Q_out - Q_in) / (1- coeff['loses'])
    Q_loss = Q_hotwater * coeff['loses']
    m_hotwater = Q_hotwater / (C_pw * (coeff['Steam Temp'] - ambient_t)) 
    viscera_out = viscera_in - waste_out
    wastewater_out = waste_out + m_hotwater
    print('Processer')
    return[{'name' : 'Hot Water (Processer)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_hotwater,
             'flow_type': 'Steam', 'Temperature': coeff['Steam Temp'], 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_hotwater},
           {'name' : 'Waste Water (Processer)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': wastewater_out,
             'flow_type': 'Wastewater', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss},
           {'name' : 'Tripe', 'components' : ['Tripe'], 'composition' :[1], 'mass_flow_rate' : viscera_out,
             'flow_type': 'Product', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_out}]

Unit7.calculations = {'Viscera': Viscera_processing_func}

# Unit 8: Trimming 
Unit8 = Unit('Trimmer')
Unit8.expected_flows_in = ['Meat', 'Electricity (Trimmer)']
Unit8.expected_flows_out = ['Boneless Meat', 'Trim']

Unit8.coefficients = {'Trim Ratio': (175/1000), 'Electricity (kw/kg)': 0.15}

def Trimming_func(meat_flow, coeff):
    meat_in = meat_flow.attributes['mass_flow_rate']
    Q_in = meat_flow.attributes['heat_flow_rate']
    trim_out = meat_in * coeff['Trim Ratio']
    electricity_in = meat_in * coeff['Electricity (kw/kg)']
    meat_out = meat_in - trim_out
    print('Trimming')
    
    return[{'name' : 'Electricity (Trimmer)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Boneless Meat', 'components' : ['Meat'], 'composition' :[1], 'mass_flow_rate' : meat_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_in},
           {'name' : 'Trim', 'components' : ['Meat'], 'composition' :[1], 'mass_flow_rate' : trim_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0}]

Unit8.calculations = {'Meat': Trimming_func}

# Unit 9: Chilling - We would need split estimates on this
Unit9 = Unit('Chiller')
Unit9.expected_flows_in = ['Boneless Meat', 'Electricity (Chiller)']
Unit9.expected_flows_out = ['Boneless Chilled Meat', 'Raw Meat']

Unit9.coefficients = {'Electricity (kw/kg)': .225, 'Raw Meat Product Split': 0.25}

def Chiller_func(meat_flow, coeff):
    meat_in = meat_flow.attributes['mass_flow_rate']
    waste_heat = meat_flow.attributes['heat_flow_rate']
    raw_meat_product = meat_in * coeff['Raw Meat Product Split']
    bonless_chilled_meat_out = meat_in - raw_meat_product
    electricity_in = coeff['Electricity (kw/kg)'] * meat_in
    print('Chiller')
    return[{'name' : 'Electricity (Chiller)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': waste_heat},
           {'name' : 'Boneless Chilled Meat', 'components' : ['Meat'], 'composition' :[1], 'mass_flow_rate' : bonless_chilled_meat_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           {'name' : 'Raw Meat', 'components' : ['Meat'], 'composition' :[1], 'mass_flow_rate' : raw_meat_product,
             'flow_type': 'Product', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit9.calculations = {'Boneless Meat': Chiller_func}

# Unit 10: Cutting and Deboning - We need new splits to match products
Unit10 = Unit('Cutter')
Unit10.expected_flows_in = ['Boneless Chilled Meat', 'Electricity (Cutter)']
Unit10.expected_flows_out = ['Inedible Product', 'Edible Product', 'Smoking Product']

Unit10.coefficients = {'Electricity (kw/kg)': 0.15, 'Inedible Split': 0.15, 'Edible Split': 0.65}

def Cutter_func(meat_flow, coeff):
    meat_in = meat_flow.attributes['mass_flow_rate']
    electricity_in = coeff['Electricity (kw/kg)'] * meat_in
    inedible_out = meat_in * coeff['Inedible Split']
    edible_out = meat_in * coeff['Edible Split']
    smoking_out = meat_in - inedible_out - edible_out
    print('Cutter')
    return[{'name' : 'Electricity (Cutter)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Inedible Product', 'components' : ['Meat'], 'composition' :[1], 'mass_flow_rate' : inedible_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           {'name' : 'Edible Product', 'components' : ['Meat'], 'composition' :[1], 'mass_flow_rate' : edible_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           {'name' : 'Smoking Product', 'components' : ['Meat'], 'composition' :[1], 'mass_flow_rate' : smoking_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0}]

Unit10.calculations = {'Boneless Chilled Meat': Cutter_func}

# Unit 11: Inedible Rending - Electricity?
Unit11 = Unit('Inedible Renderer')
Unit11.expected_flows_in = ['Inedible Product', 'Steam (Inedible Rendering)', 'Electricity (Indeble Rendering)']
Unit11.expected_flows_out = ['Condensate (Inedible Rendering)', 'Waste (Inedible Rendering)', 'Protein Meal']

Unit11.coefficients = {'Steam Demand': 150. , 'Protein Meal per Input': (40/150), 'Electricity (kw/kg)': 0.15,
                       'Steam Temp': 120. }

def Inedible_rendering_func(product_flow, coeff):
    product_in = product_flow.attributes['mass_flow_rate']
    protein_out = product_in * coeff['Protein Meal per Input']
    electricity_in = product_in * coeff['Electricity (kw/kg)']
    waste_out = product_in - protein_out
    Q_steam = coeff['Steam Demand'] * product_in
    m_steam = Q_steam / Hvap
    print('Indedible Rendering')
    return[{'name' : 'Steam (Inedible Rendering)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Steam', 'Temperature': coeff['Steam Temp'], 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Inedible Rendering)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Condensate', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Electricity (Indeble Rendering)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Protein Meal', 'components' : ['Tallow'], 'composition' :[1], 'mass_flow_rate' : protein_out,
             'flow_type': 'Product', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Waste (Inedible Rendering)', 'components' : ['Waste'], 'composition' :[1], 'mass_flow_rate' : waste_out,
             'flow_type': 'Waste', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_steam}]

Unit11.calculations = {'Inedible Product': Inedible_rendering_func} 

# Unit 12: Smoking and Curing -
Unit12 = Unit('Smoker')
Unit12.expected_flows_in = ['Smoking Product', 'Electricity (Smoker)', 'Steam (Smoker)']
Unit12.expected_flows_out = ['Smoked Meat', 'Condensate (Smoker)']

Unit12.coefficients = {'Electricity (kw/kg)': .275, 'Steam Demand': 1000., 'Steam Temp': 115}

def Smoker_func(product_flow, coeff):
    product_in = product_flow.attributes['mass_flow_rate']
    Q_steam = product_in * coeff['Steam Demand']
    m_steam = Q_steam / Hvap
    electricity_in = coeff['Electricity (kw/kg)'] * product_in
    print('Smoker')
    return[{'name' : 'Steam (Smoker)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Steam', 'Temperature': coeff['Steam Temp'], 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Smoker)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Condensate', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Electricity (Smoker)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Smoked Meat', 'components' : ['Meat'], 'composition' :[1], 'mass_flow_rate' : product_in,
             'flow_type': 'Product', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_steam}]

Unit12.calculations = {'Smoking Product': Smoker_func}

# Unit 13: Edible Rendering - Electricity?
Unit13 = Unit('Edible Renderer')
Unit13.expected_flows_in = ['Trim', 'Edible Product', 'Electricity (Edible Rendering)', 'Steam (Edible Rendering)']
Unit13.required_calc_flows = 2
Unit13.expected_flows_out = ['Waste (Edible Rendering)', 'Condensate (Edible Rendering)', 'Lard']

Unit13.coefficients = {'Steam Demand': 150. , 'Lard per Input': (50/150), 'Electricity (kw/kg)': 0.125,
                       'Steam Temp': 120. }

def Edible_rendering_func(ablist, coeff):
    trim_flow = ablist[0]
    edible_product_flow = ablist[1]
    input_flows = (trim_flow.attributes['mass_flow_rate']) + (edible_product_flow.attributes['mass_flow_rate'])
    Q_in = (trim_flow.attributes['heat_flow_rate']) + (edible_product_flow.attributes['heat_flow_rate'])
    Q_steam = (input_flows * coeff['Steam Demand']) - Q_in
    m_steam = Q_steam / Hvap
    electricity_in = coeff['Electricity (kw/kg)']
    lard_out = input_flows * coeff['Lard per Input']
    waste_out = input_flows - lard_out
    print('Edible Rendering')
    return[{'name' : 'Steam (Edible Rendering)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Steam', 'Temperature': coeff['Steam Temp'], 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Edible Rendering)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Condensate', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Electricity (Edible Rendering)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Lard', 'components' : ['Lard'], 'composition' :[1], 'mass_flow_rate' : lard_out,
             'flow_type': 'Product', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Waste (Edible Rendering)', 'components' : ['Waste'], 'composition' :[1], 'mass_flow_rate' : waste_out,
             'flow_type': 'Waste', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_steam}]

Unit13.calculations = (['Trim', 'Edible Product'], Edible_rendering_func)

#############################################################################################################################################################################
processunits = [Unit1, Unit2, Unit3, Unit4, Unit5, Unit6, Unit7, Unit8,
                Unit9, Unit10, Unit11, Unit12, Unit13]
main(allflows, processunits)

for unit in processunits:
    unit.check_heat_balance(allflows)
    unit.check_mass_balance(allflows)

utilities_recap('utility_recap_animal_slaughtering_2', allflows, processunits)

for flow in allflows:
    if flow.attributes['flow_type'] == 'Product':
        print(flow)
        

