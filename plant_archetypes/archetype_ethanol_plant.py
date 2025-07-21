# # -*- coding: utf-8 -*-
"""
Created on Wednesday April 2nd 9:18:53 2025

@author: Aidan ONeil
"""

# Imports
import pandas as pd
import numpy as np

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
c_pair = 1.000
effiency_improvements = 1
ethanol_kg_per_gal = 2.989

#######################################################Units############################################################
# Unit 1: Cleaning
Unit1 = Unit('Cleaner')
Unit1.expected_flows_in = ['Dirty Corn', 'Electricity (Cleaner)', 'Compressed Air (Cleaner)']
Unit1.expected_flows_out = ['Corn', 'Air (Cleaner)']
Unit1.coefficients = {'Electricity (kw/kg)': .092 , 'Compressed Air': 1.0}

def Cleaner_corn(dirty_corn_flow, coeff):
    corn_flow = dirty_corn_flow.attributes['mass_flow_rate']
    electricity_in = corn_flow * coeff['Electricity (kw/kg)']
    compressed_air = corn_flow * coeff['Compressed Air']
    return({'name' : 'Corn', 'components' : ['Solids', 'Water'], 'composition' : [(1-0.155), 0.155], 'mass_flow_rate' : corn_flow,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           {'name' : 'Electricity (Cleaner)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Compressed Air (Cleaner)', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : compressed_air,
             'flow_type': 'Compressed Air', 'In or out' : 'In', 'Set calc' : False},
           {'name' : 'Compressed Air (Cleaner)', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : compressed_air,
             'flow_type': 'Compressed Air', 'In or out' : 'Out', 'Set calc' : False})
           

Unit1.calculations = {'Dirty Corn' : Cleaner_corn}
FlowA = Flow(name = 'Dirty Corn', components = ['Solids', 'Water'], composition = [(1-0.155), 0.155], flow_type = 'input', mass_flow_rate = 138364)
FlowA.set_calc_flow()
allflows.append(FlowA)

# Unit 2: Miller - https://www.horningmfg.com/sites/default/files/2020-05/2020-Roller-Mills-Brochure.pdf
## I used this mill brochure to calculate an average electricity of small scale mills 
Unit2 = Unit('Miller')
Unit2.expected_flows_in = ['Corn', 'Electricity (Miller)']
Unit2.expected_flows_out = ['Milled Corn']
Unit2.coefficients = {'Electricity (kw/kg)': 0.01}

def Millerfunc_corn(corn_flow, coeff):
    corn_in = corn_flow.attributes['mass_flow_rate']
    electricity_in = corn_in * coeff['Electricity (kw/kg)'] / effiency_improvements
    return[{'name' : 'Milled Corn', 'components' : ['Solids', 'Water'], 'composition' : [(1-0.155), 0.155], 'mass_flow_rate' : corn_in,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           {'name' : 'Electricity (Miller)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit2.calculations = {'Corn': Millerfunc_corn}

# Unit 3: Hydrolyzer
Unit3 = Unit('Hydrolyzer')
Unit3.expected_flows_in = ['Milled Corn', 'Water (Hydrolyzer)']
Unit3.expected_flows_out = ['Corn Slurry']

Unit3.coefficients = {'Water to Corn Ratio': (1/9.94), 'Ethanol wt%': .294}

def Hydrolyzerfunc_milledcorn(milled_corn_flow, coeff):
    milled_corn_in = milled_corn_flow.attributes['mass_flow_rate']
    solids_index = milled_corn_flow.attributes['components'].index('Solids')
    solids_in = (milled_corn_flow.attributes['composition'][solids_index]) * milled_corn_in
    ethanol_in = solids_in * coeff['Ethanol wt%']
    other_solids = solids_in - ethanol_in
    corn_moisture_in = milled_corn_in - solids_in
    water_in = milled_corn_in * coeff['Water to Corn Ratio']
    corn_slurry_out = water_in + milled_corn_in
    water_wt = (water_in + corn_moisture_in) / corn_slurry_out
    ethanol_wt = (ethanol_in)/ corn_slurry_out
    other_solids_wt = 1 - ethanol_wt - water_wt

    return[{'name' : 'Corn Slurry', 'components' : ['Ethanol','Solids', 'Water'], 'composition' : [ethanol_wt, other_solids_wt, water_wt], 'mass_flow_rate' : corn_slurry_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           
           {'name' : 'Water (Hydrolyzer)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : water_in,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit3.calculations = {'Milled Corn': Hydrolyzerfunc_milledcorn}

# Unit 4: Heater
Unit4 = Unit('Heater')
Unit4.expected_flows_in = ['Corn Slurry', 'Steam (Heater)']
Unit4.expected_flows_out = ['Hot Slurry', 'Condensate (Heater)']

Unit4.coefficients = {'c_pcorn': 2.42, 'unit_t': 85., 'loses': 0.10}

def Heaterfunc_cornslurry(corn_slurry_flow, coeff):
    corn_slurry_in = corn_slurry_flow.attributes['mass_flow_rate']
    water_index = corn_slurry_flow.attributes['components'].index('Water')
    water_wt = corn_slurry_flow.attributes['composition'][water_index]
    solids_wt = 1- water_wt
    
    # Keeping the same percentage
    ethanol_index = corn_slurry_flow.attributes['components'].index('Ethanol')
    ethanol_wt = corn_slurry_flow.attributes['composition'][ethanol_index]
    solids_percent = 1 - ethanol_wt - water_wt
    
    Q_corn_in = corn_slurry_flow.attributes['heat_flow_rate']
    t_out = coeff['unit_t']
    Q_corn_out = (corn_slurry_in * solids_wt * coeff['c_pcorn'] * (t_out - ambient_t)) + (corn_slurry_in * water_wt * C_pw * (t_out - ambient_t))
    Q_steam = (Q_corn_out - Q_corn_in) / (1 - coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss = Q_steam * coeff['loses']

    return[{'name' : 'Hot Slurry', 'components' : ['Ethanol','Solids', 'Water'], 'composition' : [ethanol_wt, solids_percent, water_wt], 'mass_flow_rate' : corn_slurry_in,
             'flow_type': 'Process stream', 'temperature' : t_out ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_corn_out},

           {'name' : 'Steam (Heater)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Heater)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           
           {'Heat loss': Q_loss}]

Unit4.calculations = {'Corn Slurry': Heaterfunc_cornslurry}

# Unit 5: Liquifaction and Saccherization
Unit5 = Unit('Liquifaction Tank')
Unit5.expected_flows_in = ['Hot Slurry', 'Steam (Liquifaction Tank)', 'Yeast, Enzymes and Acid']
Unit5.expected_flows_out = ['Glucose Slurry', 'Condensate (Liquifaction Tank)']

Unit5.coefficients = {'c_pcorn': 2.42, 'unit_t': 90., 'loses': 0.10, 'Inputs Ratio': .0009}

def Liquifactionfunc_hotslurry(hot_slurry_flow, coeff):
    slurry_in = hot_slurry_flow.attributes['mass_flow_rate']
    t_in = hot_slurry_flow.attributes['temperature']
    yea_in = slurry_in * coeff['Inputs Ratio']
    Q_in = hot_slurry_flow.attributes['heat_flow_rate']
    c_pslurry = Q_in / (slurry_in * (t_in - ambient_t))
    slurry_out = yea_in + slurry_in 
    Q_slurry_out = slurry_out * (coeff['unit_t'] - ambient_t) * c_pslurry
    Q_steam = (Q_slurry_out - Q_in) / (1 - coeff['loses'])
    Q_loss = Q_steam * coeff['loses']
    m_steam = Q_steam / Hvap

    # getting the composition
    ethanol_index = hot_slurry_flow.attributes['components'].index('Ethanol')
    ethanol_wt = hot_slurry_flow.attributes['composition'][ethanol_index]
    water_index = hot_slurry_flow.attributes['components'].index('Water')
    water_wt = hot_slurry_flow.attributes['composition'][water_index]
    other_solids_wt = 1 - water_wt - ethanol_wt
    
    return[{'name' : 'Glucose Slurry', 'components' : ['Ethanol','Solids', 'Water'], 'composition' : [ethanol_wt, other_solids_wt, water_wt], 'mass_flow_rate' : slurry_out,
             'flow_type': 'Process stream', 'temperature' : coeff['unit_t'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_slurry_out},

           {'name' : 'Yeast, Enzymes and Acid', 'components' : ['Solids'], 'composition' : [1], 'mass_flow_rate' : yea_in,
             'flow_type': 'Process stream', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Steam (Liquifaction Tank)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Liquifaction Tank)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           
           {'Heat loss': Q_loss}]

Unit5.calculations = {'Hot Slurry': Liquifactionfunc_hotslurry}

# Unit 6: Knockout Tank
Unit6 = Unit('Knockout Tank')
Unit6.expected_flows_in = ['Glucose Slurry', 'Cooling Water (Knockout Tank)']
Unit6.expected_flows_out = ['Cooled Slurry', 'Recovered Heat (Knockout Tank)']

Unit6.coefficients = {'unit_t': 32., 'loses': 0.10}

def Knockouttankfunc_glucoseslurry(glucose_slurry_flow, coeff):
    glucose_slurry_in = glucose_slurry_flow.attributes['mass_flow_rate']
    composition = glucose_slurry_flow.attributes['composition']
    Q_slurry_in = glucose_slurry_flow.attributes['heat_flow_rate']
    t_in = glucose_slurry_flow.attributes['temperature']
    c_pslurry = Q_slurry_in / (glucose_slurry_in * (t_in - ambient_t))
    Q_slurry_out = c_pslurry * glucose_slurry_in * (coeff['unit_t'] - ambient_t)
    Q_heated_water = (Q_slurry_in - Q_slurry_out) * (1 - coeff['loses'])
    Q_loss = (Q_slurry_in - Q_slurry_out) * (coeff['loses'])

    return[{'name' : 'Cooled Slurry', 'components' : ['Ethanol','Solids', 'Water'], 'composition' : composition, 'mass_flow_rate' : glucose_slurry_in,
             'flow_type': 'Process stream', 'temperature' : coeff['unit_t'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_slurry_out},

           {'Heat loss': Q_loss},

           {'name' : 'Cooling Water (Knockout Tank)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : 0,
             'flow_type': 'Cooling Water', 'temperature' : 20 ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Recovered Heat (Knockout Tank)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : 0,
             'flow_type': 'Cooling Water', 'temperature' : 0 ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_heated_water}]

Unit6.calculations = {'Glucose Slurry': Knockouttankfunc_glucoseslurry}

# Unit 7: Fermentation
Unit7 = Unit('Fermentation')
Unit7.expected_flows_in = ['Cooled Slurry']
Unit7.expected_flows_out = ['Cooled Mash', 'Carbon Dioxide']

Unit7.coefficients = {'Ethanol - CO2 Molar Ratio': 1.00, 'Ethanol MM': 46.07, 'CO2 MM': 44.009, 'loses': 0.10}

def Fermentationtankfunc_cooledslurry(cooled_slurry_flow, coeff):
    slurry_in = cooled_slurry_flow.attributes['mass_flow_rate']
    Q_in =  cooled_slurry_flow.attributes['heat_flow_rate']
    t_in = cooled_slurry_flow.attributes['temperature']
    c_p = Q_in / (slurry_in * (t_in - ambient_t))
    Q_out = Q_in * (1-coeff['loses'])
    Q_loss = Q_in * coeff['loses']
    t_out = ambient_t + (Q_out / (slurry_in * c_p))
    
    ethanol_index = cooled_slurry_flow.attributes['components'].index('Ethanol')
    ethanol_in = (cooled_slurry_flow.attributes['composition'][ethanol_index]) * slurry_in
    co2_out = (ethanol_in / coeff['Ethanol MM']) * coeff['Ethanol - CO2 Molar Ratio']  * coeff['CO2 MM']
    mash_out = slurry_in - co2_out

    # Adjusting the composition
    solids_index = cooled_slurry_flow.attributes['components'].index('Solids')
    solids_in = (cooled_slurry_flow.attributes['composition'][solids_index]) * slurry_in
    solids_out = solids_in - co2_out
    solids_wt = solids_out / mash_out
    ethanol_wt = ethanol_in / mash_out
    water_wt = 1 - solids_wt - ethanol_wt
    
    
    return[{'name' : 'Cooled Mash', 'components' : ['Ethanol','Solids', 'Water'], 'composition' : [ethanol_wt, solids_wt, water_wt], 'mass_flow_rate' : mash_out,
             'flow_type': 'Process stream', 'temperature' : t_out ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_out},

           {'name' : 'Carbon Dioxide', 'components' : ['CO2'], 'composition' : [1], 'mass_flow_rate' : co2_out,
             'flow_type': 'Product', 'temperature' : t_out ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_loss}]

Unit7.calculations = {'Cooled Slurry': Fermentationtankfunc_cooledslurry}

# Unit 14: Preheater - 71 deg C is optimal
Unit14 = Unit('Preheater')
Unit14.expected_flows_in = ['Cooled Mash', 'Steam (Preheater)']
Unit14.expected_flows_out = ['Condensate (Preheater)', 'Mash']

Unit14.coefficients = {'unit_t': 71.1, 'loses': 0.10}

def Preaheaterfunc_cooledmash(cooled_mash_flow, coeff):
    cooled_mash_in = cooled_mash_flow.attributes['mass_flow_rate']
    composition = cooled_mash_flow.attributes['composition']
    Q_in = cooled_mash_flow.attributes['heat_flow_rate']
    t_in = cooled_mash_flow.attributes['temperature']
    c_pmash = Q_in / (cooled_mash_in * (t_in - ambient_t))
    Q_out = c_pmash * cooled_mash_in * (coeff['unit_t'] - ambient_t)
    Q_steam = (Q_out - Q_in) / (1 - coeff['loses'])
    Q_loss = Q_steam * coeff['loses']
    m_steam = Q_steam / Hvap
    return[{'name' : 'Steam (Preheater)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Preheater)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'Heat loss': (Q_loss)},

           {'name' : 'Mash', 'components' : ['Ethanol','Solids', 'Water'], 'composition' : composition, 'mass_flow_rate' : cooled_mash_in,
             'flow_type': 'Process stream', 'temperature' : coeff['unit_t'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_out}]

Unit14.calculations = {'Cooled Mash': Preaheaterfunc_cooledmash}
           
# Unit 8: Distillation Column - Check the bottoms of this 
Unit8 = Unit('Distillation Column')
Unit8.expected_flows_in = ['Mash', 'Steam (Distillation Column)']
Unit8.expected_flows_out = ['Tops', 'Bottoms', 'Condensate (Distillation Column)']

Unit8.coefficients = {'Ethanol wt out': .956, 'loses': 0.15, 'Reflux Ratio': 2.0, 'Boiler Percentage': .25, 'T_top': 78.2, 'T_bottom': 95.0, 'c_pethanol': 2.44,
                      'c_pcorn': 2.42, 'Hvap_ethanol': 885.0}

def Distillationcolumnfunc_mash(mash_flow, coeff):
    mash_in = mash_flow.attributes['mass_flow_rate']
    ethanol_mash_index = mash_flow.attributes['components'].index('Ethanol')
    ethanol_in = (mash_flow.attributes['composition'][ethanol_mash_index]) * mash_in
    tops_out = ethanol_in / coeff['Ethanol wt out']
    tops_water_wt = 1 - coeff['Ethanol wt out']
    bottoms_out = mash_in - tops_out
    tops_water_out = tops_water_wt * tops_out
    water_mash_index = mash_flow.attributes['components'].index('Water')
    water_in = (mash_flow.attributes['composition'][water_mash_index]) * mash_in
    bottoms_water_out = water_in - tops_water_out
    bottoms_water_wt = bottoms_water_out / bottoms_out
    bottoms_solid_wt = 1 - bottoms_water_wt
    # Heat Balance
    c_ptops = (tops_water_wt * C_pw) + (coeff['Ethanol wt out'] * coeff['c_pethanol'])
    c_pbottoms = (bottoms_water_wt * C_pw) + (bottoms_solid_wt * coeff['c_pcorn'])
    Q_tops_out = c_ptops * tops_out * (coeff['T_top'] - ambient_t) 
    Q_bottoms_out = c_pbottoms * bottoms_out * (coeff['T_bottom'] - ambient_t)
    Q_in = mash_flow.attributes['heat_flow_rate']
    Q_reboiler = (bottoms_water_out * coeff['Boiler Percentage'] * Hvap) + (bottoms_water_out * coeff['Boiler Percentage'] * C_pw * (100 - coeff['T_bottom']))
    Q_reflux = (1/ (1 + coeff['Reflux Ratio'])) * (tops_out * coeff['Ethanol wt out'] * coeff['Hvap_ethanol']) + (tops_out * tops_water_wt * Hvap)
    # Assumption: all the heat lost in Q_reflux is used to reboil
    Q_steam = (Q_tops_out + Q_bottoms_out + Q_reboiler + Q_reflux - Q_in) / (1 - coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss = Q_steam * coeff['loses']
    print(f'Distillation Heat Demand: {Q_steam}')
    

    return[{'name' : 'Tops', 'components' : ['Ethanol', 'Water'], 'composition' : [coeff['Ethanol wt out'], tops_water_wt], 'mass_flow_rate' : tops_out,
             'flow_type': 'Process stream', 'temperature' : coeff['T_top'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': (Q_tops_out)},

           {'name' : 'Bottoms', 'components' : ['Solids', 'Water'], 'composition' : [bottoms_solid_wt, bottoms_water_wt], 'mass_flow_rate' : bottoms_out,
             'flow_type': 'Process stream', 'temperature' : coeff['T_bottom'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_bottoms_out},

           {'name' : 'Steam (Distillation Column)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Distillation Column)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'Heat loss': (Q_loss + Q_reboiler + Q_reflux)}]


Unit8.calculations = {'Mash': Distillationcolumnfunc_mash}

# Unit 13: Vaporizer, according to Homeland Energy Solutions, Molecular Sieves require a gasous stream
Unit13 = Unit('Vaporizer')
Unit13.expected_flows_in = ['Tops', 'Steam (Vaporizer)']
Unit13.expected_flows_out = ['Vaporized Tops', 'Condensate (Vaporizer)']

Unit13.coefficients = {'loses': .10, 'Hvap_ethanol': 885.0, 'c_pethanol': 2.44, 'Ethanol wt': .956}

def Vaporizerfunc_tops(tops_flow, coeff):
    tops_in = tops_flow.attributes['mass_flow_rate']
    composition = tops_flow.attributes['composition']
    Q_in = tops_flow.attributes['heat_flow_rate']
    Hvap_tops = (coeff['Hvap_ethanol'] * coeff['Ethanol wt']) + ((1-coeff['Ethanol wt']) * (Hvap))
    Q_vaporization = Hvap_tops * tops_in
    Q_steam = Q_vaporization / (1 - coeff['loses'])
    Q_loss = Q_steam * coeff['loses']
    m_steam = Q_steam / Hvap
    
    return[{'name' : 'Vaporized Tops', 'components' : ['Ethanol', 'Water'], 'composition' : composition, 'mass_flow_rate' : tops_in,
             'flow_type': 'Process stream', 'temperature' : 78.2 ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': (Q_in+Q_vaporization)},

           {'name' : 'Steam (Vaporizer)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Vaporizer)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'Heat loss': Q_loss}]
    
Unit13.calculations = {'Tops': Vaporizerfunc_tops}

# Unit 9: Molecular Sieves
Unit9 = Unit('Molecular Sieve')
Unit9.expected_flows_in = ['Vaporized Tops']
Unit9.expected_flows_out = ['Wastewater (Sieves)', 'Ethanol', 'Waste Heat']

Unit9.coefficients = {'Ethanol wt out': .997, 'loses': 0.10}

def Molecularsievefunc_vaporizedtops(vaporized_tops_flow, coeff):
    vtops_in = vaporized_tops_flow.attributes['mass_flow_rate']
    ethanol_index = vaporized_tops_flow.attributes['components'].index('Ethanol')
    ethanol_in = vaporized_tops_flow.attributes['composition'][ethanol_index] * vtops_in
    ethanol_out = ethanol_in / coeff['Ethanol wt out']
    wastewater_out = vtops_in - ethanol_out
    Q_in = vaporized_tops_flow.attributes['heat_flow_rate']
    Q_loss = Q_in * coeff['loses']
    Q_available = Q_in - Q_loss
    print(f'ethanol out mass_flow: {ethanol_out}')

    return[{'name' : 'Ethanol', 'components' : ['Ethanol', 'Water'], 'composition' : [coeff['Ethanol wt out'], 1-coeff['Ethanol wt out']], 'mass_flow_rate' : ethanol_out,
             'flow_type': 'Product', 'temperature' : 25 ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Wastewater', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : wastewater_out,
             'flow_type': 'Waste water', 'temperature' : 25 ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Waste Heat', 'flow_type': 'Waste', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_available},

           {'Heat loss': Q_loss}]

Unit9.calculations = {'Vaporized Tops': Molecularsievefunc_vaporizedtops}

# Unit 10: Centrifuge
Unit10 = Unit('Centrifuge')
Unit10.expected_flows_in = ['Bottoms', 'Electricity (Centrifuge)']
Unit10.expected_flows_out = ['Wet Cake', 'Thin Stillage']

Unit10.coefficients = {'Electricity (kw/kg)': 0.01, 'Thin Stillage solid wt': 0.075, 'Wet Cake solid wt': .65}

def Centrifugefunc_bottoms(bottoms_flow, coeff):
    bottoms_in = bottoms_flow.attributes['mass_flow_rate']
    t_in = bottoms_flow.attributes['temperature']
    Q_in = bottoms_flow.attributes['heat_flow_rate']
    solids_index = bottoms_flow.attributes['components'].index('Solids')
    solids_in = (bottoms_flow.attributes['composition'][solids_index]) * bottoms_in
    wet_cake_out = ((solids_in - (coeff['Thin Stillage solid wt'] * bottoms_in))/(coeff['Wet Cake solid wt'] - coeff['Thin Stillage solid wt']))
    thin_stillage_out = bottoms_in - wet_cake_out
    electricity_in = coeff['Electricity (kw/kg)'] * bottoms_in
    # Heat Balance Adjustment
    percent_wetcake = wet_cake_out / (wet_cake_out + thin_stillage_out)
    Q_wetcake = percent_wetcake * Q_in
    Q_thinstillage = Q_in - Q_wetcake
    

    return[{'name' : 'Wet Cake', 'components' : ['Solids', 'Water'], 'composition' : [coeff['Wet Cake solid wt'], 1-coeff['Wet Cake solid wt']], 'mass_flow_rate' : wet_cake_out,
             'flow_type': 'Process stream', 'temperature' : t_in ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_wetcake},

           {'name' : 'Thin Stillage', 'components' : ['Solids', 'Water'], 'composition' : [coeff['Thin Stillage solid wt'], 1-coeff['Thin Stillage solid wt']], 'mass_flow_rate' : thin_stillage_out,
             'flow_type': 'Process stream', 'temperature' : t_in ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_thinstillage},

           {'name' : 'Electricity (Centrifuge)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit10.calculations = {'Bottoms': Centrifugefunc_bottoms}
    

# Unit 11: Evaporator
Unit11 = Unit('Evaporator')
Unit11.expected_flows_in = ['Thin Stillage', 'Steam (Evaporator)']
Unit11.expected_flows_out = ['Stillage', 'Condensate (Evaporator)', 'Water (Evaporator)']

Unit11.coefficients = {'Stillage solid wt': .42, 'loses': 0.10, 'unit_t': 90}

def Evaporatorfunc_thinstillage(thin_still_flow, coeff):
    thin_stillage_in = thin_still_flow.attributes['mass_flow_rate']
    water_index = thin_still_flow.attributes['components'].index('Water')
    water_in = (thin_still_flow.attributes['composition'][water_index]) * thin_stillage_in
    solids_in = thin_stillage_in - water_in
    stillage_out = solids_in / coeff['Stillage solid wt']
    water_out = thin_stillage_in - stillage_out
    # Energy Balance
    Q_in = thin_still_flow.attributes['heat_flow_rate']
    t_in = thin_still_flow.attributes['temperature']
    c_p = Q_in / (thin_stillage_in * (t_in - 20))
    unit_t = coeff['unit_t']
    Q_out = stillage_out * c_p * (unit_t - ambient_t)
    Q_water_evap = water_out * Hvap + (water_out * C_pw * (100 - t_in))
    Q_steam = (Q_water_evap + Q_out - Q_in) / (1 - coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss = coeff['loses'] * Q_steam
    
    return[{'name' : 'Stillage', 'components' : ['Solids', 'Water'], 'composition' : [coeff['Stillage solid wt'], 1-coeff['Stillage solid wt']], 'mass_flow_rate' : stillage_out,
             'flow_type': 'Process stream', 'temperature' : unit_t ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_out},

           {'name' : 'Water (Evaporator)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : water_out,
             'flow_type': 'Waste Water', 'temperature' : 100 ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_water_evap},

           {'name' : 'Steam (Evaporator)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Evaporator)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'Heat loss': Q_loss}]

Unit11.calculations = {'Thin Stillage': Evaporatorfunc_thinstillage}

# Unit 12: Drum Dryer
Unit12 = Unit('Drum Dryer')
Unit12.required_calc_flows = 2
Unit12.expected_flows_in = ['Stillage', 'Wet Cake', 'Steam (Drum Dryer)']
Unit12.expected_flows_out = ['DDGS', 'Water (Drum Dryer)', 'Condensate (Drum Dryer)']

Unit12.coefficients = {'DDGS Moisture Content': .10, 'Unit Temp': 107., 'loses': 0.10, 'c_pcorn': 2.42}

def Drumdryerfunc_multi(ablist, coeff):
    wet_cake_flow = ablist[0]
    stillage_flow = ablist[1]
    stillage_in = stillage_flow.attributes['mass_flow_rate']
    stillage_solids_index = stillage_flow.attributes['components'].index('Solids')
    stillage_solids_in = (stillage_flow.attributes['composition'][stillage_solids_index]) * stillage_in
    wetcake_in = wet_cake_flow.attributes['mass_flow_rate']
    wetcake_solids_index = wet_cake_flow.attributes['components'].index('Solids')
    wetcake_solids_in = (wet_cake_flow.attributes['composition'][wetcake_solids_index]) * wetcake_in
    ddgs_solids_out = wetcake_solids_in + stillage_solids_in
    ddgs_out = ddgs_solids_out / (1 - coeff['DDGS Moisture Content'])
    ddgs_moisture_out = ddgs_out * (coeff['DDGS Moisture Content'])
    moisture_in = (stillage_in - stillage_solids_in) + (wetcake_in - wetcake_solids_in)
    moisture_out = moisture_in - ddgs_moisture_out

    # Energy Balance
    Q_wetcake = wet_cake_flow.attributes['heat_flow_rate']
    Q_stillage = stillage_flow.attributes['heat_flow_rate']
    Q_water_evap = moisture_out * (Hvap + (C_pw * (100-ambient_t)))
    Q_solids = ddgs_solids_out * coeff['c_pcorn'] * (coeff['Unit Temp'] - ambient_t)
    Q_steam = (Q_solids + Q_water_evap - Q_wetcake - Q_stillage) / (1- coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss_1 = Q_steam * coeff['loses']
    print(f'Heat Demand Drum Dryer: {Q_steam}')
    
    
    return[{'name' : 'DDGS', 'components' : ['Solids', 'Water'], 'composition' : [1- coeff['DDGS Moisture Content'], coeff['DDGS Moisture Content']], 'mass_flow_rate': ddgs_out,
             'flow_type': 'Product', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_solids},
           
           {'name' : 'Water (Drum Dryer)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : moisture_out,
             'flow_type': 'Process Stream', 'temperature' : 100 ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_water_evap},

           {'name' : 'Steam (Drum Dryer)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Drum Dryer)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'Heat loss': Q_loss_1}]

Unit12.calculations = (['Wet Cake', 'Stillage'], Drumdryerfunc_multi)

    
processunits = [Unit1, Unit2, Unit3, Unit4, Unit5, Unit6, Unit7, Unit8, Unit9, Unit10, Unit11, Unit12, Unit13, Unit14]

main(allflows, processunits)

'''
for unit in processunits:
    unit.check_heat_balance(allflows)
    unit.check_mass_balance(allflows)

for flow in allflows:
    if flow.attributes['flow_type'] == 'Product':
        print(flow)
'''

utilities_recap('ethanol_plant_recap_6', allflows, processunits)

total_steam_demand = 0
total_electricity = 0

for flow in allflows:
    if flow.attributes['flow_type'] == 'Steam':
        total_steam_demand = total_steam_demand + flow.attributes['heat_flow_rate']
    elif flow.attributes['flow_type'] == 'Electricity':
        total_electricity = total_electricity + flow.attributes['elec_flow_rate']

        

        
