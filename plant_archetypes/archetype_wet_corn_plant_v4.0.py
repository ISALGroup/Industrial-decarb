# -*- coding: utf-8 -*-
"""
Created on Tuesday Feb 11th 14:28:53 2025

@author: Aidan ONeil
"""

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
conversion_starch_split = .33
ethanol_starch_split = .33
product_starch_split = 1 - ethanol_starch_split - conversion_starch_split 

#### Reminder, go back in and add lose and heat embodied within changing the temp
# Unit 1: Cleaning
Unit1 = Unit('Cleaner')
Unit1.expected_flows_in = ['Dirty Corn', 'Electricity (Cleaner)', 'Compressed Air (Cleaner)']
Unit1.expected_flows_out = ['Corn', 'Air (Cleaner)']
Unit1.coefficients = {'Electricity (kw/kg)': 34.90 , 'Compressed Air': 1.0}

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
FlowA = Flow(name = 'Dirty Corn', components = ['Solids', 'Water'], composition = [(1-0.155), 0.155], flow_type = 'input', mass_flow_rate = 100000)
FlowA.set_calc_flow()
allflows.append(FlowA)


## Assumption: Using table 3 from the Wet Corn Energy Guide, I will redefine the flows so 100% of starch and oil goes to hydrated corn and the rest of the solid fraction
## is the same ratio split of feed, meal
# Unit 2 : Steep Tank
Unit2 = Unit('Steep Tank')
Unit2.expected_flows_in = ['SO2', 'Corn', 'Water (steep tank)', 'Steam (steep tank)']
Unit2.expected_flows_out = ['Hydrated Corn', 'Light Steep Water', 'Condensate (steep tank)']
Unit2.coefficients = {'Corn to Water Ratio': (2./3.), 'SO2 Ratio to Water': 0.002, 'Outlet Temp': 51.,
                      'Cp_water': 4.21, 'Cp_corn': 2.42, 'Hydrated Corn Moisture': 0.45,
                      'Steep to Hydrated Corn':(0.96/1.6), 'Losses': 0.10}

def Steeptankfunc_corn(corn_flow, coeff):
    corn_amount = corn_flow.attributes['mass_flow_rate']
    water_amount = corn_amount / coeff['Corn to Water Ratio']
    solids_index = corn_flow.attributes['components'].index('Solids')
    moisture_index = corn_flow.attributes['components'].index('Water')
    corn_moisture = corn_flow.attributes['composition'][moisture_index]
    corn_solids = corn_flow.attributes['composition'][solids_index]
    print(corn_solids)
    so2_amount = water_amount * coeff['SO2 Ratio to Water']
    t_out = coeff['Outlet Temp']
    mass_in = water_amount + corn_amount
    steep_liquor_out = corn_amount * corn_solids * 0.065
    germ_out = corn_amount * corn_solids * 0.075
    bran_out = .120 * corn_amount * corn_solids
    gluten_out = 0.056 * corn_amount * corn_solids
    starch_out = .680 * corn_amount * corn_solids
    dry_amount_hydratedcorn = starch_out + gluten_out + bran_out + germ_out
    dry_amount_lsw = (corn_amount * corn_solids) - dry_amount_hydratedcorn
    hydrated_corn_out = dry_amount_hydratedcorn / (1- coeff['Hydrated Corn Moisture'])
    water_amount_hydratedcorn = hydrated_corn_out * coeff['Hydrated Corn Moisture']
    lsw_out = corn_amount + water_amount + so2_amount - hydrated_corn_out
    lsw_solids_coeff = dry_amount_lsw / lsw_out
    lsw_so2_coeff = so2_amount / lsw_out
    lsw_moisture_coeff = 1 - lsw_solids_coeff - lsw_so2_coeff
    water_amount_lsw = lsw_out * lsw_moisture_coeff
    #Energy Balance 
    Q_steepwater = ((dry_amount_hydratedcorn * coeff['Cp_corn']) + (water_amount_hydratedcorn * coeff['Cp_water'])) * (t_out - ambient_t)
    Q_hydratedcorn = ((dry_amount_lsw * coeff['Cp_corn']) + (water_amount_lsw * coeff['Cp_water'])) * (t_out - ambient_t)
    Q_out = Q_steepwater + Q_hydratedcorn
    Q_req = Q_out / (1 - coeff['Losses'])
    Q_loss = coeff['Losses'] * Q_req
    steam_amount = Q_req / Hvap
    condensate_amount = steam_amount
    

    return[{'name' : 'Steam (steep tank)', 'components' : 'Water', 'mass_flow_rate' : steam_amount,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_req},

           {'name' : 'Condensate (steep tank)', 'components' : 'Water', 'mass_flow_rate' : condensate_amount,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Water (steep tank)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : water_amount,
             'flow_type': 'Water', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'SO2', 'components' : ['SO2'], 'composition': [1], 'mass_flow_rate' : so2_amount,
             'flow_type': 'Process stream', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},

            {'name' : 'Hydrated Corn', 'components' : ['Starch', 'Germ', 'Bran', 'Gluten',  'Moisture'], 'composition': [(starch_out/hydrated_corn_out),(germ_out/hydrated_corn_out),(bran_out/hydrated_corn_out),(gluten_out/hydrated_corn_out) ,coeff['Hydrated Corn Moisture']],
             'mass_flow_rate' : hydrated_corn_out, 'flow_type': 'Process stream', 'temperature' : t_out, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_hydratedcorn},
            
            {'name' : 'Light Steep Water', 'components' : ['Solids', 'Moisture', 'SO2'], 'composition' : [lsw_solids_coeff, lsw_moisture_coeff , lsw_so2_coeff] , 'mass_flow_rate' : lsw_out,
                     'flow_type': 'Process stream', 'temperature' : t_out, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate' : Q_steepwater },

            {'Heat loss': Q_loss}]


  
Unit2.calculations = {'Corn' : Steeptankfunc_corn}



# Unit 3: Steep Water Evaporator
Unit3 = Unit('Steep Water Evaporator')
Unit3.expected_flows_in = ['Light Steep Water', 'Steam (MEE)']
Unit3.expected_flows_out = ['Steep Concentrate', 'Condensate (MLE, process)', 'Condensate (MLE, utility)']
Unit3.coefficients = {'Steam economy' : 2.5, 'Moisture out' : 0.525, 'Temperature steam in' : 120,
                       'Temperature steep out' : 93.3, 'Latent heat of vaporization at given T' : 2260.1, 'Water Cp' : 4.2}

def Steepwaterevaporator_lsw(lsw_flow, coeff):
    moisture_in = lsw_flow.attributes['composition'][lsw_flow.attributes['components'].index('Moisture')]
    lsw_in = lsw_flow.attributes['mass_flow_rate']
    moisture_in_amount = lsw_in * moisture_in
    solids_amount = lsw_in - moisture_in_amount
    solids_ratio = lsw_flow.attributes['composition'][lsw_flow.attributes['components'].index('Solids')]
    solids_amount = solids_ratio * lsw_in
    
    mu_out = coeff['Moisture out']
    moisture_out_amount = (mu_out/(1 - mu_out))*solids_amount
    water_out_amount = moisture_in_amount - moisture_out_amount
    steam_in_amount = water_out_amount/coeff['Steam economy']
    Q_lsw = lsw_flow.attributes['heat_flow_rate']
    steam_in_t = coeff['Temperature steam in']
    Q_steam_in = steam_in_amount * (((coeff['Water Cp']) * (steam_in_t - ambient_t)) + coeff['Latent heat of vaporization at given T'])
    Q_cond_out = steam_in_amount * (((coeff['Water Cp']) * (steam_in_t - ambient_t)))
    cp_solids = 2.42
    cp_water = coeff['Water Cp']
    liq_t = coeff['Temperature steep out']
    concentrate_out_amount = solids_amount + moisture_out_amount
    Q_liq_out = ((cp_solids * solids_amount) + (cp_water * moisture_out_amount)) * (liq_t - ambient_t)
    Q_total_in = Q_steam_in + Q_lsw
    delta_T_vap = (((Q_steam_in - Q_cond_out) + (Q_lsw - Q_liq_out)))/(water_out_amount * cp_water)
    t_vap_out = ambient_t+ delta_T_vap
    Q_condensate = Q_total_in - Q_cond_out - Q_liq_out
    missing_water = lsw_in - (concentrate_out_amount + water_out_amount)
    water_out_amount = water_out_amount + missing_water
    
    return [{'name' : 'Steam (MEE)', 'components' : ['Water'], 'mass_flow_rate' : steam_in_amount,
             'flow_type': 'Steam', 'temperature' : steam_in_t,  'In or out' : 'In', 'heat_flow_rate' : Q_steam_in ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Steep Concentrate', 'components' : ['Solids', 'Water'], 'composition': [1-mu_out , mu_out], 'mass_flow_rate' : concentrate_out_amount,
                     'flow_type': 'Process', 'temperature' : liq_t, 'pressure':1 , 'heat_flow_rate' :Q_liq_out ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Condensate (MLE, process)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : water_out_amount,
                     'flow_type': 'Wastewater', 'temperature' : t_vap_out, 'pressure':1 , 'heat_flow_rate' :Q_condensate ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Condensate (MLE, utility)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : steam_in_amount,
                     'flow_type': 'Condensate', 'temperature' : steam_in_t, 'pressure':1.98 , 'heat_flow_rate' :Q_cond_out ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
           ]

Unit3.calculations = {'Light Steep Water' : Steepwaterevaporator_lsw}


# Unit 4: Degerminator and Germ Seperator
Unit4 = Unit('Degerminator')
Unit4.expected_flows_in = ['Hydrated Corn', 'Electricity (Degerminator)']
Unit4.expected_flows_out = ['Kernal', 'Germ']

Unit4.coefficients = {'Electricity (kw/kg)' : 0.0004559 , 'loses': .10}

def Degerminatorfunc_hydratedcorn(hydrated_corn_flow, coeff):
    hydrated_corn_amount = hydrated_corn_flow.attributes['mass_flow_rate']
    germ_index = hydrated_corn_flow.attributes['components'].index('Germ')
    germ_solids = hydrated_corn_flow.attributes['composition'][germ_index]
    germ_amount = hydrated_corn_amount * germ_solids
    germ_out = germ_amount / .45
    # Kernal Coefficients Adjustments
    kernal_amount = hydrated_corn_amount - germ_out
    starch_index = hydrated_corn_flow.attributes['components'].index('Starch')
    starch_solids = hydrated_corn_flow.attributes['composition'][starch_index]
    bran_index = hydrated_corn_flow.attributes['components'].index('Bran')
    bran_solids = hydrated_corn_flow.attributes['composition'][bran_index]
    gluten_index = hydrated_corn_flow.attributes['components'].index('Gluten')
    gluten_solids = hydrated_corn_flow.attributes['composition'][gluten_index]
    total_solids = gluten_solids + bran_solids + starch_solids
    adjusted_starch_index = .45 * (starch_solids/total_solids)
    adjusted_bran_index = .45 * (bran_solids/total_solids)
    adjusted_gluten_index = .45 * (gluten_solids/total_solids)
    
    electricity_amount = hydrated_corn_amount * coeff['Electricity (kw/kg)'] / effiency_improvements
    # Loses:
    t_in = hydrated_corn_flow.attributes['temperature']
    Q_in = hydrated_corn_flow.attributes['heat_flow_rate']
    c_p = Q_in / ((t_in-20) * hydrated_corn_amount)
    Q_losses = coeff['loses'] * Q_in
    degrerminator_delT = Q_losses / (hydrated_corn_amount * c_p)
    t_out = t_in - degrerminator_delT
    Q_germ = germ_out * c_p * (t_out - ambient_t)
    Q_kernal = kernal_amount * c_p * (t_out - ambient_t)
    

    return[{'name' : 'Germ', 'components' : ['Germ', 'Water'], 'composition' : [.45,.55], 'mass_flow_rate' : germ_out,
             'flow_type': 'Process stream', 'temperature' : t_out ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_germ},
           {'name' : 'Kernal', 'components' : ['Starch', 'Bran', 'Gluten', 'Water'], 'composition' : [adjusted_starch_index, adjusted_bran_index, adjusted_gluten_index,.55], 'mass_flow_rate' : kernal_amount,
             'flow_type': 'Process stream', 'temperature' : t_out , 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_kernal},
           {'name' : 'Electricity (Degerminator)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_amount, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_losses}]


Unit4.calculations = {'Hydrated Corn': Degerminatorfunc_hydratedcorn}


    
# Unit 5: Germ Dryer - This was checked against the Energy Star document, 127% steam mass to water evaportated in line with the 120% reported
Unit5 = Unit('Germ Dryer')
Unit5.expected_flows_in = ['Steam (Germ Dryer)', 'Air (Germ Dryer)', 'Germ']
Unit5.expected_flows_out = ['Condensate (Germ Dryer)', 'Exhuast (Germ Dryer)', 'Dried Germ']

Unit5.coefficients = {'Dry Germ Moisture Content': 0.03, 'Air Ratio': 3.0, 'Cp_corn': 2.42, 'Cp_air': 1.00, 'loses': 0.10}

def Germdryierfunc_germ (germ_flow, coeff):
    germ_amount = germ_flow.attributes['mass_flow_rate']
    solids_index = germ_flow.attributes['components'].index('Germ')
    solids_in = (germ_flow.attributes['composition'][solids_index]) * germ_amount
    dry_germ_out = solids_in / (1 - coeff['Dry Germ Moisture Content'])
    germ_moisture_out = dry_germ_out * coeff['Dry Germ Moisture Content']
    germ_moisture_in = germ_amount * (1-(germ_flow.attributes['composition'][solids_index]))
    water_exhaust = germ_moisture_in - germ_moisture_out
    t_in = germ_flow.attributes['temperature']
    air_in = coeff['Air Ratio'] * germ_amount
    air_out = air_in + water_exhaust
    # Energy Balance
    exhaust_temp = 90
    Q_germ_in = germ_flow.attributes['heat_flow_rate']
    Q_water_evap = water_exhaust * (Hvap + (C_pw * (exhaust_temp - t_in)))
    Q_solids = dry_germ_out * (coeff['Cp_corn']) * (exhaust_temp - t_in)
    Q_air = air_in * coeff['Cp_air'] * (exhaust_temp - t_in)
    Q_in = (Q_water_evap + Q_solids + Q_air - Q_germ_in) / (1 - coeff['loses'])
    m_steam = Q_in / Hvap
    m_condensate = m_steam
    Q_loss = Q_in * coeff['loses']

    return[{'name' : 'Steam (Germ Dryer)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_in},
           {'name' : 'Dried Germ', 'components' : ['Solids', 'Water'], 'composition' : [1- coeff['Dry Germ Moisture Content'],coeff['Dry Germ Moisture Content']], 'mass_flow_rate' : dry_germ_out,
             'flow_type': 'Process stream', 'temperature' : exhaust_temp, 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_solids},
           {'name' : 'Condensate (Germ Dryer)', 'components' : 'Water', 'mass_flow_rate' : m_condensate,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Exhaust (Germ Dryer)', 'components' : 'Air', 'mass_flow_rate' : air_out,
             'flow_type': 'Exhaust', 'temperature' : exhaust_temp ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_air+Q_water_evap},
           {'name' : 'Air (Germ Dryer)', 'components' : 'Air', 'mass_flow_rate' : air_in,
             'flow_type': 'Air', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss}]


Unit5.calculations = {'Germ': Germdryierfunc_germ}


# Unit 6: Oil Extractor
Unit6 = Unit('Oil Extractor')
Unit6.expected_flows_in = ['Dried Germ', 'Electricity (Oil Extractor)', 'Steam (Oil Extractor)', 'Cooling Water In (Oil Extractor)']
Unit6.expected_flows_out = ['Corn Oil Meal', 'Corn Oil', 'Condensate (Oil Extractor)', 'Cooling Water Out (Oil Extractor)']

Unit6.coefficients = {'Electricity (kw/kg)': 23.3, 'Oil to Germ Ratio': (.050/.108), 'loses': 0.10, 'Cp_corn': 2.42, 'temp': 90, 'delT_cw': 20.0}

def Oilextractorfunc_driedgerm(dried_germ_flow, coeff):
    germ_in = dried_germ_flow.attributes['mass_flow_rate']
    Q_germ = dried_germ_flow.attributes['heat_flow_rate']
    t_in = dried_germ_flow.attributes['temperature']
    water_index = dried_germ_flow.attributes['components'].index('Water')
    solid_index = dried_germ_flow.attributes['components'].index('Solids')
    water_in = (dried_germ_flow.attributes['composition'][water_index]) * germ_in
    solids_in = (dried_germ_flow.attributes['composition'][solid_index]) * germ_in
    
    electricity_in = germ_in * coeff['Electricity (kw/kg)'] / effiency_improvements
    corn_oil_out = solids_in * coeff['Oil to Germ Ratio']
    meal_out = germ_in - water_in - corn_oil_out
    Q_water_evap = (water_in * (Hvap)) + (water_in * C_pw * (100 - t_in))
    Q_oil = corn_oil_out * coeff['Cp_corn'] * (coeff['temp'] - ambient_t)
    Q_meal = meal_out * coeff['Cp_corn'] * (coeff['temp'] - ambient_t)
    Q_in = (Q_water_evap + Q_oil + Q_meal - Q_germ) / (1 - coeff['loses'])
    m_steam = Q_in / Hvap
    m_condensate = m_steam + water_in
    m_cw = (Q_in * coeff['loses'])/(C_pw * coeff['delT_cw'])
    Q_cw = m_cw *C_pw * coeff['delT_cw']
    
    return({'name' : 'Steam (Oil Extractor)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_in},
           {'name' : 'Condensate (Oil Extractor)', 'components' : 'Water', 'mass_flow_rate' : m_condensate,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_water_evap},
           {'name' : 'Electricity (Oil Extractor)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Cooling Water In (Oil Extractor)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : m_cw,
             'flow_type': 'Cooling Water',  'temperature': 20,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Cooling Water Out (Oil Extractor)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : m_cw,
             'flow_type': 'Cooling Water',  'temperature': (20+coeff['delT_cw']) ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_cw},
           {'name' : 'Corn Oil Meal', 'components' : ['Solids'], 'composition' : [1], 'mass_flow_rate' : meal_out,
             'flow_type': 'Product', 'temperature' : coeff['temp'], 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_meal},
           {'name' : 'Corn Oil', 'components' : ['Solids'], 'composition' : [1], 'mass_flow_rate' : corn_oil_out,
             'flow_type': 'Process stream', 'temperature' : coeff['temp'], 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_oil})


Unit6.calculations = {'Dried Germ': Oilextractorfunc_driedgerm}

    
# Unit 7: Oil Separators and Filters
Unit7 = Unit('Oil Separators')
Unit7.expected_flows_in = ['Corn Oil', 'Electricity (Oil Seperator)']
Unit7.expected_flows_out = ['Refined Corn Oil']

Unit7.coefficients = {'Electricity (kw/kg)': 695.6, 'loses': 0.10, 'c_p': 1.67}

def Oilseperatorfunc_cornoil(corn_oil_flow, coeff):
    corn_oil = corn_oil_flow.attributes['mass_flow_rate']
    Q_in = corn_oil_flow.attributes['heat_flow_rate']
    t_in = corn_oil_flow.attributes['temperature']
    electricity_in = corn_oil * coeff['Electricity (kw/kg)']
    Q_loss = Q_in * coeff['loses']
    t_out = t_in - (Q_loss / (corn_oil * coeff['c_p']))
    Q_cornoil = Q_in - Q_loss
    return({'name' : 'Electricity (Oil Seperator)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Corn Oil', 'components' : ['Solids'], 'composition' : [1], 'mass_flow_rate' : corn_oil,
             'flow_type': 'Product', 'temperature' : t_out, 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_cornoil})

Unit7.calculations = {'Corn Oil': Oilseperatorfunc_cornoil}

    
# Unit 8: Grinding Mills
Unit8 = Unit('Grinding Mill')
Unit8.expected_flows_in = ['Kernal', 'Electricity (Grinding Mill)']
Unit8.expected_flows_out = ['Ground Kernal']

Unit8.coefficients = {'Electricity (kw/kg)': 32.3, 'loses': .10}

def Grindingmillfunc_kernal(kernal_flow, coeff):
    kernal_in = kernal_flow.attributes['mass_flow_rate']
    composition = kernal_flow.attributes['composition']
    t_in = kernal_flow.attributes['temperature'] 
    electricity_in = coeff['Electricity (kw/kg)'] * kernal_in / effiency_improvements
    kernal_out = kernal_in
    Q_in = kernal_flow.attributes['heat_flow_rate']
    c_p = Q_in / (kernal_in * (t_in - ambient_t))
    Q_loss = Q_in * coeff['loses']
    Q_out = Q_in - Q_loss
    t_out = ambient_t + (Q_out / (c_p * kernal_out))

    return[{'name' : 'Ground Kernal', 'components' : ['Starch', 'Bran', 'Gluten', 'Water'], 'composition' : composition, 'mass_flow_rate' : kernal_out,
             'flow_type': 'Process stream', 'temperature' : t_out ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_out},

           {'name' : 'Electricity (Grinding Mill)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},

           {'Heat loss': Q_loss}]


Unit8.calculations = {'Kernal': Grindingmillfunc_kernal}


# Unit 9: Washing Screens
Unit9 = Unit('Washing Screens')
Unit9.expected_flows_in = ['Water (Washing Screens)', 'Electricity (Washing Screens)', 'Ground Kernal']
Unit9.expected_flows_out = ['Wet Fiber', 'Starch']

Unit9.coefficients = {'Water to Kernal': (0.60/1.440), 'loses' : 0.10, 'Electricity (kw/kg)': 0.930}

def Washingscreensfunc_groundkernal(ground_kernal_flow, coeff):
    kernal_in = ground_kernal_flow.attributes['mass_flow_rate']
    water_index = ground_kernal_flow.attributes['components'].index('Water')
    water_fraction_in = ground_kernal_flow.attributes['composition'][water_index]
    kernal_moisture_in = water_fraction_in * kernal_in
    fiber_index = ground_kernal_flow.attributes['components'].index('Bran')
    fiber_fraction_in = ground_kernal_flow.attributes['composition'][fiber_index]
    kernal_fiber_in = fiber_fraction_in * kernal_in 
    water_in = coeff['Water to Kernal'] * kernal_in
    fiber_out = water_in + kernal_fiber_in
    hull_out = kernal_in + water_in - fiber_out
    electricity_in = coeff['Electricity (kw/kg)'] * kernal_in / effiency_improvements
    # Adjust the compositions of hull
    starch_index = ground_kernal_flow.attributes['components'].index('Starch')
    starch_fraction_in = ground_kernal_flow.attributes['composition'][starch_index]
    gluten_index = ground_kernal_flow.attributes['components'].index('Gluten')
    gluten_fraction_in = ground_kernal_flow.attributes['composition'][gluten_index]
    hull_water_content = 0.55
    adjusted_starch_index = (starch_fraction_in / (starch_fraction_in + gluten_fraction_in)) * (1- hull_water_content)
    adjusted_gluten_index = 1 - hull_water_content - adjusted_starch_index
    # Adjust T
    Q_in = ground_kernal_flow.attributes['heat_flow_rate']
    Q_out = (1 - coeff['loses']) * Q_in
    Q_loss = Q_in * coeff['loses']
    Q_hull = (hull_out / (hull_out + fiber_out)) * Q_out
    Q_fiber = Q_out - Q_hull
    c_p = (hull_water_content * 4.21) + ((1-hull_water_content) * 2.42)
    t_out = ambient_t + (Q_hull/(hull_out*c_p))
    print(t_out)
    
    return[{'name' : 'Water (Washing Screens)', 'components' : 'Water', 'mass_flow_rate' : water_in,
             'flow_type': 'Water', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Electricity (Washing Screens)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity','elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Wet Fiber', 'components' : ['Solids', 'Water'], 'composition' : [(kernal_fiber_in/fiber_out), (water_in/fiber_out)], 'mass_flow_rate' : fiber_out,
             'flow_type': 'Process stream','elec_flow_rate' : 0,'temperature': t_out , 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_fiber},

           {'name' : 'Hull', 'components' : ['Starch', 'Gluten', 'Water'], 'composition' : [adjusted_starch_index, adjusted_gluten_index, hull_water_content], 'mass_flow_rate' : hull_out,
             'flow_type': 'Process stream','elec_flow_rate' : 0,'temperature': t_out , 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_hull},
           {'Heat loss': Q_loss}]
    

Unit9.calculations = {'Ground Kernal': Washingscreensfunc_groundkernal}


# Unit 10: Centrigugal Seperators
Unit10 = Unit('Centrifugal Seperators')
Unit10.expected_flows_in = ['Hull', 'Water (Centrifugal Seperators)', 'Electricity (Centrifugal Seperators)']
Unit10.expected_flows_out = ['Wet Gluten', 'Unpure Starch']

Unit10.coefficients = {'Water to Hull Ratio': (.50/.64), 'Electricity (kw/kg)': 90.87, 'loses': 0.10}

def Centrifugalseperatorsfunc_hull(hull_flow, coeff):
    hull_in = hull_flow.attributes['mass_flow_rate']
    water_index = hull_flow.attributes['components'].index('Water')
    water_fraction_in = hull_flow.attributes['composition'][water_index]
    gluten_index = hull_flow.attributes['components'].index('Gluten')
    gluten_fraction_in = hull_flow.attributes['composition'][gluten_index]
    gluten_in = gluten_fraction_in * hull_in 
    hull_moisture_in = water_fraction_in * hull_in
    water_in = coeff['Water to Hull Ratio'] * hull_in
    electricity_in = coeff['Electricity (kw/kg)'] * hull_in / effiency_improvements
    gluten_out = gluten_in + water_in
    gluten_coeff = gluten_in / gluten_out
    starch_out = hull_in + water_in - gluten_out 
    # Starch coefficient adjustment
    starch_index = 1 - water_fraction_in 
    # Adjust T
    Q_in = hull_flow.attributes['heat_flow_rate']
    Q_loss = Q_in * coeff['loses']
    Q_out = Q_in * (1-coeff['loses'])
    Q_starch = (starch_out / (starch_out + gluten_out)) * Q_out
    Q_gluten = Q_out - Q_starch
    c_p = (water_fraction_in * 4.21) + (2.42 * (starch_index))
    t_out = ambient_t + (Q_starch/(starch_out * c_p))
    print(f'T_out is {t_out}')
    
    return[{'name' : 'Water (Centrifugal Seperators)', 'components' : 'Water', 'mass_flow_rate' : water_in,
             'flow_type': 'Water', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Electricity (Centrifugal Seperators)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Wet Gluten', 'components' : ['Solids', 'Water'], 'composition' : [ gluten_coeff, 1-gluten_coeff], 'mass_flow_rate' : gluten_out,
             'flow_type': 'Process stream','elec_flow_rate' : 0, 'temperature': t_out , 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_gluten},

           {'name' : 'Unpure Starch', 'components' : ['Solids', 'Water'], 'composition' : [starch_index, water_fraction_in], 'mass_flow_rate' : starch_out,
             'flow_type': 'Process stream','elec_flow_rate' : 0,  'temperature': t_out ,'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_starch},
           {'Heat loss': Q_loss}]



Unit10.calculations = {'Hull': Centrifugalseperatorsfunc_hull}

# Unit 20: Gluten Dewater - Dewaterer takes gluten to 43% solids
## https://www.alfalaval.com/globalassets/documents/industries/food-dairy-and-beverage/starch-and-sweetener/corn-gluten-dewatering-solutions.pdf
Unit20 = Unit('Gluten Dewater')
Unit20.expected_flows_in = ['Wet Gluten']
Unit20.expected_flows_out = ['Gluten', 'Gluten Water']

Unit20.coefficients = {'Moisture Content Out': .57}

def Glutendewaterfunc_wetgluten(wet_gluten_flow, coeff):
    wet_gluten_in = wet_gluten_flow.attributes['mass_flow_rate']
    temp = wet_gluten_flow.attributes['temperature']
    Q_gluten = wet_gluten_flow.attributes['heat_flow_rate']
    water_index = wet_gluten_flow.attributes['components'].index('Water')
    water_fraction_in = wet_gluten_flow.attributes['composition'][water_index]
    solid_fraction_in = 1 - water_fraction_in
    dry_gluten_out = (solid_fraction_in * wet_gluten_in)/ (1 - coeff['Moisture Content Out'])
    water_out = wet_gluten_in - dry_gluten_out

    return[{'name' : 'Gluten', 'components' : ['Solids', 'Water'], 'composition' : [ (1-coeff['Moisture Content Out']), coeff['Moisture Content Out']], 'mass_flow_rate' : dry_gluten_out,
             'flow_type': 'Process stream','elec_flow_rate' : 0, 'temperature': temp , 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_gluten},
           {'name' : 'Gluten Water', 'components' : ['Water'], 'composition' : [1] , 'mass_flow_rate' : water_out,
             'flow_type': 'Process stream','elec_flow_rate' : 0, 'temperature': temp , 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit20.calculations = {'Wet Gluten': Glutendewaterfunc_wetgluten}

# Unit 21: Fiber Dewater - Dewaterer takes fiber to <= 60% Moisture
## https://myande.en.made-in-china.com/product/UBlEzbRcaghi/China-Corn-Germ-Screw-Press-Starch-Fibre-Dewatering-Machine.html
Unit21 = Unit('Fiber Dewater')
Unit21.expected_flows_in = ['Wet Fiber']
Unit21.expected_flows_out = ['Fiber', 'Fiber Water']

Unit21.coefficients = {'Moisture Content Out': .60}


def Fiberdewaterfunc_wetfiber(wet_fiber_flow, coeff):
    wet_fiber_in = wet_fiber_flow.attributes['mass_flow_rate']
    temp = wet_fiber_flow.attributes['temperature']
    Q_fiber = wet_fiber_flow.attributes['heat_flow_rate']
    water_index = wet_fiber_flow.attributes['components'].index('Water')
    water_fraction_in = wet_fiber_flow.attributes['composition'][water_index]
    solid_fraction_in = 1 - water_fraction_in
    solids_in = solid_fraction_in * wet_fiber_in
    dry_fiber_out = (solids_in)/ (1 - coeff['Moisture Content Out'])
    fiber_water_out = wet_fiber_in - dry_fiber_out

    return[{'name' : 'Fiber', 'components' : ['Solids', 'Water'], 'composition' : [(1-coeff['Moisture Content Out']), coeff['Moisture Content Out']], 'mass_flow_rate' : dry_fiber_out,
             'flow_type': 'Process stream','elec_flow_rate' : 0, 'temperature': temp , 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_fiber},
           {'name' : 'Fiber Water', 'components' : ['Water'], 'composition' : [1] , 'mass_flow_rate': fiber_water_out,
             'flow_type': 'Process stream','elec_flow_rate' : 0, 'temperature': temp , 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit21.calculations = {'Wet Fiber': Fiberdewaterfunc_wetfiber}


# Unit 11: Feed Dryer --> this is where I should check for units from GHGRP...
# we also need to talk about how we are handling this; direct use of fuels 
Unit11 = Unit('Feed Dryer')
Unit11.required_calc_flows = 3
Unit11.expected_flows_in = ['Gluten', 'Fiber', 'Steep Concentrate', 'Fuel (Feed Dryer)', 'Air (Feed Dryer)']
Unit11.expected_flows_out = ['Feed', 'Exhuast (Feed Dryer)']

Unit11.coefficients = {'Feed Moisture Content': 0.10, 't_out': 120, 'c_pcorn': 2.42, 'exhaust_t': 200, 'Air Ratio': 3.0,
                       'Loses': .10, 'HHV Fuel (kJ/kg)':  52200.0 }

def Feeddryerfunc_multi(ablist, coeff):
    steep_concentrate_flow = ablist[2]
    fiber_flow = ablist[1]
    gluten_flow = ablist[0]
    steep_in = steep_concentrate_flow.attributes['mass_flow_rate']
    water_index = steep_concentrate_flow.attributes['components'].index('Water')
    water_fraction_in = steep_concentrate_flow.attributes['composition'][water_index]
    steep_moisture_in = water_fraction_in * steep_in
    steep_solids_in = (1 - water_fraction_in) * steep_in
    gluten_in = gluten_flow.attributes['mass_flow_rate']
    water_index = gluten_flow.attributes['components'].index('Water')
    water_fraction_in = gluten_flow.attributes['composition'][water_index]
    gluten_moisture_in = water_fraction_in * gluten_in
    gluten_solids_in = (1- water_fraction_in) * gluten_in
    fiber_in = fiber_flow.attributes['mass_flow_rate']
    water_index = fiber_flow.attributes['components'].index('Water')
    water_fraction_in = fiber_flow.attributes['composition'][water_index]
    fiber_moisture_in = water_fraction_in * fiber_in
    fiber_solids_in = (1 - water_fraction_in) * fiber_in
    moisture_in = fiber_moisture_in + gluten_moisture_in + steep_moisture_in
    solids_in = (gluten_solids_in + steep_solids_in + fiber_solids_in)
    feed_out = solids_in / (1 - coeff['Feed Moisture Content'])
    feed_moisture_out = feed_out * coeff['Feed Moisture Content']
    feed_solids_out = feed_out - feed_moisture_out 
    water_evaporated = moisture_in - feed_moisture_out
    air_in = coeff['Air Ratio'] * (moisture_in + solids_in)
    
    # Energy Balance:
    Q_feed = feed_out * coeff['c_pcorn'] * (coeff['t_out'] - ambient_t)
    Q_steep = steep_concentrate_flow.attributes['heat_flow_rate']
    Q_gluten = gluten_flow.attributes['heat_flow_rate']
    Q_fiber = fiber_flow.attributes['heat_flow_rate']
    Q_in = Q_steep + Q_gluten + Q_fiber
    Q_dryair = air_in * (1.006) * (coeff['exhaust_t'] - 20)
    Q_water_evaporated = water_evaporated * (Hvap + C_pw * (coeff['exhaust_t'] - 20))
    Q_out = Q_dryair + Q_water_evaporated + Q_feed
    Q_fuel = (Q_out - Q_in)/ (1- coeff['Loses'])
    m_fuel = Q_fuel / coeff['HHV Fuel (kJ/kg)']
    Q_loss = Q_fuel * coeff['Loses']

    
    # Need a method for emissions - this is probably where mass balance needs to be done
    
    return [{'name' : 'Fuel (Feed Dryer)', 'mass_flow_rate' : m_fuel,
             'flow_type': 'Fuel', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'combustion_energy_content': Q_fuel},
            {'name' : 'Air (Feed Dryer)', 'components' : 'Air', 'mass_flow_rate' : air_in,
             'flow_type': 'Air', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
            {'name' : 'Exhaust (Feed Dryer)', 'components' : 'Air', 'mass_flow_rate' : (air_in + water_evaporated+m_fuel),
             'flow_type': 'Exhaust', 'temperature' : coeff['exhaust_t'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_dryair+Q_water_evaporated},
            {'name' : 'Feed', 'components' : ['Solids', 'Water'], 'composition' : [(1-coeff['Feed Moisture Content']), coeff['Feed Moisture Content']], 'mass_flow_rate' : feed_out,
             'flow_type': 'Product', 'elec_flow_rate' : 0,  'temperature': coeff['t_out'] ,'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_feed},
            {'Heat of reaction': Q_fuel},
            {'Heat loss': Q_loss}]


Unit11.calculations = (['Gluten', 'Fiber', 'Steep Concentrate'], Feeddryerfunc_multi)

    

# Unit 12: Starch Washing Filters
Unit12 = Unit('Starch Washing Filter')
Unit12.expected_flows_in = ['Unpure Starch', 'Electricity (Washing Filters)', 'Water (Washing Filters)']
Unit12.expected_flows_out = ['Waste Water (Washing Filter)', 'Starch']

Unit12.coefficients = {'Starch Purity': 0.50, 'Electricity (kw/kg)': 69.24, 'Water to Starch': (1.5/.84), 'loses': 0.10}

def Starchwashingfiltersfunc_unpurestarch(unpure_starch_flow, coeff):
    starch_in = unpure_starch_flow.attributes['mass_flow_rate']
    solids_index = unpure_starch_flow.attributes['components'].index('Solids')
    solids_fraction_in = unpure_starch_flow.attributes['composition'][solids_index]
    starch_out = solids_fraction_in * starch_in / coeff['Starch Purity']
    water_in = starch_in * coeff['Water to Starch']
    waste_out = (starch_in - starch_out) + water_in
    electricity_in = coeff['Electricity (kw/kg)'] * starch_in
    Q_in = unpure_starch_flow.attributes['heat_flow_rate']
    Q_loss = Q_in * coeff['loses']
    Q_out = Q_in - Q_loss
    t_in = unpure_starch_flow.attributes['temperature']
    cp = Q_in / ((t_in - ambient_t) * starch_in)
    t_out = (Q_out / (starch_out * cp)) + ambient_t
    

    return[{'name' : 'Electricity (Washing Filters)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Starch', 'components' : ['Solids', 'Water'], 'composition' : [coeff['Starch Purity'], 1 - coeff['Starch Purity']], 'mass_flow_rate' : starch_out,
             'flow_type': 'Process stream', 'temperature': t_out , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : True ,'heat_flow_rate': Q_out},
           {'name' : 'Water (Washing Filters)', 'components' : 'Water', 'mass_flow_rate' : water_in,
             'flow_type': 'Water', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Waste Water (Washing Filters)', 'components' : 'Water', 'mass_flow_rate' : waste_out,
             'flow_type': 'Waste Water', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss}]


Unit12.calculations = {'Unpure Starch': Starchwashingfiltersfunc_unpurestarch}

# Unit 13: Arbitrary Starch Splitter
Unit13 = Unit('Starch Splitter')
Unit13.expected_flows_in = ['Starch']
Unit13.expected_flows_out = ['Conversion Starch', 'Product Starch', 'Ethanol Starch']

Unit13.coefficients = {'Conversion Starch Split': conversion_starch_split, 'Ethanol Starch Split': ethanol_starch_split}

def Starchsplitterfunc_starch(starch_flow, coeff):
    starch_in = starch_flow.attributes['mass_flow_rate']
    starch_for_conversion = (coeff['Conversion Starch Split']) * starch_in
    starch_for_ethanol = coeff['Ethanol Starch Split'] * starch_in
    starch_for_dryer = starch_in - starch_for_conversion - starch_for_ethanol
    Q_in = starch_flow.attributes['heat_flow_rate']
    Q_dryer = (Q_in) * (starch_for_dryer/ (starch_in))
    Q_fermentation = (Q_in) * (starch_for_ethanol / (starch_in))
    Q_conversion = Q_in - Q_dryer - Q_fermentation 
    return[{'name' : 'Conversion Starch', 'components' :  starch_flow.attributes['components'], 'composition' : starch_flow.attributes['composition'], 'mass_flow_rate' : starch_for_conversion,
             'flow_type': 'Process stream', 'temperature': starch_flow.attributes['temperature'] , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : True ,'heat_flow_rate': Q_conversion},
           {'name' : 'Product Starch', 'components' :  starch_flow.attributes['components'], 'composition' : starch_flow.attributes['composition'], 'mass_flow_rate' : starch_for_dryer,
             'flow_type': 'Process stream', 'temperature': starch_flow.attributes['temperature'] , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : True ,'heat_flow_rate': Q_dryer},
           {'name' : 'Ethanol Starch', 'components' :  starch_flow.attributes['components'], 'composition' : starch_flow.attributes['composition'], 'mass_flow_rate' : starch_for_ethanol,
             'flow_type': 'Process stream', 'temperature': starch_flow.attributes['temperature'] , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : True ,'heat_flow_rate': Q_fermentation}]

Unit13.calculations = {'Starch': Starchsplitterfunc_starch}


#Unit 14: Starch Dryer
Unit14 = Unit('Starch Dryer')
Unit14.expected_flows_in = ['Product Starch', 'Air (Starch Dryer)', 'Steam (Starch Dryer)']
Unit14.expected_flows_out = ['Dry Starch', 'Dry Starch - Dextrin' , 'Condensate (Starch Dryer)', 'Exhaust (Starch Dryer)']

Unit14.coefficients = {'Dextrin Split': 0.0, 'Dry Starch Moisture Content': .12, 'Air Ratio': (.930/.370), 'Exhaust Temp': 71.1,
                       'Loses': 0.10, 'unit_temp': 54.4}

def Starchdryerfunc_starch(product_starch_flow, coeff):
    starch_in = product_starch_flow.attributes['mass_flow_rate']
    Q_in = product_starch_flow.attributes['heat_flow_rate']
    t_in = product_starch_flow.attributes['temperature']
    c_p = (Q_in / (starch_in * (t_in - ambient_t)))
    moisture_index = product_starch_flow.attributes['components'].index('Water')
    moisture_in = product_starch_flow.attributes['composition'][moisture_index] * starch_in
    solids_in = starch_in - moisture_in
    dry_starch_amount = solids_in / (1 - coeff['Dry Starch Moisture Content'])
    water_evaporated = starch_in - dry_starch_amount
    dextrin_starch = coeff['Dextrin Split'] * dry_starch_amount
    product_starch = dry_starch_amount - dextrin_starch
    air_in = coeff['Air Ratio'] * starch_in
    # Energy Balance
    t_exhaust = coeff['Exhaust Temp']
    Q_air = c_pair * (t_exhaust - ambient_t) * air_in
    Q_water_evaporation = water_evaporated * (Hvap + (C_pw * (100-t_in)))
    Q_starch = c_p * dry_starch_amount * (coeff['unit_temp'] - ambient_t)
    Q_steam = (Q_air + Q_water_evaporation + Q_starch  - Q_in) / (1 - coeff['Loses'])
    Q_loss = Q_steam * coeff['Loses']
    m_steam = Q_steam / Hvap
    m_condensate = m_steam
    Q_dextrin = Q_starch * (dextrin_starch/dry_starch_amount)
    Q_product = Q_starch - Q_dextrin

    return[{'name' : 'Dry Starch - Dextrin', 'components' : ['Solids', 'Moisture'], 'composition' : [1-coeff['Dry Starch Moisture Content'], coeff['Dry Starch Moisture Content']], 'mass_flow_rate' : dextrin_starch,
             'flow_type': 'Process stream', 'temperature': coeff['unit_temp'] , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : True ,'heat_flow_rate': Q_dextrin},
           {'name' : 'Dry Starch', 'components' : ['Solids', 'Moisture'], 'composition' : [coeff['Dry Starch Moisture Content'], 1- coeff['Dry Starch Moisture Content']], 'mass_flow_rate' : product_starch,
             'flow_type': 'Product', 'temperature': coeff['unit_temp'] , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : False ,'heat_flow_rate': Q_product},
           {'name' : 'Air (Starch Dryer)', 'components' : 'Air', 'mass_flow_rate' : air_in,
             'flow_type': 'Air', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Exhaust (Starch Dryer)', 'components' : 'Air', 'mass_flow_rate' : (air_in + water_evaporated),
             'flow_type': 'Exhaust', 'temperature' : t_exhaust ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_air+Q_water_evaporation},
           {'name' : 'Steam (Starch Dryer)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Starch Dryer)', 'components' : 'Water', 'mass_flow_rate' : m_condensate,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss}]

Unit14.calculations = {'Product Starch' : Starchdryerfunc_starch}

# Unit 15: Dextrin Roasters
Unit15 = Unit('Dextrin Roaster')
Unit15.expected_flows_in = ['Dry Starch - Dextrin', 'Air (Dextrin Roaster)', 'Fuel (Dextrin Roaster)']
Unit15.expected_flows_out = ['Dextrin', 'Exhaust (Dextrin Roaster)']

Unit15.coefficients = {'Dry Dextrin Moisture Content': .03, 'Air Ratio': (.640/.070), 'Exhaust Temp': 350.0,
                       'Loses': .10, 'unit_temp': 93.3, 'c_pdextrin': 2.04, 'HHV Fuel (kJ/kg)':  52200.0}

def Dextrinroasterfunc_drystarch(dry_starch_dextrin_flow, coeff):
    dry_starch_in = dry_starch_dextrin_flow.attributes['mass_flow_rate']
    moisture_index = dry_starch_dextrin_flow.attributes['components'].index('Moisture')
    moisture_in = dry_starch_in * dry_starch_dextrin_flow.attributes['composition'][moisture_index]
    solids_in = dry_starch_in - moisture_in
    dextrin_out = solids_in / (1 - coeff['Dry Dextrin Moisture Content'])
    moisture_out = dry_starch_in - dextrin_out
    air_in = dry_starch_in * coeff['Air Ratio']
    # Energy Balance
    Q_in = dry_starch_dextrin_flow.attributes['heat_flow_rate']
    c_p = coeff['c_pdextrin']
    Q_dextrin = dextrin_out * c_p * (coeff['unit_temp'] - ambient_t)
    Q_air = c_pair * air_in * (coeff['Exhaust Temp'] - ambient_t)
    Q_water = moisture_out * (Hvap + ((C_pw)*(coeff['unit_temp'] - ambient_t)))
    Q_fuel = (Q_water + Q_air + Q_dextrin - Q_in) / (1- coeff['Loses'])
    m_fuel = (Q_fuel / coeff['HHV Fuel (kJ/kg)'])
    m_exhaust = m_fuel + air_in + moisture_out
    Q_loss = Q_fuel * coeff['Loses']
    
    return[{'name' : 'Dextrin', 'components' : ['Solids', 'Moisture'], 'composition' : [1- coeff['Dry Dextrin Moisture Content'], coeff['Dry Dextrin Moisture Content']], 'mass_flow_rate' : dextrin_out,
             'flow_type': 'Product', 'temperature': coeff['unit_temp'] , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : False ,'heat_flow_rate': Q_dextrin},
           {'name' : 'Air (Dextrin Roaster)', 'components' : 'Air', 'mass_flow_rate' : air_in,
             'flow_type': 'Air', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Exhaust (Dextrin Roaster)', 'components' : 'Air', 'mass_flow_rate' : m_exhaust,
             'flow_type': 'Exhaust', 'temperature' : coeff['Exhaust Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_air+Q_water},
           {'name' : 'Fuel (Dextrin Roaster)', 'mass_flow_rate' : m_fuel,
             'flow_type': 'Fuel', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'combustion_energy_content': Q_fuel},
           {'Heat of reaction': Q_fuel},
           {'Heat loss': Q_loss}]

Unit15.calculations = {'Dry Starch - Dextrin': Dextrinroasterfunc_drystarch}

# Unit 16: Starch Acid Hydrolysis
Unit16 = Unit('Starch Converter')
Unit16.expected_flows_in = ['Conversion Starch', 'Acid In', 'Steam (Starch Converter)', 'Electricity (Starch Converter)']
Unit16.expected_flows_out = ['Hydrolysized Starch', 'Condensate (Starch Converter)']

Unit16.coefficients = {'Acid to Input': (.06/.480), 'Electricity (kw/kg)': 121.297, 'Temp': 88, 'Loses':0.10}

def Starchhydrolyzerfunc_conversionstarch(conversion_starch_flow, coeff):
    starch_in = conversion_starch_flow.attributes['mass_flow_rate']
    acid_in = coeff['Acid to Input'] * starch_in
    starch_out = acid_in + starch_in
    electricity_in = coeff['Electricity (kw/kg)'] * starch_in
    # Energy Balance
    Q_in = conversion_starch_flow.attributes['heat_flow_rate']
    t_in = conversion_starch_flow.attributes['temperature']
    c_p = Q_in / (starch_in * (t_in - ambient_t))
    t_out = coeff['Temp']
    Q_out = c_p * (t_out - ambient_t) * starch_out
    Q_steam = (Q_out - Q_in) / (1 - coeff['Loses'])
    Q_loss = Q_steam * coeff['Loses']
    m_steam = Q_steam / Hvap
    return[{'name' : 'Hydrolysized Starch', 'components' : ['Solids'], 'composition' : [1], 'mass_flow_rate' : starch_out,
             'flow_type': 'Process stream', 'temperature': t_out , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : True ,'heat_flow_rate': Q_out},
           {'name' : 'Acid In', 'components' : ['Acid'], 'composition' : [1], 'mass_flow_rate' : acid_in,
             'flow_type': 'Process stream', 'temperature': ambient_t , 'elec_flow_rate' : 0 ,'In or out' : 'In', 'Set calc' : False ,'heat_flow_rate': 0},
           {'name' : 'Electricity (Starch Converter)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss},
           {'name' : 'Steam (Starch Converter)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Starch Converter)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit16.calculations = {'Conversion Starch': Starchhydrolyzerfunc_conversionstarch}

# Unit 17: Filter
Unit17 = Unit('Filter')
Unit17.expected_flows_in = ['Hydrolysized Starch', 'Electricity (Filter)']
Unit17.expected_flows_out = ['Starch for Syrup']

Unit17.coefficients = {'loses': .10, 'Electricity (kw/kg)': 43.078}

def Filterfunc_hydrolysizedstarch(hydrolysized_starch_flow, coeff):
    starch_in = hydrolysized_starch_flow.attributes['mass_flow_rate']
    electricity_in = starch_in * coeff['Electricity (kw/kg)']
    # Heat Adjustment
    Q_in = hydrolysized_starch_flow.attributes['heat_flow_rate']
    t_in = hydrolysized_starch_flow.attributes['temperature']
    c_p = Q_in / (starch_in * (t_in - ambient_t))
    Q_loss = Q_in * coeff['loses']
    Q_out = Q_in - Q_loss
    t_out = (Q_out / (starch_in * c_p)) + ambient_t
    return[{'name' : 'Electricity (Filter)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss},
           {'name' : 'Starch for Syrup', 'components' : ['Solids'], 'composition' : [1], 'mass_flow_rate' : starch_in,
             'flow_type': 'Process stream', 'temperature': t_out , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : True ,'heat_flow_rate': Q_out}]

Unit17.calculations = {'Hydrolysized Starch': Filterfunc_hydrolysizedstarch}


# Unit 18: Light Refining
Unit18 = Unit('Light Refiner')
Unit18.expected_flows_in = ['Starch for Syrup', 'Steam (Light Refiner)', 'Electricity (Light Refiner)']
Unit18.expected_flows_out = ['Light HFCS', 'Condensate (Refiner)']

Unit18.coefficients = {'t_unit': 93.3, 'Electricity (kw/kg)': 43.078, 'Loses': .10}

def Starchrefinerfunc_hydrolysizedstarch(Starch_for_Syrup_flow, coeff):
    starch_in = Starch_for_Syrup_flow.attributes['mass_flow_rate']
    light_cs = starch_in
    t_in = Starch_for_Syrup_flow.attributes['temperature']
    electricity_in = starch_in * coeff['Electricity (kw/kg)']
    # Energy Balance
    Q_in = Starch_for_Syrup_flow.attributes['heat_flow_rate']
    c_p = Q_in / (starch_in * (t_in - ambient_t))
    Q_l_hfcs = light_cs * c_p * (coeff['t_unit'] - ambient_t)
    Q_steam = (Q_l_hfcs - Q_in) / (1 - coeff['Loses'])
    m_steam = Q_steam / Hvap
    Q_loss = Q_steam * coeff['Loses']
    return[{'Heat loss': Q_loss},
           {'name' : 'Light HFCS', 'components' : ['Solids'], 'composition' : [1], 'mass_flow_rate' : light_cs,
             'flow_type': 'Process stream', 'temperature': coeff['t_unit'] , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : True ,'heat_flow_rate': Q_l_hfcs},
           {'name' : 'Steam (Light Refiner)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Light Refiner)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Electricity (Light Refiner)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0}]
   
Unit18.calculations = {'Starch for Syrup' : Starchrefinerfunc_hydrolysizedstarch}

# Unit 19: Evaporator
Unit19 = Unit('HFCS Evaporator')
Unit19.expected_flows_in = ['Light HFCS', 'Steam (HFCS Evaporator)']
Unit19.expected_flows_out = ['Sugar', 'Corn Syrup', 'Condensate (HFCS Evaportator)']

Unit19.coefficients = {'Syrup Conversion': (.05/.340), 'unit_t': 121.1, 'c_pcs': 2.72, 'Loses': 0.10}

def Evaporatorfunc_lighthfcs(Light_HFCS_flow, coeff):
    light_cs_in = Light_HFCS_flow.attributes['mass_flow_rate']
    t_in = Light_HFCS_flow.attributes['temperature']
    syrup_out = light_cs_in * coeff['Syrup Conversion']
    sugar_out = light_cs_in - syrup_out
    # Energy Balance
    Q_in = Light_HFCS_flow.attributes['heat_flow_rate']
    c_p = Q_in / (light_cs_in * (t_in - ambient_t))
    Q_cs = syrup_out * coeff['c_pcs'] * (coeff['unit_t'] - ambient_t)
    Q_sugar = sugar_out * c_p * (coeff['unit_t'] - ambient_t)
    Q_steam = (Q_cs + Q_sugar - Q_in) / (1- coeff['Loses'])
    Q_loss = Q_steam * coeff['Loses']
    m_steam = Q_steam / Hvap
    return[{'Heat loss': Q_loss},
           {'name' : 'Corn Syrup', 'components' : ['Solids'], 'composition' : [1], 'mass_flow_rate' : syrup_out,
             'flow_type': 'Product', 'temperature': coeff['unit_t'] , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : False ,'heat_flow_rate': Q_cs},
           {'name' : 'Sugar', 'components' : ['Solids'], 'composition' : [1], 'mass_flow_rate' : sugar_out,
             'flow_type': 'Product', 'temperature': coeff['unit_t'] , 'elec_flow_rate' : 0 ,'In or out' : 'Out', 'Set calc' : False ,'heat_flow_rate': Q_sugar},
           {'name' : 'Steam (HFCS Evaporator)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (HFCS Evaporator)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit19.calculations = {'Light HFCS': Evaporatorfunc_lighthfcs}

# Unit 22: Picks up at the heater from ethanol manufacturing
## Conversion: .3104 grams of ethanol per gram of glucose, or gram of starch 
Unit22 = Unit('Heater')
Unit22.expected_flows_in = ['Ethanol Starch', 'Steam (Heater)']
Unit22.expected_flows_out = ['Hot Slurry', 'Condensate (Heater)']

Unit22.coefficients = {'c_pcorn': 2.42, 'unit_t': 85., 'loses': 0.10, 'market_yield': (.3104/1.00)}

def Heaterfunc_cornslurry(corn_slurry_flow, coeff):
    corn_slurry_in = corn_slurry_flow.attributes['mass_flow_rate']
    water_index = corn_slurry_flow.attributes['components'].index('Water')
    water_wt = corn_slurry_flow.attributes['composition'][water_index]
    solids_wt = 1- water_wt
    ethanol_wt = solids_wt * coeff['market_yield']
    solids_wt = 1 - water_wt - ethanol_wt
    # Keeping the same percentage
    
    Q_corn_in = corn_slurry_flow.attributes['heat_flow_rate']
    t_out = coeff['unit_t']
    Q_corn_out = (corn_slurry_in * solids_wt * coeff['c_pcorn'] * (t_out - ambient_t)) + (corn_slurry_in * water_wt * C_pw * (t_out - ambient_t))
    Q_steam = (Q_corn_out - Q_corn_in) / (1 - coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss = Q_steam * coeff['loses']

    return[{'name' : 'Hot Slurry', 'components' : ['Ethanol','Solids', 'Water'], 'composition' : [ethanol_wt, solids_wt, water_wt], 'mass_flow_rate' : corn_slurry_in,
             'flow_type': 'Process stream', 'temperature' : t_out ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_corn_out},

           {'name' : 'Steam (Heater)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Heater)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           
           {'Heat loss': Q_loss}]

Unit22.calculations = {'Ethanol Starch': Heaterfunc_cornslurry}

# Unit 23: Liquifaction and Saccherization
Unit23 = Unit('Liquifaction Tank')
Unit23.expected_flows_in = ['Hot Slurry', 'Steam (Liquifaction Tank)', 'Yeast, Enzymes and Acid']
Unit23.expected_flows_out = ['Glucose Slurry', 'Condensate (Liquifaction Tank)']

Unit23.coefficients = {'c_pcorn': 2.42, 'unit_t': 90., 'loses': 0.10, 'Inputs Ratio': .0009}

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

Unit23.calculations = {'Hot Slurry': Liquifactionfunc_hotslurry}

# Unit 24: Knockout Tank
Unit24 = Unit('Knockout Tank')
Unit24.expected_flows_in = ['Glucose Slurry', 'Cooling Water (Knockout Tank)']
Unit24.expected_flows_out = ['Cooled Slurry', 'Recovered Heat (Knockout Tank)']

Unit24.coefficients = {'unit_t': 32., 'loses': 0.10}

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

Unit24.calculations = {'Glucose Slurry': Knockouttankfunc_glucoseslurry}

# Unit 25: Fermentation
Unit25 = Unit('Fermentation')
Unit25.expected_flows_in = ['Cooled Slurry']
Unit25.expected_flows_out = ['Cooled Mash', 'Carbon Dioxide']

Unit25.coefficients = {'Ethanol - CO2 Molar Ratio': 1.00, 'Ethanol MM': 46.07, 'CO2 MM': 44.009, 'loses': 0.10}

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

Unit25.calculations = {'Cooled Slurry': Fermentationtankfunc_cooledslurry}

# Unit 26: Preheater - 71 deg C is optimal
Unit26 = Unit('Preheater')
Unit26.expected_flows_in = ['Cooled Mash', 'Steam (Preheater)']
Unit26.expected_flows_out = ['Condensate (Preheater)', 'Mash']

Unit26.coefficients = {'unit_t': 71.1, 'loses': 0.10}

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

Unit26.calculations = {'Cooled Mash': Preaheaterfunc_cooledmash}
           
# Unit 27: Distillation Column - Check the bottoms of this 
Unit27 = Unit('Distillation Column')
Unit27.expected_flows_in = ['Mash', 'Steam (Distillation Column)']
Unit27.expected_flows_out = ['Tops', 'Bottoms', 'Condensate (Distillation Column)']

Unit27.coefficients = {'Ethanol wt out': .956, 'loses': 0.15, 'Reflux Ratio': 2.0, 'Boiler Percentage': .25, 'T_top': 78.2, 'T_bottom': 95.0, 'c_pethanol': 2.44,
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
    

    return[{'name' : 'Tops', 'components' : ['Ethanol', 'Water'], 'composition' : [coeff['Ethanol wt out'], tops_water_wt], 'mass_flow_rate' : tops_out,
             'flow_type': 'Process stream', 'temperature' : coeff['T_top'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': (Q_tops_out)},

           {'name' : 'Bottoms', 'components' : ['Solids', 'Water'], 'composition' : [bottoms_solid_wt, bottoms_water_wt], 'mass_flow_rate' : bottoms_out,
             'flow_type': 'Process stream', 'temperature' : coeff['T_bottom'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_bottoms_out},

           {'name' : 'Steam (Distillation Column)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Distillation Column)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'Heat loss': (Q_loss + Q_reboiler + Q_reflux)}]


Unit27.calculations = {'Mash': Distillationcolumnfunc_mash}

# Unit 28: Vaporizer, according to Homeland Energy Solutions, Molecular Sieves require a gasous stream
Unit28 = Unit('Vaporizer')
Unit28.expected_flows_in = ['Tops', 'Steam (Vaporizer)']
Unit28.expected_flows_out = ['Vaporized Tops', 'Condensate (Vaporizer)']

Unit28.coefficients = {'loses': .10, 'Hvap_ethanol': 885.0, 'c_pethanol': 2.44, 'Ethanol wt': .956}

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
    
Unit28.calculations = {'Tops': Vaporizerfunc_tops}

# Unit 29: Molecular Sieves
Unit29 = Unit('Molecular Sieve')
Unit29.expected_flows_in = ['Vaporized Tops']
Unit29.expected_flows_out = ['Wastewater (Sieves)', 'Ethanol', 'Waste Heat']

Unit29.coefficients = {'Ethanol wt out': .997, 'loses': 0.10}

def Molecularsievefunc_vaporizedtops(vaporized_tops_flow, coeff):
    vtops_in = vaporized_tops_flow.attributes['mass_flow_rate']
    ethanol_index = vaporized_tops_flow.attributes['components'].index('Ethanol')
    ethanol_in = vaporized_tops_flow.attributes['composition'][ethanol_index] * vtops_in
    ethanol_out = ethanol_in / coeff['Ethanol wt out']
    wastewater_out = vtops_in - ethanol_out
    Q_in = vaporized_tops_flow.attributes['heat_flow_rate']
    Q_loss = Q_in * coeff['loses']
    Q_available = Q_in - Q_loss

    return[{'name' : 'Ethanol', 'components' : ['Ethanol', 'Water'], 'composition' : [coeff['Ethanol wt out'], 1-coeff['Ethanol wt out']], 'mass_flow_rate' : ethanol_out,
             'flow_type': 'Product', 'temperature' : 25 ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Wastewater', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : wastewater_out,
             'flow_type': 'Waste water', 'temperature' : 25 ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'name' : 'Waste Heat', 'flow_type': 'Waste', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_available},

           {'Heat loss': Q_loss}]

Unit29.calculations = {'Vaporized Tops': Molecularsievefunc_vaporizedtops}

# Unit 30: Centrifuge
Unit30 = Unit('Centrifuge')
Unit30.expected_flows_in = ['Bottoms', 'Electricity (Centrifuge)']
Unit30.expected_flows_out = ['Wet Cake', 'Thin Stillage']

Unit30.coefficients = {'Electricity (kw/kg)': 0.01, 'Thin Stillage solid wt': 0.075, 'Wet Cake solid wt': .65}

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

Unit30.calculations = {'Bottoms': Centrifugefunc_bottoms}
    

# Unit 31: Evaporator
Unit31 = Unit('Evaporator')
Unit31.expected_flows_in = ['Thin Stillage', 'Steam (Evaporator)']
Unit31.expected_flows_out = ['Stillage', 'Condensate (Evaporator)', 'Water (Evaporator)']

Unit31.coefficients = {'Stillage solid wt': .42, 'loses': 0.10, 'unit_t': 90}

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

Unit31.calculations = {'Thin Stillage': Evaporatorfunc_thinstillage}

# Unit 32: Drum Dryer
Unit32 = Unit('Drum Dryer')
Unit32.required_calc_flows = 2
Unit32.expected_flows_in = ['Stillage', 'Wet Cake', 'Steam (Drum Dryer)']
Unit32.expected_flows_out = ['DDGS', 'Water (Drum Dryer)', 'Condensate (Drum Dryer)']

Unit32.coefficients = {'DDGS Moisture Content': .10, 'Unit Temp': 107., 'loses': 0.10, 'c_pcorn': 2.42}

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
    
    
    return[{'name' : 'DDGS', 'components' : ['Solids', 'Water'], 'composition' : [1- coeff['DDGS Moisture Content'], coeff['DDGS Moisture Content']], 'mass_flow_rate': ddgs_out,
             'flow_type': 'Product', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_solids},
           
           {'name' : 'Water (Drum Dryer)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : moisture_out,
             'flow_type': 'Process Stream', 'temperature' : 100 ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_water_evap},

           {'name' : 'Steam (Drum Dryer)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Drum Dryer)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'Heat loss': Q_loss_1}]

Unit32.calculations = (['Wet Cake', 'Stillage'], Drumdryerfunc_multi)

processunits = [Unit1, Unit2, Unit3, Unit4, Unit5, Unit6, Unit7, Unit8, Unit9,
                Unit10, Unit11, Unit12, Unit13, Unit14, Unit15, Unit16, Unit17,
                Unit18, Unit19, Unit20, Unit21, Unit22, Unit23, Unit24, Unit25,
                Unit26, Unit27, Unit28, Unit29, Unit30, Unit31, Unit32]
                
main(allflows, processunits)

#utilities_recap('heat_intensity_wet_corn_mill', allflows, processunits)

'''
for unit in processunits:
    print(unit)
    unit.check_heat_balance(allflows)
    unit.check_mass_balance(allflows)

'''
for flow in allflows:
    if flow.attributes['flow_type'] == 'Product':
        print(flow)










