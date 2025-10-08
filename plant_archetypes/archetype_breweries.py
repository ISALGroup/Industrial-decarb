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
C_pwort = 4.05
C_pcan = 0.9

####################################################UNITS####################################################
# go back through and add electricity requirements and check mass balances 

#Unit 1: Grinder
Unit1 = Unit('Grinder')
Unit1.expected_flows_in = ['Malt', 'Electricity (Grinder)']
Unit1.expected_flows_out = ['Milled Cooker Malt', 'Milled Mash Malt']

Unit1.coefficients = {'Electricity (kw/kg)': 0.0001, 'Split to Cooker': 0.10}

def Grinderfunc_malt(malt_flow, coeff):
    malt_in = malt_flow.attributes['mass_flow_rate']
    electricity_in = malt_in * coeff['Electricity (kw/kg)']
    cooker_malt_out = coeff['Split to Cooker'] * malt_in
    mash_malt_out = malt_in - cooker_malt_out
    return[{'name' : 'Electricity (Grinder)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Milled Cooker Malt', 'components' : malt_flow.attributes['components'], 'composition' : malt_flow.attributes['composition'], 'mass_flow_rate' : cooker_malt_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           {'name' : 'Milled Mash Malt', 'components' : malt_flow.attributes['components'], 'composition' : malt_flow.attributes['composition'], 'mass_flow_rate' : mash_malt_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0}]

Unit1.calculations = {'Malt': Grinderfunc_malt}
FlowA = Flow(name = 'Malt', components = ['Solids', 'Water'], composition = [(1-0.05), 0.05], flow_type = 'input', mass_flow_rate = 100000)
FlowA.set_calc_flow()
allflows.append(FlowA)

# Unit 2: Cooker
Unit2 = Unit('Cooker')
Unit2.expected_flows_in = ['Milled Cooker Malt', 'Hot Water (Cooker)', 'Cooker Yeast', 'Steam (Cooker)']
Unit2.expected_flows_out = ['Cereal', 'Condensate (Cooker)']

Unit2.coefficients = {'C_pmalt': 1.84184, 'Water to Malt Ratio': (.214/.014), 'Yeast to Malt Ratio': (.05/.014), 'Hot Water Temp': 82.2,
                      'Unit Temp': 100.0, 'loses': 0.15}

def Cookerfunc_malt(cooker_malt_flow, coeff):
    cooker_malt_in = cooker_malt_flow.attributes['mass_flow_rate']
    solids_index = cooker_malt_flow.attributes['components'].index('Solids')
    solids_in = (cooker_malt_flow.attributes['composition'][solids_index]) * cooker_malt_in
    Q_malt = cooker_malt_flow.attributes['heat_flow_rate']
    water_in = cooker_malt_in * coeff['Water to Malt Ratio']
    yeast_in = cooker_malt_in * coeff['Yeast to Malt Ratio']
    mash_out = cooker_malt_in + water_in + yeast_in
    Q_hotwater = water_in * C_pw * (coeff['Hot Water Temp'] - ambient_t)
    solids_wt_out = (solids_in + yeast_in) / mash_out
    c_pmash = (solids_wt_out * coeff['C_pmalt']) + ((1- solids_wt_out) * C_pw)
    Q_mash = mash_out * c_pmash * (coeff['Unit Temp'] - ambient_t)
    Q_steam = (Q_mash - Q_malt - Q_hotwater) / (1- coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss = Q_steam * coeff['loses']
    return[{'name' : 'Hot Water (Cooker)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': water_in,
             'flow_type': 'Process Stream', 'temperature' : coeff['Hot Water Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_hotwater},
           {'name' : 'Cooker Yeast', 'components' : ['Yeast'], 'composition' : [1], 'mass_flow_rate': yeast_in,
             'flow_type': 'Process Stream', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Cereal', 'components' : ['Solids', 'Water'], 'composition' : [solids_wt_out, 1 - solids_wt_out], 'mass_flow_rate': mash_out,
             'flow_type': 'Process Stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_mash},
           {'name' : 'Steam (Cooker)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Steam', 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Cooker)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Condensate', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss}]

Unit2.calculations = {'Milled Cooker Malt': Cookerfunc_malt}

# Unit 3: Mash Turn
Unit3 = Unit('Mash Turn')
Unit3.required_calc_flows = 2
Unit3.expected_flows_in = ['Cereal', 'Milled Mash Malt', 'Hot Water (Mash Turn)', 'Steam (Mash Turn)']
Unit3.expected_flows_out = ['Mash', 'Condensate (Mash Turn)']

Unit3.coefficients = {'Unit Temp': 82, 'Water to Process Flows Ratio': (.47/(.126 + .268)), 'loses': 0.15,
                      'Hot Water Temp': ((71+82)/2), 'C_pmalt': 1.84184, 'Steam temp':100}
def Mashturnfunc_multi(ablist, coeff):
    cereal_flow = ablist[0]
    mash_malt_flow = ablist[1]
    cereal_in = cereal_flow.attributes['mass_flow_rate']
    mash_in = mash_malt_flow.attributes['mass_flow_rate']
    water_in = (cereal_in + mash_in) * coeff['Water to Process Flows Ratio']
    mash_out = water_in + mash_in + cereal_in
    cereal_solid_index = cereal_flow.attributes['components'].index('Solids')
    cereal_solids_in = (cereal_flow.attributes['composition'][cereal_solid_index]) * cereal_in
    mash_solid_index = mash_malt_flow.attributes['components'].index('Solids')
    mash_solids_in = (mash_malt_flow.attributes['composition'][mash_solid_index]) * mash_in
    solids_wt = (mash_solids_in + cereal_solids_in) / mash_out
    # Heat Balance
    Q_mash = mash_malt_flow.attributes['heat_flow_rate']
    Q_cereal = cereal_flow.attributes['heat_flow_rate']
    Q_water = water_in * C_pw * (coeff['Hot Water Temp'] - ambient_t)
    c_pmash = (coeff['C_pmalt'] * solids_wt) + ((1-solids_wt) * C_pw)
    Q_mash_out = mash_out * c_pmash * (coeff['Unit Temp'] - ambient_t)
    Q_steam = (Q_mash_out - Q_mash - Q_cereal - Q_water) / (1 - coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss = Q_steam * coeff['loses']
    
    return[{'name' : 'Hot Water (Mash Turn)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': water_in,
             'flow_type': 'Process Stream', 'temperature' : coeff['Hot Water Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_water},
           {'name' : 'Mash', 'components' : ['Solids', 'Water'], 'composition' : [solids_wt, 1 - solids_wt], 'mass_flow_rate': mash_out,
             'flow_type': 'Process Stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_mash_out},
           {'name' : 'Steam (Mash Turn)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Steam', 'In or out' : 'In', 'temperature': coeff['Steam temp'], 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Mash Turn)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Condensate', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss}]
    
Unit3.calculations = (['Cereal', 'Milled Mash Malt'], Mashturnfunc_multi)

# Unit 4: Lautering
Unit4 = Unit('Lautering')
Unit4.expected_flows_in = ['Mash','Hot Water (Lautering)', 'Electricity (Lautering)']
Unit4.expected_flows_out = ['Wort', 'Spent Grain']

Unit4.coefficients = {'Water to Mash': (.300/.843), 'Spent Grain to Wort': (.127/1.037), 'Electricity (kw/kg)': 0.001,
                      'Unit Temp': 71.1}

def Lauteringfunc_mash(mash_flow, coeff):
    mash_in = mash_flow.attributes['mass_flow_rate']
    water_in = mash_in * coeff['Water to Mash']
    electricity_in = mash_in * coeff['Electricity (kw/kg)']
    wort_out = (mash_in + water_in) / (1 + coeff['Spent Grain to Wort'])
    spent_grain_out = wort_out * coeff['Spent Grain to Wort']
    water_wt_sg = .775
    # Energy Balance
    Q_mash = mash_flow.attributes['heat_flow_rate']
    Q_water = water_in * C_pw * (coeff['Unit Temp'] - ambient_t)
    Q_wort = wort_out * C_pwort * (coeff['Unit Temp'] - ambient_t)
    c_pgrain = (water_wt_sg * C_pw) + ((1- water_wt_sg) * 1.84)
    Q_grain = spent_grain_out * c_pgrain * (coeff['Unit Temp'] - ambient_t)
    Q_loss = Q_mash + Q_water - Q_wort - Q_grain
    return[{'name' : 'Hot Water (Lautering)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': water_in,
             'flow_type': 'Process Stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_water},
           {'name' : 'Wort', 'components' : ['Wort'], 'composition' : [1], 'mass_flow_rate': wort_out,
             'flow_type': 'Process Stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_wort},
           {'name' : 'Spent Grain', 'components' : ['Solids', 'Water'], 'composition' : [1-water_wt_sg, water_wt_sg], 'mass_flow_rate': spent_grain_out,
             'flow_type': 'Process Stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_grain},
           {'Heat loss': Q_loss},
           {'name' : 'Electricity (Lautering)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit4.calculations = {'Mash': Lauteringfunc_mash}

# Unit 5: Wort Boiling
Unit5 = Unit('Wort Boiler')
Unit5.expected_flows_in = ['Wort', 'Hops', 'Steam (Wort Boiler)']
Unit5.expected_flows_out = ['Strong Wort', 'Condensate (Wort Boiler', 'Exhaust (Wort Boiler)']

Unit5.coefficients = {'Exhaust Ratio': (.08), 'Unit Temp': 100.0, 'loses': 0.10, 'Hops to Wort': (0.004/1.037)}

def Brewerfunc_wort(wort_flow, coeff):
    wort_in = wort_flow.attributes['mass_flow_rate']
    hops_in = wort_in * coeff['Hops to Wort']
    exhaust_out = wort_in * coeff['Exhaust Ratio']
    wort_out = wort_in + hops_in - exhaust_out
    hops_wt = hops_in / wort_out
    # Energy Balance
    Q_wort_in = wort_flow.attributes['heat_flow_rate']
    Q_wort_out = wort_out * C_pwort * (coeff['Unit Temp'] - ambient_t)
    Q_exhaust = exhaust_out * ((C_pw * (100 - ambient_t)) + Hvap)
    Q_steam = (Q_wort_out + Q_exhaust - Q_wort_in) / (1 - coeff['loses'])
    Q_loss = Q_steam * coeff['loses']
    m_steam = Q_steam / Hvap
    
    return[{'name' : 'Strong Wort', 'components' : ['Hops','Wort'], 'composition' : [hops_wt, 1-hops_wt], 'mass_flow_rate': wort_out,
             'flow_type': 'Process Stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_wort_out},
           {'name' : 'Hops', 'components' : ['Hops'], 'composition' : [1], 'mass_flow_rate': hops_in,
             'flow_type': 'Process Stream', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Steam (Wort Boiler)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Steam' , 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Exhaust (Wort Boiler)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': exhaust_out,
             'flow_type': 'Waste Heat', 'temperature' : 100 ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_exhaust},
           {'name' : 'Condensate (Wort Boiler)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': m_steam,
             'flow_type': 'Condensate', 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss}]

Unit5.calculations = {'Wort': Brewerfunc_wort}

# Unit 16: Clarification Tank
Unit16 = Unit('Clarification Tank')
Unit16.expected_flows_in = ['Strong Wort']
Unit16.expected_flows_out = ['Waste (Clarification Tank)', 'Clear Wort']

Unit16.coefficients = {'Removal Ratio': .01, 'Loses': .25}

def Clarifyerwort_func(strong_wort_flow, coeff):
    wort_in = strong_wort_flow.attributes['mass_flow_rate']
    waste_out = wort_in * coeff['Removal Ratio']
    wort_out = wort_in - waste_out
    Q_in = strong_wort_flow.attributes['heat_flow_rate']
    Q_loss = Q_in * coeff['Loses']
    Q_out = Q_in - Q_loss
    t_out = (Q_out / (wort_out * C_pwort)) + ambient_t
    
    return[{'name' : 'Clear Wort', 'components' : ['Hops','Wort'], 'composition' : strong_wort_flow.attributes['composition'], 'mass_flow_rate': wort_out,
             'flow_type': 'Process Stream', 'temperature' : t_out ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_out},
           {'name' : 'Waste (Clarification Tank)', 'components' : ['Waste'], 'composition' : [1], 'mass_flow_rate': waste_out,
             'flow_type': 'Waste', 'temperature' : t_out ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss}]

Unit16.calculations = {'Strong Wort': Clarifyerwort_func}

# Unit 6: Coolers
Unit6 = Unit('Cooler')
Unit6.expected_flows_in = ['Clear Wort', 'Air (Cooler)', 'Electricity (Cooler)']
Unit6.expected_flows_out = ['Hops', 'Cooled Wort', 'Exhaust (Cooler)']

Unit6.coefficients = {'Outlet Temp': 10.0, 'Air to Wort Ratio': (7.32/1.021), 'Electricity (kw/kg)': 0.000}

def Coolerwort_func(clear_wort_flow, coeff):
    wort_in = clear_wort_flow.attributes['mass_flow_rate']
    air_in = wort_in * coeff['Air to Wort Ratio']
    hops_index = clear_wort_flow.attributes['components'].index('Hops')
    hops_in = wort_in * (clear_wort_flow.attributes['composition'][hops_index])
    wort_out = wort_in - hops_in
    hops_out = hops_in
    electricity_in = wort_in * coeff['Electricity (kw/kg)']
    # Energy Balance
    Q_wort_in = clear_wort_flow.attributes['heat_flow_rate']
    Q_wort_out = wort_in * C_pwort * (coeff['Outlet Temp'] - ambient_t)
    Q_air = Q_wort_in - Q_wort_out
    t_air = (Q_air / (air_in * C_pair)) + ambient_t
    
    return[{'name' : 'Cooled Wort', 'components' : ['Wort'], 'composition' : [1], 'mass_flow_rate': wort_out,
             'flow_type': 'Process Stream', 'temperature' : coeff['Outlet Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_wort_out},
           {'name' : 'Exhaust (Cooler)', 'components' : ['Air'], 'composition' : [1], 'mass_flow_rate': air_in,
             'flow_type': 'Waste Heat', 'temperature' : t_air ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_air},
           {'name' : 'Air (Cooler)', 'components' : ['Air'], 'composition' : [1], 'mass_flow_rate': air_in,
             'flow_type': 'Air', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Hops', 'components' : ['Hops'], 'composition' : [1], 'mass_flow_rate': hops_out,
             'flow_type': 'Process Stream', 'temperature' : coeff['Outlet Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Electricity (Cooler)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit6.calculations = {'Clear Wort': Coolerwort_func}

#Unit 7: Fermentation
Unit7 = Unit('Fermentor')
Unit7.expected_flows_in = ['Cooled Wort', 'Yeast', 'Cooling Water (Fermentor)']
Unit7.expected_flows_out = ['Beer', 'Carbon Dioxide', 'Spent Yeast', 'Water (Fermentor)']

Unit7.coefficients = {'Unit Temp': 7.222, 'CO2 to Wort Ratio': (.05/1.004), 'Yeast to Wort': (0.024/1.004), 'Cooling Water Temp': 5.0}

def Fermentorwort_func(cooled_wort_flow, coeff):
    wort_in = cooled_wort_flow.attributes['mass_flow_rate']
    yeast_in = wort_in * coeff['Yeast to Wort']
    yeast_out = yeast_in 
    co2_out = wort_in * coeff['CO2 to Wort Ratio']
    beer_out = wort_in - co2_out
    # Energy Balance
    Q_in = cooled_wort_flow.attributes['heat_flow_rate']
    Q_beer = beer_out * C_pwort * (coeff['Unit Temp'] - ambient_t)
    C_pco2 = 0.846 
    Q_co2 = co2_out * C_pco2 * (coeff['Unit Temp'] - ambient_t)
    Q_coolingwater = Q_co2 + Q_beer - Q_in 
    m_coolingwater = Q_coolingwater / ((coeff['Cooling Water Temp'] - ambient_t) * C_pw)
    mass_in = wort_in + yeast_in + m_coolingwater
    mass_out = beer_out + co2_out + yeast_out + m_coolingwater

    
    return[{'name' : 'Beer', 'components' : ['Beer'], 'composition' : [1], 'mass_flow_rate': beer_out,
             'flow_type': 'Process Stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_beer},
           
           {'name' : 'Yeast', 'components' : ['Yeast'], 'composition' : [1], 'mass_flow_rate': yeast_in,
             'flow_type': 'Process Stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           
           {'name' : 'Spent Yeast', 'components' : ['Yeast'], 'composition' : [1], 'mass_flow_rate': yeast_out,
             'flow_type': 'Process Stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           
           {'name' : 'Carbon Dioxide', 'components' : ['CO2'], 'composition' : [1], 'mass_flow_rate': co2_out,
             'flow_type': 'Process Stream','Pressure': 101.3, 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_co2},
           
           {'name' : 'Cooling Water (Fermentor)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : m_coolingwater,
             'flow_type': 'Cooling Water', 'temperature' : coeff['Cooling Water Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_coolingwater},
           
           {'name' : 'Water (Fermentor)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : m_coolingwater,
             'flow_type': 'Cooling Water', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit7.calculations = {'Cooled Wort': Fermentorwort_func}

# Unit 8: Compressor
# Assumption: Isothermal Compression of a ideal gas
Unit8 = Unit('Compressor')
Unit8.expected_flows_in = ['Carbon Dioxide', 'Electricity (Compressor)']
Unit8.expected_flows_out = ['Compressed Carbon Dioxide']

Unit8.coefficients = {'P Out': 1723.7}

def Compressorco2_func(co2_flow, coeff):
    p_in = co2_flow.attributes['pressure']
    p_out = coeff['P Out']
    co2_in = co2_flow.attributes['mass_flow_rate']
    t_in = co2_flow.attributes['temperature'] + 273
    w_input = np.log(p_out / p_in) * ( co2_in * 8.314 * t_in ) / 44.01
    w_input_kJ = w_input * 10**-3
    
    return[{'name' : 'Electricity (Compressor)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : w_input_kJ, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Compressed Carbon Dioxide', 'components' : ['CO2'], 'composition' : [1], 'mass_flow_rate': co2_in,
             'flow_type': 'Process Stream', 'Pressure': p_out,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': co2_flow.attributes['heat_flow_rate']}]

Unit8.calculations = {'Carbon Dioxide': Compressorco2_func}

#Unit 9: Beer Cooler
Unit9 = Unit('Beer Cooler')
Unit9.expected_flows_in = ['Beer', 'Electricity (Beer Cooler)', 'Cooling Water (Beer Cooler)']
Unit9.expected_flows_out = ['Chilled Beer', 'Water (Beer Cooler)']

Unit9.coefficients = {'Unit Temp': 4.44, 'Electricity (kw/kg)': 0.0000 , 'Cooling Water Temp': 5.0}

def Beercooler_func(beer_flow, coeff):
    beer_in = beer_flow.attributes['mass_flow_rate']
    electricity_in = beer_in * coeff['Electricity (kw/kg)']
    Q_in = beer_flow.attributes['heat_flow_rate']
    Q_beer = beer_in * C_pwort * (coeff['Unit Temp'] - ambient_t)
    Q_cw = Q_beer - Q_in
    m_coolingwater = Q_cw / (C_pw * (coeff['Cooling Water Temp'] - ambient_t))
    
    return[{'name' : 'Electricity (Beer Cooler)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Chilled Beer', 'components' : ['Beer'], 'composition' : [1], 'mass_flow_rate': beer_in,
             'flow_type': 'Process Stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_beer},
           {'name' : 'Cooling Water (Beer Cooler)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : m_coolingwater,
             'flow_type': 'Cooling Water', 'temperature' : coeff['Cooling Water Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_cw},
           {'name' : 'Water (Beer Cooler)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : m_coolingwater,
             'flow_type': 'Cooling Water', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit9.calculations = {'Beer': Beercooler_func}

#Unit 10: Canning
Unit10 = Unit('Canner')
Unit10.required_calc_flows = 2
Unit10.expected_flows_in = ['Chilled Beer', 'Compressed Carbon Dioxide', 'Cans', 'Electricity (Canner)']
Unit10.expected_flows_out = ['Canned Beer']

Unit10.coefficients = {'Electricity (kw/kg)': 0.0000 , 'KG per Can': .355, 'Can Weight': 0.013}

def Cannermulti_func(ablist, coeff):
    co2_flow = ablist[0]
    chilled_beer_flow = ablist[1]
    mass_in = (co2_flow.attributes['mass_flow_rate']) + (chilled_beer_flow.attributes['mass_flow_rate'])
    beers_out = mass_in / coeff['KG per Can']
    electricity_in = mass_in * coeff['Electricity (kw/kg)']
    cans_in = beers_out * coeff['Can Weight']
    mass_out = mass_in + cans_in
    can_wt = (.013/.355)
    Q_in = (co2_flow.attributes['heat_flow_rate']) + (chilled_beer_flow.attributes['heat_flow_rate'])

    
    return[{'name' : 'Electricity (Canner)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Canned Beer', 'components' : ['Can', 'Beer'], 'composition' : [can_wt, 1-can_wt], 'mass_flow_rate': mass_out,
             'flow_type': 'Process Stream', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           {'name' : 'Cans', 'components' : ['Can'], 'composition' : [1], 'mass_flow_rate': cans_in,
             'flow_type': 'Process Stream', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_in}]

Unit10.calculations = (['Compressed Carbon Dioxide', 'Chilled Beer'], Cannermulti_func)

# Unit 11: Pastuerization
Unit11 = Unit('Pasteurizer')
Unit11.expected_flows_in = ['Canned Beer' , 'Hot Water (Pasteurizer)']
Unit11.expected_flows_out = ['Product Beer' , 'Water (Pasteurizer)']

Unit11.coefficients = {'Hot Water Temp': 82.22, 'Unit Temp':62.7}

def Pasteurizationbeer_func(can_flow, coeff):
    cans_in = can_flow.attributes['mass_flow_rate']
    c_p = (can_flow.attributes['composition'][can_flow.attributes['components'].index('Can')] * C_pcan) + (can_flow.attributes['composition'][can_flow.attributes['components'].index('Beer')] * C_pw)
    Q_in = can_flow.attributes['heat_flow_rate']
    Q_out = cans_in * c_p * (coeff['Unit Temp'] - ambient_t)
    Q_hw = Q_out - Q_in
    hot_water_in = Q_hw / (C_pw * (coeff['Hot Water Temp'] - ambient_t))
    
    return[{'name' : 'Product Beer', 'components' : ['Can', 'Beer'], 'composition' : can_flow.attributes['composition'], 'mass_flow_rate': cans_in,
             'flow_type': 'Product', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_out},
           {'name' : 'Hot Water (Pasteurizer)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': hot_water_in,
             'flow_type': 'Steam', 'temperature' : coeff['Hot Water Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_hw},
           {'name' : 'Water (Pasteurizer)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': hot_water_in,
             'flow_type': 'Water', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit11.calculations = {'Canned Beer': Pasteurizationbeer_func}

# Unit 12: Screen Press
Unit12 = Unit('Screen Press')
Unit12.expected_flows_in = ['Spent Grain', 'Electricity (Screen Press)']
Unit12.expected_flows_out =['Wet Feed', 'Wet Grain', 'Wastewater (Screen Press)']

Unit12.coefficients = {'Outlet Moisture Content': 0.50, 'Electricity (kw/kg)': 0.0001, 'Wet Feed Ratio': (2/3),
                       'loses': 0.10}

def Screenpresssg_func(spent_grain_flow, coeff):
    spent_grain_in = spent_grain_flow.attributes['mass_flow_rate']
    solids_index = spent_grain_flow.attributes['components'].index('Solids')
    solids_in = (spent_grain_in) * (spent_grain_flow.attributes['composition'][solids_index])
    wet_grain_out = solids_in / (1 - coeff['Outlet Moisture Content'])
    water_out = spent_grain_in - wet_grain_out
    electricity_in = spent_grain_in * coeff['Electricity (kw/kg)']
    wet_feed_out = coeff['Wet Feed Ratio'] * wet_grain_out
    to_dryer_out = wet_grain_out - wet_feed_out
    Q_in = spent_grain_flow.attributes['heat_flow_rate']
    Q_loss = Q_in * coeff['loses']
    Q_out = Q_in - Q_loss
    Q_to_product = Q_out * coeff['Wet Feed Ratio']
    Q_to_dryer = Q_out - Q_to_product
    
    return[{'name' : 'Electricity (Screen Press)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss},
           {'name' : 'Wet Feed', 'components' : ['Solids', 'Water'], 'composition' : [1-coeff['Outlet Moisture Content'], coeff['Outlet Moisture Content']], 'mass_flow_rate': wet_feed_out,
             'flow_type': 'Product', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_to_product},
           {'name' : 'Wet Grain', 'components' : ['Solids', 'Water'], 'composition' : [1-coeff['Outlet Moisture Content'], coeff['Outlet Moisture Content']], 'mass_flow_rate': to_dryer_out,
             'flow_type': 'Process Stream', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_to_dryer},
           {'name' : 'Wastewater (Screen Press)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate': water_out,
             'flow_type': 'Wastewater', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit12.calculations = {'Spent Grain': Screenpresssg_func}

#Unit 13: Spent Grain Dryer
Unit13 = Unit('Spent Grain Dryer')
Unit13.expected_flows_in = ['Wet Grain', 'Fuel (Feed Dryer)', 'Air (Feed Dryer)']
Unit13.expected_flows_out = ['Feed', 'Exhuast (Feed Dryer)']

Unit13.coefficients = {'Feed Moisture Content': 0.10, 't_out': 93.3, 'exhaust_t': 315.1, 'Air Ratio': 3.0,
                       'Loses': .10, 'HHV Fuel (kJ/kg)':  52200.0}

def Graindryersg_func(wet_grain_flow, coeff):
    feed_in = wet_grain_flow.attributes['mass_flow_rate']
    solids_index = wet_grain_flow.attributes['components'].index('Solids')
    solids_in = (feed_in) * (wet_grain_flow.attributes['composition'][solids_index])
    feed_out = solids_in / (1-coeff['Feed Moisture Content'])
    water_evaporated = feed_in - feed_out
    air_in = coeff['Air Ratio'] * feed_in 

    # Energy Balance:
    Q_feed = feed_out * C_pwort * (coeff['t_out'] - ambient_t)
    Q_in = wet_grain_flow.attributes['heat_flow_rate']
    Q_dryair = air_in * (1.006) * (coeff['exhaust_t'] - 20)
    Q_water_evaporated = water_evaporated * (Hvap + C_pw * (coeff['exhaust_t'] - 20))
    Q_out = Q_dryair + Q_water_evaporated + Q_feed
    Q_fuel = (Q_out - Q_in)/ (1- coeff['Loses'])
    m_fuel = Q_fuel / coeff['HHV Fuel (kJ/kg)']
    Q_loss = Q_fuel * coeff['Loses']
    
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

Unit13.calculations = {'Wet Grain': Graindryersg_func}  
             
###########################################################################################################
processunits = [Unit1, Unit2, Unit3, Unit4, Unit5, Unit16, Unit6, Unit7, Unit8,
                Unit9, Unit10, Unit11, Unit12, Unit13]
main(allflows, processunits)

for unit in processunits:
    unit.check_heat_balance(allflows)
    unit.check_mass_balance(allflows)

for flow in allflows:
    if flow.attributes['flow_type'] == 'Product':
        print(flow)

    

utilities_recap('heat_intensity_breweries_2.0', allflows, processunits)



           
