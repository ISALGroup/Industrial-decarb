# -*- coding: utf-8 -*-
"""
Created on Thursday May 1st 14:04:34 2025

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
C_pgrain = 1.829
C_pethanol = 2.44


##################################################UNITS#########################################
# Add electricity requirements
#Unit 1: Miller
Unit1 = Unit('Miller')
Unit1.expected_flows_in = ['Mash Bill', 'Electricity (Miller)']
Unit1.expected_flows_out = ['Milled Mash']
Unit1.coefficients = {'Electricity (kw/kg)': 0.0001}

def Millerfunc_mashbill(mash_bill_flow, coeff):
    mash_in = mash_bill_flow.attributes['mass_flow_rate']
    electricity_in = mash_in * coeff['Electricity (kw/kg)']
    
    return[{'name' : 'Electricity (Miller)', 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Milled Mash', 'components' : ['Solids'], 'composition' : [1], 'mass_flow_rate' : mash_in,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0}]


Unit1.calculations = {'Mash Bill': Millerfunc_mashbill}
FlowA = Flow(name = 'Mash Bill', components = ['Mash'], composition = [1], flow_type = 'input', mass_flow_rate = 10000)
FlowA.set_calc_flow()
allflows.append(FlowA)

#Unit 2: Mash Tub
Unit2 = Unit('Mash Tub')
Unit2.expected_flows_in = ['Milled Mash', 'Water (Mash Tub)']
Unit2.expected_flows_out = ['Mash']
Unit2.coefficients = {'Gallonage': (35/56.0)}

def Mashtubfunc_mash(milled_mash_flow, coeff):
    mash_in = milled_mash_flow.attributes['mass_flow_rate']
    water_in = mash_in * coeff['Gallonage']
    mash_out = mash_in + water_in
    water_wt = water_in / mash_out
    
    return[{'name' : 'Mash', 'components' : ['Solids', 'Water'], 'composition' : [1-water_wt, water_wt], 'mass_flow_rate' : mash_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': 0},
           {'name' : 'Water (Mash Tub)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : water_in,
             'flow_type': 'Water', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0}]
Unit2.calculations = {'Milled Mash': Mashtubfunc_mash}

#Unit 3: Mash Cookers
Unit3 = Unit('Mash Cooker')
Unit3.expected_flows_in = ['Mash', 'Steam (Mash Cooker)']
Unit3.expected_flows_out = ['Beer Mash', 'Condensate (Mash Cooker)']
Unit3.coefficients = {'Unit Temp': 100, 'loses': .10}

def Mashcookerfunc_mash(mash_flow, coeff):
    mash_in = mash_flow.attributes['mass_flow_rate']
    Q_in = mash_flow.attributes['heat_flow_rate']
    solid_index = mash_flow.attributes['components'].index('Solids')
    solid_composition = mash_flow.attributes['composition'][solid_index]
    c_pmash = (solid_composition * C_pgrain) + ((1-solid_composition) * C_pw)
    Q_mash = mash_in * c_pmash * (coeff['Unit Temp'] - ambient_t)
    Q_steam = (Q_mash - Q_in) / (1 - coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss = Q_steam * coeff['loses']
    return[{'name' : 'Beer Mash', 'components' : ['Solids', 'Water'], 'composition' : [solid_composition, 1-solid_composition], 'mass_flow_rate' : mash_in,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_mash},
           {'name' : 'Steam (Mash Cooker)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam','Temperature': 124.0, 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Mash Cooker)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat loss': Q_loss}]

Unit3.calculations = {'Mash': Mashcookerfunc_mash}
           
#Unit 4: Mash Coolers
Unit4 = Unit('Mash Cooler')
Unit4.expected_flows_in = ['Beer Mash', 'Cooling Water (Mash Cooler)']
Unit4.expected_flows_out = ['Cooled Beer Mash', 'Water (Mash Cooler)']
Unit4.coefficients = {'Unit Temp': 27.5, 'Cooling Water Max Temp': 20.0, 'Cooling Water Min Temp': 5.0}

def Mashcoolerfunc_mash(beer_mash_flow, coeff):
    mash_in = beer_mash_flow.attributes['mass_flow_rate']
    Q_in = beer_mash_flow.attributes['heat_flow_rate']
    solid_index = beer_mash_flow.attributes['components'].index('Solids')
    solid_composition = beer_mash_flow.attributes['composition'][solid_index]
    c_pmash = (solid_composition * C_pgrain) + ((1-solid_composition) * C_pw)
    Q_out = mash_in * c_pmash * (coeff['Unit Temp'] - ambient_t)
    Q_water = Q_in - Q_out
    m_coolingwater = Q_water / (C_pw * (coeff['Cooling Water Max Temp'] - coeff['Cooling Water Min Temp']))
    return[{'name' : 'Cooled Beer Mash', 'components' : ['Solids', 'Water'], 'composition' : [solid_composition, 1-solid_composition], 'mass_flow_rate' : mash_in,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_out},
           {'name' : 'Cooling Water (Mash Cooler)', 'components' : 'Water', 'mass_flow_rate' : m_coolingwater,
             'flow_type': 'Water', 'temperature' : coeff['Cooling Water Min Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'name' : 'Water (Mash Cooler)', 'components' : 'Water', 'mass_flow_rate' : m_coolingwater,
             'flow_type': 'Water', 'temperature' : coeff['Cooling Water Max Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_water}]
Unit4.calculations = {'Beer Mash': Mashcoolerfunc_mash}

               
#Unit 5: Fermentor
Unit5 = Unit('Fermentor')
Unit5.expected_flows_in = ['Cooled Beer Mash', 'Active Yeast', 'Cooling Water (Fermentor)']
Unit5.expected_flows_out = ['Beer','Carbon Dioxide', 'Water (Fermentor)']
Unit5.coefficients = {'Unit Temp': 29.444, 'Ethanol wt': 0.095, 'Ethanol MM': 46.07, 'CO2 MM': 44.009,
                      'Ethanol - CO2 Molar Ratio': 1.00, 'Yeast to Mash': (1/3800), 'Cooling Water Min Temp': 5.0,
                      'Reaction Enthalpy': 74.0}

def Fermentorfunc_beermash(cooled_beer_mash_flow, coeff):
    mash_in = cooled_beer_mash_flow.attributes['mass_flow_rate']
    solid_index = cooled_beer_mash_flow.attributes['components'].index('Solids')
    solids_in = (cooled_beer_mash_flow.attributes['composition'][solid_index]) * mash_in
    water_in = mash_in - solids_in
    yeast_in = mash_in * coeff['Yeast to Mash']
    ethanol_out = water_in * coeff['Ethanol wt']
    co2_out = (ethanol_out / coeff['Ethanol MM']) * coeff['Ethanol - CO2 Molar Ratio']  * coeff['CO2 MM']
    solids_out = solids_in + yeast_in - ethanol_out - co2_out
    mash_out = ethanol_out + solids_out + water_in
    water_wt = water_in / mash_out
    solids_wt = solids_out / mash_out

    Q_in = cooled_beer_mash_flow.attributes['heat_flow_rate']
    C_pbeer = (solids_wt * C_pgrain) + (water_wt * C_pw) + (coeff['Ethanol wt'] * C_pethanol)
    Q_out = mash_out * C_pbeer * (coeff['Unit Temp'] - ambient_t)
    Q_rxn = (co2_out * coeff['Reaction Enthalpy']) / ( 2 * coeff['CO2 MM'])
    Q_co2 = co2_out * 0.846 * (coeff['Unit Temp'] - ambient_t)
    Q_water = 0
    Q_cw = (Q_in + Q_rxn) - (Q_out + Q_co2 + Q_water)
    m_cw = Q_cw / (C_pw * (coeff['Cooling Water Min Temp'] - ambient_t))
    
    
    return[{'name' : 'Beer', 'components' : ['Solids','Ethanol', 'Water'], 'composition' : [solids_wt, coeff['Ethanol wt'], water_wt], 'mass_flow_rate' : mash_out,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_out},
           {'name' : 'Carbon Dioxide', 'components' : ['CO2'], 'composition' : [1], 'mass_flow_rate' : co2_out,
             'flow_type': 'Product', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_co2},
           {'name' : 'Cooling Water (Fermentor)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : m_cw,
             'flow_type': 'Cooling Water', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_cw},
           {'name' : 'Water (Fermentor)', 'components' : ['Water'], 'composition' : [1], 'mass_flow_rate' : m_cw,
             'flow_type': 'Cooling Water', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': ((Q_cw + Q_rxn + Q_in) - (Q_out + Q_co2 + Q_water))},
           {'name' : 'Active Yeast', 'components' : ['Yeast'], 'composition' : [1], 'mass_flow_rate' : yeast_in,
             'flow_type': 'Process Stream', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': 0},
           {'Heat of reaction': Q_rxn}]

Unit5.calculations = {'Cooled Beer Mash': Fermentorfunc_beermash}

#Unit 9: Preaheater
Unit9 = Unit('Preheater')
Unit9.expected_flows_in = ['Beer', 'Steam (Preheater)']
Unit9.expected_flows_out = ['Condensate (Preheater)', 'Beer Feed']

Unit9.coefficients = {'unit_t': 71.1, 'loses': 0.10}

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

           {'name' : 'Beer Feed', 'components' : ['Solids','Ethanol', 'Water'], 'composition' : composition, 'mass_flow_rate' : cooled_mash_in,
             'flow_type': 'Process stream', 'temperature' : coeff['unit_t'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_out}]

Unit9.calculations = {'Beer': Preaheaterfunc_cooledmash}
    
#Unit 6: Beer Still
Unit6 = Unit('Distillation Column')
Unit6.expected_flows_in = ['Beer Feed', 'Steam (Distillation Column)']
Unit6.expected_flows_out = ['Tops', 'Bottoms', 'Condensate (Distillation Column)']

Unit6.coefficients = {'Ethanol wt out': .55, 'loses': 0.15, 'Reflux Ratio': 2.0, 'Boiler Percentage': .25, 'T_top': 78.2, 'T_bottom': 95.0, 'c_pethanol': 2.44,
                      'c_pcorn': 2.42, 'Hvap_ethanol': 885.0, 'KJ/L Distillation': 8000}

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
    # Static Q_steam using a literature value
    Q_in = mash_flow.attributes['heat_flow_rate']
    Q_steam = ((coeff['KJ/L Distillation'] * tops_out) - Q_in) / (1 - coeff['loses'])
    m_steam = Q_steam / Hvap
    Q_loss = Q_steam * coeff['loses']
    c_ptops = (tops_water_wt * C_pw) + (coeff['Ethanol wt out'] * coeff['c_pethanol'])
    c_pbottoms = (bottoms_water_wt * C_pw) + (bottoms_solid_wt * coeff['c_pcorn'])
    Q_tops_out = c_ptops * tops_out * (coeff['T_top'] - ambient_t) 
    Q_bottoms_out = c_pbottoms * bottoms_out * (coeff['T_bottom'] - ambient_t)

    print('Column')
    
    return[{'name' : 'Tops', 'components' : ['Ethanol', 'Water'], 'composition' : [coeff['Ethanol wt out'], tops_water_wt], 'mass_flow_rate' : tops_out,
             'flow_type': 'Process stream', 'temperature' : coeff['T_top'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': (Q_tops_out)},

           {'name' : 'Bottoms', 'components' : ['Solids', 'Water'], 'composition' : [bottoms_solid_wt, bottoms_water_wt], 'mass_flow_rate' : bottoms_out,
             'flow_type': 'Process stream', 'temperature' : coeff['T_bottom'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_bottoms_out},

           {'name' : 'Steam (Distillation Column)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},

           {'name' : 'Condensate (Distillation Column)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0},

           {'Heat loss': (Q_loss + (Q_steam - Q_tops_out - Q_bottoms_out))}]


Unit6.calculations = {'Beer Feed': Distillationcolumnfunc_mash}

#Unit 7: Rectifying - a little bit of a black box because it is very niche to producers
Unit7 = Unit('Rectifyer')
Unit7.expected_flows_in = ['Tops']
Unit7.expected_flows_out = ['Burbon']
Unit7.coefficients = {'Unit Temp': 95}

def Rectifyerfunc_tops(tops_flow, coeff):
    tops_out = tops_flow.attributes['mass_flow_rate']
    Q_out = tops_flow.attributes['heat_flow_rate']
    print('Rectifyer')
    return[{'name' : 'Burbon', 'components' : ['Burbon'], 'composition' : [1], 'mass_flow_rate' : tops_out,
             'flow_type': 'Process stream', 'temperature' : coeff['Unit Temp'] ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : True, 'heat_flow_rate': Q_out}]

Unit7.calculations = {'Tops': Rectifyerfunc_tops}

#Unit 8: Bottling
Unit8 = Unit('Bottler')
Unit8.expected_flows_in = ['Burbon', 'Steam (Bottler)']
Unit8.expected_flows_out = ['Bottled Burbon', 'Condensate (Bottler)']

Unit8.coefficients = {'Steam Demand (kJ/L)': 600.0}

def Bottlerfunc_burbon(burbon_flow, coeff):
    burbon_in = burbon_flow.attributes['mass_flow_rate']
    Q_steam = burbon_in * coeff['Steam Demand (kJ/L)']
    m_steam = Q_steam / Hvap
    Q_in = burbon_flow.attributes['heat_flow_rate']
    Q_out = Q_steam + Q_in
    print('Bottler')
    return[{'name' : 'Bottled Burbon', 'components' : ['Burbon'], 'composition' : [1], 'mass_flow_rate': burbon_in,
             'flow_type': 'Product', 'temperature' : ambient_t ,'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': Q_out},
           {'name' : 'Steam (Bottler)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Steam', 'elec_flow_rate' : 0, 'In or out' : 'In', 'Set calc' : False, 'heat_flow_rate': Q_steam},
           {'name' : 'Condensate (Bottler)', 'components' : 'Water', 'mass_flow_rate' : m_steam,
             'flow_type': 'Condensate', 'elec_flow_rate' : 0, 'In or out' : 'Out', 'Set calc' : False, 'heat_flow_rate': 0}]

Unit8.calculations = {'Burbon': Bottlerfunc_burbon}

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

###########################
processunits = [Unit1, Unit2, Unit3, Unit4, Unit5, Unit9, Unit6,
                Unit10, Unit11, Unit12, Unit7, Unit8]


main(allflows, processunits)

for unit in processunits:
    unit.check_heat_balance(allflows)
    unit.check_mass_balance(allflows)

for flow in allflows:
    if flow.attributes['flow_type'] == 'Product':
        print(flow)
        

utilities_recap('heat_intensity_distilleries_2.0', allflows, processunits)


