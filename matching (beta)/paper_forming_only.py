# -*- coding: utf-8 -*-
"""
Created on Fri Nov 15 12:04:45 2024

@author: Antoine
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

## Global variables
## Most of them are variables that are influenced by the type of wood, the 
## efficiency of the process units, etc...
## Of note are the amounts of white liquor and of white wash + their temperatures
## They have to be fixed here due to being in recovery loops in order to avoid discrepancies
amb_t = 20 
recovery_yield = 0.5
NaOH_in_WL = 40 #g/kg WL
Na2S_in_WL = 15.6 #g/kg WL
green_liq_moist = 0.9
wl_amount = 1000. #kg/t wood in digester
white_liquor_t = 45
wood_reference_flow = 1000. #kg of wood in the digester
cao_t = 650
bark_in = 0.15
bark_out = 0.01
rejects_a = 0.05
wood_flow_amount = wood_reference_flow * (1/(1-rejects_a)) * (1/((1-bark_in) + (bark_out/(1-bark_out)) * (1-bark_in)))
wood_moisture = 0.55
normal_bark_in = bark_in * (1-wood_moisture)
normal_wood_in = 1 - wood_moisture - normal_bark_in
heat_bark = 5900 #kJ/kg Heating value of bark 


#Physical/chemical constants
Na_in_NaOH = 23./40.
Na_in_Na2S = (46./78.)
CaO_hyd_hor_mol = 63.7 #kJ/mol of CaO
CaO_gmol = 56. #g/mol
CaO_hyd_hor_g = CaO_hyd_hor_mol/CaO_gmol #kJ/g
Na2CO3_amount = (106./80.) * NaOH_in_WL * wl_amount #g per t of wood in digester
CaO_req = Na2CO3_amount * (56./106.) #g per t of wood in digester
cao_molcp = 32.5 #J/mol.K
cao_kgcp = cao_molcp / CaO_gmol #kJ/kg.K
abs_wl_amount = wl_amount * wood_reference_flow / 1000.
white_liquor_cp = 3.411 #kJ/kg.K
lime_calcination_t = 1000.
lime_calcination_h = 168. #kJ/mol
caco3_gmol = 100
lime_calcination_hmass = (lime_calcination_h/caco3_gmol) * 1000. #kJ/kg


#Unit 1: Pulper
            
Unit1 = Unit('Pulper')
Unit1.expected_flows_in = ['Market pulp (Pulper)' , 'Water (Pulper)', 'Electricity (Pulper)']
Unit1.expected_flows_out = ['Stock',  'Wastewater (Pulper)']
Unit1.coefficients = {'Electricity per ton of paper' : 100, 'Stock consistency' : 0.01, 'Additional water coefficient' : 2} 
def pulper_func(mkt_pulp, coeff):
    pulp_amount = mkt_pulp.attributes['mass_flow_rate']
    elec_amount = pulp_amount * coeff['Electricity per ton of paper']/1000.
    dry_pulp_in = pulp_amount * mkt_pulp.attributes['composition'][mkt_pulp.attributes['components'].index('Dry pulp')]
    moisture_in = pulp_amount - dry_pulp_in
    total_water_out = ((1 - coeff['Stock consistency'])/coeff['Stock consistency'])*dry_pulp_in
    water_in = total_water_out * coeff['Additional water coefficient']
    wastewater_out = water_in + moisture_in - total_water_out
    out_m = dry_pulp_in + total_water_out
    
    return [{'name' : 'Electricity (Pulper)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' :0,  'In or out' : 'In', 'elec_flow_rate' : elec_amount ,  'Set calc' : False, 'Set shear' : False},
            {'name' : 'Water (Pulper)', 'components' : ['Water'], 'mass_flow_rate' :water_in,
                     'flow_type': 'Water', 'temperature' : amb_t,  'In or out' : 'In', 'elec_flow_rate' :0 ,  'Set calc' : False, 'Set shear' : False},
            {'name' : 'Stock', 'components' : ['Dry pulp', 'Water'], 'composition' : [coeff['Stock consistency'], 1 - coeff['Stock consistency']] ,'mass_flow_rate' : out_m,
                     'flow_type': 'Process', 'temperature' : amb_t,  'In or out' : 'Out', 'elec_flow_rate' : 0 ,  'Set calc' : True, 'Set shear' : False},
            {'name' : 'Wastewater (Pulper)', 'components' : ['Water'], 'mass_flow_rate' : wastewater_out,
                     'flow_type': 'Wastewater', 'temperature' : amb_t,  'In or out' : 'Out', 'elec_flow_rate' : 0 ,  'Set calc' : False, 'Set shear' : False}    
        ]


Unit1.calculations = {'Market pulp (Pulper)' : pulper_func}

#Unit 2: Stock preparation

Unit2 = Unit('Stock preparation')
Unit2.expected_flows_in = ['Stock' , 'Make-up water (Stock preparation)', 'Additives', 'Steam (Stock preparation)', 'Electricity (Stock preparation)' ]
Unit2.expected_flows_out = ['Thick stock', 'Condensate (Stock preparation)']
Unit2.coefficients = {'Electricity per ton of paper' : 274, 'Stock consistency' : 0.04, 'Energy consumption per ton of paper' : 0.7, 'Losses' : 0.1, 'Effluent per ton paper' : 10, 'Amount of additives per ton' : 50} 

def stock_prep(washpulp_flow, coeff):
    washpulp_amount = washpulp_flow.attributes['mass_flow_rate']
    paper_amount = washpulp_amount*washpulp_flow.attributes['composition'][washpulp_flow.attributes['components'].index('Dry pulp')]
    water_in = washpulp_amount*washpulp_flow.attributes['composition'][washpulp_flow.attributes['components'].index('Water')]
    elec_in = paper_amount*coeff['Electricity per ton of paper']/1000. #kWh
    heat_in = paper_amount*coeff['Energy consumption per ton of paper']*1000. #GJ
    Q_steam = heat_in/(1 - coeff['Losses']) #GJ
    additives_amount = paper_amount*coeff['Amount of additives per ton']/1000.
    treated_paper_amount = paper_amount + additives_amount
    total_water = ((1 - coeff['Stock consistency'])/coeff['Stock consistency'])*treated_paper_amount
    makeup_water_amount = treated_paper_amount*coeff['Effluent per ton paper']
    recirculated_water_amount = total_water - makeup_water_amount - water_in
    cp_stock = 4.2
    water_vap_h = 2200
    steam_amount = Q_steam / water_vap_h
    Q_dew_point = steam_amount * cp_stock * (100-amb_t)
    Q_loss = Q_steam - heat_in
    total_outlet_amount = treated_paper_amount + total_water
    outlet_t = amb_t + ((washpulp_flow.attributes['heat_flow_rate'] + heat_in)/(total_outlet_amount*cp_stock))
    
    return[{'name' : 'Electricity (Stock preparation)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : elec_in ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Make-up water (Stock preparation)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : makeup_water_amount,
                     'flow_type': 'Water', 'temperature' : amb_t, 'pressure': 1 , 'heat_flow_rate' : 0, 'combustion_energy_content' : 0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            # {'name' : 'Recirculated water (Stock preparation)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : recirculated_water_amount,
            #          'flow_type': 'Process flow', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Additives', 'components' : ['Additives'], 'composition': [1], 'mass_flow_rate' : additives_amount,
                     'flow_type': 'Process flow', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Steam (Stock preparation)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : steam_amount,
                     'flow_type': 'Steam', 'temperature' : 100, 'pressure':1 , 'heat_flow_rate' :Q_dew_point + Q_steam,'In or out' : 'In', 'Set calc' :False, 'Set shear' : False},
            {'name' : 'Condensate (Stock preparation)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : steam_amount,
                     'flow_type': 'Condensate', 'temperature' : 100, 'pressure':1 , 'heat_flow_rate' :Q_dew_point ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Thick stock', 'components' : ['Dry pulp', 'Additives', 'Water'], 'composition': [paper_amount/total_outlet_amount, additives_amount/total_outlet_amount, total_water/total_outlet_amount], 'mass_flow_rate' : total_outlet_amount,
                     'flow_type': 'Process flow', 'temperature' : outlet_t, 'pressure':1 , 'heat_flow_rate' : heat_in ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'Heat loss' : Q_loss}
        ]

Unit2.calculations = {'Stock' :  stock_prep  }

#Unit 3: Forming and press
Unit3 = Unit('Forming and press')
Unit3.expected_flows_in = ['Thick stock' , 'Electricity (Forming and press)']
Unit3.expected_flows_out = ['Formed paper', 'Wastewater (Forming and press)']
Unit3.coefficients = {'Electricity per ton of paper' : 238, 'Press consistency' : 0.4}
def form_and_press(thk_stock_flow, coeff):
    cp_stock = 4.2
    Q_in = thk_stock_flow.attributes['heat_flow_rate']
    paper_amount = thk_stock_flow.attributes['mass_flow_rate']*(thk_stock_flow.attributes['composition'][thk_stock_flow.attributes['components'].index('Dry pulp')] + thk_stock_flow.attributes['composition'][thk_stock_flow.attributes['components'].index('Additives')])
    water_in = thk_stock_flow.attributes['mass_flow_rate'] - paper_amount
    moisture_in_presspaper = ((1 - coeff['Press consistency']) * paper_amount)/coeff['Press consistency']
    ww_amount = water_in - moisture_in_presspaper
    elec_amount = paper_amount * coeff['Electricity per ton of paper']/1000.
    t_process = thk_stock_flow.attributes['temperature']
    total_paper_amount = moisture_in_presspaper + paper_amount
    Q_formed_p = cp_stock * (t_process - amb_t) * total_paper_amount
    Q_wastewater = Q_in -  Q_formed_p
    return [{'name' : 'Electricity (Forming and press)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : elec_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Formed paper', 'components' : ['Paper', 'Water'], 'composition': [paper_amount/total_paper_amount, (1 - paper_amount)/total_paper_amount], 'mass_flow_rate' : total_paper_amount,
                     'flow_type': 'Process flow', 'temperature' : t_process, 'pressure': 1 , 'heat_flow_rate' : Q_formed_p, 'combustion_energy_content' : 0 ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Wastewater (Forming and press)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : ww_amount,
                     'flow_type': 'Wastewater', 'temperature' : t_process, 'pressure':1 , 'heat_flow_rate' :Q_wastewater ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False}
        ]

Unit3.calculations = {'Thick stock': form_and_press}

#Unit 4: Drying

Unit4= Unit('Dryer (paper)')
Unit4.expected_flows_in = ['Formed paper', 'Electricity (Dryer, paper)', 'Air (Dryer, paper)', 'Steam (Dryer, paper)']
Unit4.expected_flows_out = ['Dried paper', 'Exhaust gas (Dryer, paper)', 'Condensate (Dryer, paper)']

Unit4.coefficients = {'Dry paper moisture' : 0.05 , 'Air temperature' : 80, 'd_T' : 5, 'Exhaust gas temperature' : 60, 'Electricity (kWh) per ton of paper' : 21, 'Paper temperature' : 45, 'Loss' : 0.1}


def Dryerfunc_paper(paper_pulp_flow, coeff):
    steam_t = coeff['Air temperature'] + coeff['d_T']
    paper_pulp_amount = paper_pulp_flow.attributes['mass_flow_rate']
    dry_paper_amount = paper_pulp_amount * paper_pulp_flow.attributes['composition'][paper_pulp_flow.attributes['components'].index('Paper')]
    water_amount = paper_pulp_amount - dry_paper_amount
    mu_m_out = coeff['Dry paper moisture']
    mass_m_out = (mu_m_out/(1-mu_m_out)) * dry_paper_amount
    paper_out_amount = dry_paper_amount + mass_m_out
    water_out_amount =  water_amount - mass_m_out
    air_t = coeff['Air temperature']
    exhaust_t = coeff['Exhaust gas temperature']
    electricity_amount = coeff['Electricity (kWh) per ton of paper'] * dry_paper_amount / 1000.
    t_in = paper_pulp_flow.attributes['temperature']
    dried_paper_out_t = coeff['Paper temperature']
    cp_water = 4.2 #kJ/kg.K
    cp_steam = 1.89 #kJ/kg.K
    cp_dry_paper = 2.3
    cp_air = 1.01
    vap_heat = 2256.4 #kJ/kg
    Q_water_out = water_out_amount * ((cp_steam * (exhaust_t - 100)) + (cp_water *(100 - t_in ) ) + vap_heat)
    Q_dry_paper_out = dry_paper_amount * cp_dry_paper * (dried_paper_out_t - t_in)
    Q_moisture_out = mass_m_out * cp_water * (dried_paper_out_t - t_in)
    Q_dried_paper = Q_dry_paper_out + Q_moisture_out
    m_air = (Q_water_out + Q_dried_paper)/(cp_air * (air_t - exhaust_t))
    Q_air_out = m_air * cp_air * (exhaust_t - amb_t)
    Q_air_in = m_air * cp_air * (air_t - amb_t)
    Q_in = paper_pulp_flow.attributes['heat_flow_rate']
    Q_exhaust = Q_air_out + Q_water_out
    Q_steam = ((Q_exhaust + Q_dried_paper)/(1 - coeff['Loss'])) - Q_in
    Q_loss = Q_steam * coeff['Loss']
    vap_heat_130 = 2173.7
    m_steam = Q_steam/vap_heat_130
    cp_wat_130 = 4.26
    Q_condensate = m_steam * cp_wat_130 * (steam_t - amb_t)
    Q_steam_in = Q_steam + Q_condensate
    exhaust_amount = water_out_amount + m_air
    exhaust_gas_water_ratio = water_out_amount/exhaust_amount
    
    return [{'name' : 'Electricity (Dryer, paper)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Air (Dryer, paper)', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : m_air,
                     'flow_type': 'Air', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Condensate (Dryer, paper)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                     'flow_type': 'Condensate', 'temperature' : steam_t, 'pressure':2.7 , 'heat_flow_rate' :Q_condensate ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Steam (Dryer, paper)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                     'flow_type': 'Steam', 'temperature' : steam_t, 'pressure':2.7 , 'heat_flow_rate' :Q_steam_in ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Exhaust gas (Dryer, paper)', 'components' : ['Water', 'Air'], 'composition': [exhaust_gas_water_ratio, 1 - exhaust_gas_water_ratio], 'mass_flow_rate' : exhaust_amount,
                     'flow_type': 'Exhaust', 'temperature' : exhaust_t, 'pressure':1 , 'heat_flow_rate' :Q_exhaust ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Dried paper', 'components' : ['Dry pulp', 'Water'], 'composition': [1 - mu_m_out, mu_m_out], 'mass_flow_rate' : paper_out_amount,
                     'flow_type': 'Product', 'temperature' : dried_paper_out_t, 'pressure':1 , 'heat_flow_rate' :Q_dried_paper ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss'  : Q_loss}
            
            ]

Unit4.calculations = {'Formed paper' : Dryerfunc_paper}


processunits = [Unit1,Unit2,Unit3,Unit4]



FlowA = Flow('Market pulp (Pulper)',['Water', 'Dry pulp'],'input', amb_t, 1, [0.05, 0.95 ], None , None, 1000, np.nan, 0)
FlowA.set_calc_flow()
allflows.append(FlowA)

main(allflows, processunits, f_print = True)
# print(are_units_calced(processunits))

# for unit in processunits:
#     unit.is_calc = True

# print(are_units_calced(processunits))

#print_flows(allflows)
for unit in processunits:
    unit.check_heat_balance(allflows)
    unit.check_mass_balance(allflows)


# for unit in processunits:
#     print(unit.is_calc)

unit_recap_to_file('new_pparch', allflows, processunits)

utilities_recap('new_pparch', allflows, processunits)