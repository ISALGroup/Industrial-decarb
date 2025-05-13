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



            
#Unit 1 : Debarker definition                  
Unit1 = Unit('Barking')
Unit1.expected_flows_in = ['Logs', 'Electricity (debarker)']
Unit1.expected_flows_out = ['Bark', 'Wood']
Unit1.coefficients = {'Power per t log' : 8.5, 'Bark out': bark_out}

def Debarkerfunc_logs(wood_flow, coeff):
    wood_amount = wood_flow.attributes['mass_flow_rate']
    electricity_amount = wood_amount * coeff['Power per t log']/1000
    bark_index = wood_flow.attributes['components'].index('Bark')
    wood_index = wood_flow.attributes['components'].index('Wood')
    moisture_index = wood_flow.attributes['components'].index('Water')
    bark_amount = wood_flow.attributes['composition'][bark_index]
    wood_ratio = wood_flow.attributes['composition'][wood_index]
    moisture_amount = wood_flow.attributes['composition'][moisture_index]
    bark_to_wood_ratio = bark_amount/(wood_ratio+bark_amount)
    bark_out_ratio = coeff['Bark out']
    wood_out = (1-bark_to_wood_ratio) +  (bark_out_ratio/(1-bark_out_ratio))*(1-bark_to_wood_ratio)
    bark_out = 1 - wood_out
    bark_out_amount = wood_amount * bark_out
    wood_out_amount = wood_amount * wood_out
    return [{'name' : 'Electricity (debarker)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_amount, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Bark', 'components' : ['Bark', 'Water'], 'composition': [1-moisture_amount, moisture_amount], 'mass_flow_rate' : bark_out_amount,
                     'flow_type': 'Process flow', 'temperature' : amb_t, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Wood', 'components' : ['Wood', 'Water'], 'composition' : [1-moisture_amount, moisture_amount] , 'mass_flow_rate' : wood_out_amount,
                     'flow_type': 'Process flow', 'temperature' : amb_t, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False}]

Unit1.calculations = {'Logs' : Debarkerfunc_logs}



#Unit 2 : Bark press definition  
Unit2 = Unit('Barking press')
Unit2.expected_flows_in = ['Bark', 'Electricity(barking press)']
Unit2.expected_flows_out = ['Dry bark', 'Bark water']
Unit2.coefficients = {'Removed moisture' : 0.15, 'Electricity demand per ton of removed water' : 5, 'Bark heating value' : heat_bark}

def Barkpressfunc_bark(bark_flow, coeff):
    bark_in = bark_flow.attributes['mass_flow_rate']
    moisture_index = bark_flow.attributes['components'].index('Water')
    moisture_in = bark_flow.attributes['composition'][moisture_index]
    moisture_out = moisture_in - coeff['Removed moisture']
    bark_out = ((1-moisture_in) + ((moisture_out)/(1-moisture_out)) * (1-moisture_in))*bark_in
    water_out = bark_in - bark_out
    electricity_in = (water_out/1000)*coeff['Electricity demand per ton of removed water']
    fuel_value_bark = bark_out * coeff['Bark heating value']
    return [{'name' : 'Electricity(barking press)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_in, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'De-watered bark', 'components' : ['Bark', 'Moisture'], 'composition': [1-moisture_out, moisture_out], 'mass_flow_rate' : bark_out,
                     'flow_type': 'Fuel (produced on-site)','combustion_energy_content' : fuel_value_bark ,'temperature' : amb_t, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Bark water', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : water_out,
                     'flow_type': 'Process flow', 'temperature' : amb_t, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False}]

Unit2.calculations = {'Bark' : Barkpressfunc_bark}

#Unit 3 : Chipper definition  
Unit3 = Unit('Chipper')
Unit3.expected_flows_in = ['Wood', 'Electricity (chipper)']
Unit3.expected_flows_out = ['Chips and rejects']
Unit3.coefficients = {'kWh per ton of wood' : 30.3}


def Chipperfunc_wood(wood_flow, coeff):
    wood_amount = wood_flow.attributes['mass_flow_rate']
    wood_flow_moisture = wood_flow.attributes['composition'][wood_flow.attributes['components'].index('Water')]
    chips_amount = wood_amount
    elec_amount = (wood_amount/1000.)*coeff['kWh per ton of wood']
    return [{'name' : 'Electricity (chipper)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : elec_amount, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Chips and rejects', 'components' : ['Chips and rejects', 'Water'], 'composition': [1-wood_flow_moisture, wood_flow_moisture], 'mass_flow_rate' : chips_amount,
                     'flow_type': 'Process flow', 'temperature' : amb_t, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False}]

Unit3.calculations = {'Wood' : Chipperfunc_wood}



#Unit 4 : Screening definition  
Unit4 = Unit('Screening')
Unit4.expected_flows_in = ['Chips and rejects']
Unit4.expected_flows_out = ['Chips','Rejects']
Unit4.coefficients = {'Rejects ratio' : rejects_a}

def Screeningfunc_cnr(cnr_flow, coeff):
    cnr_amount = cnr_flow.attributes['mass_flow_rate']
    moisture_amount = cnr_flow.attributes['composition'][cnr_flow.attributes['components'].index('Water')]
    rejects_amount = coeff['Rejects ratio']*cnr_amount
    chips_amount = cnr_amount - rejects_amount
    rejects_combustion_energy = rejects_amount * heat_bark
    return [{'name' : 'Chips', 'components' : ['Chips', 'Water'], 'composition': [1-moisture_amount, moisture_amount], 'mass_flow_rate' : chips_amount,
             'flow_type': 'Process flow', 'temperature' : amb_t, 'pressure':1 , 'In or out' : 'Out', 'heat_flow_rate' : 0, 'Set calc' : True, 'Set shear' : False},     
            {'name' : 'Rejects', 'components' : ['Rejects'], 'composition': [1], 'mass_flow_rate' : rejects_amount,
                     'flow_type': 'Fuel (produced on-site)', 'combustion_energy_content': rejects_combustion_energy, 'temperature' : amb_t, 'pressure':1 , 'In or out' : 'Out', 'heat_flow_rate' : 0, 'Set calc' : False, 'Set shear' : False}]
    
Unit4.calculations = {'Chips and rejects' : Screeningfunc_cnr}


#Unit 5 : Digester definition  
Unit5 = Unit('Digester')
Unit5.expected_flows_in = ['Chips', 'Steam (digester)', 'White liquor']
Unit5.expected_flows_out = ['Pulp mix']
Unit5.coefficients = {'White liquor ratio' : 1, 'White liquor temperature' : white_liquor_t, 'Heat loss' : 0.05,
                      'Steam temperature' : 165}


def Digesterfunc_chips(chips_flow, coeff):
    chips_amount = chips_flow.attributes['mass_flow_rate']
    white_liquor_amount = chips_amount*coeff['White liquor ratio']
    white_liquor_temperature = coeff['White liquor temperature']
    steam_loss_ratio = coeff['Heat loss']
    white_liquor_cp = 3.411 #kJ/kg.K
    white_liquor_heat = white_liquor_cp*white_liquor_amount*(white_liquor_temperature - amb_t)
    pulp_cp = 3.495
    steam_temperature =  coeff['Steam temperature']
    pulp_temperature = coeff['Steam temperature']
    steam_cp = 2.4424
    steam_latent = 2065.5
    steam_amount = (pulp_cp*(white_liquor_amount + chips_amount)*(pulp_temperature-amb_t) - white_liquor_heat)/(((1-steam_loss_ratio)*(steam_cp*(steam_temperature-amb_t) + steam_latent)) -pulp_cp*(pulp_temperature-amb_t) )
    steam_heat = steam_amount * (steam_cp*(steam_temperature - amb_t) + steam_latent)
    heat_loss = steam_heat * steam_loss_ratio
    pulp_amount = chips_amount + white_liquor_amount + steam_amount
    pulp_heat = pulp_amount * pulp_cp * (pulp_temperature - amb_t)
    water_in_chips = chips_amount * chips_flow.attributes['composition'][chips_flow.attributes['components'].index('Water')]
    total_water = water_in_chips + white_liquor_amount * 0.85 + steam_amount
    dry_wl = white_liquor_amount * 0.15
    dry_pulp = chips_amount * chips_flow.attributes['composition'][chips_flow.attributes['components'].index('Chips')]
    water_ratio = total_water/pulp_amount
    dry_pulp_ratio = dry_pulp/pulp_amount
    dry_wl_ratio = dry_wl/pulp_amount
    
    return [{'name' : 'Steam (digester)', 'components' : ['Steam'], 'composition': [1], 'mass_flow_rate' : steam_amount,
             'flow_type': 'Steam', 'temperature' : steam_temperature, 'pressure':8 , 'In or out' : 'In', 'heat_flow_rate' : steam_heat ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'White liquor', 'components' : ['White liquor (dry)', 'Water'], 'composition': [0.15, 0.85], 'mass_flow_rate' : white_liquor_amount,
                     'flow_type': 'Process flow', 'temperature' : white_liquor_temperature, 'pressure':1 , 'In or out' : 'In', 'heat_flow_rate' : white_liquor_heat, 'Set calc' : False, 'Set shear' : True}, 
            {'name' : 'Pulp mix', 'components' : ['Water', 'Dry pulp', 'Dry white liquor'], 'composition' : [water_ratio, dry_pulp_ratio, dry_wl_ratio], 'mass_flow_rate' : pulp_amount,
             'flow_type' : 'Process flow', 'temperature' : pulp_temperature , 'heat_flow_rate' : pulp_heat, 'Set calc' : True, 'In or out' : 'Out', 'Set shear' : False},
            {'Heat loss' : heat_loss}]

Unit5.calculations = {'Chips' : Digesterfunc_chips}

#Unit 6 : Blow tank definition  
Unit6 = Unit('Blow tank')
Unit6.expected_flows_in = ['Pulp mix']
Unit6.expected_flows_out = ['Low pressure steam (blow tank)', 'Liquid pulp']

def Blowtankfunc_pm(pm_flow, coeff):
    pulp_mix_amount = pm_flow.attributes['mass_flow_rate']
    pulp_mix_heat = pm_flow.attributes['heat_flow_rate']
    liq_pulp_cp = 2.927
    steam_t = 100
    liq_pulp_t = 100
    d_T = 75
    steam_latent = 2256.4
    steam_cp = 2.03
    water_in = pulp_mix_amount*pm_flow.attributes['composition'][pm_flow.attributes['components'].index('Water')]
    dry_wl_in = pulp_mix_amount*pm_flow.attributes['composition'][pm_flow.attributes['components'].index('Dry white liquor')]
    dry_pulp_in = pulp_mix_amount*pm_flow.attributes['composition'][pm_flow.attributes['components'].index('Dry pulp')]
    steam_amount = (pulp_mix_heat - (liq_pulp_cp * d_T * pulp_mix_amount))/(steam_latent + (steam_cp - liq_pulp_cp) * d_T)
    water_in_pulp = water_in - steam_amount
    liq_pulp_amount = pulp_mix_amount - steam_amount
    water_pulp_out_ratio = water_in_pulp/liq_pulp_amount
    liq_pulp_heat = liq_pulp_cp * d_T * liq_pulp_amount
    steam_heat = pulp_mix_heat - liq_pulp_heat
    
    return [{'name' : 'Liquid pulp', 'components' : ['Water', 'Dry white liquor', 'Dry pulp'], 'composition': [water_pulp_out_ratio, dry_wl_in/liq_pulp_amount, dry_pulp_in/liq_pulp_amount], 'mass_flow_rate' : liq_pulp_amount,
             'flow_type': 'Process flow', 'temperature' : liq_pulp_t, 'pressure':1 , 'In or out' : 'Out', 'heat_flow_rate' : liq_pulp_heat ,  'Set calc' : True, 'Set shear' : False},     
            {'name' : 'Low pressure steam (blow tank)', 'components' : ['Steam'], 'composition': [1], 'mass_flow_rate' : steam_amount,
                     'flow_type': 'Process flow', 'temperature' : steam_t, 'pressure':1 , 'heat_flow_rate' :steam_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False}]

Unit6.calculations = {'Pulp mix' : Blowtankfunc_pm}



#Unit 7 : Washing filters definition  
Unit7 = Unit('Washing filters')
Unit7.expected_flows_in = ['Liquid pulp', 'Water (Washing filters)', 'Electricity (Washing filters)']
Unit7.expected_flows_out = ['Black liquor', 'Washed pulp']
Unit7.coefficients = {'Water/liquid pulp ratio' : 1. , 'Electricity per t of pulp' : 3}
def Washingfiltersfunc_liqpulp(liq_pulp_flow, coeff):
    liq_pulp_amount = liq_pulp_flow.attributes['mass_flow_rate']
    water_amount = liq_pulp_amount*coeff['Water/liquid pulp ratio']
    dry_white_liquor_amount = liq_pulp_amount*liq_pulp_flow.attributes['composition'][liq_pulp_flow.attributes['components'].index('Dry white liquor')]
    dry_wood = liq_pulp_amount*liq_pulp_flow.attributes['composition'][liq_pulp_flow.attributes['components'].index('Dry pulp')]
    # print('Dry wood' + str(dry_wood))
    dry_pulp = dry_wood * recovery_yield
    # print('Dry pulp' + str(dry_pulp))
    digested_wood = dry_wood - dry_pulp
    electricity_amount = liq_pulp_amount*coeff['Electricity per t of pulp']/1000.
    water_cp = 4.18
    total_mass = water_amount + liq_pulp_amount
    total_dry_mass = dry_white_liquor_amount + dry_wood
    washed_pulp_ratio = dry_pulp/total_dry_mass
    washed_pulp_amount = total_mass * washed_pulp_ratio
    black_liquor_amount = total_mass - washed_pulp_amount
    total_heat = liq_pulp_flow.attributes['heat_flow_rate']
    washed_pulp_heat = total_heat*washed_pulp_ratio
    black_liquor_heat = total_heat - washed_pulp_heat
    washed_pulp_temp = amb_t + (washed_pulp_heat/(water_cp*washed_pulp_amount))
    black_liquor_temp = washed_pulp_temp
    water_ratio_inbl = (black_liquor_amount - digested_wood - dry_white_liquor_amount)/black_liquor_amount
    
    return [{'name' : 'Electricity (Washing filters)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Black liquor', 'components' : ['Chemicals', 'Digested wood', 'Water'], 'composition': [dry_white_liquor_amount/black_liquor_amount, digested_wood/black_liquor_amount, water_ratio_inbl], 'mass_flow_rate' : black_liquor_amount,
                     'flow_type': 'Process flow', 'temperature' : black_liquor_temp, 'pressure':1 , 'heat_flow_rate' :black_liquor_heat ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Washed pulp', 'components' : ['Dry pulp', 'Water'], 'composition': [dry_pulp/washed_pulp_amount, 1 - (dry_pulp/washed_pulp_amount) ], 'mass_flow_rate' : washed_pulp_amount,
                     'flow_type': 'Process flow', 'temperature' : washed_pulp_temp, 'pressure':1 , 'heat_flow_rate' :washed_pulp_heat ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Water (Washing filters)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : water_amount,
                     'flow_type': 'Water', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False}]

Unit7.calculations = {'Liquid pulp' : Washingfiltersfunc_liqpulp}

#Unit 8 : Second screening step definition  
Unit8 = Unit('Screening 2')
Unit8.expected_flows_in = ['Washed pulp', 'Electricity(Screening 2)']
Unit8.expected_flows_out = ['Rejects (Screening 2)', 'Screened pulp']
Unit8.coefficients = {'Rejects ratio' : 0.02, 'Eletricity per t of pulp' : 10}

def Screening2func_washpulp(wash_pulp_flow, coeff):
    wash_pulp_in_amount = wash_pulp_flow.attributes['mass_flow_rate']
    dry_pulp_amount = wash_pulp_flow.attributes['composition'][wash_pulp_flow.attributes['components'].index('Dry pulp')]*wash_pulp_in_amount
    moisture_content = wash_pulp_flow.attributes['composition'][wash_pulp_flow.attributes['components'].index('Water')]
    dry_rejects_amount = coeff['Rejects ratio'] * dry_pulp_amount
    electricity_amount = wash_pulp_in_amount*coeff['Eletricity per t of pulp']/1000.
    rejects_amount = coeff['Rejects ratio']*wash_pulp_in_amount
    dry_rejects_ratio = dry_rejects_amount/rejects_amount
    screened_pulp_amount = wash_pulp_in_amount - rejects_amount
    temp = wash_pulp_flow.attributes['temperature']
    total_heat = wash_pulp_flow.attributes['heat_flow_rate']
    Q_rejects = coeff['Rejects ratio'] * total_heat
    Q_screened_pulp = (1. - coeff['Rejects ratio']) * total_heat
    combustion_energy_rejects = heat_bark * rejects_amount
    return [{'name' : 'Electricity(Screening 2)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Screened pulp', 'components' : ['Dry washed pulp', 'Water'], 'composition': [1-moisture_content, moisture_content], 'mass_flow_rate' : screened_pulp_amount,
                     'flow_type': 'Process flow', 'temperature' : temp, 'pressure':1 , 'heat_flow_rate' : Q_screened_pulp,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Rejects (Screening 2)', 'components' : ['Dry rejects', 'Water'], 'composition': [dry_rejects_ratio, 1 - dry_rejects_ratio ], 'mass_flow_rate' : rejects_amount,
                     'flow_type': 'Fuel (produced on-site)','combustion_energy_content' : combustion_energy_rejects , 'temperature' : temp, 'pressure':1 , 'heat_flow_rate' : Q_rejects ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False}]

Unit8.calculations = {'Washed pulp' :  Screening2func_washpulp}


#Unit 9 : Bleaching definition  
Unit9 = Unit('Bleaching')
Unit9.expected_flows_in = ['Screened pulp', 'Electricity(Bleaching)', 'Chemicals', 'Water (Bleaching)', 'Steam (Bleaching)']
Unit9.expected_flows_out = [ 'Bleached pulp', 'Contaminated water', 'Condensate (Bleaching)']

Unit9.coefficients = {'Bleaching cycles' : 3,  'Consistency' : 0.1, 'kg ClO2 per t pulp' : 15, 'kg O2 per t pulp' : 10, 'kg NaOH per t pulp' : 20, 'Bleaching rate' : 0.1, 'Electricity consumption per bleaching step and per t of pulp' : 3, 'Bleaching temperature' : 100, 'Losses' : 0.1}


def Bleachingfunc_screenpulp(screened_pulp_flow, coeff):
    s_pulp_amount = screened_pulp_flow.attributes['mass_flow_rate']
    dry_s_pulp_amount = s_pulp_amount * screened_pulp_flow.attributes['composition'][screened_pulp_flow.attributes['components'].index('Dry washed pulp')]
    bleached_material = dry_s_pulp_amount * coeff['Bleaching rate']
    dry_bleached_pulp = dry_s_pulp_amount - bleached_material
    clo2_amount = s_pulp_amount * coeff['kg ClO2 per t pulp']/1000
    o2_amount = s_pulp_amount * coeff['kg O2 per t pulp']/1000
    naoh_amount = s_pulp_amount * coeff['kg NaOH per t pulp']/1000
    bl_t = coeff['Bleaching temperature']
    steam_t = bl_t + 20
    loss = coeff['Losses']
    chem_amount = clo2_amount + o2_amount + naoh_amount
    moisture_amount_in = s_pulp_amount- dry_s_pulp_amount
    onestage_water_amount = ((dry_s_pulp_amount + chem_amount) / coeff['Consistency']) - moisture_amount_in
    added_water = (coeff['Bleaching cycles'] + 1) *onestage_water_amount
    bleached_pulp = moisture_amount_in + dry_bleached_pulp + onestage_water_amount
    contaminated_water_amount = chem_amount + (added_water - onestage_water_amount) + bleached_material
    electricity_amount = coeff['Electricity consumption per bleaching step and per t of pulp']*coeff['Bleaching cycles']*s_pulp_amount/1000.
    dry_pulp_ratio = dry_bleached_pulp/bleached_pulp
    Q_in = screened_pulp_flow.attributes['heat_flow_rate']
    water_cp = 4.18
    cp_dry_pulp = 2.3
    steam_hvap = 2256.4
    h_bleached = (dry_bleached_pulp + chem_amount) * cp_dry_pulp * (bl_t - amb_t) + (moisture_amount_in + onestage_water_amount) * water_cp * (bl_t - amb_t)
    washed_t = amb_t + (h_bleached/(((moisture_amount_in + 2*onestage_water_amount) * water_cp) +  ((dry_bleached_pulp + chem_amount) * cp_dry_pulp)))
    Q_bp = cp_dry_pulp * dry_bleached_pulp * (washed_t - amb_t) + water_cp * (moisture_amount_in + onestage_water_amount) * (washed_t - amb_t)
    Q_wastewater = cp_dry_pulp * (chem_amount + bleached_material) * (washed_t - amb_t) + coeff['Bleaching cycles'] * onestage_water_amount * (washed_t - amb_t)
    Q_req = Q_bp + Q_wastewater - Q_in
    Q_steam = Q_req/(1-coeff['Losses'])
    m_steam = Q_steam/steam_hvap
    Q_steam_in = m_steam * water_cp * (steam_t - amb_t) + Q_steam
    Q_cond = m_steam * water_cp * (steam_t - amb_t)
    Q_loss = Q_steam - Q_req
    
    return [{'name' : 'Electricity(Bleaching)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Bleached pulp', 'components' : ['Dry bleached pulp', 'Water'], 'composition': [dry_pulp_ratio, 1-dry_pulp_ratio], 'mass_flow_rate' : bleached_pulp,
                     'flow_type': 'Process flow', 'temperature' : washed_t, 'pressure':1 , 'heat_flow_rate' :Q_bp ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Steam (Bleaching)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                     'flow_type': 'Steam', 'temperature' : steam_t, 'pressure':2.7 , 'heat_flow_rate' :Q_steam_in,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Condensate (Bleaching)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                     'flow_type': 'Condensate', 'temperature' : steam_t, 'pressure':2.7 , 'heat_flow_rate' :Q_cond ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Contaminated water', 'components' : ['Chemicals', 'Bleaching rejects' ,'Water'], 'composition': [chem_amount/contaminated_water_amount, bleached_material/contaminated_water_amount, (contaminated_water_amount -chem_amount-bleached_material )/ contaminated_water_amount], 'mass_flow_rate' : contaminated_water_amount,
                     'flow_type': 'Waste water', 'temperature' : washed_t, 'pressure':1 , 'heat_flow_rate' :Q_wastewater ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Water (Bleaching)', 'components' : [ 'Water'], 'composition': [1], 'mass_flow_rate' : added_water,
                     'flow_type': 'Water', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Chemicals', 'components' : [ 'ClO2', 'O2', 'NaOH'], 'composition': [clo2_amount/chem_amount, o2_amount/chem_amount, naoh_amount/chem_amount], 'mass_flow_rate' : chem_amount,
                     'flow_type': 'Process', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss'  : Q_loss}
            ]

Unit9.calculations = {'Screened pulp' : Bleachingfunc_screenpulp}

#Unit 10 : Washing definition  
Unit10 = Unit('Washing 2')
Unit10.expected_flows_in = ['Bleached pulp', 'Electricity(Washing 2)', 'Water (Washing 2)']
Unit10.expected_flows_out = ['Washed pulp (Washing 2)', 'Wastewater (washing 2)']

Unit10.coefficients = {'Water/liquid pulp ratio' : 1. , 'Electricity per t of pulp' : 3}
def Washingtwofunc_bleachpulp(bleach_pulp_flow, coeff):
    bleach_pulp_amount = bleach_pulp_flow.attributes['mass_flow_rate']
    water_amount = bleach_pulp_amount*coeff['Water/liquid pulp ratio']
    dry_pulp_ratio = bleach_pulp_flow.attributes['composition'][bleach_pulp_flow.attributes['components'].index('Dry bleached pulp')]
    dry_pulp = bleach_pulp_amount * dry_pulp_ratio
    electricity_amount = bleach_pulp_amount*coeff['Electricity per t of pulp']/1000.
    water_cp = 4.18
    total_mass = water_amount + bleach_pulp_amount
    total_heat = bleach_pulp_flow.attributes['heat_flow_rate']
    t_out = amb_t + ( total_heat / (water_cp * total_mass))
    waste_water_heat = total_heat * (water_amount/total_mass)
    washed_pulp_heat = total_heat - waste_water_heat
    return [{'name' : 'Electricity(Washing 2)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Waste water (washing 2)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : water_amount,
                     'flow_type': 'Waste water', 'temperature' : t_out, 'pressure':1 , 'heat_flow_rate' :waste_water_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Washed pulp (Washing 2)', 'components' : ['Dry pulp', 'Water'], 'composition': [dry_pulp_ratio, 1 - dry_pulp_ratio ], 'mass_flow_rate' : bleach_pulp_amount,
                     'flow_type': 'Process flow', 'temperature' : t_out, 'pressure':1 , 'heat_flow_rate' :washed_pulp_heat ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Water (Washing 2)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : water_amount,
                     'flow_type': 'Process water', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False}]

Unit10.calculations = {'Bleached pulp' : Washingtwofunc_bleachpulp}

#Unit 11 : Drying press definition  
Unit11 = Unit('Drying press')
Unit11.expected_flows_in = ['Washed pulp (Washing 2)', 'Electricity(Drying press)']
Unit11.expected_flows_out = ['Dewatered pulp', 'Wastewater (Drying press)']
Unit11.coefficients = { 'Electricity (kWh) per ton of dry pulp' : 1.2, 'Moisture cutoff point' : 0.68, 'Moisture removal' : 0.1}

def Drypressfunc_washpulp(wash_pulp_flow, coeff):
    wash_pulp_amount = wash_pulp_flow.attributes['mass_flow_rate']
    dry_pulp_ratio = wash_pulp_flow.attributes['composition'][wash_pulp_flow.attributes['components'].index('Dry pulp')]
    water_in_amount = (1-dry_pulp_ratio) * wash_pulp_amount
    dry_pulp_amount = wash_pulp_amount * dry_pulp_ratio
    electricity_amount = dry_pulp_amount * coeff['Electricity (kWh) per ton of dry pulp']/1000.
    moisture_in = 1 - dry_pulp_ratio
    if moisture_in >= coeff['Moisture cutoff point'] + coeff['Moisture removal']:
        moisture_out = coeff['Moisture cutoff point']
    else:
        moisture_out = moisture_in -  coeff['Moisture removal']
    
    kept_moisture_mass = (moisture_out/(1-moisture_out))*dry_pulp_amount
    dewatered_pulp_amount = dry_pulp_amount + kept_moisture_mass
    water_amount_out = water_in_amount - kept_moisture_mass
    total_Q = wash_pulp_flow.attributes['heat_flow_rate']
    dewatered_pulp_heat = total_Q * (dewatered_pulp_amount/wash_pulp_amount)
    water_out_heat = total_Q - dewatered_pulp_heat
    t_in = wash_pulp_flow.attributes['temperature']
    t_out = t_in
    return [{'name' : 'Electricity(Drying press)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Wastewater (Drying press)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : water_amount_out,
                     'flow_type': 'Waste water', 'temperature' : t_out, 'pressure':1 , 'heat_flow_rate' :water_out_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Dewatered pulp', 'components' : ['Dry pulp', 'Water'], 'composition': [1 - moisture_out, moisture_out], 'mass_flow_rate' : dewatered_pulp_amount,
                     'flow_type': 'Process flow', 'temperature' : t_out, 'pressure':1 , 'heat_flow_rate' :dewatered_pulp_heat ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False}
            ]

Unit11.calculations = {'Washed pulp (Washing 2)' : Drypressfunc_washpulp}

#Unit 12 : Dryer definition  
Unit12 = Unit('Dryer')
Unit12.expected_flows_in = ['Dewatered pulp', 'Electricity(Dryer)', 'Air (Dryer)', 'Steam (Dryer)']
Unit12.expected_flows_out = ['Dried pulp', 'Exhaust gas', 'Condensate (Dryer)']

Unit12.coefficients = {'Dry pulp moisture' : 0.1 , 'Air temperature' : 80, 'd_T' : 5, 'Exhaust gas temperature' : 60, 'Electricity (kWh) per ton of pulp' : 155, 'Pulp temperature' : 45, 'Loss' : 0.1}


def Dryerfunc_dewatpulp(dewat_pulp_flow, coeff):
    steam_t = coeff['Air temperature'] + coeff['d_T']
    dewat_pulp_amount = dewat_pulp_flow.attributes['mass_flow_rate']
    dry_pulp_amount = dewat_pulp_amount * dewat_pulp_flow.attributes['composition'][dewat_pulp_flow.attributes['components'].index('Water')]
    water_amount = dewat_pulp_amount - dry_pulp_amount
    mu_m_out = coeff['Dry pulp moisture']
    mass_m_out = (mu_m_out/(1-mu_m_out)) * dry_pulp_amount
    pulp_out_amount = dry_pulp_amount + mass_m_out
    water_out_amount =  water_amount - mass_m_out
    air_t = coeff['Air temperature']
    exhaust_t = coeff['Exhaust gas temperature']
    electricity_amount = coeff['Electricity (kWh) per ton of pulp'] * dewat_pulp_amount / 1000.
    t_in = dewat_pulp_flow.attributes['temperature']
    dried_pulp_out_t = coeff['Pulp temperature']
    cp_water = 4.2 #kJ/kg.K
    cp_steam = 1.89 #kJ/kg.K
    cp_dry_pulp = 2.3
    cp_air = 1.01
    vap_heat = 2256.4 #kJ/kg
    Q_water_out = water_out_amount * ((cp_steam * (exhaust_t - 100)) + (cp_water *(100 - t_in ) ) + vap_heat)
    Q_dry_pulp_out = dry_pulp_amount * cp_dry_pulp * (dried_pulp_out_t - t_in)
    Q_moisture_out = mass_m_out * cp_water * (dried_pulp_out_t - t_in)
    Q_dried_pulp = Q_dry_pulp_out + Q_moisture_out
    m_air = (Q_water_out + Q_dried_pulp)/(cp_air * (air_t - exhaust_t))
    Q_air_out = m_air * cp_air * (exhaust_t - amb_t)
    Q_air_in = m_air * cp_air * (air_t - amb_t)
    Q_in = dewat_pulp_flow.attributes['heat_flow_rate']
    Q_exhaust = Q_air_out + Q_water_out
    Q_steam = ((Q_exhaust + Q_dried_pulp)/(1 - coeff['Loss'])) - Q_in
    Q_loss = Q_steam * coeff['Loss']
    vap_heat_130 = 2173.7
    m_steam = Q_steam/vap_heat_130
    cp_wat_130 = 4.26
    Q_condensate = m_steam * cp_wat_130 * (steam_t - amb_t)
    Q_steam_in = Q_steam + Q_condensate
    exhaust_amount = water_out_amount + m_air
    exhaust_gas_water_ratio = water_out_amount/exhaust_amount
    
    return [{'name' : 'Electricity(Dryer)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Air (Dryer)', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : m_air,
                     'flow_type': 'Air', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Condensate (Dryer)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                     'flow_type': 'Condensate', 'temperature' : steam_t, 'pressure':2.7 , 'heat_flow_rate' :Q_condensate ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Steam (Dryer)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                     'flow_type': 'Steam', 'temperature' : steam_t, 'pressure':2.7 , 'heat_flow_rate' :Q_steam_in ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Exhaust gas', 'components' : ['Water', 'Air'], 'composition': [exhaust_gas_water_ratio, 1 - exhaust_gas_water_ratio], 'mass_flow_rate' : exhaust_amount,
                     'flow_type': 'Exhaust', 'temperature' : exhaust_t, 'pressure':1 , 'heat_flow_rate' :Q_exhaust ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Dried pulp', 'components' : ['Dry pulp', 'Water'], 'composition': [1 - mu_m_out, mu_m_out], 'mass_flow_rate' : pulp_out_amount,
                     'flow_type': 'Product', 'temperature' : dried_pulp_out_t, 'pressure':1 , 'heat_flow_rate' :Q_dried_pulp ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss'  : Q_loss}
            
            ]

Unit12.calculations = {'Dewatered pulp' : Dryerfunc_dewatpulp}

#Unit 13 : Evaporator definition  
Unit13 = Unit('Evaporators')
Unit13.expected_flows_in = ['Black liquor', 'Steam (MEE)']
Unit13.expected_flows_out = ['Black liquor (strong)', 'Condensate (MLE, process)', 'Condensate (MLE, utility)']
Unit13.coefficients = {'Steam economy' : 2.5, 'Moisture out' : 0.2, 'Temperature steam in' : 120, 'Temperature liquor out' : 55, 'Latent heat of vaporization at given T' : 2202.1, 'Water Cp' : 4.2}

def Evaporatorfunc_blckliq(black_liquor_flow, coeff):
    moisture_in = black_liquor_flow.attributes['composition'][black_liquor_flow.attributes['components'].index('Water')]
    black_liquor_in_amount = black_liquor_flow.attributes['mass_flow_rate']
    moisture_in_amount = black_liquor_in_amount * moisture_in
    solids_amount = black_liquor_in_amount - moisture_in_amount
    digested_wood_ratio = black_liquor_flow.attributes['composition'][black_liquor_flow.attributes['components'].index('Digested wood')]
    digested_wood_amount = digested_wood_ratio * black_liquor_in_amount
    chem_amount = black_liquor_in_amount - moisture_in_amount - digested_wood_amount
    mu_out = coeff['Moisture out']
    moisture_out_amount = (mu_out/(1 - mu_out))*solids_amount
    water_out_amount = moisture_in_amount - moisture_out_amount
    steam_in_amount = water_out_amount/coeff['Steam economy']
    Q_black_liquor = black_liquor_flow.attributes['heat_flow_rate']
    steam_in_t = coeff['Temperature steam in']
    Q_steam_in = steam_in_amount * (((coeff['Water Cp']) * (steam_in_t - amb_t)) + coeff['Latent heat of vaporization at given T'])
    Q_cond_out = steam_in_amount * (((coeff['Water Cp']) * (steam_in_t - amb_t)))
    cp_solids = 1.34
    cp_water = coeff['Water Cp']
    liq_t = coeff['Temperature liquor out']
    strg_liq_out_amount = solids_amount + moisture_out_amount
    Q_liq_out = ((cp_solids * solids_amount) + (cp_water * moisture_out_amount)) * (liq_t - amb_t)
    Q_total_in = Q_steam_in + Q_black_liquor
    delta_T_vap = (((Q_steam_in - Q_cond_out) + (Q_black_liquor - Q_liq_out)))/(water_out_amount * cp_water)
    t_vap_out = amb_t+ delta_T_vap
    Q_condensate = Q_total_in - Q_cond_out - Q_liq_out
    dig_wood_out_ratio = digested_wood_amount/strg_liq_out_amount
    chem_out_ratio = chem_amount/strg_liq_out_amount
    return [{'name' : 'Steam (MEE)', 'components' : ['Water'], 'mass_flow_rate' : steam_in_amount,
             'flow_type': 'Steam', 'temperature' : steam_in_t,  'In or out' : 'In', 'heat_flow_rate' : Q_steam_in ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Black liquor (strong)', 'components' : ['Digested wood', 'Chemicals', 'Water'], 'composition': [dig_wood_out_ratio, chem_out_ratio , mu_out], 'mass_flow_rate' : strg_liq_out_amount,
                     'flow_type': 'Process', 'temperature' : liq_t, 'pressure':1 , 'heat_flow_rate' :Q_liq_out ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Condensate (MLE, process)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : water_out_amount,
                     'flow_type': 'Wastewater', 'temperature' : t_vap_out, 'pressure':1 , 'heat_flow_rate' :Q_condensate ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Condensate (MLE, utility)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : steam_in_amount,
                     'flow_type': 'Condensate', 'temperature' : steam_in_t, 'pressure':1.98 , 'heat_flow_rate' :Q_cond_out ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
           ]

Unit13.calculations = {'Black liquor' : Evaporatorfunc_blckliq}

#Unit 14 : Recovery boiler definition  
Unit14 = Unit('Recovery boiler')
Unit14.expected_flows_in = ['Black liquor (strong)', 'Air (Recovery boiler)', 'Water (Recovery boiler)']
Unit14.expected_flows_out = ['Smelt', 'Stack (Recovery boiler)', 'Steam (from Recovery boiler)']
Unit14.coefficients = {'Black liquor energy content (MJ/kg dry solids)' : 15, 'Carbon in solids' : 0.35, 'Outlet steam temperature' : 300, 'Efficiency' : 0.9}

def Recoveryboilerfunc_strblkliq(str_blk_liq_flow, coeff):
    str_blk_liq_amount = str_blk_liq_flow.attributes['mass_flow_rate']
    blk_liq_moisture = str_blk_liq_flow.attributes['composition'][str_blk_liq_flow.attributes['components'].index('Water')]
    blk_liq_water_amount = blk_liq_moisture*str_blk_liq_amount
    blk_liq_t = str_blk_liq_flow.attributes['temperature']
    blk_liq_dry_amount = str_blk_liq_amount - blk_liq_water_amount
    chem_amount = str_blk_liq_amount * str_blk_liq_flow.attributes['composition'][str_blk_liq_flow.attributes['components'].index('Chemicals')]
    Q_in = str_blk_liq_flow.attributes['heat_flow_rate']
    Q_combustion = blk_liq_dry_amount * coeff['Black liquor energy content (MJ/kg dry solids)'] * 1000
#    print('Q combustion = ' + str(Q_combustion) + ' kJ')
    NaOH_in = (NaOH_in_WL/(NaOH_in_WL + Na2S_in_WL))*chem_amount
    Na2S_in = chem_amount - NaOH_in
    Na2CO3_out = (106./80.) * NaOH_in
    t_steam_out = coeff['Outlet steam temperature']
    t_smelt_out = 800
    t_stack = coeff['Outlet steam temperature']
    smelt_amount = Na2CO3_out + Na2S_in
    smelt_cp = 1.1
    Q_smelt = smelt_amount*smelt_cp*(t_smelt_out - amb_t)
    carbon_in = blk_liq_dry_amount * coeff['Carbon in solids']
    oxygen_required = carbon_in * (32./12.)
    co2_amount = carbon_in * (44./12.)
    air_in_amount = oxygen_required/0.21
    vap_material = str_blk_liq_amount - smelt_amount
    vaporized_water_in_stack_amount = blk_liq_water_amount
    water_cp = 4.2
    water_vap_h = 2200
    steam_cp = 1.8
    air_cp = 1.05
    stack_amount = air_in_amount + str_blk_liq_amount - smelt_amount
    Q_water_stack = vaporized_water_in_stack_amount * ((water_cp*(100-blk_liq_t)) + water_vap_h + (steam_cp * (t_stack - 100) ))
    Q_air_stack = air_cp * (t_stack - amb_t)
    Q_stack = Q_water_stack + Q_air_stack
    Q_loss = (1- coeff['Efficiency']) * Q_combustion
    Q_steam = Q_in + Q_combustion - Q_smelt - Q_stack - Q_loss
    m_steam = Q_steam/((water_cp * (100 -amb_t)) + water_vap_h + (steam_cp * (coeff['Outlet steam temperature'] - 100)))
    return [{'name' : 'Water (Recovery boiler)', 'components' : ['Water'], 'mass_flow_rate' : m_steam,
             'flow_type': 'Water', 'temperature' : amb_t,  'In or out' : 'In', 'heat_flow_rate' : 0 ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Smelt', 'components' : ['Na2S', 'Na2CO3'], 'composition': [Na2S_in/smelt_amount, Na2CO3_out/smelt_amount], 'mass_flow_rate' : smelt_amount,
                     'flow_type': 'Process', 'temperature' : t_smelt_out, 'pressure':1 , 'heat_flow_rate' :Q_smelt ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Stack (Recovery boiler)', 'components' : ['Water', 'Combustion products', 'Air'], 'composition': [blk_liq_water_amount/stack_amount, (stack_amount-blk_liq_water_amount-air_in_amount)/stack_amount ,air_in_amount/stack_amount], 'mass_flow_rate' :stack_amount,
                     'flow_type': 'Exhaust', 'temperature' : t_stack, 'pressure':1 , 'heat_flow_rate' : Q_stack ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Steam (from Recovery boiler)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                     'flow_type': 'Steam (produced on-site)', 'temperature' : t_steam_out, 'pressure':86 , 'heat_flow_rate' :Q_steam ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Air (Recovery boiler)', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : air_in_amount,
                    'flow_type': 'Air', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss' : Q_loss},
            {'Heat of reaction' : Q_combustion},
            {'Emissions' : {'CO2' : co2_amount}}
            ]
Unit14.calculations = {'Black liquor (strong)' : Recoveryboilerfunc_strblkliq}

#Unit 15 : Smelt dissolver definition  
Unit15 = Unit('Smelt dissolver')
Unit15.expected_flows_in = ['Smelt', 'Water (Smelt dissolver)']
Unit15.expected_flows_out = ['Green liquor']
green_liq_moist = 0.95
Unit15.coefficients = {'Green liquor moisture' : green_liq_moist}

def smeltdissolverfunc_smelt(smelt_flow, coeff):
    smelt_amount = smelt_flow.attributes['mass_flow_rate']
    green_liq_moist = coeff['Green liquor moisture']
    water_in_green_liq = smelt_amount * (green_liq_moist/(1 - green_liq_moist))
    water_in_amount = water_in_green_liq
    Q_smelt = smelt_flow.attributes['heat_flow_rate']
    water_cp = 4.2
    smelt_cp = 1.1
    Na2S_ratio = smelt_flow.attributes['composition'][smelt_flow.attributes['components'].index('Na2S')]
    Na2S_amount = Na2S_ratio * smelt_amount
    Na2CO3_ratio = smelt_flow.attributes['composition'][smelt_flow.attributes['components'].index('Na2CO3')]
    Na2CO3_amount = Na2CO3_ratio * smelt_amount
    gl_amount = smelt_amount + water_in_amount
    Na2S_in_gl = Na2S_amount/gl_amount
    Na2CO3_in_gl = Na2CO3_amount/gl_amount
    total_Q = Q_smelt
    gl_t = amb_t + (total_Q/((smelt_amount * smelt_cp) + (water_in_green_liq * water_cp)))
    return [{'name' : 'Water (Smelt dissolver)', 'components' : ['Water'], 'mass_flow_rate' : water_in_amount,
             'flow_type': 'Water', 'temperature' : amb_t,  'In or out' : 'In', 'heat_flow_rate' : 0 ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Green liquor', 'components' : ['Water', 'Na2S', 'Na2CO3'], 'composition': [1 - (Na2S_in_gl + Na2CO3_in_gl), Na2S_in_gl, Na2CO3_in_gl], 'mass_flow_rate' :gl_amount,
                     'flow_type': 'Process', 'temperature' : gl_t, 'pressure':1 , 'heat_flow_rate' : total_Q ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            ]

Unit15.calculations = {'Smelt' : smeltdissolverfunc_smelt}
#Unit 16 : Slaking/causticizing definition  
Unit16 = Unit('Slaking/causticizing')
Unit16.expected_flows_in = ['Green liquor', 'Quicklime']
Unit16.expected_flows_out = ['White liquor and mud']
Unit16.coefficients = {'Reaction losses' : 0.4}

def slakingfunc_greenliq(gl_flow, coeff):
    gl_amount = gl_flow.attributes['mass_flow_rate']
    Q_gl = gl_flow.attributes['heat_flow_rate']
    quicklime_amount = CaO_req/1000.
    quicklime_t = cao_t
    ql_cp = cao_kgcp
    Q_ql = ((quicklime_t - amb_t) * quicklime_amount)/ql_cp
    na2s_amount = gl_amount * gl_flow.attributes['composition'][gl_flow.attributes['components'].index('Na2S')]
    na2co3_amount = gl_amount * gl_flow.attributes['composition'][gl_flow.attributes['components'].index('Na2CO3')]
    naoh_amount = (80./100.) * na2co3_amount
    caco3_amount = quicklime_amount * (100./56.)
    Q_reaction = 1000 * CaO_hyd_hor_g * quicklime_amount #kJ
    Q_loss = coeff['Reaction losses'] * Q_reaction
    Q_wl_mud = Q_ql + Q_gl + Q_reaction - Q_loss
    wl_mud_amount = gl_amount + quicklime_amount
    caco3_ratio = caco3_amount/wl_mud_amount
    na2s_ratio = na2s_amount/wl_mud_amount
    naoh_ratio = naoh_amount/wl_mud_amount
    dry_solids_cp = 1.1
    water_cp = 4.2
    water_ratio = 1 - naoh_ratio - caco3_ratio - na2s_ratio
    dry_solids_amount = (naoh_ratio + caco3_ratio + na2s_ratio) * wl_mud_amount
    water_amount = water_ratio * wl_mud_amount
    t_wl_mud = amb_t + (Q_wl_mud/((dry_solids_amount * dry_solids_cp) + (water_cp *water_amount )))
    return [{'name' : 'Quicklime', 'components' : ['CaO'], 'composition' : [1] , 'mass_flow_rate' : quicklime_amount,
             'flow_type': 'Shear', 'temperature' : quicklime_t,  'In or out' : 'In', 'heat_flow_rate' : Q_ql ,  'Set calc' : False, 'Set shear' : True},     
            {'name' : 'White liquor and mud', 'components' : ['Water', 'NaOH', 'CaCO3', 'Na2S'], 'composition': [water_ratio, naoh_ratio,caco3_ratio, na2s_ratio ], 'mass_flow_rate' : wl_mud_amount,
                     'flow_type': 'Process', 'temperature' : t_wl_mud, 'pressure':1 , 'heat_flow_rate' : Q_wl_mud ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'Heat loss' : Q_loss},
            {'Heat of reaction' : Q_reaction}
            ]

Unit16.calculations = {'Green liquor' :  slakingfunc_greenliq  }

#Unit 17 : Lime mud filter definition  
Unit17 = Unit('Lime mud filter')
Unit17.expected_flows_in = ['White liquor and mud', 'Electricity (Lime mud filter)']
Unit17.expected_flows_out = ['Lime mud', 'White liquor']
Unit17.coefficients = {'Electricity per t of white liquor and mud' : 3 } #kWh per t of white liquor and mud
def lmfiltfunc_wlnm(wlm_flow, coeff):
    wlnm_amount = wlm_flow.attributes['mass_flow_rate']
    caco3_amount = wlnm_amount * wlm_flow.attributes['composition'][wlm_flow.attributes['components'].index('CaCO3')]
    naoh_amount = wlnm_amount * wlm_flow.attributes['composition'][wlm_flow.attributes['components'].index('NaOH')]
    na2s_amount = wlnm_amount * wlm_flow.attributes['composition'][wlm_flow.attributes['components'].index('Na2S')]
    water_amount = wlnm_amount - (caco3_amount + naoh_amount + na2s_amount)
    wl_amount = abs_wl_amount
    electricity_amount = wlnm_amount * coeff['Electricity per t of white liquor and mud']/1000.
    out_t = white_liquor_t
    waterinwl_amount = abs_wl_amount - naoh_amount - na2s_amount
    white_liquor_cp = 3.411 #kJ/kg.K
    Q_wl = wl_amount * white_liquor_cp * (out_t - amb_t)
    waterinmud = water_amount - waterinwl_amount
    caco3_cp = 0.9 #kJ/kg.K
    water_cp = 4.2 #kJ/kg.K
    mud_amount = caco3_amount + waterinmud
    Q_mud = (waterinmud * water_cp * (out_t - amb_t)) + (caco3_amount * caco3_cp * (out_t - amb_t))
    Q_loss = wlm_flow.attributes['heat_flow_rate'] - Q_mud - Q_wl
    
    return [{'name' : 'Electricity (Lime mud filter)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Lime mud', 'components' : ['CaCO3', 'Water'], 'composition': [caco3_amount/mud_amount ,waterinmud/mud_amount ], 'mass_flow_rate' : mud_amount,
                     'flow_type': 'Process flow', 'temperature' : out_t, 'pressure':1 , 'heat_flow_rate' : Q_mud ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'White liquor', 'components' : ['Na2S', 'NaOH' ,'Water'], 'composition': [na2s_amount/wl_amount , naoh_amount/wl_amount , waterinwl_amount/wl_amount ], 'mass_flow_rate' : wl_amount,
                     'flow_type': 'Shear stream', 'temperature' : out_t, 'pressure':1 , 'heat_flow_rate' :Q_wl ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : True},
            {'Heat loss' : Q_loss}]

Unit17.calculations = {'White liquor and mud' :  lmfiltfunc_wlnm  }
#Unit 18 : Lime mud wash definition  
Unit18 = Unit('Lime mud wash')
Unit18.expected_flows_in = ['Water (Lime mud wash)', 'Lime mud', 'Electricity (Lime mud wash)']
Unit18.expected_flows_out = ['Washed lime mud', 'Wastewater (Lime mud wash)']
Unit18.coefficients = {'Electricity per ton of lime mud' : 3, 'Washing ratio' : 1, 'Moisture in lime mud' : 0.3}

def lmwfunc_limemud(lm_flow, coeff):
    
    lime_mud_amount = lm_flow.attributes['mass_flow_rate']
    mu_in = lm_flow.attributes['composition'][lm_flow.attributes['components'].index('Water')]
    mu_out = coeff['Moisture in lime mud']
    caco3_amount = lime_mud_amount * lm_flow.attributes['composition'][lm_flow.attributes['components'].index('CaCO3')]
    moist_in = lime_mud_amount * mu_in 
    Q_limemud = lm_flow.attributes['heat_flow_rate']
    electricity_amount = lime_mud_amount * coeff['Electricity per ton of lime mud'] / 1000.
    caco3_cp = 0.9 #kJ/kg.K
    water_cp = 4.2 #kJ/kg.K
    m_water_in = lime_mud_amount * coeff['Washing ratio']
    m_washed_lm = caco3_amount/(1 - mu_out)
    total_water = moist_in + m_water_in
    water_in_washed_pulp = m_washed_lm - caco3_amount
    wwout = total_water - water_in_washed_pulp
    t_out = amb_t + Q_limemud/((total_water*water_cp) + (caco3_amount*caco3_cp))
    Q_wlm = (t_out - amb_t) *  ((water_cp * water_in_washed_pulp) + (caco3_cp * caco3_amount))
    Q_waste_water = Q_limemud - Q_wlm
    return [{'name' : 'Electricity (Lime mud wash)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Washed lime mud', 'components' : ['CaCO3', 'Water'], 'composition': [caco3_amount/m_washed_lm , (m_washed_lm - caco3_amount)/m_washed_lm ], 'mass_flow_rate' : m_washed_lm,
                     'flow_type': 'Process flow', 'temperature' : t_out, 'pressure':1 , 'heat_flow_rate' : Q_wlm ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Wastewater (Lime mud wash)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : wwout,
                     'flow_type': 'Wastewater', 'temperature' : t_out, 'pressure':1 , 'heat_flow_rate' :Q_waste_water ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Water (Lime mud wash)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_water_in,
                     'flow_type': 'Water', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False}]

Unit18.calculations = {'Lime mud' :  lmwfunc_limemud  }
#Unit 19 : Lime kiln definition  
Unit19 = Unit('Lime kiln')
Unit19.expected_flows_in = ['Air (Lime kiln)', 'Washed lime mud', 'Natural gas (Lime kiln)', 'Electricity (Lime kiln)']
Unit19.expected_flows_out = ['Quicklime', 'Exhaust (Lime kiln)']
Unit19.coefficients = {'Electricity per ton of CaCO3' : 60, 'Quicklime temperature' : cao_t, 'Stack temperature' : 400, 'Air ratio' : 1, 'Losses' : 0.1} 

def limekiln_wlm(wlm_flow, coeff):
    wlm_amount = wlm_flow.attributes['mass_flow_rate']
    wlm_t = wlm_flow.attributes['temperature']
    Q_wlm = wlm_flow.attributes['heat_flow_rate']
    stack_t = coeff['Stack temperature']
    caco3_amount = wlm_amount * wlm_flow.attributes['composition'][wlm_flow.attributes['components'].index('CaCO3')]/1000.
    water_in_amount = wlm_amount * wlm_flow.attributes['composition'][wlm_flow.attributes['components'].index('Water')]
    electricity_amount = caco3_amount * coeff['Electricity per ton of CaCO3'] / 1000.
    Q_calcination = caco3_amount * lime_calcination_hmass
    cp_CaO = cao_kgcp
    cao_amount = caco3_amount * (56./100.)
    CaO_t = coeff['Quicklime temperature']
    Q_cao = cao_amount * cp_CaO * (CaO_t - amb_t)
    cp_water = 4.2
    water_vap_h = 2200
    cp_steam = 2.
    cp_air = 1.05
    Q_water = water_in_amount * ((cp_water * (100 - wlm_t)) + water_vap_h + (cp_steam * (stack_t - 100)))
    m_air = coeff['Air ratio'] * wlm_amount
    co2_emissions = (44./100.) * caco3_amount
    Q_air_stack = cp_air * (m_air + co2_emissions) * (stack_t - amb_t)
    m_stack = m_air + co2_emissions + water_in_amount
    Q_stack = Q_air_stack + Q_water
    Q_fuel = (Q_stack + Q_cao + Q_calcination - Q_wlm)/( 1 - coeff['Losses'])
    Q_losses = coeff['Losses'] * Q_fuel
    print(Q_wlm)
    print(Q_calcination)
    print(Q_cao)
    print(Q_water)
    print(Q_air_stack)
    print(Q_stack)
    print(Q_fuel)
    print(Q_losses)
    return [{'name' : 'Electricity (Lime kiln)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Natural gas (Lime kiln)', 'components' : ['CH4'], 'composition': [1], 'mass_flow_rate' : 0,
                     'flow_type': 'Fuel', 'temperature' : amb_t, 'pressure': 1 , 'heat_flow_rate' : 0, 'combustion_energy_content' : Q_fuel ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Air (Lime kiln)', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : m_air,
                     'flow_type': 'Air', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Quicklime', 'components' : ['CaO'], 'composition': [1], 'mass_flow_rate' : cao_amount,
                     'flow_type': 'Process flow', 'temperature' : CaO_t, 'pressure':1 , 'heat_flow_rate' :Q_cao ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : True},
            {'name' : 'Exhaust (Lime kiln)', 'components' : ['Air', 'CO2'], 'composition': [(m_stack - co2_emissions)/m_stack, co2_emissions/m_stack], 'mass_flow_rate' : m_stack,
                     'flow_type': 'Exhaust', 'temperature' : stack_t, 'pressure':1 , 'heat_flow_rate' :Q_stack ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'Heat of reaction' : (Q_fuel - Q_calcination)},
            {'Heat loss' : Q_losses}
            
            ]
Unit19.calculations = {'Washed lime mud' :  limekiln_wlm  }
# Unit2.attach_available_flow()
# Unit2.calc()
# Unit3.attach_available_flow()
# Unit3.calc()
# def all_units_calc(unit_list):
#     return all(unit.is_calc for unit in unit_list)



# calculate_flow_sheet = True

# if calculate_flow_sheet:
#     calced = False
#     while not calced:
#         print(calced)
#         for unit in processunits:
#             print(unit.name)
#             unit.attach_available_flow()
#             if unit.is_fully_calc():
#                 pass
            
#             elif len(unit.input_flows) != 0 or len(unit.output_flows) != 0:
#                 print('Getting calced')
#                 unit.calc()
#                 unit.is_calc = True
#         calced = all_units_calc(processunits)




# FlowA.set_destination(Unit16)
# FlowA.set_calc_flow()

# Unit16.calc()


# for flow in allflows:
#     print(flow.attributes)

# print(len(allflows))
processunits = [Unit1,Unit2,Unit3,Unit4,Unit5,Unit6,Unit7,Unit8,Unit9,Unit10,
                Unit11,Unit12,Unit13,Unit14,Unit15,Unit16,Unit17,Unit18,Unit19]



FlowA = Flow('Logs',['Water', 'Bark', 'Wood'],'input', amb_t, 1, [wood_moisture, normal_bark_in, 1 - wood_moisture - normal_bark_in ], None , None, wood_flow_amount, np.nan, 0)
FlowA.set_calc_flow()
allflows.append(FlowA)

#main(allflows, processunits, f_print = True)
# print(are_units_calced(processunits))

# for unit in processunits:
#     unit.is_calc = True

# print(are_units_calced(processunits))

#print_flows(allflows)
#for unit in processunits:
#    unit.check_heat_balance(allflows)
#    unit.check_mass_balance(allflows)


