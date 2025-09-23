# -*- coding: utf-8 -*-
"""
Created on Wed, May 28, 2025

@author: Jason Ye
"""

#Import the required modules
import pandas as pd
import numpy as np
import csv
import inspect
from archetypes_base import *
from scipy.optimize import fsolve

#Initialize the list of flow streams and the list of process units
allflows = []
processunits = []

###Define the global variables

#Energy balance
Cp_water_liquid = 4.184 #kJ/(kg*K)
H_v_water = 2256.159822 #kJ/kg, at 100 C
Cp_sucrose = 1.242 #kJ/(kg*K)
Cp_pulp = 3.3755 #kJ/(kg*K)
T_ref = 25 #C
Cp_CaO2H2 = 1.207941371 #kJ/(kg*K)
T_water_boiling = 100 #C

#Beet inlet stream
inlet_soil = 36.3*114.155
inlet_stone = 4.8*114.155
inlet_organic_matter = 4.5*114.155
inlet_water_in_beet = 284.85*114.155
inlet_sucrose = 79.758*114.155
inlet_pulp = 15.192*114.155
inlet_beet_mixture = np.sum([inlet_soil,inlet_stone,inlet_organic_matter,inlet_water_in_beet,inlet_sucrose,inlet_pulp])

#Molar masses
M_CaO = 56.0774
M_CaCO3 = 100.0869
M_CO2 = 44.009
M_water = 18.02
M_air = 29
M_SO2 = 64.066
M_CaO2H2 = 74.093

#Liming
mol_calcin = inlet_beet_mixture*0.021/M_CaO #mol of CaO, CaCO3, etc.
CaCO3_inlet = mol_calcin*M_CaCO3 #inlet mass rate of limestone into lime kiln

def Cp_dT_steam(T): #in kJ/kg, and start integrating from 100 C
    Cp_dT = (0.03346*T + (0.6880*10**(-5))/2*T**2 + (0.7604*10**(-8))/3*T**3 - (3.593*10**(-12))/4*T**4 - 3.382844842)*1000/M_water
    return Cp_dT

def Cp_dT_CO2(T): #in kJ/kg, and start integrating from 25 C
    Cp_dT = (0.03611*T + 4.233*10**(-5)/2*T**2 - 2.887*10**(-8)/3*T**3 + 7.464*10**(-12)/4*T**4 - 0.9158284893)*1000/M_CO2
    return Cp_dT

def Cp_dT_air(T): #in kJ/kg, and start integrating from 25 C
    Cp_dT = (0.02894*T + 0.4147*10**(-5)/2*T**2 + (0.3191*10**(-8))/3*T**3 - 1.965*10**(-12)/4*T**4 - 0.7248123654)*1000/M_air
    return Cp_dT

def Cp_dT_SO2(T): #in kJ/kg, and start integrating from 25 C
    Cp_dT = (0.03891*T + (3.904*10**(-5))/2*T**2 - (3.104*10**(-8))/3*T**3 + (8.606*10**(-12))/4*T**4 - 0.9847891738)*1000/M_SO2
    return Cp_dT

def Cp_dT_CaO(T): #in kJ/kg, start integrating from 298.15K, and temperature must be in Kelvins
    Cp_dT = (0.04184*T + 2.03*10**(-5)/2*T**2 - 4.52*10**2/-1*T**(-1) - 14.89287967)*1000/M_CaO
    return Cp_dT

def Cp_dT_CaCO3(T): #in kJ/kg, start integrating from 298.15K, and temperature must be in Kelvins
    Cp_dT = (0.08234*T + 4.975*10**(-5)/2*T**2 + 12.87*10**2*T**(-1) - 31.07751404)*1000/M_CaCO3
    return Cp_dT

#Unit 1: Fluming and washing                 
Unit1 = Unit('Fluming washing')
Unit1.expected_flows_in = ['Beet', 'Water (fluming)', 'Electricity (fluming)']
Unit1.expected_flows_out = ['Pure beet', 'Wastewater']
Unit1.coefficients = {'kWh per t beet' : 1.172284, 'water to beet for wash' : 546.5/425.4}

def beet_fluming_washing(beet_flow, coeff):

    #Extract the required information on all inlet streams
    beet_amount = beet_flow.attributes['mass_flow_rate']
    electricity_amount = beet_amount * coeff['kWh per t beet']/1000
    wash_water = beet_amount * coeff['water to beet for wash']

    #Obtain the indices of all components in the inlet beet stream
    soil_index = beet_flow.attributes['components'].index('soil')
    stone_index = beet_flow.attributes['components'].index('stone')
    organic_matter_index = beet_flow.attributes['components'].index('organic matter')
    water_in_beet_index = beet_flow.attributes['components'].index('water')
    sucrose_index = beet_flow.attributes['components'].index('sucrose')
    pulp_index = beet_flow.attributes['components'].index('pulp')

    #Record the mass fractions of all components in the inlet beet stream
    soil_mass_fraction_inlet = beet_flow.attributes['composition'][soil_index]
    stone_mass_fraction_inlet = beet_flow.attributes['composition'][stone_index]
    organic_matter_mass_fraction_inlet = beet_flow.attributes['composition'][organic_matter_index]
    water_in_beet_mass_fraction_inlet = beet_flow.attributes['composition'][water_in_beet_index]
    sucrose_mass_fraction_inlet = beet_flow.attributes['composition'][sucrose_index]
    pulp_mass_fraction_inlet = beet_flow.attributes['composition'][pulp_index]

    #Compute the mass flow rates of all components in the pure beet outlet stream
    pure_beet_out_mass_flow = np.sum([water_in_beet_mass_fraction_inlet,sucrose_mass_fraction_inlet,pulp_mass_fraction_inlet])*beet_amount
    water_in_beet_out_mass_flow = water_in_beet_mass_fraction_inlet*beet_amount
    sucrose_out_mass_flow = sucrose_mass_fraction_inlet*beet_amount
    pulp_out_mass_flow = pulp_mass_fraction_inlet*beet_amount

    #Obtain the composition of the pure beet outlet stream
    water_in_beet_out_comp = water_in_beet_out_mass_flow/pure_beet_out_mass_flow
    sucrose_out_comp = sucrose_out_mass_flow/pure_beet_out_mass_flow
    pulp_out_comp = pulp_out_mass_flow/pure_beet_out_mass_flow

    #Compute the mass flow rates of all components in the wastewater outlet stream
    water_out_mass_flow = wash_water
    soil_out_mass_flow = soil_mass_fraction_inlet*beet_amount
    stone_out_mass_flow = stone_mass_fraction_inlet*beet_amount
    organic_matter_out_mass_flow = organic_matter_mass_fraction_inlet*beet_amount
    wastewater_out_mass_flow = np.sum([water_out_mass_flow, soil_out_mass_flow, stone_out_mass_flow, organic_matter_out_mass_flow])

    #Obtain the composition of the wastewater outlet stream
    water_out_comp = water_out_mass_flow/wastewater_out_mass_flow
    soil_out_comp = soil_out_mass_flow/wastewater_out_mass_flow
    stone_out_comp = stone_out_mass_flow/wastewater_out_mass_flow
    organic_matter_out_comp = organic_matter_out_mass_flow/wastewater_out_mass_flow

    return [{'name' : 'Electricity (fluming)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_amount, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Water (fluming)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : wash_water,
                     'flow_type': 'Water', 'temperature' : 25, 'pressure':1 , 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Pure beet', 'components' : ['water','sucrose','pulp'], 'composition' : [water_in_beet_out_comp,sucrose_out_comp,pulp_out_comp] , 'mass_flow_rate' : pure_beet_out_mass_flow,
                     'flow_type': 'Process flow', 'temperature' : 25, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Wastewater', 'components' : ['water','soil','stone','organic matter'], 'composition' : [water_out_comp,soil_out_comp,stone_out_comp,organic_matter_out_comp] , 'mass_flow_rate' : wastewater_out_mass_flow,
                     'flow_type': 'Waste water', 'temperature' : 25, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False}]                   

Unit1.calculations = {'Beet' : beet_fluming_washing}

#Unit 2: Slicing                 
Unit2 = Unit('Slicing')
Unit2.expected_flows_in = ['Pure beet','Electricity (slicing)']
Unit2.expected_flows_out = ['Cossette']
Unit2.coefficients = {'kWh per t pure beet' : 3.51686}

def beet_slicing(pure_beet_flow, coeff):

    #Extract the required information on all streams
    pure_beet_amount = pure_beet_flow.attributes['mass_flow_rate']
    electricity_amount = pure_beet_amount * coeff['kWh per t pure beet']/1000

    #Obtain the indices of all components in the inlet pure beet stream
    water_in_beet_index = pure_beet_flow.attributes['components'].index('water')
    sucrose_index = pure_beet_flow.attributes['components'].index('sucrose')
    pulp_index = pure_beet_flow.attributes['components'].index('pulp')

    #Record the mass fractions of all components in the inlet pure beet stream
    water_in_beet_mass_fraction_inlet = pure_beet_flow.attributes['composition'][water_in_beet_index]
    sucrose_mass_fraction_inlet = pure_beet_flow.attributes['composition'][sucrose_index]
    pulp_mass_fraction_inlet = pure_beet_flow.attributes['composition'][pulp_index]   

    return [{'name' : 'Electricity (slicing)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_amount, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Cossette', 'components' : ['water','sucrose','pulp'], 'composition': [water_in_beet_mass_fraction_inlet,sucrose_mass_fraction_inlet,pulp_mass_fraction_inlet], 'mass_flow_rate' : pure_beet_amount,
                     'flow_type': 'Process flow', 'temperature' : 25, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False}]                

Unit2.calculations = {'Pure beet' : beet_slicing}

#Unit 3: Diffusion/extraction                 
Unit3 = Unit('Diffuser')
Unit3.expected_flows_in = ['Cossette','Hot water (diffuser)','Electricity (diffuser)','Steam (diffuser)']
Unit3.expected_flows_out = ['Sugar beet pulp','Raw juice','Steam condensate (from diffuser)']
Unit3.coefficients = {'kWh per t cossette' : 1.172284}

def beet_diffusion(cossette_flow, coeff):

    #Extract the required information on all streams
    cossette_amount = cossette_flow.attributes['mass_flow_rate']
    electricity_amount = cossette_amount * coeff['kWh per t cossette']/1000

    #Obtain the indices of all components in the inlet cossette stream
    water_in_cossette_index = cossette_flow.attributes['components'].index('water')
    sucrose_index = cossette_flow.attributes['components'].index('sucrose')
    pulp_index = cossette_flow.attributes['components'].index('pulp')

    #Record the mass fractions of all components in the inlet cossette stream
    water_in_cossette_mass_fraction_inlet = cossette_flow.attributes['composition'][water_in_cossette_index]
    sucrose_mass_fraction_inlet = cossette_flow.attributes['composition'][sucrose_index]
    pulp_mass_fraction_inlet = cossette_flow.attributes['composition'][pulp_index] 

    #Compute the mass flow rates of pulp and sucrose in the two outlet process streams
    pulp_mass_in_sugar_beet_pulp = pulp_mass_fraction_inlet * cossette_amount
    sucrose_mass_in_raw_juice = sucrose_mass_fraction_inlet * cossette_amount  

    #Obtain the mass flow rates of water in the outlet process streams using literature info
    water_mass_in_raw_juice = (sucrose_mass_in_raw_juice - 0.12*sucrose_mass_in_raw_juice)/0.12
    water_mass_in_sugar_beet_pulp = (pulp_mass_in_sugar_beet_pulp*0.8)/(1 - 0.8)

    #Obtain the composition and total mass flow rate of the sugar beet pulp stream
    sugar_beet_pulp_mass = np.sum([pulp_mass_in_sugar_beet_pulp, water_mass_in_sugar_beet_pulp])
    pulp_out_comp = pulp_mass_in_sugar_beet_pulp/sugar_beet_pulp_mass
    water_in_pulp_out_comp = water_mass_in_sugar_beet_pulp/sugar_beet_pulp_mass

    #Obtain the composition and total mass flow rate of the raw juice stream
    raw_juice_mass = np.sum([sucrose_mass_in_raw_juice, water_mass_in_raw_juice])
    sucrose_out_comp = sucrose_mass_in_raw_juice/raw_juice_mass
    water_in_raw_juice_out_comp = water_mass_in_raw_juice/raw_juice_mass

    #Use mass balance to obtain the mass flow rate of hot water needed
    hot_water_mass = water_mass_in_raw_juice + water_mass_in_sugar_beet_pulp - water_in_cossette_mass_fraction_inlet * cossette_amount

    #Note down the data needed to carry out energy balance
    T_outlet_process_stream = 71.1 #C
    T_steam_condensate = 82.2 #C
    T_steam_inlet = 121 #C
    T_hot_water_inlet = 60 #C
    Q_loss_as_fraction_of_steam_inlet_energy = 0.11875

    #Define some heat flow rates
    Q_water_in_raw_juice = water_mass_in_raw_juice*Cp_water_liquid*(T_outlet_process_stream - T_ref)
    Q_water_in_pulp = water_mass_in_sugar_beet_pulp*Cp_water_liquid*(T_outlet_process_stream - T_ref)
    Q_sucrose_in_raw_juice = sucrose_mass_in_raw_juice*Cp_sucrose*(T_outlet_process_stream - T_ref)
    Q_pulp_in_sugar_beet_pulp = pulp_mass_in_sugar_beet_pulp*Cp_pulp*(T_outlet_process_stream - T_ref)
    Q_hot_water_in = hot_water_mass*Cp_water_liquid*(T_hot_water_inlet - T_ref)
    Q_raw_juice = Q_sucrose_in_raw_juice + Q_water_in_raw_juice
    Q_pulp = Q_pulp_in_sugar_beet_pulp + Q_water_in_pulp

    #Calculate the required mass flow rate of steam using energy balance
    steam_mass = (Q_water_in_raw_juice + Q_water_in_pulp + Q_sucrose_in_raw_juice + Q_pulp_in_sugar_beet_pulp - Q_hot_water_in)/(H_v_water*(1 - Q_loss_as_fraction_of_steam_inlet_energy) + (1 - Q_loss_as_fraction_of_steam_inlet_energy)*(T_water_boiling - T_ref)*Cp_water_liquid - Cp_water_liquid*(T_steam_condensate - T_ref) + Cp_dT_steam(T_steam_inlet)*(1 - Q_loss_as_fraction_of_steam_inlet_energy))

    #Define the rest of the heat flow rates, specifically for the steam utility streams
    Q_steam_inlet = steam_mass*H_v_water + steam_mass*(T_water_boiling - T_ref)*Cp_water_liquid + steam_mass*Cp_dT_steam(T_steam_inlet)
    Q_steam_condensate = steam_mass*(T_steam_condensate - T_ref)*Cp_water_liquid

    #Specify the heat lost
    Q_loss = Q_loss_as_fraction_of_steam_inlet_energy*Q_steam_inlet

    return [{'name' : 'Electricity (diffuser)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_amount, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Hot water (diffuser)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : hot_water_mass,
                     'flow_type': 'Hot water', 'temperature' : 60, 'pressure':1 , 'heat_flow_rate' :Q_hot_water_in, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Steam (diffuser)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : steam_mass,
                     'flow_type': 'Steam', 'temperature' : 121, 'pressure':1 , 'heat_flow_rate' :Q_steam_inlet, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Raw juice', 'components' : ['sucrose','water'], 'composition': [sucrose_out_comp,water_in_raw_juice_out_comp], 'mass_flow_rate' : raw_juice_mass,
                     'flow_type': 'Process flow', 'temperature' : 71.1, 'pressure':1 , 'heat_flow_rate' :Q_raw_juice, 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Sugar beet pulp', 'components' : ['pulp','water'], 'composition': [pulp_out_comp,water_in_pulp_out_comp], 'mass_flow_rate' : sugar_beet_pulp_mass,
                     'flow_type': 'Process flow', 'temperature' : 71.1, 'pressure':1 , 'heat_flow_rate' :Q_pulp, 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Steam condensate (from diffuser)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : steam_mass,
                     'flow_type': 'Condensate', 'temperature' : 82.2, 'pressure':1 , 'heat_flow_rate' :Q_steam_condensate, 'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss' : Q_loss}]                

Unit3.calculations = {'Cossette' : beet_diffusion}

#Unit 4: Juice heating                 
Unit4 = Unit('Juice heater')
Unit4.expected_flows_in = ['Raw juice', 'Steam (Juice heater)']
Unit4.expected_flows_out = ['Heated juice', 'Condensate (Juice heater)']
Unit4.coefficients = {'Steam temperature' : 121, 'Condensate temperature' : 82.2, 'Heated juice temperature' : 85, 'Losses' : 0.25} 

def juiceheater(juice_flow, coeff):

    #Extract the mass and heat of main inlet stream
    juice_amount = juice_flow.attributes['mass_flow_rate']
    juice_in_heat = juice_flow.attributes['heat_flow_rate']

    #Obtain the indices of all components in the main inlet stream
    water_index = juice_flow.attributes['components'].index('water')
    sucrose_index = juice_flow.attributes['components'].index('sucrose')

    #Record the mass fractions of all components in the main inlet stream
    water_mass_fraction = juice_flow.attributes['composition'][water_index]
    sucrose_mass_fraction = juice_flow.attributes['composition'][sucrose_index]

    #Obtain the masses of all components in the main inlet stream
    water_amount = water_mass_fraction * juice_amount
    sucrose_amount = sucrose_mass_fraction * juice_amount

    #Find the heat of the main outlet stream
    juice_out_heat = water_amount*Cp_water_liquid*(coeff['Heated juice temperature'] - T_ref) + sucrose_amount*Cp_sucrose*(coeff['Heated juice temperature'] - T_ref)

    #Compute the required mass of steam
    steam_mass = (juice_out_heat - juice_in_heat)/(Cp_water_liquid*(T_water_boiling - T_ref)*(1 - coeff['Losses']) + H_v_water*(1 - coeff['Losses']) + Cp_dT_steam(coeff['Steam temperature'])*(1 - coeff['Losses']) - Cp_water_liquid*(coeff['Condensate temperature'] - T_ref)) #kg/hr

    #Obtain the heat of steam and condensate streams
    Q_steam = steam_mass*H_v_water + steam_mass*Cp_water_liquid*(T_water_boiling - T_ref) + steam_mass*Cp_dT_steam(coeff['Steam temperature'])
    Q_condensate = steam_mass*Cp_water_liquid*(coeff['Condensate temperature'] - T_ref)

    return [{'name' : 'Steam (Juice heater)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : steam_mass,
                     'flow_type': 'Steam', 'temperature' : coeff['Steam temperature'], 'pressure':1 , 'heat_flow_rate' :Q_steam ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Condensate (Juice heater)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : steam_mass,
                     'flow_type': 'Condensate', 'temperature' : coeff['Condensate temperature'], 'pressure':1 , 'heat_flow_rate' :Q_condensate ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Heated juice', 'components' : ['water','sucrose'], 'composition': [water_mass_fraction, sucrose_mass_fraction], 'mass_flow_rate' : juice_amount,
                     'flow_type': 'Process flow', 'temperature' : coeff['Heated juice temperature'], 'pressure':1 , 'heat_flow_rate' :juice_out_heat ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'Heat loss' : Q_steam*coeff['Losses']}]                

Unit4.calculations = {'Raw juice' : juiceheater}

#Unit 5: Carbonation               
Unit5 = Unit('Carbonation')
Unit5.expected_flows_in = ['Heated juice', 'Exhaust (Lime kiln)', 'Calcium hydroxide', 'Electricity (Carbonation)']
Unit5.expected_flows_out = ['Thin juice', 'Precipitate', 'Exhaust (Carbonation)']
Unit5.coefficients = {'Air ratio' : 9.25, 'Gas in temperature' : 315.6, 'CaO2H2 temperature' : 126, 'Precipitate temperature' : 204.4, 'Losses' : 13.4/92.3, 'Electricity per raw material' : 2.0/1.2*0.000293071/0.453592, 'Reaction heat' : -1285713.542} 

def carbonation_tank(heated_juice_flow, coeff):

    #Obtain the specs for the gas inlet stream
    mass_CO2 = mol_calcin*M_CO2
    mass_air = coeff['Air ratio']*CaCO3_inlet
    Q_CO2 = mass_CO2*Cp_dT_CO2(coeff['Gas in temperature'])
    Q_air = mass_air*Cp_dT_air(coeff['Gas in temperature'])
    mass_gas = mass_CO2 + mass_air
    Q_gas = Q_CO2 + Q_air

    #Obtain the specs for the CaO2H2 inlet stream
    mass_CaO2H2 = mol_calcin*M_CaO2H2
    Q_CaO2H2 = mass_CaO2H2*Cp_CaO2H2*(coeff['CaO2H2 temperature'] - T_ref)

    #Extract the mass and heat of main inlet stream
    heated_juice_amount = heated_juice_flow.attributes['mass_flow_rate']
    heated_juice_in_heat = heated_juice_flow.attributes['heat_flow_rate']

    #Obtain the indices of all components in the main inlet stream
    water_index = heated_juice_flow.attributes['components'].index('water')
    sucrose_index = heated_juice_flow.attributes['components'].index('sucrose')

    #Record the mass fractions of all components in the main inlet stream
    water_mass_fraction = heated_juice_flow.attributes['composition'][water_index]
    sucrose_mass_fraction = heated_juice_flow.attributes['composition'][sucrose_index]

    #Obtain the masses of all components in the main inlet stream
    water_amount = water_mass_fraction * heated_juice_amount
    sucrose_amount = sucrose_mass_fraction * heated_juice_amount

    #Compute the electricity requirement
    carbonation_electricity = coeff['Electricity per raw material'] * (water_amount+sucrose_amount)

    #Obtain the specs for the outlet exhaust stream
    mass_water_out = mol_calcin*M_water
    heat_water_out = mass_water_out*Cp_water_liquid*(T_water_boiling - T_ref) + mass_water_out*H_v_water + mass_water_out*Cp_dT_steam(coeff['Precipitate temperature'])
    Q_air_out = mass_air*Cp_dT_air(coeff['Precipitate temperature'])

    #Obtain the specs for the precipitate stream
    mass_CaCO3 = mol_calcin*M_CaCO3
    Q_CaCO3 = mass_CaCO3*Cp_dT_CaCO3(coeff['Precipitate temperature'] + 273.15)

    #Compute the temperature of the main outlet stream
    residual_heat = Q_gas + heated_juice_in_heat + Q_CaO2H2 - coeff['Reaction heat'] - coeff['Losses']*heated_juice_in_heat - heat_water_out - Q_air_out - Q_CaCO3
    T_out_main = (residual_heat + sucrose_amount*Cp_sucrose*T_ref + water_amount*Cp_water_liquid*T_ref)/(sucrose_amount*Cp_sucrose + water_amount*Cp_water_liquid)

    return [{'name' : 'Thin juice', 'components' : ['water','sucrose'], 'composition': [water_mass_fraction,sucrose_mass_fraction], 'mass_flow_rate' : heated_juice_amount,
                     'flow_type': 'Process flow', 'temperature' : T_out_main, 'pressure':1 , 'heat_flow_rate' :residual_heat ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Exhaust (Lime kiln)', 'components' : ['Air','CO2'], 'composition': [mass_air/mass_gas,mass_CO2/mass_gas], 'mass_flow_rate' : mass_gas,
                     'flow_type': 'Process flow', 'temperature' : coeff['Gas in temperature'], 'pressure':1 , 'heat_flow_rate' :Q_gas ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : True},
            {'name' : 'Calcium hydroxide', 'components' : ['CaO2H2'], 'composition': [1], 'mass_flow_rate' : mass_CaO2H2,
                     'flow_type': 'Process flow', 'temperature' : coeff['CaO2H2 temperature'], 'pressure':1 , 'heat_flow_rate' :Q_CaO2H2 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : True},
            {'name' : 'Exhaust (Carbonation)', 'components' : ['Air','water'], 'composition': [mass_air/(mass_air+mass_water_out),mass_water_out/(mass_air+mass_water_out)], 'mass_flow_rate' : mass_air+mass_water_out,
                     'flow_type': 'Exhaust', 'temperature' : coeff['Precipitate temperature'], 'pressure':1 , 'heat_flow_rate' :heat_water_out+Q_air_out ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Precipitate', 'components' : ['CaCO3'], 'composition': [1], 'mass_flow_rate' : mass_CaCO3,
                     'flow_type': 'Waste', 'temperature' : coeff['Precipitate temperature'], 'pressure':1 , 'heat_flow_rate' :Q_CaCO3 ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Electricity (Carbonation)', 'components' : None, 'mass_flow_rate' : 0,
                     'flow_type': 'Electricity', 'elec_flow_rate' : carbonation_electricity, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'Heat of reaction' : -coeff['Reaction heat']},                      
            {'Heat loss' : heated_juice_in_heat*coeff['Losses']}]                

Unit5.calculations = {'Heated juice' : carbonation_tank}

#Unit 6: Lime kiln                 
Unit6 = Unit('Lime kiln')
Unit6.expected_flows_in = ['Air (Lime kiln)', 'Limestone', 'Natural gas (Lime kiln)']
Unit6.expected_flows_out = ['Quicklime', 'Exhaust (Lime kiln)']
Unit6.coefficients = {'Quicklime temperature' : 121.1, 'Stack temperature' : 315.6, 'Air ratio' : 9.25, 'Losses' : 0.1} 

def limekiln(limestone_flow, coeff):

    #Extract some required information
    limestone_amount = limestone_flow.attributes['mass_flow_rate']
    air_amount = limestone_amount * coeff['Air ratio']

    #Obtain the heat of reaction for limestone's calcination into CaO
    H_rxn_calcin = 179.2*mol_calcin*1000 #kJ/hr

    #Compute the masses of products using stoichiometry
    mass_CaO = mol_calcin * M_CaO #kg/hr
    mass_CO2 = mol_calcin * M_CO2 #kg/hr

    #Obtain the heat rates
    Q_CaO = mass_CaO * Cp_dT_CaO(coeff['Quicklime temperature'] + 273.15)
    Q_CO2 = mass_CO2 * Cp_dT_CO2(coeff['Stack temperature'])
    Q_air = air_amount * Cp_dT_air(coeff['Stack temperature'])
    Q_stack = Q_CO2 + Q_air

    #Find the fuel amount and heat loss
    E_fuel = (Q_CaO + Q_stack + H_rxn_calcin)/(1 - coeff['Losses']) #kJ/hr
    Q_loss = coeff['Losses']*E_fuel

    return [{'name' : 'Exhaust (Lime kiln)', 'components' : ['Air', 'CO2'], 'composition': [air_amount/(air_amount + mass_CO2), mass_CO2/(air_amount + mass_CO2)], 'mass_flow_rate' : air_amount + mass_CO2,
                     'flow_type': 'Process flow', 'temperature' : coeff['Stack temperature'], 'pressure':1 , 'heat_flow_rate' :Q_stack ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : True},
            {'name' : 'Quicklime', 'components' : ['CaO'], 'composition': [1], 'mass_flow_rate' : mass_CaO,
                     'flow_type': 'Process flow', 'temperature' : coeff['Quicklime temperature'], 'pressure':1 , 'heat_flow_rate' :Q_CaO ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Air (Lime kiln)', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : air_amount,
                     'flow_type': 'Air', 'temperature' : 25, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Natural gas (Lime kiln)', 'components' : ['Natural gas'], 'composition': [1], 'mass_flow_rate' : 0,
                     'flow_type': 'Fuel', 'temperature' : 25, 'pressure':1 , 'heat_flow_rate' :0 , 'combustion_energy_content' : E_fuel , 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss' : Q_loss},
            {'Heat of reaction' : (E_fuel - H_rxn_calcin)}]                

Unit6.calculations = {'Limestone' : limekiln}

#Unit 7: Lime slaker                 
Unit7 = Unit('Lime slaker')
Unit7.expected_flows_in = ['Quicklime', 'Water (Lime slaker)']
Unit7.expected_flows_out = ['Calcium hydroxide']
Unit7.coefficients = {'Water temperature' : 87.5} 

def limeslaker(quicklime_flow, coeff):

    #Extract some required information
    Q_quicklime = quicklime_flow.attributes['heat_flow_rate']

    #Obtain the water requirement
    water_amount = mol_calcin*M_water #kg/hr
    water_heat = water_amount*Cp_water_liquid*(coeff['Water temperature'] - T_ref)

    #Compute the specs for CaO2H2
    CaO2H2_amount = mol_calcin * M_CaO2H2 #kg/hr
    Q_CaO2H2 = Q_quicklime + water_heat
    T_CaO2H2 = Q_CaO2H2/(CaO2H2_amount*Cp_CaO2H2) + T_ref


    return [{'name' : 'Calcium hydroxide', 'components' : ['CaO2H2'], 'composition': [1], 'mass_flow_rate' : CaO2H2_amount,
                     'flow_type': 'Process flow', 'temperature' : T_CaO2H2, 'pressure':1 , 'heat_flow_rate' :Q_CaO2H2 ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : True},
            {'name' : 'Water (Lime slaker)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : water_amount,
                     'flow_type': 'Water', 'temperature' : coeff['Water temperature'], 'pressure':1 , 'heat_flow_rate' :water_heat ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False}]                

Unit7.calculations = {'Quicklime' : limeslaker}

#Unit 8: Sulfonation               
Unit8 = Unit('Sulfonation')
Unit8.expected_flows_in = ['Thin juice', 'Sulfur', 'Water (Sulfonation)']
Unit8.expected_flows_out = ['Sulfonated juice', 'Sludge']
Unit8.coefficients = {'Sulfur ppm' : 15, 'Water to sulfur ratio' : 1/9, 'Losses' : 8/93.5} 

def sulfonation_tank(thin_juice_flow, coeff):

    #Extract the mass and heat of main inlet stream
    thin_juice_amount = thin_juice_flow.attributes['mass_flow_rate']
    thin_juice_heat = thin_juice_flow.attributes['heat_flow_rate']

    #Obtain the indices of all components in the main inlet stream
    water_index = thin_juice_flow.attributes['components'].index('water')
    sucrose_index = thin_juice_flow.attributes['components'].index('sucrose')

    #Record the mass fractions of all components in the main inlet stream
    water_mass_fraction = thin_juice_flow.attributes['composition'][water_index]
    sucrose_mass_fraction = thin_juice_flow.attributes['composition'][sucrose_index]

    #Obtain the masses of all components in the main inlet stream
    water_amount = water_mass_fraction * thin_juice_amount
    sucrose_amount = sucrose_mass_fraction * thin_juice_amount

    #Estimate the quantities of other components
    SO2_amount = coeff['Sulfur ppm']*thin_juice_amount/1000000
    water_added = coeff['Water to sulfur ratio']*SO2_amount

    #Define the function used for calculating the temperature of the two outlet streams
    def sulfonation_Tout(T):
        energy_balance = sucrose_amount*Cp_sucrose*(T-T_ref) + (water_amount+water_added)*Cp_water_liquid*(T-T_ref) + SO2_amount*(0.03891*T + 3.904*10**(-5)/2*T**2 - 3.104*10**(-8)/3*T**3 + 8.606*10**(-12)/4*T**4 - 0.9847891738)*1000/M_SO2 + coeff['Losses']*thin_juice_heat - thin_juice_heat
        return energy_balance
    
    #Iterate for outlet temperature
    T_out = fsolve(sulfonation_Tout, 75)

    #Compute the outlet heat rates
    Q_sludge = SO2_amount*Cp_dT_SO2(T_out) + water_added*Cp_water_liquid*(T_out - T_ref)
    Q_juice = sucrose_amount*Cp_sucrose*(T_out - T_ref) + water_amount*Cp_water_liquid*(T_out - T_ref)
    Q_lost = coeff['Losses']*thin_juice_heat

    return [{'name' : 'Sulfonated juice', 'components' : ['water','sucrose'], 'composition': [water_mass_fraction,sucrose_mass_fraction], 'mass_flow_rate' : thin_juice_amount,
                     'flow_type': 'Process flow', 'temperature' : T_out, 'pressure':1 , 'heat_flow_rate' :Q_juice ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Sludge', 'components' : ['water','SO2'], 'composition': [water_added/(water_added+SO2_amount),SO2_amount/(water_added+SO2_amount)], 'mass_flow_rate' : water_added+SO2_amount,
                     'flow_type': 'Waste water', 'temperature' : T_out, 'pressure':1 , 'heat_flow_rate' :Q_sludge ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Sulfur', 'components' : ['SO2'], 'composition': [1], 'mass_flow_rate' : SO2_amount,
                     'flow_type': 'Process flow', 'temperature' : 25, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Water (Sulfonation)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : water_added,
                     'flow_type': 'Water', 'temperature' : 25, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss' : Q_lost}]                

Unit8.calculations = {'Thin juice' : sulfonation_tank}

#Unit 9: Heater + Multi-effect evaporator               
Unit9 = Unit('Evaporator')
Unit9.expected_flows_in = ['Sulfonated juice', 'Steam (Evaporator)', 'Cooling water (Into evaporator)']
Unit9.expected_flows_out = ['Thick juice', 'Moisture', 'Cooling water (Out of evaporator)', 'Condensate (Evaporator)']
Unit9.coefficients = {'Thick juice temperature' : 93.3, 'Sucrose content in thick juice' : 0.72, 'Steam economy' : 3, 'Losses' : 57.5/84.0, 'Cooling water outlet temperature' : 37.8, 'Steam temperature' : 121.1, 'Condensate temperature' : 82.2, 'Cooling water to main inlet ratio' : 10} 

def multi_effect_evap(sulfonated_juice_flow, coeff):

    #Extract the mass and heat of main inlet stream
    sulfonated_juice_amount = sulfonated_juice_flow.attributes['mass_flow_rate']
    sulfonated_juice_heat = sulfonated_juice_flow.attributes['heat_flow_rate']

    #Obtain the indices of all components in the main inlet stream
    water_index = sulfonated_juice_flow.attributes['components'].index('water')
    sucrose_index = sulfonated_juice_flow.attributes['components'].index('sucrose')

    #Record the mass fractions of all components in the main inlet stream
    water_mass_fraction = sulfonated_juice_flow.attributes['composition'][water_index]
    sucrose_mass_fraction = sulfonated_juice_flow.attributes['composition'][sucrose_index]

    #Obtain the masses of all components in the main inlet stream
    water_amount = water_mass_fraction * sulfonated_juice_amount
    sucrose_amount = sucrose_mass_fraction * sulfonated_juice_amount

    #Obtain the specs for thick juice
    water_amount_in_thick_juice = (1 - coeff['Sucrose content in thick juice'])/coeff['Sucrose content in thick juice']*sucrose_amount
    Q_thick_juice = water_amount_in_thick_juice*Cp_water_liquid*(coeff['Thick juice temperature'] - T_ref) + sucrose_amount*Cp_sucrose*(coeff['Thick juice temperature'] - T_ref)

    #Approximate the value of Q_loss
    Q_lost = coeff['Losses']*sulfonated_juice_heat

    #Compute the mass of water evaporated
    water_evap = water_amount - water_amount_in_thick_juice

    #Obtain the specs for the required steam
    steam_amount = water_evap/coeff['Steam economy']
    steam_heat = steam_amount*Cp_water_liquid*(T_water_boiling - T_ref) + steam_amount*H_v_water + steam_amount*Cp_dT_steam(coeff['Steam temperature'])
    condensate_heat = steam_amount*Cp_water_liquid*(coeff['Condensate temperature'] - T_ref)

    #Obtain the cooling water specs
    cooling_water_mass = coeff['Cooling water to main inlet ratio']*sulfonated_juice_amount
    cooling_water_heat_out = cooling_water_mass*Cp_water_liquid*(coeff['Cooling water outlet temperature'] - T_ref)

    #Compute the temperature of the moisture outlet
    residual_heat = sulfonated_juice_heat + steam_heat - Q_lost - condensate_heat - Q_thick_juice - cooling_water_heat_out
    T_moisture = (residual_heat + water_evap*Cp_water_liquid*T_ref)/(water_evap*Cp_water_liquid)

    return [{'name' : 'Thick juice', 'components' : ['water','sucrose'], 'composition': [water_amount_in_thick_juice/(water_amount_in_thick_juice+sucrose_amount),sucrose_amount/(water_amount_in_thick_juice+sucrose_amount)], 'mass_flow_rate' : water_amount_in_thick_juice+sucrose_amount,
                     'flow_type': 'Process flow', 'temperature' : coeff['Thick juice temperature'], 'pressure':1 , 'heat_flow_rate' :Q_thick_juice ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Steam (Evaporator)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : steam_amount,
                     'flow_type': 'Steam', 'temperature' : coeff['Steam temperature'], 'pressure':1 , 'heat_flow_rate' :steam_heat ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Cooling water (Into evaporator)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : cooling_water_mass,
                     'flow_type': 'Cooling water', 'temperature' : 25, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Cooling water (Out of evaporator)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : cooling_water_mass,
                     'flow_type': 'Cooling water', 'temperature' : coeff['Cooling water outlet temperature'], 'pressure':1 , 'heat_flow_rate' :cooling_water_heat_out ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Moisture', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : water_evap,
                     'flow_type': 'Waste water', 'temperature' : T_moisture, 'pressure':1 , 'heat_flow_rate' :residual_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Condensate (Evaporator)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : steam_amount,
                     'flow_type': 'Condensate', 'temperature' : coeff['Condensate temperature'], 'pressure':1 , 'heat_flow_rate' :condensate_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss' : Q_lost}]                

Unit9.calculations = {'Sulfonated juice' : multi_effect_evap}

#Unit 10: Vacuum pans/crystallizer              
Unit10 = Unit('Crystallizer')
Unit10.expected_flows_in = ['Thick juice', 'Steam (Crystallizer)', 'Electricity (Crystallizer)']
Unit10.expected_flows_out = ['Massecuite', 'Wastewater (Crystallizer)', 'Condensate (Crystallizer)']
Unit10.coefficients = {'Steam to thick juice ratio' : 0.1/0.23, 'Wastewater to thick juice ratio' : 0.05/0.23, 'Steam temperature' : 137.8, 'Condensate temperature' : 82.2, 'Wastewater temperature' : 35, 'Massecuite temperature' : 54.4, 'Electricity per input energy' : 5/25}

def vacuum_crystallizer(thick_juice_flow, coeff):

    #Extract the mass and heat of main inlet stream
    thick_juice_amount = thick_juice_flow.attributes['mass_flow_rate']
    thick_juice_heat = thick_juice_flow.attributes['heat_flow_rate']

    #Obtain the indices of all components in the main inlet stream
    water_index = thick_juice_flow.attributes['components'].index('water')
    sucrose_index = thick_juice_flow.attributes['components'].index('sucrose')

    #Record the mass fractions of all components in the main inlet stream
    water_mass_fraction = thick_juice_flow.attributes['composition'][water_index]
    sucrose_mass_fraction = thick_juice_flow.attributes['composition'][sucrose_index]

    #Obtain the masses of all components in the main inlet stream
    water_amount = water_mass_fraction * thick_juice_amount
    sucrose_amount = sucrose_mass_fraction * thick_juice_amount

    #Compute the steam and condensate heat
    steam_amount = coeff['Steam to thick juice ratio']*thick_juice_amount
    steam_heat = steam_amount*Cp_water_liquid*(T_water_boiling - T_ref) + steam_amount*H_v_water + steam_amount*Cp_dT_steam(coeff['Steam temperature'])
    condensate_heat = steam_amount*Cp_water_liquid*(coeff['Condensate temperature'] - T_ref)

    #Compute the electricity requirement
    electricity_required = coeff['Electricity per input energy']*thick_juice_heat*0.000278

    #Obtain the wastewater specs
    wastewater_amount = coeff['Wastewater to thick juice ratio']*thick_juice_amount
    wastewater_heat = wastewater_amount*Cp_water_liquid*(coeff['Wastewater temperature'] - T_ref)

    #Compute the temperature and composition of massecuite
    water_in_massecuite = water_amount - wastewater_amount
    massecuite_heat = water_in_massecuite*Cp_water_liquid*(coeff['Massecuite temperature'] - T_ref) + sucrose_amount*Cp_sucrose*(coeff['Massecuite temperature'] - T_ref)

    #Obtain the heat loss
    Q_lost = thick_juice_heat + steam_heat - condensate_heat - wastewater_heat - massecuite_heat

    return [{'name' : 'Massecuite', 'components' : ['water','sucrose'], 'composition': [water_in_massecuite/(water_in_massecuite+sucrose_amount),sucrose_amount/(water_in_massecuite+sucrose_amount)], 'mass_flow_rate' : water_in_massecuite+sucrose_amount,
                     'flow_type': 'Process flow', 'temperature' : coeff['Massecuite temperature'], 'pressure':1 , 'heat_flow_rate' :massecuite_heat ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Steam (Crystallizer)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : steam_amount,
                     'flow_type': 'Steam', 'temperature' : coeff['Steam temperature'], 'pressure':1 , 'heat_flow_rate' :steam_heat ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Condensate (Crystallizer)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : steam_amount,
                     'flow_type': 'Condensate', 'temperature' : coeff['Condensate temperature'], 'pressure':1 , 'heat_flow_rate' :condensate_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Wastewater (Crystallizer)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : wastewater_amount,
                     'flow_type': 'Waste water', 'temperature' : coeff['Wastewater temperature'], 'pressure':1 , 'heat_flow_rate' :wastewater_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Electricity (Crystallizer)', 'components' : None, 'mass_flow_rate' : 0,
                     'flow_type': 'Electricity', 'elec_flow_rate' : electricity_required, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss' : Q_lost}]                

Unit10.calculations = {'Thick juice' : vacuum_crystallizer}

#Unit 11: Centrifugation              
Unit11 = Unit('Centrifuge')
Unit11.expected_flows_in = ['Massecuite', 'Electricity (Centrifuge)']
Unit11.expected_flows_out = ['Molasses', 'Sugar (From centrifuge)']
Unit11.coefficients = {'Electricity to input energy ratio' : 1, 'Final temperature' : 37.8, 'Input split to molasses' : 0.143, 'Purity of molasses' : 0.5}

def centrifugal(massecuite_flow, coeff):

    #Extract the mass and heat of main inlet stream
    massecuite_amount = massecuite_flow.attributes['mass_flow_rate']
    massecuite_heat = massecuite_flow.attributes['heat_flow_rate']

    #Obtain the indices of all components in the main inlet stream
    water_index = massecuite_flow.attributes['components'].index('water')
    sucrose_index = massecuite_flow.attributes['components'].index('sucrose')

    #Record the mass fractions of all components in the main inlet stream
    water_mass_fraction = massecuite_flow.attributes['composition'][water_index]
    sucrose_mass_fraction = massecuite_flow.attributes['composition'][sucrose_index]

    #Obtain the masses of all components in the main inlet stream
    water_amount = water_mass_fraction * massecuite_amount
    sucrose_amount = sucrose_mass_fraction * massecuite_amount

    #Compute the molasses specs
    molasses_mixture_amount = coeff['Input split to molasses']*massecuite_amount
    sucrose_in_molasses = coeff['Purity of molasses']*molasses_mixture_amount
    water_in_molasses = molasses_mixture_amount - sucrose_in_molasses
    molasses_heat = water_in_molasses*Cp_water_liquid*(coeff['Final temperature'] - T_ref) + sucrose_in_molasses*Cp_sucrose*(coeff['Final temperature'] - T_ref)

    #Obtain the specs for the sugar stream leaving the centrifuge
    sucrose_to_granulator = sucrose_amount - sucrose_in_molasses
    water_to_granulator = water_amount - water_in_molasses
    heat_to_granulator = water_to_granulator*Cp_water_liquid*(coeff['Final temperature'] - T_ref) + sucrose_to_granulator*Cp_sucrose*(coeff['Final temperature'] - T_ref)

    #Obtain the electricity requirement
    electricity_required = coeff['Electricity to input energy ratio'] * massecuite_heat * 0.000278

    #Compute the heat lost
    Q_lost = massecuite_heat - molasses_heat - heat_to_granulator

    return [{'name' : 'Molasses', 'components' : ['water','sucrose'], 'composition': [coeff['Purity of molasses'],coeff['Purity of molasses']], 'mass_flow_rate' : molasses_mixture_amount,
                     'flow_type': 'Process flow', 'temperature' : coeff['Final temperature'], 'pressure':1 , 'heat_flow_rate' :molasses_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Sugar (From centrifuge)', 'components' : ['water','sucrose'], 'composition': [water_to_granulator/(water_to_granulator+sucrose_to_granulator),sucrose_to_granulator/(water_to_granulator+sucrose_to_granulator)], 'mass_flow_rate' : water_to_granulator+sucrose_to_granulator,
                     'flow_type': 'Process flow', 'temperature' : coeff['Final temperature'], 'pressure':1 , 'heat_flow_rate' :heat_to_granulator ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Electricity (Centrifuge)', 'components' : None, 'mass_flow_rate' : 0,
                     'flow_type': 'Electricity', 'elec_flow_rate' : electricity_required, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss' : Q_lost}]                

Unit11.calculations = {'Massecuite' : centrifugal}

#Unit 12: Granulation              
Unit12 = Unit('Granulator')
Unit12.expected_flows_in = ['Sugar (From centrifuge)', 'Air (Granulator)', 'Steam (Granulator)']
Unit12.expected_flows_out = ['Final sugar', 'Exhaust (Granulator)', 'Condensate (Granulator)']
Unit12.coefficients = {'Air to input ratio' : 0.015/0.155, 'Steam to input ratio' : 0.005/0.155, 'Steam temperature' : 121.1, 'Final sugar purity' : 0.9995, 'Condensate temperature' : 82.2, 'Exhaust temperature' : 104.4, 'Losses' : 0.5}

def granulation(centrifugal_sugar_flow, coeff):

    #Extract the mass and heat of main inlet stream
    centrifugal_sugar_amount = centrifugal_sugar_flow.attributes['mass_flow_rate']
    centrifugal_sugar_heat = centrifugal_sugar_flow.attributes['heat_flow_rate']

    #Obtain the indices of all components in the main inlet stream
    water_index = centrifugal_sugar_flow.attributes['components'].index('water')
    sucrose_index = centrifugal_sugar_flow.attributes['components'].index('sucrose')

    #Record the mass fractions of all components in the main inlet stream
    water_mass_fraction = centrifugal_sugar_flow.attributes['composition'][water_index]
    sucrose_mass_fraction = centrifugal_sugar_flow.attributes['composition'][sucrose_index]

    #Obtain the masses of all components in the main inlet stream
    water_amount = water_mass_fraction * centrifugal_sugar_amount
    sucrose_amount = sucrose_mass_fraction * centrifugal_sugar_amount

    #Compute the water content of final sugar product
    water_in_final_sugar = (1 - coeff['Final sugar purity'])/coeff['Final sugar purity']*sucrose_amount

    #Obtain the exhaust specs
    air_required = coeff['Air to input ratio']*centrifugal_sugar_amount
    water_in_exhaust = water_amount - water_in_final_sugar
    exhaust_mass = air_required + water_in_exhaust
    exhaust_heat = air_required*Cp_dT_air(coeff['Exhaust temperature']) + water_in_exhaust*Cp_water_liquid*(T_water_boiling - T_ref) + water_in_exhaust*H_v_water + water_in_exhaust*Cp_dT_steam(coeff['Exhaust temperature'])

    #Obtain the steam and condensate specs
    steam_amount = coeff['Steam to input ratio']*centrifugal_sugar_amount
    steam_heat = steam_amount*Cp_water_liquid*(T_water_boiling - T_ref) + steam_amount*H_v_water + steam_amount*Cp_dT_steam(coeff['Steam temperature'])
    condensate_heat = steam_amount*Cp_water_liquid*(coeff['Condensate temperature'] - T_ref)

    #Estimate the heat loss
    Q_lost = coeff['Losses']*centrifugal_sugar_heat

    #Compute the final sugar temperature
    final_sugar_heat = centrifugal_sugar_heat + steam_heat - exhaust_heat - condensate_heat - Q_lost
    final_sugar_T = (final_sugar_heat + sucrose_amount*Cp_sucrose*T_ref + water_in_final_sugar*Cp_water_liquid*T_ref)/(sucrose_amount*Cp_sucrose + water_in_final_sugar*Cp_water_liquid)

    return [{'name' : 'Final sugar', 'components' : ['water','sucrose'], 'composition': [1 - coeff['Final sugar purity'],coeff['Final sugar purity']], 'mass_flow_rate' : water_in_final_sugar+sucrose_amount,
                     'flow_type': 'Product', 'temperature' : final_sugar_T, 'pressure':1 , 'heat_flow_rate' :final_sugar_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Exhaust (Granulator)', 'components' : ['water','Air'], 'composition': [water_in_exhaust/exhaust_mass,air_required/exhaust_mass], 'mass_flow_rate' : exhaust_mass,
                     'flow_type': 'Exhaust', 'temperature' : coeff['Exhaust temperature'], 'pressure':1 , 'heat_flow_rate' :exhaust_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Steam (Granulator)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : steam_amount,
                     'flow_type': 'Steam', 'temperature' : coeff['Steam temperature'], 'pressure':1 , 'heat_flow_rate' :steam_heat ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Condensate (Granulator)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : steam_amount,
                     'flow_type': 'Condensate', 'temperature' : coeff['Condensate temperature'], 'pressure':1 , 'heat_flow_rate' :condensate_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Air (Granulator)', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : air_required,
                     'flow_type': 'Air', 'temperature' : 25, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss' : Q_lost}]                

Unit12.calculations = {'Sugar (From centrifuge)' : granulation}

#Unit 13: Pulp press             
Unit13 = Unit('Pulp press')
Unit13.expected_flows_in = ['Sugar beet pulp', 'Electricity (Pulp press)']
Unit13.expected_flows_out = ['Wastewater (Pulp press)', 'Pressed pulp']
Unit13.coefficients = {'Wastewater part of input' : 4/7, 'Electricity to input energy ratio' : 7/31.3}

def pulp_pressing(sugar_beet_pulp_flow, coeff):

    #Extract the mass, heat and temperature of the main inlet stream
    sugar_beet_pulp_amount = sugar_beet_pulp_flow.attributes['mass_flow_rate']
    sugar_beet_pulp_heat = sugar_beet_pulp_flow.attributes['heat_flow_rate']
    T = sugar_beet_pulp_flow.attributes['temperature']

    #Obtain the indices of all components in the main inlet stream
    water_index = sugar_beet_pulp_flow.attributes['components'].index('water')
    pulp_index = sugar_beet_pulp_flow.attributes['components'].index('pulp')

    #Record the mass fractions of all components in the main inlet stream
    water_mass_fraction = sugar_beet_pulp_flow.attributes['composition'][water_index]
    pulp_mass_fraction = sugar_beet_pulp_flow.attributes['composition'][pulp_index]

    #Obtain the masses of all components in the main inlet stream
    water_amount = water_mass_fraction * sugar_beet_pulp_amount
    pulp_amount = pulp_mass_fraction * sugar_beet_pulp_amount

    #Compute the wastewater specs
    wastewater_amount = coeff['Wastewater part of input'] * sugar_beet_pulp_amount
    wastewater_heat = wastewater_amount * Cp_water_liquid * (T - T_ref)

    #Compute the specs for the pressed pulp
    water_in_pressed_pulp = water_amount - wastewater_amount
    pressed_pulp_heat = water_in_pressed_pulp * Cp_water_liquid * (T - T_ref) + pulp_amount * Cp_pulp * (T - T_ref)

    #Obtain the electricity requirement
    electricity_required = coeff['Electricity to input energy ratio'] * sugar_beet_pulp_heat * 0.000278

    return [{'name' : 'Pressed pulp', 'components' : ['water','pulp'], 'composition': [water_in_pressed_pulp/(water_in_pressed_pulp+pulp_amount),pulp_amount/(water_in_pressed_pulp+pulp_amount)], 'mass_flow_rate' : water_in_pressed_pulp+pulp_amount,
                     'flow_type': 'Process flow', 'temperature' : T, 'pressure':1 , 'heat_flow_rate' :pressed_pulp_heat ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Wastewater (Pulp press)', 'components' : ['water'], 'composition': [1], 'mass_flow_rate' : wastewater_amount,
                     'flow_type': 'Waste water', 'temperature' : T, 'pressure':1 , 'heat_flow_rate' :wastewater_heat ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Electricity (Pulp press)', 'components' : None, 'mass_flow_rate' : 0,
                     'flow_type': 'Electricity', 'elec_flow_rate' : electricity_required, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False}]

Unit13.calculations = {'Sugar beet pulp' : pulp_pressing}

#Unit 14: Pulp drying and palletizing             
Unit14 = Unit('Pulp drying')
Unit14.expected_flows_in = ['Pressed pulp', 'Air (Pulp drying)', 'Electricity (Pulp drying)', 'Fuel (Pulp drying)']
Unit14.expected_flows_out = ['Exhaust (Pulp drying)', 'Final pulp']
Unit14.coefficients = {'Air to input ratio' : 3.84/0.3, 'Electricity to input energy ratio' : 6/10.3, 'Fuel per kg beet entering' : 352.8022208, 'Final pulp purity' : 0.98, 'Losses' : 33/10.3}

def drying_and_palletizing(pressed_pulp_flow, coeff):

    #Extract the mass and heat of the main inlet stream
    pressed_pulp_amount = pressed_pulp_flow.attributes['mass_flow_rate']
    pressed_pulp_heat = pressed_pulp_flow.attributes['heat_flow_rate']

    #Obtain the indices of all components in the main inlet stream
    water_index = pressed_pulp_flow.attributes['components'].index('water')
    pulp_index = pressed_pulp_flow.attributes['components'].index('pulp')

    #Record the mass fractions of all components in the main inlet stream
    water_mass_fraction = pressed_pulp_flow.attributes['composition'][water_index]
    pulp_mass_fraction = pressed_pulp_flow.attributes['composition'][pulp_index]

    #Obtain the masses of all components in the main inlet stream
    water_amount = water_mass_fraction * pressed_pulp_amount
    pulp_amount = pulp_mass_fraction * pressed_pulp_amount

    #Obtain the electricity requirement
    electricity_required = coeff['Electricity to input energy ratio'] * pressed_pulp_heat * 0.000278

    #Compute the fuel requirement
    fuel_amount = coeff['Fuel per kg beet entering'] * inlet_beet_mixture

    #Obtain the air requirement
    air_required = coeff['Air to input ratio'] * pressed_pulp_amount

    #Compute the final pulp specs
    water_in_final_pulp = (1 - coeff['Final pulp purity'])/coeff['Final pulp purity'] * pulp_amount

    #Find the mass flow rate of water in the exhaust stream
    water_in_exhaust = water_amount - water_in_final_pulp

    #Estimate the heat loss
    Q_lost = coeff['Losses']*pressed_pulp_heat

    #Obtain the heat of the exhaust stream
    heat_exhaust = pressed_pulp_heat + fuel_amount - Q_lost

    #Define the function used for calculating the temperature of the exhaust stream
    def exhaust_T(T):
        energy_balance = water_in_exhaust*Cp_water_liquid*(T_water_boiling - T_ref) + water_in_exhaust*H_v_water + water_in_exhaust*(0.03346*T + 0.688*10**(-5)/2*T**2 + 0.7604*10**(-8)/3*T**3 - 3.593*10**(-12)/4*T**4 - 3.382844842)*1000/M_water + air_required*(0.02894*T + 0.4148*10**(-5)/2*T**2 + 0.3191*10**(-8)/3*T**3 - 1.965*10**(-12)/4*T**4 - 0.7248123654)*1000/M_air - heat_exhaust
        return energy_balance
    
    #Iterate for the exhaust temperature
    T_out = fsolve(exhaust_T, 200)

    return [{'name' : 'Final pulp', 'components' : ['water','pulp'], 'composition': [1 - coeff['Final pulp purity'], coeff['Final pulp purity']], 'mass_flow_rate' : water_in_final_pulp+pulp_amount,
                     'flow_type': 'Product', 'temperature' : 25, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Exhaust (Pulp drying)', 'components' : ['water','Air'], 'composition': [water_in_exhaust/(water_in_exhaust+air_required),air_required/(water_in_exhaust+air_required)], 'mass_flow_rate' : water_in_exhaust+air_required,
                     'flow_type': 'Exhaust', 'temperature' : T_out, 'pressure':1 , 'heat_flow_rate' :heat_exhaust ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Fuel (Pulp drying)', 'components' : ['Natural gas'], 'composition': [1], 'mass_flow_rate' : 0,
                     'flow_type': 'Fuel', 'temperature' : 25, 'pressure':1 , 'heat_flow_rate' :0 , 'combustion_energy_content' : fuel_amount , 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Air (Pulp drying)', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : air_required,
                     'flow_type': 'Air', 'temperature' : 25, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Electricity (Pulp drying)', 'components' : None, 'mass_flow_rate' : 0,
                     'flow_type': 'Electricity', 'elec_flow_rate' : electricity_required, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'Heat loss' : Q_lost},
            {'Heat of reaction' : fuel_amount}]

Unit14.calculations = {'Pressed pulp' : drying_and_palletizing}

#Append all process units
processunits = [Unit1, Unit2, Unit3, Unit4, Unit5, Unit6, Unit7, Unit8, Unit9, Unit10, Unit11, Unit12, Unit13, Unit14]

#Define the inlet beet stream
FlowA = Flow('Beet',['soil','stone','organic matter','water','sucrose','pulp'],'input', 25, 1, [inlet_soil/inlet_beet_mixture,inlet_stone/inlet_beet_mixture,inlet_organic_matter/inlet_beet_mixture,inlet_water_in_beet/inlet_beet_mixture,inlet_sucrose/inlet_beet_mixture,inlet_pulp/inlet_beet_mixture], None , None, inlet_beet_mixture, np.nan, 0)
FlowA.set_calc_flow()
allflows.append(FlowA)

#Define the inlet limestone stream
FlowB = Flow('Limestone',['CaCO3'],'input', 25, 1, [1], None , None, CaCO3_inlet, np.nan, 0)
FlowB.set_calc_flow()
allflows.append(FlowB)

#Attach flow streams to process units and perform the required calculations
f_print = True
main(allflows, processunits, f_print)

#Check mass and energy balances
for unit in processunits:
    unit.check_heat_balance(allflows)
    unit.check_mass_balance(allflows)
