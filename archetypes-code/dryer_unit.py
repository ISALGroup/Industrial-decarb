# -*- coding: utf-8 -*-
"""
Created on Thu Jan 30 10:16:43 2025

@author: Antoine
"""

import pandas as pd
import numpy as np
import csv
from CoolProp.CoolProp import PropsSI
from archetypes_base import *


## Here is the function to make an 'easy' Dryer. It takes in at least the name of the dryer and what flow would go in.
## It can also take in different tweakable values for it's coefficients, like the heat capacity of the different materials involved,
## or the heat loss amount and the different temperatures required. If not specified, these coefficients will take default values.
def MakeSteamDryer(u_name, inc_s_name, heat_loss = 0.1, elec_demand = 3, dry_matter_cp = 1.5,
                   hvap = 2200, cp_water = 4.2, cp_air = 1.0, cp_steam = 2.1, amb_t = 25, steamtemp = 130,
                   steampress = 2.8, moisture_out = 0.2, air_temp = 125, exhaust_temperature = 90, dryflow_temp = 50):
    Dryer = Unit(u_name)
    if elec_demand == 0:
        Dryer.expected_flows_in =  [inc_s_name, 'Steam (' + u_name + ')', 'Air (' + u_name + ')']
    else:
        Dryer.expected_flows_in =  [inc_s_name, 'Steam (' + u_name + ')', 'Air (' + u_name + ')', 'Electricity (' + u_name + ')']
    
    Dryer.expected_flows_out = ['Stack (' + u_name + ')', 'Dried ' + inc_s_name, 'Condensate (' + u_name + ')']
    Dryer.coefficients = {'Ambient temperature' : amb_t, 'Steam temperature' : steamtemp, 'Steam pressure' : steampress, 'Moisture out' : moisture_out,
                          'Electricity demand' : elec_demand, 'Air temperature' : air_temp, 'Exhaust temperature' : exhaust_temperature,
                          'Dried flow temperature' : dryflow_temp, 'Latent heat of vaporization' : hvap, 'Cp water' : cp_water, 
                          'Cp air' : cp_air, 'Cp dry matter' : dry_matter_cp, 'Losses' : heat_loss
                          }
    def Dryerfunc(flow, coeff):
        mass_flow_in = flow.attributes['mass_flow_rate']
        amb_t = coeff['Ambient temperature']
        t_steam = coeff['Steam temperature']
        steam_p = coeff['Steam pressure']
        Q_flow = flow.attributes['heat_flow_rate']
        t_in = flow.attributes['temperature']
        moisture_in = flow.attributes['composition'][flow.attributes['components'].index('Water')]
        water_in = flow.attributes['mass_flow_rate'] * moisture_in
        moisture_out = coeff['Moisture out']
        electricity_amount = mass_flow_in * coeff['Electricity demand'] / 1000
        dry_matter = flow.attributes['mass_flow_rate'] - water_in
        water_out = (moisture_out/(1 - moisture_out)) * dry_matter
        dried_flow_amount = dry_matter + water_out
        water_exhaust = water_in - water_out
        air_t = coeff['Air temperature']
        exhaust_t = coeff['Exhaust temperature']
        dry_flow_t = coeff['Dried flow temperature']
        hvap = coeff['Latent heat of vaporization']
        cp_water = coeff['Cp water']
        cp_air = coeff['Cp air']
        cp_dry = coeff['Cp dry matter']
        Q_water_evap = water_exhaust * (hvap + cp_water * (exhaust_t - t_in))
        Q_dried_flow = ((water_out * cp_water) + (dry_matter * cp_dry)) * (dry_flow_t - t_in)
        m_air = (Q_water_evap + Q_dried_flow - Q_flow) / (cp_air * (air_t - exhaust_t))
        Q_air_exhaust = m_air * cp_air * (air_t - amb_t)
        Q_in = (Q_water_evap + Q_dried_flow + Q_air_exhaust - Q_flow)/(1 - coeff['Losses'])
        Q_losses = coeff['Losses'] * Q_in
        m_steam = Q_in / hvap
        Q_condensate = m_steam * cp_water * (t_steam - amb_t)
        Q_steam = Q_condensate + Q_in
        m_exhaust = water_exhaust + m_air
        Q_exhaust = Q_water_evap + Q_air_exhaust
        components = flow.attributes['components']
        in_composition = flow.attributes['composition']
        dry_composition_in = 1 - flow.attributes['composition'][flow.attributes['components'].index('Water')]
        dry_composition_out = 1 - moisture_out
        out_composition = []
        for component in components:
            if component == 'Water':
                out_composition.append(moisture_out)
            else:
                in_comp = flow.attributes['composition'][flow.attributes['components'].index(component)]
                out_composition.append(in_comp * (dry_composition_out/dry_composition_in))
        
        return [{'name' : 'Electricity (' + u_name + ')', 'components' : None, 'mass_flow_rate' : 0,
                 'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
                {'name' : 'Air (' + u_name + ')', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : m_air,
                         'flow_type': 'Air', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
                {'name' : 'Condensate (' + u_name + ')', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                         'flow_type': 'Condensate', 'temperature' : t_steam, 'pressure':steam_p , 'heat_flow_rate' :Q_condensate ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
                {'name' : 'Steam (' + u_name + ')', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                         'flow_type': 'Steam', 'temperature' : t_steam, 'pressure': steam_p , 'heat_flow_rate' :Q_steam ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
                {'name' : 'Stack (' + u_name + ')', 'components' : ['Water', 'Air'], 'composition': [water_exhaust/m_exhaust, m_air/m_exhaust], 'mass_flow_rate' : m_exhaust,
                         'flow_type': 'Exhaust', 'temperature' : exhaust_t, 'pressure':1 , 'heat_flow_rate' :Q_exhaust ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
                {'name' : 'Dried ' + inc_s_name, 'components' : components, 'composition': out_composition, 'mass_flow_rate' : dried_flow_amount,
                         'flow_type': 'Process', 'temperature' : dry_flow_t, 'pressure':1 , 'heat_flow_rate' :Q_dried_flow ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False}
                ]
    Dryer.calculations = {inc_s_name : Dryerfunc}
    return Dryer

## Here is how to make a simple Dryer unit called 'Wood dryer' that takes in a stream called 'Wood' with the base coefficients
# WoodDryer = MakeSteamDryer('Wood dryer', 'Wood')



# FlowA = Flow('Wood',['Water','Wood'],'input', 25, 1, [0.6, 0.4 ], None , None, 1000, np.nan, 0)
# FlowA.set_calc_flow()
# print(WoodDryer)

# allflows.append(FlowA)
# WoodDryer.attach_available_flow()
# WoodDryer.calc()
# print(WoodDryer)
# for flow in allflows:
    
#     print(flow)
    
# print(WoodDryer)