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

amb_t = 20 
soybean_amount = 1000

            
#Unit 1 :CLeaner definition                  
Unit1 = Unit('Cleaner')
Unit1.expected_flows_in = ['Soybeans', 'Water (Cleaner)']
Unit1.expected_flows_out = ['Wastewater (Cleaner)', 'Cleaned soybeans']
Unit1.coefficients = {'Cleaner water ratio' : 1.67}

def Cleaner_func(soybeans_flow, coeff):
    soybeans_in = soybeans_flow.attributes['mass_flow_rate']
    water_in = soybeans_in * coeff['Cleaner water ratio']
    
    return [{'name' : 'Water (Cleaner)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : water_in,
                     'flow_type': 'Water', 'temperature' : amb_t, 'pressure' : 1 , 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Wastewater (Cleaner)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : water_in,
                     'flow_type': 'Water', 'temperature' : amb_t, 'pressure' : 1 , 'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Cleaned soybeans', 'components' : soybeans_flow.components, 'composition' : soybeans_flow.composition, 'mass_flow_rate' : soybeans_in,
                     'flow_type': 'Process flow', 'temperature' : amb_t, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False}
            ]

Unit1.calculations = {'Soybeans' : Cleaner_func}



#Unit 2 : Dryer definition  
Unit2= Unit('Dryer (cleaned soybeans)')
Unit2.expected_flows_in = ['Cleaned soybeans', 'Electricity (Dryer, soybeans)', 'Air (Dryer, soybeans)', 'Steam (Dryer, soybeans)']
Unit2.expected_flows_out = ['Dried soybeans', 'Exhaust gas (Dryer, soybeans)', 'Condensate (Dryer, soybeans)']

Unit2.coefficients = {'Dry beans moisture' : 0.858 , 'Air temperature' : 80, 'd_T' : 5, 'Exhaust gas temperature' : 60, 'Electricity (kWh) per ton of soybeans' : 21, 'Soybeans temperature' : 45, 'Loss' : 0.1}


def Dryerfunc_soybean(clsoybean_flow, coeff):
    steam_t = coeff['Air temperature'] + coeff['d_T']
    clsoybean_amount = clsoybean_flow.attributes['mass_flow_rate']
    dry_clsoybean_amount = clsoybean_amount * clsoybean_flow.attributes['composition'][clsoybean_flow.attributes['components'].index('Dry bean')]
    water_amount = clsoybean_amount - dry_clsoybean_amount
    mu_m_out = coeff['Dry beans moisture']
    mass_m_out = (mu_m_out/(1-mu_m_out)) * dry_clsoybean_amount
    clsoybean_out_amount = dry_clsoybean_amount + mass_m_out
    water_out_amount =  water_amount - mass_m_out
    air_t = coeff['Air temperature']
    exhaust_t = coeff['Exhaust gas temperature']
    electricity_amount = coeff['Electricity (kWh) per ton of paper'] * dry_clsoybean_amount / 1000.
    t_in = clsoybean_flow.attributes['temperature']
    dried_clsoybean_out_t = coeff['Soy beans temperature']
    cp_water = 4.2 #kJ/kg.K
    cp_steam = 1.89 #kJ/kg.K
    cp_dry_bean = 0.81
    cp_air = 1.01
    vap_heat = 2256.4 #kJ/kg
    Q_water_out = water_out_amount * ((cp_steam * (exhaust_t - 100)) + (cp_water *(100 - t_in ) ) + vap_heat)
    Q_dry_soybean_out = dry_clsoybean_amount * cp_dry_bean * (dried_clsoybean_out_t - t_in)
    Q_moisture_out = mass_m_out * cp_water * (dried_clsoybean_out_t - t_in)
    Q_dried_soybean = Q_dry_soybean_out + Q_moisture_out
    m_air = (Q_water_out + Q_dried_soybean)/(cp_air * (air_t - exhaust_t))
    Q_air_out = m_air * cp_air * (exhaust_t - amb_t)
    Q_air_in = m_air * cp_air * (air_t - amb_t)
    Q_in = clsoybean_flow.attributes['heat_flow_rate']
    Q_exhaust = Q_air_out + Q_water_out
    Q_steam = ((Q_exhaust + Q_dried_soybean)/(1 - coeff['Loss'])) - Q_in
    Q_loss = Q_steam * coeff['Loss']
    vap_heat_130 = 2173.7
    m_steam = Q_steam/vap_heat_130
    cp_wat_130 = 4.26
    Q_condensate = m_steam * cp_wat_130 * (steam_t - amb_t)
    Q_steam_in = Q_steam + Q_condensate
    exhaust_amount = water_out_amount + m_air
    exhaust_gas_water_ratio = water_out_amount/exhaust_amount
    
    return [{'name' : 'Electricity (Dryer, soybeans)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
            {'name' : 'Air (Dryer, soybeans)', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : m_air,
                     'flow_type': 'Air', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Condensate (Dryer, soybeans)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                     'flow_type': 'Condensate', 'temperature' : steam_t, 'pressure':2.7 , 'heat_flow_rate' :Q_condensate ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Steam (Dryer, soybeans)', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                     'flow_type': 'Steam', 'temperature' : steam_t, 'pressure':2.7 , 'heat_flow_rate' :Q_steam_in ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Exhaust gas (Dryer, soybeans)', 'components' : ['Water', 'Air'], 'composition': [exhaust_gas_water_ratio, 1 - exhaust_gas_water_ratio], 'mass_flow_rate' : exhaust_amount,
                     'flow_type': 'Exhaust', 'temperature' : exhaust_t, 'pressure':1 , 'heat_flow_rate' :Q_exhaust ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Dried soybeans', 'components' : ['Dry bean', 'Water'], 'composition': [1 - mu_m_out, mu_m_out], 'mass_flow_rate' : clsoybean_out_amount,
                     'flow_type': 'Process', 'temperature' : dried_clsoybean_out_t, 'pressure':1 , 'heat_flow_rate' :Q_dried_soybean ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'Heat loss'  : Q_loss}
            
            ]

Unit2.calculations = {'Cleaned soybeans' : Dryerfunc_soybean}


#Unit 3 : Storage definition  
Unit3 = Unit('Storage')
Unit3.expected_flows_in = ['Dried soybeans']
Unit3.expected_flows_out = ['Stored soybeans']
Unit3.coefficients = {}



Unit3.calculations = {}



#Unit 4 : Cracking mill definition  
Unit4 = Unit('Cracking mill')
Unit4.expected_flows_in = ['Stored soybeans', 'Electricity (Cracking)']
Unit4.expected_flows_out = ['Cracked soybeans']
Unit4.coefficients = {'Electricity per ton of beans' : 4.7}


Unit4.calculations = {}


#Unit 5 : Dehuller definition  
Unit5 = Unit('Dehuller')
Unit5.expected_flows_in = ['Cracked soybeans', 'Electricity (Dehuller)']
Unit5.expected_flows_out = ['Hulls', 'Dehulled beans']
Unit5.coefficients = {}



Unit5.calculations = {}

#Unit 6 : Conditioner definition  
Unit6 = Unit('Conditioner')
Unit6.expected_flows_in = ['Dehulled beans', 'Electricity (Dehuller)', 'Steam (Dehuller)']
Unit6.expected_flows_out = ['Conditioned beans', 'Condensate (Dehuller)']


Unit6.calculations = {}



#Unit 7 : Flaking mill definition  
Unit7 = Unit('Flaking mill')
Unit7.expected_flows_in = ['Conditioned beans', 'Electricity (Flaking mill)']
Unit7.expected_flows_out = ['Flaked beans']
Unit7.coefficients = {}

Unit7.calculations = {}

#Unit 8 : Oil extractor definition  
Unit8 = Unit('Oil extractor')
Unit8.expected_flows_in = ['Flaked beans', 'Hexane', 'Electricity (Oil extractor)']
Unit8.expected_flows_out = ['Soybean flakes', 'Oil']
Unit8.coefficients = {}


Unit8.calculations = {}


#Unit 9 : Desolventizer definition  
Unit9 = Unit('Desolventizer')
Unit9.expected_flows_in = ['Soybean flakes', 'Electricity (Desolventizer)', 'Steam (Desolventizer)']
Unit9.expected_flows_out = [ 'Desolventized flakes', 'Hexane vapor', 'Condensate (Desolventizer)']

Unit9.coefficients = {}


Unit9.calculations = {}

#Unit 10 : Meal dryer definition  
Unit10 = Unit('Meal dryer')
Unit10.expected_flows_in = ['Desolventized flakes', 'Electricity (Meal dryer)', 'Steam (Meal dryer)']
Unit10.expected_flows_out = ['Dried meal', 'Condensate (Meal dryer)']

Unit10.coefficients = {}

Unit10.calculations = {}

#Unit 11 : Meal cooler definition  
Unit11 = Unit('Meal cooler')
Unit11.expected_flows_in = ['Dried meal', 'Electricity (Meal cooler)']
Unit11.expected_flows_out = ['Cooled meal', 'Chilling (Meal cooler)']
Unit11.coefficients = {}



Unit11.calculations = {}

#Unit 12 : Preheater/condenser definition  
Unit12 = Unit('Preheater/condenser')
Unit12.expected_flows_in = []
Unit12.expected_flows_out = []

Unit12.coefficients = {}




Unit12.calculations = {}

#Unit 13 : Hexane storage definition  
Unit13 = Unit('Hexane storage')
Unit13.expected_flows_in = []
Unit13.expected_flows_out = []
Unit13.coefficients = {}


Unit13.calculations = {}

#Unit 14 : Mineral oil scrubber definition  
Unit14 = Unit('Mineral oil scrubber')
Unit14.expected_flows_in = []
Unit14.expected_flows_out = []
Unit14.coefficients = {}


Unit14.calculations = {}

#Unit 15 : Main vent definition  
Unit15 = Unit('Main vent')
Unit15.expected_flows_in = []
Unit15.expected_flows_out = []
Unit15.coefficients = {}


Unit15.calculations = {}

#Unit 16 : Flash chamber definition  
Unit16 = Unit('Flash chamber')
Unit16.expected_flows_in = []
Unit16.expected_flows_out = []
Unit16.coefficients = {}


Unit16.calculations = {}


#Unit 17 : Hexane Steam Condenser definition  
Unit17 = Unit('Hexane steam condenser')
Unit17.expected_flows_in = []
Unit17.expected_flows_out = []
Unit17.coefficients = {}


Unit17.calculations = {}

#Unit 18 : Evaporator definition  
Unit18 = Unit('Evaporator')
Unit18.expected_flows_in = []
Unit18.expected_flows_out = []
Unit18.coefficients = {}


Unit18.calculations = {}

#Unit 19 : Vacuum stripper definition  
Unit19 = Unit('Vacuum stripper')
Unit19.expected_flows_in = []
Unit19.expected_flows_out = []
Unit19.coefficients = {}


Unit19.calculations = {}

#Unit 20 : Hexane steam condenser definition  
Unit20 = Unit('Hexane steam condenser')
Unit20.expected_flows_in = []
Unit20.expected_flows_out = []
Unit20.coefficients = {}


Unit20.calculations = {}

#Unit 21 : Hexane water separator definition  
Unit21 = Unit('Hexane water separator')
Unit21.expected_flows_in = []
Unit21.expected_flows_out = []
Unit21.coefficients = {}


Unit21.calculations = {}






FlowA = Flow('Soybeans',['Water', 'Dry bean'],'input', amb_t, 1, [0.87, 0.13], None , None, soybean_amount, np.nan, 0)
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