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
    print('Wood out :' + str(wood_out))
    bark_out = 1 - wood_out
    bark_out_amount = wood_amount * bark_out
    wood_out_amount = wood_amount * wood_out
    return [{'name' : 'Electricity (debarker)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_amount, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Bark', 'components' : ['Bark', 'Water'], 'composition': [1-moisture_amount, moisture_amount], 'mass_flow_rate' : bark_out_amount,
                     'flow_type': 'Process stream', 'temperature' : 25, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Wood', 'components' : ['Wood', 'Water'], 'composition' : [1-moisture_amount, moisture_amount] , 'mass_flow_rate' : wood_out_amount,
                     'flow_type': 'Process stream', 'temperature' : 25, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False}]

Unit1.calculations = {'Logs' : Debarkerfunc_logs}
# FlowA = Flow(name = 'Logs', components = ['Bark', 'Wood', 'Water'], composition = [0.075, 0.425, 0.5],
#              flow_type = 'input', mass_flow_rate = 1000)

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



FlowA = Flow('Logs',['Water', 'Bark', 'Wood'],'input', 25, 1, [wood_moisture, normal_bark_in, 1 - wood_moisture - normal_bark_in ], None , None, wood_flow_amount, np.nan, 0)
FlowA.set_calc_flow()
allflows.append(FlowA)

main(allflows, processunits)
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
    
