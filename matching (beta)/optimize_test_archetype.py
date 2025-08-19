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
def central(split_ratio = 0.5):
    



    allflows = []
    processunits = []       

    #Unit 1: Splitter
   
    Unit1 = Unit('Splitter')
    Unit1.expected_flows_in = ['A']
    Unit1.expected_flows_out = ['B' , 'C']
    Unit1.coefficients = {'Split_ratio' : split_ratio} 
    def split_func(a_flow, coeff):
        a_amount = a_flow.attributes['mass_flow_rate']
        b_amount = a_amount * coeff['Split_ratio']
        c_amount = a_amount * (1 - coeff['Split_ratio'])
        
        return [{'name' : 'B', 'components' : None, 'mass_flow_rate' : b_amount,
                 'flow_type': 'Product', 'temperature' :0,  'In or out' : 'Out', 'elec_flow_rate' : 0 ,  'Set calc' : False, 'Set shear' : False},
                {'name' : 'C', 'components' : None, 'mass_flow_rate' :c_amount,
                         'flow_type': 'Product', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' :0 ,  'Set calc' : False, 'Set shear' : False}
            ]
    
    
    Unit1.calculations = {'A' : split_func}
    
    
    
    processunits = [Unit1]
    
    
    
    FlowA = Flow('A', None,'input', 0, 1, None, None , None, 1000, np.nan, 0)
    FlowA.set_calc_flow() 
    allflows.append(FlowA)
    
    
    return main(allflows, processunits, f_print = True)


# print(are_units_calced(processunits))

# for unit in processunits:
#     unit.is_calc = True

# print(are_units_calced(processunits))

#print_flows(allflows)


print(central(0.4))
# for unit in processunits:
#     print(unit.is_calc)

