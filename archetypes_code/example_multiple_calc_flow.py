# -*- coding: utf-8 -*-
"""
Created on Mon Feb 10 16:57:26 2025

@author: Antoine
"""
import pandas as pd
import numpy as np
import csv
import inspect
from archetypes_base import *
allflows = []
processunits = []






MultiplecalcflowUnit = Unit('Multiple calc')
MultiplecalcflowUnit.expected_flows_in = ['A', 'B']
MultiplecalcflowUnit.expected_flows_out = ['C']
MultiplecalcflowUnit.required_calc_flows = 2

def ABMerge(ablist, coeff):
    a_flow = ablist[0]
    b_flow = ablist[1]
    a_amount = a_flow.attributes['mass_flow_rate']    
    b_amount = b_flow.attributes['mass_flow_rate']
    c_amount = a_amount + b_amount
    return [{'name' : 'C', 'components' : None, 'mass_flow_rate' : c_amount,
             'flow_type': 'Product', 'elec_flow_rate' : 0 , 'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False}, {'Heat of reaction': 1000} ]

MultiplecalcflowUnit.calculations = (['A', 'B'], ABMerge)

FlowA = Flow('A',[],'input', 25, 1, [1], None , None, 1000, np.nan, 0)
FlowA.set_calc_flow()
FlowB = Flow('B',[],'input', 25, 1, [1], None , None, 500, np.nan, 0)
FlowB.set_calc_flow()
allflows.append(FlowA)
allflows.append(FlowB)
processunits.append(MultiplecalcflowUnit)
processunits[0].attach_available_flow(allflows)
print(processunits[0].count_calc_flows(allflows))
processunits[0].calc(allflows, processunits)
print(processunits[0])

print_flows(allflows)
print(processunits[0].reaction_heat)