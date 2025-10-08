# -*- coding: utf-8 -*-
"""
Created on Wed Aug 20 11:11:31 2025

@author: Antoine
"""
import importlib
import pandas as pd
import numpy as np
import csv
import inspect
from archetypes_base import *
import copy
import os
import openbox
from openbox import space as sp


os.chdir('C:/Users/Antoine/Desktop/matching (beta)')
flow_dataframe = pd.DataFrame(columns=['name', 'components', 'flow_type',
                                       'temperature', 'pressure',
                                       'composition', 'origin', 'destination',
                                       'mass_flow_rate','elec_flow_rate',
                                       'heat_flow_rate','combustion_energy_content'])


### Initialize archetypes


archetype = 'paper_forming_only'

archetype_model = importlib.import_module(archetype)

initial_flows = copy.deepcopy(archetype_model.allflows)
unit_list = copy.deepcopy(archetype_model.processunits)

### Define the search space

params = {'x1' : {'type' : 'flow', 'name' : 'Market pulp (Pulper)',
                  'attribute': 'mass_flow_rate', 'min':50, 'max': 10000,
                  'default': 1000}}

space = sp.Space()

param_list = []
for parameter in params:
    param_ref = parameter
    param_value = params[parameter]
    parammin = param_value['min']
    parammax = param_value['max']
    paramdef = param_value['default']
    xi = sp.Real(param_ref, parammin, parammax, paramdef)
    param_list.append(xi)
    
space.add_variables(param_list)


parameters_names = list(params.keys())
# print(parameters_names)
# for key, value in space.items():
#     print(key)
### Define the objectives

objectives = {'y1' : {'type' : 'flow', 'name' : 'Steam (Dryer, paper)',
                      'attribute' : 'heat_flow_rate', 'value' : 20000000}}




### Define the archetypes results according to parameters
def central(parameter_values, paramname):
    
## ------------ Global variables --------------
## This is where physical constants that are used in Units calculation go,
## When they are used broadly through the process and not specific to the
## Unit calculation or coefficients. To change with something more replicable

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


    instance_flows = copy.deepcopy(initial_flows)
    instance_units = copy.deepcopy(unit_list)

    for name in paramname:
        parameter_i = params[name]
        if parameter_i['type'] == 'flow':
            param_index = find_Flow_index(parameter_i['name'], instance_flows)
            instance_flows[param_index].attributes[parameter_i['attribute']] = parameter_values[name]
            
    ## To do the same for units
    







   

    
    
    
    return main(instance_flows, instance_units, f_print = False)





# ### Define the objective function

def obj_func(config):
    
### To extend to list of parameters?
    # keys = []
    # for key, value in config.items():
    #     keys.append(key)
    # x1 = config['x1']
    # print(type(x1))
#    param_dict = copy.deepcopy(config.get_dictionary())
#    print(param_dict)
    results1, results2 = central(config, parameters_names)

### To extend to multiple objectives?
    if objectives['y1']['type'] == 'flow':
        flowindex = find_Flow_index(objectives['y1']['name'], results1)
#        print_flows(results1)
#        print(flowindex)
        y = getattr(results1[flowindex], 'attributes')
        y_attr = y[objectives['y1']['attribute']]
        delta = abs(y_attr - objectives['y1']['value'])
    return {'objectives' : [delta]}


print(obj_func({'x1' : 5000}))



opt = openbox.Optimizer(
    obj_func,
    space,
    max_runs=50,
    surrogate_type='auto',          # try using 'auto'!
    task_id='quick_start',
    # Have a try on the new HTML visualization feature!
    visualization='advanced',   # or 'basic'. For 'advanced', run 'pip install "openbox[extra]"' first
    auto_open_html=True,        # open the visualization page in your browser automatically
)
history = opt.run()
