# -*- coding: utf-8 -*-
"""
Created on Thu Sep 18 08:23:28 2025

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

### Put the working directory here
os.chdir('C:/Users/me/mydirectory')
flow_dataframe = pd.DataFrame(columns=['name', 'components', 'flow_type',
                                       'temperature', 'pressure',
                                       'composition', 'origin', 'destination',
                                       'mass_flow_rate','elec_flow_rate',
                                       'heat_flow_rate','combustion_energy_content'])

### Initialize archetypes

### Put the name of the archetype here
archetype = 'my_archetypes'

archetype_model = importlib.import_module(archetype)

### The script will make a copy of its initial flows and units
initial_flows = copy.deepcopy(archetype_model.allflows)
unit_list = copy.deepcopy(archetype_model.processunits)


### Define the search space
### Put the name of the Excel file with the parameters list here
parameters = 'excel_file'
sheet_name = 'excel_sheet'

if sheet_name:
    param_df = pd.read_excel(parameters, sheet_name)
    
else:
    param_df = pd.read_excel(parameters)

def transform_row(row):
    obj_type = row['Parameter group']
    element = row['Element']
    param = row['Parameter']
    param_name = row['Parameter name']
    init_val = row['Initial value']
    min_val = row['Minimum range']
    max_val = row['Maximum range']
    if param_name:
        transformed_object = {'type' : obj_type, 'name' : element, 'attribute' : param, 
                          'coefficient' : param_name, 'min': min_val, 'max' : max_val, 
                          'default' : init_val}
    else:
        transformed_object = {'type' : obj_type, 'name' : element, 'attribute' : param, 
                           'min': min_val, 'max' : max_val, 'default' : init_val}
    
    
    return transformed_object


### The parameters with min, max and initial values are transformed into a search space here

param_df['treated_row'] = param_df.apply(transform_row, axis = 1)
params = param_df['treated_row'].tolist()


space = sp.Space()

params_treated = dict()
param_list = []
for parameter in params:
    param_index = params.index(parameter)
    param_ref = f'x{param_index}'
    param_value = parameter
    parammin = param_value['min']
    parammax = param_value['max']
    paramdef = param_value['default']
    xi = sp.Real(param_ref, parammin, parammax, paramdef)
    param_list.append(xi)
    params_treated[param_ref] = parameter
    
space.add_variables(param_list)


parameters_names = list(params_treated.keys())


### Define the objectives
### You can use the structure below as a template for a typical objective

objectives = {'y1' : {'type' : 'flow', 'name' : 'Product',
                      'attribute' : 'mass_flow_rate', 'value' : 100}}

def heat_demand_objfunc(flows, units, heat_demand_value):
    heat_demand = calc_heat_demand(flows, units)
    delta_heat_demand = heat_demand - heat_demand_value
    return delta_heat_demand
    
auto = True

if auto:
### Define the archetypes results according to parameters
    def central(parameter_values, paramname):
        
    ## ------------ Global variables --------------
    ## This is where physical constants that are used in Units calculation go,
    ## When they are used broadly through the process and not specific to the
    ## Unit calculation or coefficients. To change with something more replicable
    
        # Global Variables


    
        instance_flows = copy.deepcopy(initial_flows)
        instance_units = copy.deepcopy(unit_list)
    
        for name in paramname:
            parameter_i = params_treated[name]
            if parameter_i['type'] == 'flow':
                param_index = find_Flow_index(parameter_i['name'], instance_flows)
                if parameter_i['attribute'] == 'composition':
                    component_ind = instance_flows[param_index].attributes['components'].index(parameter_i['coefficient'])
                    instance_flows[param_index].attributes['composition'][component_ind] = parameter_values[name]
                
                else:
                    instance_flows[param_index].attributes[ parameter_i['attribute']] = parameter_values[name]

                    
        ## To do the same for units
            if parameter_i['type'] == 'Unit':
                param_index = find_Unit_index(parameter_i['name'], instance_units)
                #print(parameter_values[name])
                if parameter_i['attribute'] == 'coefficient':
                    instance_units[param_index].coefficients[parameter_i['coefficient']] = parameter_values[name]
                else:
                    setattr(instance_units[param_index], parameter_i['attribute'], parameter_values[name])
                
    
    
    
    
    
    
       
    
        
        
        
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
        obj_values = []
        for objective in objectives:
            objective_name = objective
            if objectives[objective_name]['type'] == 'flow':
                flowindex = find_Flow_index(objectives[objective_name]['name'], results1)
                y = getattr(results1[flowindex], 'attributes')
                y_attr = y[objectives[objective_name]['attribute']]
                delta = abs(y_attr - objectives[objective_name]['value'])/objectives[objective_name]['value']
                obj_values.append(delta)
        ### You can put heat demand here
        delta_heat_demand = heat_demand_objfunc(results1, results2, 1000000)
        obj_values.append(delta_heat_demand)
        
        
        return {'objectives' : obj_values}


#print(obj_func({'x1' : 5000}))
### TO RE-DO: choice of a reference point
reference_pt = []
for i in range(len(objectives)):
    reference_pt.append(0)

opt = openbox.Optimizer(
    obj_func,
    space,
    num_objectives=2,
    num_constraints=0,
    max_runs=60,
    surrogate_type='prf',                # try using 'auto'!
    acq_type='ehvi',                    # try using 'auto'!
    acq_optimizer_type='random_scipy',  # try using 'auto'!
    initial_runs=6,
    init_strategy='sobol',
    ### Use the 
    ref_point= (1,1),
    task_id='my_file',
    random_state=1,
    # Have a try on the new HTML visualization feature!
    visualization='advanced',   # or 'basic'. For 'advanced', run 'pip install "openbox[extra]"' first
    # auto_open_html=True,        # open the visualization page in your browser automatically
)
history = opt.run()















