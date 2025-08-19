# -*- coding: utf-8 -*-
"""
Created on Mon Aug 18 12:20:21 2025

@author: Antoine
"""

import pandas as pd
import numpy as np
from archetypes_base import *
import os
import importlib
os.chdir('C:/Users/Antoine/Desktop/matching (beta)')
import openbox
from openbox import space as sp



### Load in archetype



import optimize_test_archetype






### Give search space (aka parameters and ranges)

space = sp.Space()
input_flow_amount = sp.Real('split_amount', 0 , 1, default_value = 0.5)
space.add_variables([input_flow_amount])

#search_space_parameters = {'input flow amount' : ['flow', 'Market pulp (Pulper)','mass_flow_rate']} ##to use for indicating which parameter is linked to which object in the archetype. The name of the parameter is
## the key of the dictionary, and the value is a list containing the type of the object, then the name of the object and finally which attribute the parameter references


### Define objectives

objectives = {'y1' : ['flow', 'C', 'mass_flow_rate']}
y1 = {'y1' : 700}


### Define objective function

def obj_func(config):
    x1 = config['split_amount']
    results1, results2 = optimize_test_archetype.central(x1)
    
    if objectives['y1'][0] == 'flow':
        flowindex = find_Flow_index(objectives['y1'][1], results1)
#        print_flows(results1)
#        print(flowindex)
        y = getattr(results1[flowindex], 'attributes')
        y_attr = y[objectives['y1'][2]]
        delta = abs(y_attr - y1['y1'])
    return {'objectives' : [delta]}

opt = openbox.Optimizer(
    obj_func,
    space,
    max_runs=50,
    surrogate_type='gp',          # try using 'auto'!
    task_id='quick_start',
    # Have a try on the new HTML visualization feature!
    visualization='advanced',   # or 'basic'. For 'advanced', run 'pip install "openbox[extra]"' first
    auto_open_html=True,        # open the visualization page in your browser automatically
)
history = opt.run()