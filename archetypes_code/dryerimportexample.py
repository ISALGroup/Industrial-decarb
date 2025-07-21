# -*- coding: utf-8 -*-
"""
Created on Mon Feb 10 14:04:37 2025

@author: Antoine
"""
import pandas as pd
import numpy as np
import csv
import inspect
from archetypes_base import *
import dryer_unit


allflows = []
processunits = []






WoodDryer = dryer_unit.MakeSteamDryer('Wood dryer', 'Wood')



FlowA = Flow('Wood',['Water','Wood'],'input', 25, 1, [0.6, 0.4 ], None , None, 1000, np.nan, 0)
FlowA.is_calc_flow = True
allflows.append(FlowA)
processunits.append(WoodDryer)
processunits[0].attach_available_flow(allflows)
processunits[0].calc(allflows, processunits)
for flow in allflows:
    print(flow)

            