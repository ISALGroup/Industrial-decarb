# -*- coding: utf-8 -*-
"""
Created on Tue May  6 09:45:10 2025

@author: Antoine
"""
import utilities_base
import utilities_test_archetype
import default_boiler
import boiler_class
import pandas as pd
from archetypes_base import *

test_boiler = default_boiler.Default_Boiler
test_boiler.req_duty = 10000
archetype_flows, archetype_units = utilities_test_archetype.main(utilities_test_archetype.allflows,  utilities_test_archetype.processunits, f_print = False)

utilities_base.utilities_main(archetype_flows, archetype_units, grid_price = 10, grid_price_sell = 5, grid_ef = {'CO2': 100}, boilers = [default_boiler.Default_Boiler], air_compressors = [], fuels_price = {'CH4': 50})
