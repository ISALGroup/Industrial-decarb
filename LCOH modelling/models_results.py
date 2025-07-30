# -*- coding: utf-8 -*-
"""
Created on Tue Jul 29 13:34:43 2025

@author: Antoine
"""

branching_naics = [322120]

temperatures_naics = {325193 : [107, 107], 311313 : [137.8, 79.5]}

non_electrifiable_units = {322120 : ['S (Stoker Boiler)'], 311313 : ['K (Kiln)']}

branches = {322120 : ['kraft', 'finishing']}

flags = {'kraft' : ('exists', ('fuel_type', 'Wood and Wood Residuals (dry basis)')), 'finishing' : ('not exists', ('fuel_type', 'Wood and Wood Residuals (dry basis)'))}

temperatures_flags = {'kraft' : [165, 73], 'finishing' : [80, 40]}

