# -*- coding: utf-8 -*-
"""
Created on Wed Aug  6 10:13:15 2025

@author: Antoine
"""

import pandas as pd
import models_results as mr
import numpy as np
import os
os.chdir('C:/Users/Antoine/Desktop/LCOH modelling')
# with open('input.txt', 'r') as file:
#     lines = file.readlines()
# print(lines)


unit_emissions = pd.read_excel('data/Facility_and_Unit_Emissions_Database_2023_v3.xlsx', sheet_name= 'unit_emissions')

facility_info = pd.read_excel('data/Facility_and_Unit_Emissions_Database_2023_v3.xlsx', sheet_name= 'descr_info')

mn_fi = facility_info[facility_info['state'] == 'MN']
mn_fi_eth = mn_fi[mn_fi['primary_naics'] == 325193]
print(mn_fi_eth)
facility_list = mn_fi_eth['facility_id'].unique()
print(facility_list)
flist = facility_list.tolist()

filtered_unit_emissions = unit_emissions[unit_emissions['facility_id'].isin(flist)]

filtered_unit_emissions.to_excel('output/filtered.xlsx')

print(filtered_unit_emissions)