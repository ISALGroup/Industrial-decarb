# -*- coding: utf-8 -*-
"""
Created on Tue Sep 30 15:44:30 2025

@author: Antoine
"""

import pandas as pd
import os
import xlsxwriter
os.chdir('C:/Users/Antoine/Desktop/final_results_prototype')




unit_emissions = pd.read_excel('data/Facility_and_Unit_Emissions_Database_2023_v3.xlsx', sheet_name= 'unit_emissions')

facility_info = pd.read_excel('data/Facility_and_Unit_Emissions_Database_2023_v3.xlsx', sheet_name= 'descr_info')

filtered_naics = 322120

paper_only_facility = facility_info[facility_info['primary_naics'] == filtered_naics]

paper_only_unit = unit_emissions[unit_emissions['primary_naics'] == filtered_naics]

unique_unit_types = paper_only_unit['unit_type'].unique().tolist()

mydf = pd.DataFrame()
mydf['unit types'] = unique_unit_types

with pd.ExcelWriter('data/paper_filtered_database.xlsx', engine='xlsxwriter') as writer:
    paper_only_facility.to_excel(writer, sheet_name='facility')
    paper_only_unit.to_excel(writer, sheet_name='unit')
    mydf.to_excel(writer, sheet_name='unit types')
    