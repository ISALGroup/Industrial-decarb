# -*- coding: utf-8 -*-
"""
Created on Fri Oct 17 09:37:43 2025

@author: Antoine
"""

import pandas as pd
import numpy as np
import os
os.chdir('C:/Users/Antoine/Desktop/final_data_pipeline')


generic_naics_list = [311221, 311224, 311225, 311230, 311313, 311314, 311411, 311422,
                      311423, 311511, 311513, 311514, 311611, 311613, 311615, 311942,
                      312120, 312140, 322291, 325110, 325120, 325180, 325193, 325194,
                      325211, 325212, 325311, 325312]

specific_longdf_names = ['322130_kraft_longform', '322120_kraft_longform', '322110_kraft_longform', '322130_finishing_longform',
                       '322120_finishing_longform', '322110_finishing_longform', '322120_sulfite_longform',
                       '322130_sulfite_longform']
specific_elec_names = ['322130_kraft_elec', '322120_kraft_elec','322110_kraft_elec', '322130_finishing_elec',
                       '322120_finishing_elec', '322110_finishing_elec', '322120_sulfite_elec',
                       '322130_sulfite_elec']
specific_elecnowhp_names = ['322130_kraft_elec_nowhp', '322120_kraft_elec_nowhp','322110_kraft_elec_nowhp', '322130_finishing_elec_nowhp',
                            '322120_finishing_elec_nowhp', '322110_finishing_elec_nowhp', '322120_sulfite_elec_nowhp',
                            '322130_sulfite_elec_nowhp']

longform_df_list = []
elec_df_list = []
no_ee_df_list = []




for naics in generic_naics_list:
    longform_data = pd.read_excel('output/' + str(naics) + 'longform.xlsx')
    elec_data = pd.read_excel('output/' + str(naics) + 'longform_elec_options.xlsx')
    no_ee_data = pd.read_excel('output/' + str(naics) + 'longform_elec_options_nowhp.xlsx')
    longform_df_list.append(longform_data)
    elec_df_list.append(elec_data)
    no_ee_df_list.append(no_ee_data)
    
    
for longdf_name in specific_longdf_names:
    data = pd.read_excel('output/' + str(longdf_name) + '.xlsx')
    longform_df_list.append(data)
for elec_name in specific_elec_names:
    data = pd.read_excel('output/' + str(elec_name) + '.xlsx')
    elec_df_list.append(data)
for elec_name_nowhp in specific_elecnowhp_names:
    data = pd.read_excel('output/' + str(elec_name_nowhp) + '.xlsx')
    no_ee_df_list.append(data)
    
longform_df = pd.concat(longform_df_list)
elec_df = pd.concat(elec_df_list)
no_ee_df = pd.concat(no_ee_df_list)
    
longform_df.to_excel('output/merged_longform.xlsx')
elec_df.to_excel('output/merged_elec_longform.xlsx')
no_ee_df.to_excel('output/merged_elecnowhp_longform.xlsx')
