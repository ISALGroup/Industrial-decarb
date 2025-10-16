# -*- coding: utf-8 -*-
"""
Created on Tue Jul 29 13:04:30 2025

@author: Antoine
"""

import pandas as pd
import numpy as np
import os
os.chdir('C:/Users/Antoine/Desktop/final_data_pipeline')


#Load process parameters
kraft_process_units = pd.read_excel('data/322130_finishing_process_parameters.xlsx', sheet_name= 'unit heat demand')
kraft_combustion_units = pd.read_excel('data/322130_finishing_process_parameters.xlsx', sheet_name= 'unit types')
kraft_non_electrifiable_fuels = pd.read_excel('data/322130_finishing_process_parameters.xlsx', sheet_name= 'non electrifiable fuels')


#Load data
unit_emissions = pd.read_excel('data/Facility_and_Unit_Emissions_Database_2023_v3.xlsx', sheet_name= 'unit_emissions')
unit_emissions = unit_emissions[unit_emissions['primary_naics'] == 322130]
unit_emissions['unit_type'] = unit_emissions['unit_type'].fillna(value = 'Other')
facility_info = pd.read_excel('data/Facility_and_Unit_Emissions_Database_2023_v3.xlsx', sheet_name= 'descr_info')
facility_info = facility_info[facility_info['primary_naics'] == 322130]
fuel_emission_factors = pd.read_excel('data/ghg_emission_factors_epa_2025.xlsx', sheet_name = 'Conversion table') #in kgCOeq. per MMBTU
unit_emissions = unit_emissions.drop(['capacity', 'capacity_utiliziation', 'vintage',	'floor_space',	'temperature',	'steam_generation_est', 'electricity_generation_est',	'prime_mover_type', 'hrsg_bypass' , 'unit_age', 'start_up',  'mfgr', 'orig_m_lb_ds_day', 'current_rating', 'no_drums', 'floor_d_decant_sf_slope_to_front_sr_slope_to_rear', 'design_psig', 'operate_psig', 'superheat_f'], axis = 1)
                                                   

unit_emissions2 = pd.merge(unit_emissions, facility_info.filter(['facility_id', 'facility_name', 'state', 'naics_title']), on= 'facility_id', how = 'outer')
#unit_emissions2.to_excel('output/merged.xlsx') 




unit_emissions2['has_chem_recovery'] = unit_emissions2["facility_id"].isin(
    unit_emissions2[unit_emissions2["unit_type"] == 'Chemical Recovery Furnace']["facility_id"]
).astype(bool)

unit_emissions2['has_lime_kiln'] = unit_emissions2["facility_id"].isin(
    unit_emissions2[unit_emissions2["unit_type"] == 'Pulp Mill Lime Kiln']["facility_id"]
).astype(bool)

#unit_emissions2.to_excel('output/unit_identification.xlsx')

def determine_process(row): 
    if row['has_chem_recovery']:
        if row['has_lime_kiln']:
            return 'kraft'
        else:
            return 'sulfite'
    else:
        return 'finishing'
    
unit_emissions2['plant type'] = unit_emissions2.apply(determine_process, axis= 1)

#unit_emissions2.to_excel('output/plant_identification.xlsx')

spent_liquor_emissions = ['Carbon Dioxide Biogenic (Spent Liquor)', 'Methane Spent Liquor',
 'Nitrous Oxide Spent Liquor']

biogenic_emissions = ['Carbon Dioxide Biogenic (Spent Liquor)', 'Methane Spent Liquor',
 'Nitrous Oxide Spent Liquor', 'Carbon Dioxide Biogenic']

biofuels = ['Agricultural Byproducts', 'Solid Byproducts', 'Wood and Wood Residuals (dry basis)',
'Landfill Gas', 'Other Biomass Gases', 'Biodiesel (100%)', 'Ethanol (100%)',
'Rendered Animal Fat', 'Vegetable Oil']


def is_biogenic(row):
    if row['fuel_type'] in biofuels or row['ghg_name'] in biogenic_emissions:
        return True
    else:
        return False

unit_emissions2['is_biogenic'] = unit_emissions2.apply(is_biogenic, axis =1)

#unit_emissions2.to_excel('output/biogenic_identification.xlsx')

#print(unit_emissions2['fuel_type'][1])
def interim_fuel_type(row):
    if pd.notna(row['fuel_type']):
        return row['fuel_type']
    else:
        if row['ghg_name'] in spent_liquor_emissions:
            return 'North American Hardwood'
        else:
            return 'Natural Gas (Weighted U.S. Average)'
        
unit_emissions2['interim_fuel_type'] = unit_emissions2.apply(interim_fuel_type, axis =1)

#unit_emissions2.to_excel('output/interim_fuel_type.xlsx')

fuel_override = unit_emissions2.copy()

fuel_override.drop('fuel_type', axis = 1, inplace = True)

new_fuels = fuel_override.rename(columns = {'interim_fuel_type' : 'fuel_type'})

#new_fuels.to_excel('output/fuels_new.xlsx')

#Filter out 0 values for ghg

non_zero_df = new_fuels[new_fuels['ghg_quantity'] != 0]

#Filter out some redundant ghg names

redundant_ghg = ['Carbon Dioxide Total', 'Methane', 'Nitrous Oxide']

def ghg_inredundant(row):
    is_redundant = row['ghg_name'] in redundant_ghg
    isaa = row['subpart'] == 'AA'
    return is_redundant and not isaa 

non_zero_df['is_redundant'] = non_zero_df.apply(ghg_inredundant, axis = 1)

non_redundant_df = non_zero_df[non_zero_df['is_redundant'] == False]

#non_redundant_df.to_excel('output/filtered_out.xlsx')

#separate into AA and C parts

aa_df = non_redundant_df[non_redundant_df['subpart'] == 'AA']
c_df = non_redundant_df[non_redundant_df['subpart'] == 'C']


#Aggregate emissions to CO2 equivalent

result_c = c_df.groupby(
    ['unit_type', 'unit_name', 'subpart', 'fuel_type','is_biogenic', 'primary_naics','facility_id',	'reporting_year',
     'facility_name', 'state', 'naics_title', 'has_chem_recovery', 'has_lime_kiln', 'plant type', 'is_redundant'

]
)['ghg_quantity'].sum().reset_index()

#result_c.to_excel('output/c_aggreg.xlsx')

#Need to do some different stuff for AA subparts

def aasub_co2eq(row):
    if row['ghg_name'] == 'Carbon Dioxide Biogenic (Spent Liquor)' or row['ghg_name'] == 'Carbon Dioxide Non-Biogenic':
        return row['ghg_quantity']
    elif row['ghg_name'] == 'Methane' or row['ghg_name'] == 'Methane Spent Liquor':
        return row['ghg_quantity'] * 28
    
    elif row['ghg_name'] == 'Nitrous Oxide' or row['ghg_name'] == 'Nitrous Oxide Spent Liquor':
        return row['ghg_quantity'] * 265

aa_df['co2eq'] = aa_df.apply(aasub_co2eq, axis = 1)
#aa_df.to_excel('output/aa_co2eq.xlsx')

aa_df_co2eq = aa_df.drop(columns = ['ghg_quantity'])
aa_df_co2eq_rename = aa_df_co2eq.rename(columns = {
    'co2eq' : 'ghg_quantity'
    })

result_aa = aa_df_co2eq_rename.groupby(
    ['unit_type', 'unit_name', 'subpart', 'fuel_type','is_biogenic', 'primary_naics','facility_id',	'reporting_year',
     'facility_name', 'state', 'naics_title', 'has_chem_recovery', 'has_lime_kiln', 'plant type', 'is_redundant'

]
)['ghg_quantity'].sum().reset_index()

#result_aa.to_excel('output/aa_agreg.xlsx')

#Remerge dataframes
column_list = result_aa.columns.tolist()

merged_co2eq = pd.merge(result_c, result_aa, how = 'outer', on = column_list)
sort_co2eq = merged_co2eq.sort_values(by = 'facility_id')
#sort_co2eq.to_excel('output/caa_co2eqaggreg.xlsx')


#Adding in EPA emission factor

ef_only = fuel_emission_factors[['fuel_type', 'emission_factor']]

merged_epa_ef = pd.merge(sort_co2eq, ef_only , how = 'left', on = 'fuel_type')
#merged_epa_ef.to_excel('output/mergwithef.xlsx')


#Adding in HHV of fuels burned

def hhv_mmbtu(row):
    return row['ghg_quantity'] * 1000 / row['emission_factor']

fuel_energy_content = merged_epa_ef.copy()
fuel_energy_content['fuel_hhv_mmbtu'] = fuel_energy_content.apply(hhv_mmbtu, axis = 1)
#fuel_energy_content.to_excel('output/mergehhv.xlsx')


#Transform HHV into process heat + electricity production TO BE REDONE WITH MORE GRANULARITY

#Right now just 80% thermal efficiency, no electricity production

fuel_energy_eff = fuel_energy_content.copy()
fuel_energy_eff['thermal_eff'] = 0.8
fuel_energy_eff['elec_eff'] = 0.

def process_energy(row):
    return row['fuel_hhv_mmbtu'] * row['thermal_eff']

fuel_energy_eff['process_heat'] = fuel_energy_eff.apply(process_energy, axis = 1)


#Add in combustion units information

unit_info = fuel_energy_eff.copy()

combustion_unit_correspondence = pd.merge(unit_info, kraft_combustion_units[['unit_type','combustion_unit_category', 'process_unit' ,'combustion_unit_electrifiable']], how = 'left')

#combustion_unit_correspondence.to_excel('output/combustion_units_corr.xlsx')


#Limit to only kraft processes

kraft_filtered_df = combustion_unit_correspondence[combustion_unit_correspondence['plant type'] == 'finishing']
#kraft_filtered_df.to_excel('output/kraft_only.xlsx')


#Do a iteration process to treat each facility

facility_list = kraft_filtered_df['facility_id'].unique().tolist()
process_unit_list = kraft_process_units['Unit name'].tolist()


def specific_unit_func(row, unit_type):
    if row['process_unit'] != unit_type:
        return 0
    else:
        return row['process_heat']

def is_excl(row, excl_unit_lst):
    if row['Unit name'] in excl_unit_lst:
        return True
    else:
        return False

def process_unit_heat_ratio(row, total_process):
    return row['Energy demand']/total_process

def spread_total_heat(row, total_spreadable_heat):
    return row['heat_ratio'] * total_spreadable_heat

def new_process_unit_heat_ratio(row, total_process):
    return row['new_heat_repartition']/total_process

def new_spread_total_heat(row, heat_ratio):
    return row['process_heat'] * heat_ratio
    
spread_heat_dflist = []

for facility in facility_list:
    plantspecific_df = kraft_filtered_df[kraft_filtered_df['facility_id'] == facility]
    process_units_copy = kraft_process_units.copy()
    comb_units_copy = kraft_combustion_units.copy()
    elec_fuel_copy = kraft_non_electrifiable_fuels.copy()
    
    #Exclude auxilliary and only assign them to no process units
    auxillary_df = plantspecific_df[plantspecific_df['combustion_unit_category'] == 'auxilliary']
    non_auxilliary_df = plantspecific_df[plantspecific_df['combustion_unit_category'] != 'auxilliary']
    

    auxillary_df['auxiliary'] = auxillary_df['process_heat']
    for process_unit in process_unit_list:
        auxillary_df[process_unit] = 0
    
    #Separate into specific exclusive and other units
    
    spec_excl_df = non_auxilliary_df[non_auxilliary_df['combustion_unit_category'] == 'specific (excl)']
    non_spec_excl_df = non_auxilliary_df[non_auxilliary_df['combustion_unit_category'] != 'specific (excl)']

    spec_excl_df['auxiliary'] = 0
    exclusive_unit_list = spec_excl_df['process_unit'].unique().tolist()
    for unit in process_unit_list:
        spec_excl_df[unit] = spec_excl_df.apply(specific_unit_func, args = (unit,), axis = 1)
    #Remove exclusive units from process units
    non_exclusive_process_units = process_units_copy[~process_units_copy['Unit name'].isin(exclusive_unit_list)]
    #Evaluate total non spec excl units for heat demand to spread it
    
    total_heat_demand = non_spec_excl_df['process_heat'].sum()
    total_process_heat = non_exclusive_process_units['Energy demand'].sum()
    #get process unit heat demand ratio
    non_exclusive_process_units['heat_ratio'] = non_exclusive_process_units.apply(process_unit_heat_ratio, args = (total_process_heat,), axis = 1)
    #get process heat repartition for the given plant
    non_exclusive_process_units['heat_repartition'] = non_exclusive_process_units.apply(spread_total_heat, args = (total_heat_demand,), axis = 1)
        
    #Separate into specific inclusive and other units
    spec_incl_df = non_spec_excl_df[non_spec_excl_df['combustion_unit_category'] == 'specific (incl)']
    non_spec_incl_df = non_spec_excl_df[non_spec_excl_df['combustion_unit_category'] != 'specific (incl)']
    non_exclusive_process_units['new_heat_repartition'] = non_exclusive_process_units['heat_repartition']
    #Assign all heat demand from spec incl units into the assigned process unit

    spec_incl_units_list = spec_incl_df['process_unit'].unique().tolist()
    spec_incl_df['auxiliary'] = 0
    for unit in process_unit_list:
        spec_incl_df[unit] = spec_incl_df.apply(specific_unit_func, args = (unit,), axis = 1)
    #Remove the heat demand of those units from process units in the heat demand repartition
    for sp_incl_unit in spec_incl_units_list:
        total_spec_unit_hd = spec_incl_df[spec_incl_df['process_unit'] == sp_incl_unit]['process_heat'].sum()
        non_exclusive_process_units[non_exclusive_process_units['Equivalent unit type in GHGRP'] == sp_incl_unit]['new_heat_repartition'] - total_spec_unit_hd
    
    #Recalculate new heat repartition
    new_heat_total = non_exclusive_process_units['new_heat_repartition'].sum()
    non_exclusive_process_units['new_heat_ratio'] = non_exclusive_process_units.apply(new_process_unit_heat_ratio, args = (new_heat_total,), axis = 1)
    #Spread heat from the rest of units (generic and whatever)
    

    non_spec_incl_df['auxiliary'] = 0
        
    for unit in process_unit_list:
        heat_ratio = non_exclusive_process_units[non_exclusive_process_units['Unit name'] == unit]['new_heat_ratio'].sum()
        non_spec_incl_df[unit] = non_spec_incl_df.apply(new_spread_total_heat, args =(heat_ratio,), axis = 1 )
    
    spread_heat_dflist.append(auxillary_df)
    spread_heat_dflist.append(spec_excl_df)
    spread_heat_dflist.append(spec_incl_df)
    spread_heat_dflist.append(non_spec_incl_df)
    
spread_heat_df = pd.concat(spread_heat_dflist)
#print(spread_heat_df.columns)
spread_heat_df = spread_heat_df.loc[:, ~spread_heat_df.columns.isna()]
#spread_heat_df.to_excel('output/spread_heat_modif.xlsx')

### Transform into a longform
pivot_columns = ['unit_type', 'unit_name', 'subpart', 'fuel_type', 'is_biogenic', 'primary_naics', 'facility_id', 'reporting_year', 'facility_name', 'state', 'naics_title', 'has_chem_recovery', 'has_lime_kiln', 'plant type', 'is_redundant', 'ghg_quantity', 'emission_factor', 'fuel_hhv_mmbtu', 'thermal_eff' , 'elec_eff', 'process_heat', 'combustion_unit_category', 'process_unit', 'combustion_unit_electrifiable']

longform_spread_heat = pd.melt(spread_heat_df, id_vars = pivot_columns, var_name = 'individual_process_unit', value_name= 'process_unit_heat_demand')
sorted_longform_spread_heat = longform_spread_heat.sort_values(by= 'facility_id')
#sorted_longform_spread_heat.to_excel('output/longform_heat_demand.xlsx')

### Remove lines with zero process unit heat demand

sorted_longform_spread_heat = sorted_longform_spread_heat.loc[~(sorted_longform_spread_heat['process_unit_heat_demand'] == 0)]

### Add in temperature per process unit

added_process_temps = pd.merge(sorted_longform_spread_heat, kraft_process_units[['Unit name','Energy demand','Temperature','Equivalent unit type in GHGRP',	'Direct electrification', 'Utility electrifiable']], left_on='individual_process_unit', right_on= 'Unit name', how= 'left')

added_process_temps = added_process_temps.drop(columns=['Unit name', 'Energy demand'])
added_process_temps.to_excel('output/process_temps_added.xlsx')



### Add in average temperature at that location for air-source hp COP calculation TO RE DO WITH BEN S HELP FOR GETTING COUNTY AVERAGE TEMP AT THE LOCATION AVERAGE TEMPS SHOULD OVERLAP WITH PLANT OPERATION HOURS

added_process_temps['average_county_temperature'] = 10

### Add in average hours worked per year at the plant TO RE DO AT SOME POINT WITH CENSUS DATA? ALSO AVERAGE TEMPS SHOULD OVERLAP WITH PLANT OPERATION HOURS

added_process_temps['yearly_hours_operation'] = 8000



non_elec_fuel_list = kraft_non_electrifiable_fuels['fuels list'].unique().tolist()

### Add in a row to indicate if the fuel is replaceable by electrified option

def elec_fuel(row, non_elec_fuel_list):
    if row['fuel_type'] in non_elec_fuel_list:
        return 'N'
    else:
        return 'Y'

added_process_temps['is_fuel_elec'] = added_process_temps.apply(elec_fuel, args =(non_elec_fuel_list,), axis = 1 )



###
### Logic to select electrification option (if any) per row
###


def electrification_option(row, process_params):
    if row['combustion_unit_category'] == 'generic' or row['combustion_unit_category'] == 'auxilliary':
        if row['Utility electrifiable'] == 'Y' and row['is_fuel_elec'] == 'Y' and row['combustion_unit_electrifiable'] == 'Y':
            return 'electrified_utilities'
        else:
            return 'not_electrifiable'
    
    elif row['combustion_unit_category'] == 'specific (excl)' or row['combustion_unit_category'] == 'specific (incl)':
        if row['Direct electrification'] == 'Y' and row['is_fuel_elec'] == 'Y' and row['combustion_unit_electrifiable'] == 'Y':
            process_unit = row['process_unit']
            electrification_option = process_params.loc[process_params['Unit name'] == process_unit]['Direct electrification option'].item()
            return electrification_option
        else:
            return 'not_electrifiable'
    
    else:
        return 'not_electrifiable'

electrified_option_df = added_process_temps.copy()

electrified_option_df['electrified_option'] = electrified_option_df.apply(electrification_option, args =(kraft_process_units,), axis = 1)




###
### Include waste heat pump possibility
###

def whp_decision(row, process_params):
    process_unit = row['individual_process_unit']
    if row['electrified_option'] == 'electrified_utilities':
        whp_ok = process_params.loc[process_params['Unit name'] == process_unit]['Waste heat pump possible'].item()
        
        if whp_ok == 'Y':
            return process_params.loc[process_params['Unit name'] == process_unit]['Waste heat pump type'].item()
        
        else:
            return None
    
    else:
        return None
    
def whp_temp(row, process_params):
    process_unit = row['individual_process_unit']
    if row['electrified_option'] == 'electrified_utilities':
        whp_ok = process_params.loc[process_params['Unit name'] == process_unit]['Waste heat pump possible'].item()
        
        if whp_ok == 'Y':
            return process_params.loc[process_params['Unit name'] == process_unit]['Waste heat temperature'].item()
        
        else:
            return None
    
    else:
        return None


waste_heat_pump_df = electrified_option_df.copy()

waste_heat_pump_df['whp_option'] = waste_heat_pump_df.apply(whp_decision, args =(kraft_process_units,), axis = 1)

waste_heat_pump_df['whp_inlet_t'] = waste_heat_pump_df.apply(whp_temp, args =(kraft_process_units,), axis = 1)
waste_heat_pump_df = waste_heat_pump_df.drop(columns=['subpart', 'has_chem_recovery', 'has_lime_kiln','plant type', 'is_redundant'],errors='ignore')




#### I PROBABLY WANT TO ADD IN EFFICIENCY MEASURES HERE




### Include the proportion of GHG for each row/process unit

electrification_df = waste_heat_pump_df.copy()
electrification_df['unit_ghg_emissions'] = electrification_df['ghg_quantity'] * electrification_df['process_unit_heat_demand'] / electrification_df['process_heat']
#electrification_df.to_excel('output/individual_ghg.xlsx')



### Per plant: isolate direct replacement options, electrified utilities and non electrifiable flows

electrified_utilities_df = electrification_df[electrification_df['electrified_option'] == 'electrified_utilities']

non_elec_df = electrification_df[electrification_df['electrified_option'] == 'not_electrifiable']

non_util_df = electrification_df[electrification_df['electrified_option'] != 'electrified_utilities']

direct_elec_df = non_util_df[non_util_df['electrified_option'] != 'not_electrifiable']


### aggregate utilities per plant, sum process unit emissions and unit heat demand
### drop fuel_type	is_biogenic  ghg_quantity	emission_factor	fuel_hhv_mmbtu combustion_unit_electrifiable
###	individual_process_unit	process_unit_heat_demand	Temperature	Equivalent unit type in GHGRP	Direct electrification	Utility electrifiable
### because they are fuel and combustion unit dependent


groupd_util = electrified_utilities_df.groupby(['primary_naics', 'facility_id', 'reporting_year',
                                  'facility_name', 'state', 'naics_title', 'electrified_option', 'whp_option',
                                  'whp_inlet_t', 'average_county_temperature',
                                  'yearly_hours_operation'], dropna=False,as_index=False).agg({'Temperature' : 'max','unit_ghg_emissions' : 'sum', 'process_unit_heat_demand' : 'sum'})





### group non electrifiable rows

groupd_non_elec = non_elec_df.groupby(['primary_naics', 'facility_id', 'reporting_year',
                                  'facility_name', 'state', 'naics_title', 'electrified_option', 'whp_option',
                                  'whp_inlet_t', 'average_county_temperature',
                                  'yearly_hours_operation'], dropna=False,as_index=False).agg({'Temperature' : 'max','unit_ghg_emissions' : 'sum', 'process_unit_heat_demand' : 'sum'})

groupd_non_elec.to_excel('output/322130_finishing_elec.xlsx')

### group direct electrification

groupd_direct_elec = direct_elec_df.groupby(['unit_name', 'primary_naics', 'facility_id', 'reporting_year',
                                  'facility_name', 'state', 'naics_title', 'electrified_option', 'whp_option',
                                  'whp_inlet_t', 'average_county_temperature',
                                  'yearly_hours_operation'], dropna=False,as_index=False).agg({'Temperature' : 'max','unit_ghg_emissions' : 'sum', 'process_unit_heat_demand' : 'sum'})




dropd_direct_elec = groupd_direct_elec.drop(columns = 'unit_name')

concd_elec_df = pd.concat([groupd_util, groupd_non_elec, dropd_direct_elec])
sorted_elec_df = concd_elec_df.sort_values(by='facility_id')
sorted_elec_df.reset_index(drop=True, inplace=True)


def cop(t_in, t_out, dt, zero_k = 273.15, carnot_ratio = 0.5):
    carnot_efficiency = (zero_k + t_out + dt)/ (t_out - t_in + (2 * dt))
    return carnot_efficiency * carnot_ratio

### Add in WHP COP

def worst_cop_whp(row):
    
    if row['whp_option'] and row['whp_inlet_t']:
        return cop(row['whp_inlet_t'], row['Temperature'], 10)
    else:
        return

def best_cop_whp(row):
    
    if row['whp_option'] and row['whp_inlet_t']:
        return cop(row['whp_inlet_t'], row['Temperature'], 5)
    else:
        return

sorted_elec_df['worst_whp_cop'] = sorted_elec_df.apply(worst_cop_whp, axis = 1)

sorted_elec_df['best_whp_cop'] = sorted_elec_df.apply(best_cop_whp, axis = 1)

### Add in ASHP COP

def best_cop_ashp(row):
    
    if row['electrified_option'] == 'electrified_utilities':
        return cop(row['average_county_temperature'], row['Temperature'], 5)
    else:
        return
    
def worst_cop_ashp(row):
    
    if row['electrified_option'] == 'electrified_utilities':
        return cop(row['average_county_temperature'], row['Temperature'], 10)
    else:
        return

sorted_elec_df['worst_ashp_cop'] = sorted_elec_df.apply(worst_cop_ashp, axis = 1)

sorted_elec_df['best_ashp_cop'] = sorted_elec_df.apply(best_cop_ashp, axis = 1)
### Add in equipment sizing

sorted_elec_df['mmbtu/hr'] = sorted_elec_df['process_unit_heat_demand'] / sorted_elec_df['yearly_hours_operation']
sorted_elec_df['sizing_kw'] = sorted_elec_df['mmbtu/hr'] * 293.07107

sorted_elec_df.to_excel('output/322130_finishing_elec.xlsx')




### No WHP dataframe

no_whp_df = sorted_elec_df.groupby(['primary_naics', 'facility_id', 'reporting_year', 'facility_name', 'state', 'naics_title', 'electrified_option',
'average_county_temperature', 'yearly_hours_operation'], dropna=False,as_index=False).agg({'Temperature' : 'max','unit_ghg_emissions' : 'sum', 'process_unit_heat_demand' : 'sum', 'worst_ashp_cop':'min', 'best_ashp_cop' : 'min',
      'mmbtu/hr': 'sum', 'sizing_kw' : 'sum'})


no_whp_df.to_excel('output/322130_finishing_elec_nowhp.xlsx')




