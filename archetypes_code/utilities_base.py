# -*- coding: utf-8 -*-
"""
Created on Tue May  6 09:45:10 2025

@author: Antoine
"""
import collections
import pandas as pd
from archetypes_base import *



def no_ok_stream(heat_prod_temp, heat_prod_heat, heat_demand_temp, heat_demand_heat):
    no_ok = True
    for stemp in heat_prod_temp:
        sindex = heat_prod_temp.index(stemp)
        if heat_prod_heat[sindex] == 0:
            continue
        else: 
            for dtemp in heat_demand_temp:
                dindex = heat_demand_temp.index(dtemp)
                if dtemp > stemp:
                    continue
                else:
                    if heat_demand_heat[dindex] == 0:
                        continue
                    
                    else:
                        no_ok = False
                        break
    return no_ok
    

def stopcondition(hpt, hph, hdt, hdh):
    no_supply = all(hs == 0 for hs in hph)
    no_demand = all(hd == 0 for hd in hdh)
    no_suitable_stream = no_ok_stream(hpt, hph, hdt, hdh)
    return no_supply or no_demand or no_suitable_stream

def distribute_heat(heat_prod_temp, heat_prod_heat, heat_demand_temp, heat_demand_heat):
    
    hpt = heat_prod_temp
    hph = heat_prod_heat
    hdt = heat_demand_temp
    hdh = heat_demand_heat
    while not stopcondition(hpt, hph, hdt, hdh):
        for stemp in hpt:
            sindex = hpt.index(stemp)
            for dtemp in hdt:
                dindex = hdt.index(dtemp)
                if dtemp > stemp:
                    continue
                else:
                    sheat = hph[sindex]
                    dheat = hdh[dindex]
                    
                    if dheat == sheat:
                        hph[sindex] = 0
                        hdh[dindex] = 0
                    elif dheat > sheat:
                        hph[sindex] = 0
                        hdh[dindex] = dheat - sheat
                    elif dheat < sheat:
                        hph[sindex] = sheat - dheat
                        hdh[dindex] = 0
    return (hpt, hph, hdt, hdh)

def cleanup_heat_curve(temp, heat):
    clean_temp = temp
    clean_heat = heat
    if len(heat) != len(temp):
        raise('Different sizes for lists')
    if not (0 in heat):
        return temp, heat
    zero_indexes = []
    if not all(h == 0 for h in heat):
        for i in range(len(heat)):
            if heat[i] == 0:
                zero_indexes.append(i)
        del(clean_temp[zero_indexes])
        del(clean_heat[zero_indexes])
    return clean_temp, clean_heat

def utilities_main(flows = [], units = [], grid_price = 0, grid_price_sell = 0, grid_ef = {}, boilers = [], air_compressors = [], fuels_price = {}):
    ghg_emissions = {}
    copollutant_emissions = {}
    st_d_t = []
    st_d_h = []
    hw_d_t = []
    hw_d_h = []
    st_s_t = []
    st_s_h = []
    hw_s_t = []
    hw_s_h = []
    elec_demand = 0
    compressed_air_demand = 0
    fuel_demand = {}
    fuel_produced = {}
    process_emissions_ghg = {}
    process_emissions_copollutants = {}
    process_emissions_ghgco2eq = 0
    
    
    for unit in units:
        hd = 0
        hd_f= {}
        ed = 0
        h_p = 0
        hw_p = 0
        f_p = {}
        wh = 0
        ca = 0
        ca_p = 0
        temp = 0
        steam_demand = False
        steam_supply = False
        hw_demand = False
        hw_supply = False
        
        if hasattr(unit, 'emissions'):
            ghgs = ['CO2', 'CH4', 'N2O', 'HFC', 'CF4', 'SF6']
            # Emissions factors from epa emissions equivalencies calculator https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator. HFC selected as HCFC-22 by default
            ghg_ef = {'CO2' : 1, 'CH4' : 28 , 'N2O' : 265, 'HFC' : 1810, 'CF4' : 6630, 'SF6' : 30130}
            for emission in unit.emissions:
                if emission in ghgs:
                    if emission in process_emissions_ghg:
                        process_emissions_ghg[emission] += unit.emissions[emission]
                        process_emissions_ghgco2eq += unit.emissions[emission]*ghg_ef[emission]
                    else:
                        process_emissions_ghg[emission] = unit.emissions[emission]    
                        process_emissions_ghgco2eq += unit.emissions[emission]*ghg_ef[emission]
                    
                else:
                    if emission in process_emissions_copollutants:
                        process_emissions_copollutants += unit.emissions[emission]
                    else:
                        process_emissions_copollutants = unit.emissions[emission]
        for flowin in unit.input_flows:
            flowin_index = find_Flow_index(flowin, flows)
            Flow = flows[flowin_index]
            
            if Flow.attributes['flow_type'] == 'Steam':
                hd += Flow.attributes['heat_flow_rate']
                temp = Flow.attributes['temperature']
                steam_demand = True
            if Flow.attributes['flow_type'] == 'Hot water':
                hd += Flow.attributes['heat_flow_rate']
                temp = Flow.attributes['temperature']
                hw_demand = True
            if Flow.attributes['flow_type'] == 'Fuel':
                if Flow.attributes['components'][0] in fuel_demand:    
                    fuel_demand[Flow.attributes['components'][0]] += Flow.attributes['combustion_energy_content']
                else:
                    fuel_demand[Flow.attributes['components'][0]] = Flow.attributes['combustion_energy_content']
            
            if Flow.attributes['flow_type'] == 'Electricity':
                elec_demand += Flow.attributes['elec_flow_rate']
            
            if Flow.attributes['flow_type'] == 'Compressed air':
                ca += Flow.attributes['mass_flow_rate']
            
        for flowout in unit.output_flows:
            flowout_index = find_Flow_index(flowout, flows)
            Flow = flows[flowout_index]
            
            if Flow.attributes['flow_type'] == 'Condensate':
                hd += (- Flow.attributes['heat_flow_rate'])
            
            if Flow.attributes['flow_type'] == 'Hot water return':
                hd += (- Flow.attributes['heat_flow_rate'])
            
            if Flow.attributes['flow_type'] == 'Fuel (produced on-site)':
                if Flow.attributes['components'][0] in fuel_produced:
                    fuel_produced[Flow.attributes['components'][0]] += Flow.attributes['combustion_energy_content']
                else:
                    fuel_produced[Flow.attributes['components'][0]] = Flow.attributes['combustion_energy_content']
            
            if Flow.attributes['flow_type'] == 'Electricity (produced on-site)':
                elec_demand += (-Flow.attributes['elec_flow_rate'])
            
            if Flow.attributes['flow_type'] == 'Steam (produced on-site)':
                temp = Flow.attributes['temperature']
                h_p += Flow.attributes['heat_flow_rate']
                steam_supply = True
            if Flow.attributes['flow_type'] == 'Hot water (produced on-site)':
                temp = Flow.attributes['temperature']
                hw_p += Flow.attributes['heat_flow_rate']
                hw_supply = True
            if Flow.attributes['flow_type'] == 'Waste' or Flow.attributes['flow_type'] == 'Waste water' or Flow.attributes['flow_type'] == 'Exhaust':
                wh += Flow.attributes['heat_flow_rate']
            
            if Flow.attributes['flow_type'] == 'Compressed air (produced on-site)':
                ca += (-Flow.attributes['mass_flow_rate'])
        
        if steam_demand:
            if temp in st_d_t:
                ind = st_d_t.index(temp)
                st_d_h[ind] += hd
            else:
                st_d_t.append(temp)
                st_d_h.append(hd)
        if hw_demand:
            if temp in hw_d_t:
                ind = hw_d_t.index(temp)
                hw_d_h[ind] += hd
            else:
                hw_d_t.append(temp)
                hw_d_h.append(hd)
        
        if steam_supply:
            if temp in st_s_t:
                ind = st_s_t.index(temp)
                st_s_h[ind] += h_p
            else:
                st_d_t.append(temp)
                st_d_h.append(h_p)
        if hw_supply:
            if temp in hw_s_t:
                ind = hw_s_t.index(temp)
                hw_s_h[ind] += hw_p
            else:
                hw_s_t.append(temp)
                hw_s_h.append(hw_p)
        elec_demand += ed
        compressed_air_demand += ca
    
    
    
    distributed_heat_steam = distribute_heat(st_s_t, st_s_h, st_d_t, st_d_h)
    
    dist_st_steam = distributed_heat_steam[0]
    dist_sh_steam = distributed_heat_steam[1]
    dist_dt_steam = distributed_heat_steam[2]
    dist_dh_steam = distributed_heat_steam[3]
    
    clean_st_steam, clean_sh_steam = cleanup_heat_curve(dist_st_steam, dist_sh_steam)
    clean_dt_steam, clean_dh_steam = cleanup_heat_curve(dist_dt_steam, dist_dh_steam)
        
        
    
    
    distributed_heat_hw = distribute_heat(hw_s_t, hw_s_h, hw_d_t, hw_d_h)
    
    dist_st_hw = distributed_heat_hw[0]
    dist_sh_hw = distributed_heat_hw[1]
    dist_dt_hw = distributed_heat_hw[2]
    dist_dh_hw = distributed_heat_hw[3]
    
    clean_st_hw, clean_sh_hw = cleanup_heat_curve(dist_st_hw, dist_sh_hw)
    clean_dt_hw, clean_dh_hw = cleanup_heat_curve(dist_dt_hw, dist_dh_hw)
    
    boilers_fuels = {}
    boilers_emissions = {}
    boilers_t = []
    boilers_duty = []
    
    for boiler in boilers:
        boilers_t.append(boiler.temp)
        if boiler.current_consumption != None and boiler.model_fuelin != None:
            boiler.run_boiler_current()
        
        elif boiler.req_duty != None and boiler.model_req_duty != None:
            boiler.run_boiler_reqduty()
            
        if boiler.fuel in boilers_fuels:
            boilers_fuels[boiler.fuel] += boiler.current_consumption
        else:
            boilers_fuels[boiler.fuel] = boiler.current_consumption
            
        for emission in boiler.emissions:
            if emission in boilers_emissions:
                boilers_emissions[emission] += boiler.emissions[emission]
            else:
                boilers_emissions[emission] = boiler.emissions[emission]
        boilers_duty.append(boiler.duty)
    
    boiler_duty_copy = boilers_duty
    boilers_t_copy = boilers_t
    
    post_boiler_heat_curves = distribute_heat(boilers_t_copy, boiler_duty_copy, clean_dt_hw, clean_dh_hw)
    
    boiler_surplus_t = post_boiler_heat_curves[0]
    boiler_surplus_duty = post_boiler_heat_curves[1]
    unmet_demand_t = post_boiler_heat_curves[2]
    unmet_demand_duty = post_boiler_heat_curves[3]
    
    surplus_duty = sum(boiler_surplus_duty)
    unmet_demand = sum(unmet_demand_duty)
    
    
    #Electricity costs and emissions
    electricity_cost = 0
    electricity_emissions = {}
    if elec_demand >= 0:
        electricity_cost = elec_demand * grid_price
        for gas in grid_ef:
            electricity_emissions[gas] = elec_demand * grid_ef[gas]
    if elec_demand < 0:
        electricity_cost = elec_demand * grid_price_sell
        for gas in grid_ef:
            electricity_emissions[gas] = elec_demand * grid_ef[gas]
    
    elec_ghg_co2e = 0
    elec_ghg_emissions = {}
    elec_copollutants_emissions = {}
    ghg_ef = {'CO2' : 1, 'CH4' : 28 , 'N2O' : 265, 'HFC' : 1810, 'CF4' : 6630, 'SF6' : 30130}
    for emission in electricity_emissions:
        if emission in ghg_ef:
            if emission in elec_ghg_emissions:
                elec_ghg_emissions[emission] += electricity_emissions[emission]
                elec_ghg_co2e += electricity_emissions[emission]*ghg_ef[emission]
            else:
                elec_ghg_emissions[emission] = electricity_emissions[emission]    
                elec_ghg_co2e += electricity_emissions[emission]*ghg_ef[emission]
            
        else:
            if emission in elec_copollutants_emissions:
                elec_copollutants_emissions[emission] += electricity_emissions[emission]
            else:
                elec_copollutants_emissions[emission] = electricity_emissions[emission]
    
    #On-site emissions and fuels costs/emissions
    
    # process_emissions_ghg
    # process_emissions_copollutants
    # process_emissions_ghgco2eq
    process_fuel_cost = 0
    for fuel in fuel_demand:
        process_fuel_cost += fuel_demand[fuel] * fuels_price[fuel]
    
    
    #Boiler fuel costs and emissions
    
    boiler_fuel_costs = 0
    for fuel in boilers_fuels:
        boiler_fuel_costs += boilers_fuels[fuel] * fuels_price[fuel]
    
    boiler_ghg_co2e = 0
    boiler_ghg_emissions = {}
    boiler_copollutants_emissions = {}
    ghg_ef = {'CO2' : 1, 'CH4' : 28 , 'N2O' : 265, 'HFC' : 1810, 'CF4' : 6630, 'SF6' : 30130}
    for emission in boilers_emissions:
        if emission in ghg_ef:
            if emission in boiler_ghg_emissions:
                boiler_ghg_emissions[emission] += boilers_emissions[emission]
                boiler_ghg_co2e += boilers_emissions[emission]*ghg_ef[emission]
            else:
                boiler_ghg_emissions[emission] = boilers_emissions[emission]    
                boiler_ghg_co2e += boilers_emissions[emission]*ghg_ef[emission]
            
        else:
            if emission in boiler_copollutants_emissions:
                boiler_copollutants_emissions[emission] += boilers_emissions[emission]
            else:
                boiler_copollutants_emissions[emission] = boilers_emissions[emission]
    
    
    print('Electricity cost ' + str(electricity_cost) + ' $')
    print('Electricity emissions :')
    print('          Total CO2e :' + str(elec_ghg_co2e))
    print('--------------------------------------------')
    print('Process fuel costs ' + str(process_fuel_cost) + ' $')
    print('Process emissions :')
    print('          Total CO2e :' + str(process_emissions_ghgco2eq))
    print('--------------------------------------------')
    print('Boiler fuel costs ' + str(boiler_fuel_costs) + ' $')
    print('Boiler emissions :')
    print('          Total CO2e :' + str(boiler_ghg_co2e))
    print('--------------------------------------------')
    return


