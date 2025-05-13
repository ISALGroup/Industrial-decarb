# -*- coding: utf-8 -*-
"""
Created on Thu May  8 11:23:34 2025

@author: Antoine
"""
import boiler_class
default_conversion_factor = 0.8



def default_model(req_duty):
    fuel_in = req_duty/default_conversion_factor
    return {'Fuel in' : fuel_in}
    
Default_Boiler = boiler_class.Boiler('default')
Default_Boiler.model_req_duty = default_model
Default_Boiler.fuel = 'CH4'
#Unit of emission factors: kg/kJ, from EPA appendix 15-A (https://downloads.regulations.gov/EERE-2007-BT-STD-0010-0053/attachment_42.pdf)
Default_Boiler.fuel_ef = {'CO2' : 50.6 * 10**(-6), 'NOx' : 40 * 10**(-9), 'SO2' : 0 }
