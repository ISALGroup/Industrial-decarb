# -*- coding: utf-8 -*-
"""
Created on Thu May  8 11:23:34 2025

@author: Antoine
"""
default_conversion_factor = 0.8
import boiler_class


def default_model(req_duty):
    fuel_in = req_duty/default_conversion_factor
    return {'Fuel in' : fuel_in}
    
Default_Boiler = boiler_class.Boiler('default')
Default_Boiler.model_req_duty = default_model
#Unit of emission factors: kg/kJ, from EPA appendix 15-A
Default_Boiler.fuel_ef = {'CO2' : 50.6 * 10**(-6), 'NOx' : 40 * 10**(-9), 'SO2' : 0 }
