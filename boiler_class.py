# -*- coding: utf-8 -*-
"""
Created on Thu May  8 10:45:46 2025

@author: Antoine
"""

class Boiler():
    
    def __init__(self, name = '', cogeneration = False, max_duty = 999999999, opti_duty = 1000, fuel = '', fuel_ef = {}, model_fuelin = None, model_req_duty = None, current_consumption = None, temp = 200, pressure = 15.5, superheat = False, hot_water = False):
        self.name = name
        self.cogeneration = cogeneration
        self.max_duty = max_duty
        self.opti_duty = opti_duty
        self.fuel = fuel
        self.fuel_ef = fuel_ef
        self.model_fuelin = model_fuelin
        self.model_req_duty = model_req_duty
        self.current_consumption = current_consumption
        self.temp = temp
        self.pressure = pressure
        self.superheat = superheat
        self.hot_water = hot_water
        
    
    def run_boiler_current(self):
        results = self.model_fuelin(self, self.current_consumption)
        self.duty = results['Duty']
        emissions = {} 
        for ef in self.fuel_ef:
            emissions[ef] = self.current_consumption * ef
        self.emissions = emissions
    
    def run_boiler_reqduty(self):
        self.duty = min(self.req_duty, self.max_duty)
        results =  self.model_req_duty(self.duty)
        self.current_consumption = results['Fuel in']
        emissions = {} 
        for ef in self.fuel_ef:
            emissions[ef] = self.current_consumption * self.fuel_ef[ef]
        self.emissions = emissions
        
    

