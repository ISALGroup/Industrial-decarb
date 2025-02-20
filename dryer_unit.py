# -*- coding: utf-8 -*-
"""
Created on Thu Jan 30 10:16:43 2025

@author: Antoine
"""

import pandas as pd
import numpy as np
import csv
from CoolProp.CoolProp import PropsSI


allflows = []
processunits = []


def find_Flow_index(name, flowlist):
    index = None
    for flow in flowlist:
        if flow.attributes['name'] == name:
            index = flowlist.index(flow)
    
    return index
    

def flow_already_present(name, flowlist):
    present = False
    for flow in flowlist:
        if flow.attributes['name'] == name:
            present = True
            return present
    
    return present

class Flow:
    def __init__(self, name = '', components = '', flow_type = '',
                 temperature=25, pressure = 1, composition = [1], origin = None, 
                 destination = None, mass_flow_rate = 0, elec_flow_rate = 0, 
                 heat_flow_rate = 0, combustion_energy_content = 0,  is_calc_flow = False, is_shear_stream = False):
        self.attributes = {'name' : name, 'components' : components, 'flow_type' :
                           flow_type, 'temperature' : temperature, 'pressure':
                           pressure, 'composition' : composition, 'origin' :
                           origin, 'destination' : destination, 'mass_flow_rate':
                           mass_flow_rate, 'elec_flow_rate' : elec_flow_rate,
                           'heat_flow_rate': heat_flow_rate, 'combustion_energy_content' : combustion_energy_content }
        self.is_calc_flow = is_calc_flow
        self.is_shear_stream = is_shear_stream
        
    
    def __str__(self):
        l1 = 'Flow ' + self.attributes['name'] + ' . Type :' +  self.attributes['flow_type'] + '. \n'
        l2 = 'Components: ' + str(self.attributes['components']) + 'with composition ' + str(self.attributes['composition']) + '. \n'
        if self.attributes['origin'] == None and self.attributes['destination'] == None:
            l3 = 'No origin or destination. \n'
        elif self.attributes['origin'] == None:
            l3 = ' No origin. Destination:' + self.attributes['destination'] + ' . \n'
        
        elif self.attributes['destination'] == None:
            l3 = 'Origin : ' + self.attributes['origin'] + ' , no destination. \n '
        else:    
            l3 = 'Origin : ' + self.attributes['origin'] + ' , destination: ' + self.attributes['destination'] + ' . \n'
        l4 = 'Mass flow rate: ' + str(self.attributes['mass_flow_rate']) + 'kg/hr. Heat flow rate: ' + str(self.attributes['heat_flow_rate']) + 'kJ/hr . \n'
        l5 = 'Electricty flow rate : '+ str(self.attributes['elec_flow_rate']) + 'kW.  Combustion energy contained: ' + str(self.attributes['combustion_energy_content']) + 'kJ / hr.\n \n'
        return   l1 + l2 + l3 +l4 + l5
    
        
    def set_destination(self, Destination_unit):
        if Destination_unit.name == self.attributes['destination'] and self.attributes['name'] in Destination_unit.input_flows:
            pass
        
        elif Destination_unit.name != self.attributes['destination'] and self.attributes['name'] not in Destination_unit.input_flows:
            Destination_unit.input_flows.append(self.attributes['name'])
            self.attributes['destination'] = Destination_unit.name
        elif Destination_unit != self.attributes['destination']:    
            self.attributes['destination'] = Destination_unit.name
        else:
            Destination_unit.input_flows.append(self.attributes['name'])


    def set_origin(self, Origin_unit):
        #Function to set the origin of the stream from a given unit.
        #It will also put the stream in the list of streams that go out of that unit
        if Origin_unit.name == self.attributes['origin'] and self.attributes['name'] in Origin_unit.output_flows:
            pass
        
        elif Origin_unit.name != self.attributes['origin'] and self.attributes['name'] not in Origin_unit.output_flows:
            Origin_unit.output_flows.append(self.attributes['name'])
            self.attributes['origin'] = Origin_unit.name
        elif Origin_unit != self.attributes['origin']:    
            self.attributes['origin'] = Origin_unit.name
        else:
            Origin_unit.input_flows.append(self.attributes['name'])
    
    def set_calc_flow(self):
        self.is_calc_flow = True
        
    def set_shear_stream(self):
        self.is_shear_stream = True

        

class Unit:
    def __init__(self, name, input_flows = None, output_flows = None, 
                 calculations = None, expected_flows_in = None, expected_flows_out = None, coefficients = None, is_calc = False):
        self.name = name
        if not input_flows:
            self.input_flows = []
        else: 
            self.input_flows = input_flows
        if not output_flows:
            self.output_flows = []
        else:
            self.output_flows = output_flows
        if not calculations:
            self.calculations = {}
        else:
            self.calculations = calculations
        if not expected_flows_in:
            self.expected_flows_in = []
        else: 
            self.expected_flows_in = expected_flows_in
        if not expected_flows_out:    
            self.expected_flows_out = []
        else: 
            self.expected_flows_out = expected_flows_out
        if not coefficients:
            self.coefficients = {}
        else:    
            self.coefficients = coefficients
        self.is_calc = is_calc
    def __str__(self):
        input_flows_indexes = []
        output_flows_indexes = []
        for flow in self.input_flows:
            index = find_Flow_index(flow, allflows)
            input_flows_indexes.append(index)
        for flow in self.output_flows:
            index = find_Flow_index(flow, allflows)
            output_flows_indexes.append(index)
        l1 = 'Unit ' + self.name + '\n'
        l2 = 'Input flows ' + str(self.input_flows) + '\n'
        l2bis = ''
        for index in input_flows_indexes:
            l2bis += 'Flow ' + allflows[index].attributes['name'] + ', mass flow rate =' + str(allflows[index].attributes['mass_flow_rate']) + ', heat flow rate =' + str(allflows[index].attributes['heat_flow_rate']) + '\n'
        l3 = 'Output flows ' + str(self.output_flows) + '\n'
        l3bis = ''
        for index in output_flows_indexes:
            l3bis += 'Flow ' + allflows[index].attributes['name'] + ', mass flow rate =' + str(allflows[index].attributes['mass_flow_rate']) + ', heat flow rate =' + str(allflows[index].attributes['heat_flow_rate']) + '\n'

        l4 = 'Expected input flows ' + str(self.expected_flows_in) + '\n'
        l5 = 'Expected output flow ' + str(self.expected_flows_out) + '\n'
        return l1 + l2 +l2bis + l3 + l3bis + l4 + l5
    def is_fully_calc(self):
        is_fully_calc = True
        flows_in = self.input_flows
        flows_out = self.output_flows
        for expected_flow in self.expected_flows_in:
            if expected_flow not in flows_in:
                is_fully_calc = False
        for expected_flow in self.expected_flows_out:
            if expected_flow not in flows_out:
                is_fully_calc = False
        
        return is_fully_calc
    
    def set_flow(self, flow_caracteristics, flowlist = allflows):
        New_Flow = Flow()
        for carac in flow_caracteristics:
            New_Flow.attributes[carac] = flow_caracteristics[carac]
            if flow_caracteristics['In or out'] == 'In':
                New_Flow.set_destination(self)
            if flow_caracteristics['In or out'] == 'Out':
                New_Flow.set_origin(self)
            if 'Set calc' in flow_caracteristics:
                if flow_caracteristics['Set calc'] == False:
                    New_Flow.is_calc_flow = False
                else:
                    New_Flow.set_calc_flow()
            if 'Set shear' in flow_caracteristics:
                if flow_caracteristics['Set shear'] == False:
                    New_Flow.is_shear_stream = False
                
                else:
                    New_Flow.set_shear_stream()
        flowlist.append(New_Flow)
            
    def calc(self, flowlist = allflows, unitlist = processunits):
        if self.is_calc:
            return
        for OutFlow in self.output_flows:
            index = 0
            for Flow in flowlist:
                if Flow.attributes['name'] == OutFlow:
                    index = flowlist.index(Flow)
            if flowlist[index].is_calc_flow:
                calculations = self.calculations[OutFlow](flowlist[index], self.coefficients)
                for calculated_object in calculations:
                    if len(calculated_object) == 1 and 'Heat loss' in calculated_object:
                        self.heat_loss = calculated_object['Heat loss']
                        calculations.remove(calculated_object)
                for calculated_object in calculations:
                    if len(calculated_object) == 1 and 'Heat of reaction' in calculated_object:
                        self.reaction_heat = calculated_object['Heat of reaction']
                        calculations.remove(calculated_object)
                calculated_flows = calculations
                for flow in calculated_flows:
                    flow_name = flow['name']
                    flow_presence = flow_already_present(flow_name, flowlist)
                    print(flow_presence)
                    if not flow['Set shear']:
                        self.set_flow(flow, flowlist, unitlist)
                    elif flow['Set shear'] == True and flow_presence:
                        print(flow_already_present(flow_name, flowlist))
                        shear_stream_index = find_Flow_index(flow['name'], flowlist)
                        original_Flow = flowlist[shear_stream_index]
                        o_mfr = original_Flow.attributes['mass_flow_rate']
                        o_hfr = original_Flow.attributes['heat_flow_rate']
                        n_mfr = flow['mass_flow_rate']
                        n_hfr = flow['heat_flow_rate']
                        rel_dif_mfr = (o_mfr - n_mfr)/o_mfr
                        rel_dif_hfr = (o_hfr - n_hfr)/o_hfr
                        print('Presence of shear stream' + str(flow['name']) + '. Original mass flow rate =' + str(o_mfr) + ' kg/hr. New mass flow rate = ' + str(n_mfr) + ' kg/hr. Relative difference = ' + str(rel_dif_mfr) + '. Original heat flow rate =' + str(o_hfr) + ' kJ/hr. New mass flow rate = ' + str(n_hfr) + ' kg/hr. Relative difference = ' + str(rel_dif_hfr))
                        if flow['In or out'] == 'Out':
                            flowlist[shear_stream_index].set_origin(self, flowlist, unitlist)
                        else:
                            flowlist[shear_stream_index].set_destination(self, flowlist, unitlist)
                    else:
                        self.set_flow(flow, flowlist)
                self.is_calc = True                                    
        for InFlow in self.input_flows:
            index = 0            
            for Flow in flowlist:
                if Flow.attributes['name'] == InFlow:
                    index = flowlist.index(Flow)
                    print('Index: ' + str(index))
            if flowlist[index].is_calc_flow:
                calculations = self.calculations[InFlow](flowlist[index], self.coefficients)
                for calculated_object in calculations:
                    if len(calculated_object) == 1 and 'Heat loss' in calculated_object:
                        self.heat_loss = calculated_object['Heat loss']
                        calculations.remove(calculated_object)
                for calculated_object in calculations:
                    if len(calculated_object) == 1 and 'Heat of reaction' in calculated_object:
                        self.reaction_heat = calculated_object['Heat of reaction']
                        calculations.remove(calculated_object)
                print(calculations)
                calculated_flows = calculations
                for flow in calculated_flows:
                    flow_name = flow['name']
                    flow_presence = flow_already_present(flow_name, flowlist)
                    print(flow_presence)
                    if not flow['Set shear']:
                        self.set_flow(flow)
                    elif flow['Set shear'] == True and flow_presence:
                        print(flow_already_present(flow_name, flowlist))
                        shear_stream_index = find_Flow_index(flow['name'], flowlist)
                        original_Flow = flowlist[shear_stream_index]
                        o_mfr = original_Flow.attributes['mass_flow_rate']
                        o_hfr = original_Flow.attributes['heat_flow_rate']
                        n_mfr = flow['mass_flow_rate']
                        n_hfr = flow['heat_flow_rate']
                        rel_dif_mfr = (o_mfr - n_mfr)/o_mfr
                        rel_dif_hfr = (o_hfr - n_hfr)/o_hfr
                        print('Presence of shear stream' + str(flow['name']) + '. Original mass flow rate =' + str(o_mfr) + ' kg/hr. New mass flow rate = ' + str(n_mfr) + ' kg/hr. Relative difference = ' + str(rel_dif_mfr) + '. Original heat flow rate =' + str(o_hfr) + ' kJ/hr. New heat flow rate = ' + str(n_hfr) + ' kJ/hr. Relative difference = ' + str(rel_dif_hfr))
                        if flow['In or out'] == 'Out':
                            flowlist[shear_stream_index].set_origin(self)
                        else:
                            flowlist[shear_stream_index].set_destination(self)
                    else:
                        self.set_flow(flow, flowlist)
                self.is_calc = True 

    
    def attach_available_flow(self, flowlist = allflows):
        flowsin = []
        flowsout = []
        for flow_in in self.input_flows:
            flowsin.append(flow_in)
    
        for flow_out in self.output_flows:
            flowsout.append(flow_out)
    

        for flow in self.expected_flows_in:
            if flow not in flowsin:
                # Check if the flow is part of the output flows first, before attaching to input
                for processflow in flowlist:
                    if processflow.attributes['name'] == flow and processflow.attributes['destination'] is None:
                        processflow.set_destination(self)
                    
        for flow in self.expected_flows_out:
            if flow not in flowsout:
                for processflow in flowlist:
                    if processflow.attributes['name'] == flow and processflow.attributes['origin'] is None:
                        processflow.set_origin(self)
    
    def are_all_flows_attached(self):
        all_flows_attached = True
        for flow in self.expected_flows_in:
            if flow in self.input_flows:
                pass
            else:
                all_flows_attached = False
                return all_flows_attached
        for flow in self.expected_flows_out:
            if flow in self.expected_flows_out:
                pass
            else:
                all_flows_attached = False
                return all_flows_attached
        return all_flows_attached
    
    def check_heat_balance(self):

        if self.are_all_flows_attached():
            Q_in = 0
            Q_out = 0
            for flow in self.input_flows:
                for Flow in allflows:
                    if Flow.attributes['name'] == flow:
                        Q_in += Flow.attributes['heat_flow_rate']
                        break
            for flow in self.output_flows:
                for Flow in allflows:
                    if Flow.attributes['name'] == flow:
                        Q_out += Flow.attributes['heat_flow_rate']
                        break
            if not hasattr(self, 'heat_loss') and not hasattr(self, 'reaction_heat'):
                if Q_out == 0 and abs(Q_in) < 0.001:
                    print(self.name + ' heat balance ok: Qin = ' + str(Q_in) + 'Qout = ' + str(Q_out) )
                    return True
                elif Q_out == 0 and abs(Q_in) >= 0.001:
                    print(self.name + ' heat balance not ok: Qin = ' + str(Q_in) + 'Qout = ' + str(Q_out) )
                    return False
                elif abs(1 - Q_in/Q_out) < 0.001:
                    print(self.name + ' heat balance ok: Qin = ' + str(Q_in) + 'Qout = ' + str(Q_out) )
                    return True
                else:
                    print(self.name + ' heat balance not ok: Qin = ' + str(Q_in) + 'Qout = ' + str(Q_out) )
                    return False
            
            elif not hasattr(self, 'heat_loss') and hasattr(self, 'reaction_heat'):
                r_heat = self.reaction_heat
                fake_Q_in = Q_in + r_heat
                if Q_out == 0 and abs(fake_Q_in) < 0.001:
                    print(self.name + ' heat balance ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qreaction =' + str(r_heat))
                    return True
                elif Q_out == 0 and abs(fake_Q_in) >= 0.001:
                    print(self.name + ' heat balance not ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qreaction =' + str(r_heat))
                    return False
                elif abs(1 - fake_Q_in/Q_out) < 0.001:
                    print(self.name + ' heat balance ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qreaction =' + str(r_heat))
                    return True
                else:
                    print(self.name + ' heat balance not ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qreaction =' + str(r_heat))
                    return False
                
            
            elif hasattr(self, 'heat_loss') and not hasattr(self, 'reaction_heat'):
                h_loss = self.heat_loss
                fake_Q_out = Q_out + h_loss
                if  fake_Q_out == 0 and Q_in < 0.001:
                    print(self.name + 'heat balance ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qloss =' + str(h_loss))
                    return True
                elif fake_Q_out == 0 and Q_in >= 0.001:
                    print(self.name + ' heat balance not ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qloss =' + str(h_loss))
                    return False
                elif abs(1 - Q_in/fake_Q_out) < 0.001:
                    print(self.name + ' heat balance ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qloss =' + str(h_loss))
                    return True
                else:
                    print(self.name + ' heat balance not ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qloss =' + str(h_loss))
                    return False
            
            
            else:
                r_heat = self.reaction_heat
                fake_Q_in = Q_in + r_heat
                h_loss = self.heat_loss
                fake_Q_out = Q_out + h_loss
                if fake_Q_out == 0 and abs(fake_Q_in) < 0.001:
                    print(self.name + ' heat balance ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qreaction =' + str(r_heat) + ', Qloss =' + str(h_loss))
                    return True
                elif fake_Q_out == 0 and abs(fake_Q_in) >= 0.001:
                    print(self.name + ' heat balance not ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qreaction =' + str(r_heat) + ', Qloss =' + str(h_loss))
                    return False
                elif abs(1 - (fake_Q_in/fake_Q_out)) < 0.001:
                    print(self.name + ' heat balance ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qreaction =' + str(r_heat) + ', Qloss =' + str(h_loss))
                    return True
                else:
                    print(self.name + ' heat balance not ok: Qin = ' + str(Q_in) + ', Qout = ' + str(Q_out) + ', Qreaction =' + str(r_heat) + ', Qloss =' + str(h_loss))
                    return False
                
        else:
            raise ValueError('All flows not attached to this unit (' + self.name + ')')
## Here is the function to make an 'easy' Dryer. It takes in at least the name of the dryer and what flow would go in.
## It can also take in different tweakable values for it's coefficients, like the heat capacity of the different materials involved,
## or the heat loss amount and the different temperatures required. If not specified, these coefficients will take default values.
def MakeSteamDryer(u_name, inc_s_name, heat_loss = 0.1, elec_demand = 3, dry_matter_cp = 1.5,
                   hvap = 2200, cp_water = 4.2, cp_air = 1.0, cp_steam = 2.1, amb_t = 25, steamtemp = 130,
                   steampress = 2.8, moisture_out = 0.2, air_temp = 125, exhaust_temperature = 90, dryflow_temp = 50):
    Dryer = Unit(u_name)
    if elec_demand == 0:
        Dryer.expected_flows_in =  [inc_s_name, 'Steam (' + u_name + ')', 'Air (' + u_name + ')']
    else:
        Dryer.expected_flows_in =  [inc_s_name, 'Steam (' + u_name + ')', 'Air (' + u_name + ')', 'Electricity (' + u_name + ')']
    
    Dryer.expected_flows_out = ['Stack (' + u_name + ')', 'Dried ' + inc_s_name, 'Condensate (' + u_name + ')']
    Dryer.coefficients = {'Ambient temperature' : amb_t, 'Steam temperature' : steamtemp, 'Steam pressure' : steampress, 'Moisture out' : moisture_out,
                          'Electricity demand' : elec_demand, 'Air temperature' : air_temp, 'Exhaust temperature' : exhaust_temperature,
                          'Dried flow temperature' : dryflow_temp, 'Latent heat of vaporization' : hvap, 'Cp water' : cp_water, 
                          'Cp air' : cp_air, 'Cp dry matter' : dry_matter_cp, 'Losses' : heat_loss
                          }
    def Dryerfunc(flow, coeff):
        mass_flow_in = flow.attributes['mass_flow_rate']
        amb_t = coeff['Ambient temperature']
        t_steam = coeff['Steam temperature']
        steam_p = coeff['Steam pressure']
        Q_flow = flow.attributes['heat_flow_rate']
        t_in = flow.attributes['temperature']
        moisture_in = flow.attributes['composition'][flow.attributes['components'].index('Water')]
        water_in = flow.attributes['mass_flow_rate'] * moisture_in
        moisture_out = coeff['Moisture out']
        electricity_amount = mass_flow_in * coeff['Electricity demand'] / 1000
        dry_matter = flow.attributes['mass_flow_rate'] - water_in
        water_out = (moisture_out/(1 - moisture_out)) * dry_matter
        dried_flow_amount = dry_matter + water_out
        water_exhaust = water_in - water_out
        air_t = coeff['Air temperature']
        exhaust_t = coeff['Exhaust temperature']
        dry_flow_t = coeff['Dried flow temperature']
        hvap = coeff['Latent heat of vaporization']
        cp_water = coeff['Cp water']
        cp_air = coeff['Cp air']
        cp_dry = coeff['Cp dry matter']
        Q_water_evap = water_exhaust * (hvap + cp_water * (exhaust_t - t_in))
        Q_dried_flow = ((water_out * cp_water) + (dry_matter * cp_dry)) * (dry_flow_t - t_in)
        m_air = (Q_water_evap + Q_dried_flow - Q_flow) / (cp_air * (air_t - exhaust_t))
        Q_air_exhaust = m_air * cp_air * (air_t - amb_t)
        Q_in = (Q_water_evap + Q_dried_flow + Q_air_exhaust - Q_flow)/(1 - coeff['Losses'])
        Q_losses = coeff['Losses'] * Q_in
        m_steam = Q_in / hvap
        Q_condensate = m_steam * cp_water * (t_steam - amb_t)
        Q_steam = Q_condensate + Q_in
        m_exhaust = water_exhaust + m_air
        Q_exhaust = Q_water_evap + Q_air_exhaust
        components = flow.attributes['components']
        in_composition = flow.attributes['composition']
        dry_composition_in = 1 - flow.attributes['composition'][flow.attributes['components'].index('Water')]
        dry_composition_out = 1 - moisture_out
        out_composition = []
        for component in components:
            if component == 'Water':
                out_composition.append(moisture_out)
            else:
                in_comp = flow.attributes['composition'][flow.attributes['components'].index(component)]
                out_composition.append(in_comp * (dry_composition_out/dry_composition_in))
        
        return [{'name' : 'Electricity (' + u_name + ')', 'components' : None, 'mass_flow_rate' : 0,
                 'flow_type': 'Electricity', 'temperature' : 0,  'In or out' : 'In', 'elec_flow_rate' : electricity_amount ,  'Set calc' : False, 'Set shear' : False},     
                {'name' : 'Air (' + u_name + ')', 'components' : ['Air'], 'composition': [1], 'mass_flow_rate' : m_air,
                         'flow_type': 'Air', 'temperature' : amb_t, 'pressure':1 , 'heat_flow_rate' :0 ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
                {'name' : 'Condensate (' + u_name + ')', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                         'flow_type': 'Condensate', 'temperature' : t_steam, 'pressure':steam_p , 'heat_flow_rate' :Q_condensate ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
                {'name' : 'Steam (' + u_name + ')', 'components' : ['Water'], 'composition': [1], 'mass_flow_rate' : m_steam,
                         'flow_type': 'Steam', 'temperature' : t_steam, 'pressure': steam_p , 'heat_flow_rate' :Q_steam ,'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
                {'name' : 'Stack (' + u_name + ')', 'components' : ['Water', 'Air'], 'composition': [water_exhaust/m_exhaust, m_air/m_exhaust], 'mass_flow_rate' : m_exhaust,
                         'flow_type': 'Exhaust', 'temperature' : exhaust_t, 'pressure':1 , 'heat_flow_rate' :Q_exhaust ,'In or out' : 'Out', 'Set calc' : False, 'Set shear' : False},
                {'name' : 'Dried ' + inc_s_name, 'components' : components, 'composition': out_composition, 'mass_flow_rate' : dried_flow_amount,
                         'flow_type': 'Process', 'temperature' : dry_flow_t, 'pressure':1 , 'heat_flow_rate' :Q_dried_flow ,'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False}
                ]
    Dryer.calculations = {inc_s_name : Dryerfunc}
    return Dryer

## Here is how to make a simple Dryer unit called 'Wood dryer' that takes in a stream called 'Wood' with the base coefficients
# WoodDryer = MakeSteamDryer('Wood dryer', 'Wood')



# FlowA = Flow('Wood',['Water','Wood'],'input', 25, 1, [0.6, 0.4 ], None , None, 1000, np.nan, 0)
# FlowA.set_calc_flow()
# print(WoodDryer)

# allflows.append(FlowA)
# WoodDryer.attach_available_flow()
# WoodDryer.calc()
# print(WoodDryer)
# for flow in allflows:
    
#     print(flow)
    
# print(WoodDryer)