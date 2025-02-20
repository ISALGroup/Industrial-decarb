# -*- coding: utf-8 -*-
"""
Created on Thu Feb 13 16:02:14 2025

@author: Antoine
"""

# -*- coding: utf-8 -*-
"""
Created on Fri Nov 15 12:04:45 2024

@author: Antoine
"""

import pandas as pd
import numpy as np
import csv
import inspect

allflows = []
processunits = []

flow_dataframe = pd.DataFrame(columns=['name', 'components', 'flow_type',
                                       'temperature', 'pressure',
                                       'composition', 'origin', 'destination',
                                       'mass_flow_rate','elec_flow_rate',
                                       'heat_flow_rate','combustion_energy_content'])



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
        l5 = 'Electricity flow rate : '+ str(self.attributes['elec_flow_rate']) + 'kW.  Combustion energy contained: ' + str(self.attributes['combustion_energy_content']) + 'kJ / hr.\n \n'
        return   l1 + l2 + l3 +l4 + l5
    
        
    def set_destination(self, Destination_unit):
        if self.attributes['destination'] == Destination_unit.name and self.attributes['name'] in Destination_unit.input_flows:
            pass  # Do nothing if already correctly assigned
        elif self.attributes['destination'] != Destination_unit.name:
            if self.attributes['destination']:  # Remove from previous destination if exists
                prev_unit = next((unit for unit in processunits if unit.name == self.attributes['destination']), None)
                if prev_unit:
                    prev_unit.input_flows.remove(self.attributes['name'])
            # Add to new destination
            Destination_unit.input_flows.append(self.attributes['name'])
            self.attributes['destination'] = Destination_unit.name
        print(f"Flow {self.attributes['name']} set destination to {self.attributes['destination']}")


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
                 calculations = None, expected_flows_in = None, expected_flows_out = None, coefficients = None, is_calc = False, required_calc_flows = 1):
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
        self.required_calc_flows = required_calc_flows
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
    
    
    def count_calc_flows(self, flowlist = allflows):
        count = 0
        for inflow in self.input_flows:
            for Flow in flowlist:
                if Flow.is_calc_flow and inflow == Flow.attributes['name']:
                    count += 1
        for outflow in self.output_flows:
            for Flow in flowlist:
                if Flow.is_calc_flow and outflow == Flow.attributes['name']:
                    count += 1
        return count
            
    def calc(self, flowlist = allflows, unitlist = processunits):
        if self.required_calc_flows == 1:
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
                        if 'Set shear' not in flow:
                            self.set_flow(flow, flowlist, unitlist)
                        elif not flow['Set shear']:
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
                        if 'Set shear' not in flow:
                            self.set_flow(flow, flowlist)
                        elif not flow['Set shear']:
                            self.set_flow(flow, flowlist)
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
        
        else:
            if self.is_calc:
                print('here')
                return
            elif self.count_calc_flows(flowlist) != self.required_calc_flows:
                print('there')
                return
            else:
                print('no, here')
                calcflows = []
                for calcflow in self.calculations[0]:
                    if calcflow in self.input_flows or calcflow in self.output_flows:
                        for Flow in flowlist:
                            if Flow.attributes['name'] == calcflow:
                                calcflows.append(Flow)
                calculations = self.calculations[1](calcflows, self.coefficients)
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
                    if 'Set shear' not in flow:
                        self.set_flow(flow, flowlist)
                    elif not flow['Set shear']:
                        self.set_flow(flow, flowlist)
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
    
    def check_mass_balance(self):
        if self.are_all_flows_attached():
            m_in = 0
            m_out = 0
            for flow in self.input_flows:
                for Flow in allflows:
                    if Flow.attributes['name'] == flow:
                        m_in += Flow.attributes['mass_flow_rate']
                        break
                    
            for flow in self.output_flows:
                for Flow in allflows:
                    if Flow.attributes['name'] == flow:
                        m_out += Flow.attributes['mass_flow_rate']
                        break
            if (m_out-m_in)/m_in < 0.001:
                print(self.name + ' mass balance ok: mass in = ' + str(m_in) + 'mass out = ' + str(m_out) )
                return True
            else:
                print(self.name + ' mass balance not ok: mass in = ' + str(m_in) + 'mass out = ' + str(m_out) )
                return False
            
            
    
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
            
MyFlow = Flow(name = 'Logs', components = ['Dry wood', 'Dry bark', 'Water'], flow_type = 'Input flow',
             temperature=25, pressure = 1, composition = [0.35, 0.05, 0.6], origin = None, 
             destination = None, mass_flow_rate = 1000, elec_flow_rate = 0, 
             heat_flow_rate = 0, combustion_energy_content = 0,  is_calc_flow = True, is_shear_stream = False)

MyFlow.attributes['mass_flow_rate']


MyUnit = Unit('MyUnit')
MyUnit.expected_flows_in = ['Logs', 'Electricity (from Debarker)']
MyUnit.expected_flows_out = ['Bark', 'Debarked logs']
MyUnit.coefficients = {'Power per t log' : 8}

def Debarkfunc_logs(log_flow, coefficients):
    log_amount = log_flow.attributes['mass_flow_rate']
    electricity_amount = coefficients['Power per t log'] * log_amount/1000.
    bark_index = log_flow.attributes['components'].index('Dry bark')
    wood_index = log_flow.attributes['components'].index('Dry wood')
    moisture_index = log_flow.attributes['components'].index('Water')
    bark_ratio = log_flow.attributes['composition'][bark_index]
    wood_ratio = log_flow.attributes['composition'][wood_index]
    moisture_ratio = log_flow.attributes['composition'][moisture_index]
    dry_bark_ratio = bark_ratio/(bark_ratio + wood_ratio)
    dry_wood_ratio = wood_ratio/(bark_ratio + wood_ratio)
    wood_flow_amount = dry_wood_ratio * log_amount
    bark_flow_amount = dry_bark_ratio * log_amount
    return [{'name' : 'Electricity (debarker)', 'components' : None, 'mass_flow_rate' : 0,
             'flow_type': 'Electricity', 'elec_flow_rate' : electricity_amount, 'In or out' : 'In', 'Set calc' : False, 'Set shear' : False},
            {'name' : 'Bark', 'components' : ['Dry bark', 'Water'], 'composition': [1-moisture_ratio, moisture_ratio], 'mass_flow_rate' : bark_flow_amount,
                     'flow_type': 'Process stream', 'temperature' : 25, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False},
            {'name' : 'Debarked logs', 'components' : ['Dry wood', 'Water'], 'composition' : [1-moisture_ratio, moisture_ratio] , 'mass_flow_rate' : wood_flow_amount,
                     'flow_type': 'Process stream', 'temperature' : 25, 'pressure':1 , 'In or out' : 'Out', 'Set calc' : True, 'Set shear' : False}]

MyUnit.calculations = {'Logs' : Debarkfunc_logs}


# Unit2.attach_available_flow()
# Unit2.calc()
# Unit3.attach_available_flow()
# Unit3.calc()
# def all_units_calc(unit_list):
#     return all(unit.is_calc for unit in unit_list)



# calculate_flow_sheet = True

# if calculate_flow_sheet:
#     calced = False
#     while not calced:
#         print(calced)
#         for unit in processunits:
#             print(unit.name)
#             unit.attach_available_flow()
#             if unit.is_fully_calc():
#                 pass
            
#             elif len(unit.input_flows) != 0 or len(unit.output_flows) != 0:
#                 print('Getting calced')
#                 unit.calc()
#                 unit.is_calc = True
#         calced = all_units_calc(processunits)




# FlowA.set_destination(Unit16)
# FlowA.set_calc_flow()

# Unit16.calc()


# for flow in allflows:
#     print(flow.attributes)

# print(len(allflows))

def are_units_calced(unit_list):
    ulist = unit_list
    are_u_calced = True
    for unit in ulist:
        if not unit.is_calc:
            are_u_calced = False
            return are_u_calced
    return are_u_calced

processunits.append(MyUnit)
allflows.append(MyFlow)
processunits[0].attach_available_flow(allflows)
processunits[0].calc(allflows, processunits)
for flow in allflows:
    print(flow)
# print(are_units_calced(processunits))

# for unit in processunits:
#     unit.is_calc = True

# print(are_units_calced(processunits))

def print_flows(flowlist):
    for flow in flowlist:
        print(flow)

f_print = False

for unit in processunits:
    print(unit.is_calc)
    
# def main():
#     while not are_units_calced(processunits):
#         for unit in processunits:
#             unit.attach_available_flow()
#             unit.calc()
#     if f_print:
#         print_flows(allflows)
#         print(len(allflows))
    
# main()
# for unit in processunits:
#     print(unit.check_heat_balance())

flows_to_file = False
unit_recap_to_file = False
utilities_recap = False

if flows_to_file:
    with open('pulp_plant_flows.csv', 'w', newline='') as csvfile:
        fieldnames = ['name', 'components', 'flow_type',
                                               'temperature', 'pressure',
                                               'composition', 'origin', 'destination',
                                               'mass_flow_rate','elec_flow_rate',
                                               'heat_flow_rate','combustion_energy_content', 'Set shear', 'In or out', 'Set calc']
        
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for flow in allflows:
            writer.writerow(flow.attributes)

if unit_recap_to_file:
    with open('pulp_plant_units.csv', 'w', newline='') as csvfile:
        unitwriter = csv.writer(csvfile)
        for unit in processunits:
            unitwriter.writerow([unit.name])
            unitwriter.writerow(['Flows in'])
            unitwriter.writerow(['Name', 'Temperature' ,'Mass flow rate', 'Heat flow rate', 'Electricity'])
            for flowin in unit.input_flows:
                flowin_index = find_Flow_index(flowin, allflows)
                Flow = allflows[flowin_index]
                
                unitwriter.writerow([flowin, str(Flow.attributes['temperature']) ,
                                     str(Flow.attributes['mass_flow_rate']), str(Flow.attributes['heat_flow_rate']), str(Flow.attributes['elec_flow_rate'])])
            
            unitwriter.writerow(['Flows out'])
            unitwriter.writerow(['Name', 'Temperature' ,'Mass flow rate', 'Heat flow rate', 'Electricity'])
            for flowin in unit.output_flows:
                flowin_index = find_Flow_index(flowin, allflows)
                Flow = allflows[flowin_index]
                unitwriter.writerow([flowin, str(Flow.attributes['temperature']) ,
                                     str(Flow.attributes['mass_flow_rate']), str(Flow.attributes['heat_flow_rate']), str(Flow.attributes['elec_flow_rate'])])

            if hasattr(unit, 'heat_loss'):
                unitwriter.writerow(['Losses', 'Heat loss'])
                unitwriter.writerow(['', unit.heat_loss])
            
                
            if hasattr(unit, 'reaction_heat'):
                unitwriter.writerow(['Reaction heat', ''])
                unitwriter.writerow(['', unit.reaction_heat])
                
                
if utilities_recap:
    with open('pulp_plant_utilites.csv', 'w', newline='') as csvfile:
        utilwriter = csv.writer(csvfile)
        utilwriter.writerow(['Unit name', 'Heat demand (Steam kJ)', 'Heat demand (Fuel kJ)', 'Electricity demand (kWh)', 'Heat produced (kJ)', 'Fuel produced (kJ)' ,'Electricity produced (kWh)', 'Waste heat (kJ)'])
        for unit in processunits:
            hd_s = 0
            hd_f = 0
            ed = 0
            h_p = 0
            f_p = 0
            ep = 0
            wh = 0
            for flowin in unit.input_flows:
                flowin_index = find_Flow_index(flowin, allflows)
                Flow = allflows[flowin_index]
                
                if Flow.attributes['flow_type'] == 'Steam':
                    hd_s += Flow.attributes['heat_flow_rate']
                
                if Flow.attributes['flow_type'] == 'Fuel':
                    hd_f += Flow.attributes['combustion_energy_content']
                
                if Flow.attributes['flow_type'] == 'Electricity':
                    ed += Flow.attributes['elec_flow_rate']
                
            for flowout in unit.output_flows:
                flowout_index = find_Flow_index(flowout, allflows)
                Flow = allflows[flowout_index]
                
                if Flow.attributes['flow_type'] == 'Condensate':
                    hd_s += (- Flow.attributes['heat_flow_rate'])
                
                if Flow.attributes['flow_type'] == 'Fuel (produced on-site)':
                    f_p += Flow.atributes['combustion_energy_content']
                
                if Flow.attributes['flow_type'] == 'Electricity (produced on-site)':
                    ep += Flow.attributes['elec_flow_rate']
                
                if Flow.attributes['flow_type'] == 'Steam (produced on-site)':
                    h_p += Flow.attributes['heat_flow_rate']
                
                if Flow.attributes['flow_type'] == 'Waste' or Flow.attributes['flow_type'] == 'Waste water' or Flow.attributes['flow_type'] == 'Exhaust':
                    wh += Flow.attributes['heat_flow_rate']
            
            utilwriter.writerow([unit.name, str(hd_s), str(hd_f), str(ed), str(h_p), str(f_p) ,str(ep), str(wh)])

            