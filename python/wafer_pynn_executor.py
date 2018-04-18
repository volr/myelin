
from collections import defaultdict
import json
import sys
import argparse
import os
import numpy as np

from pyhalbe import HICANN
import pyhalbe.Coordinate as C
from pysthal.command_line_util import init_logger

import pyhmf as pynn
from pymarocco import PyMarocco, Defects
from pymarocco.runtime import Runtime
from pymarocco.coordinates import LogicalNeuron
from pymarocco.results import Marocco

init_logger("WARN", [
    ("guidebook", "DEBUG"),
    ("marocco", "INFO"),
    ("Calibtic", "INFO"),
    ("sthal", "INFO")
])

def instrument_marocco():
    marocco = PyMarocco()
    marocco.neuron_placement.default_neuron_size(4)
    marocco.neuron_placement.minimize_number_of_sending_repeaters(False)
    marocco.merger_routing.strategy(marocco.merger_routing.one_to_one)

    marocco.bkg_gen_isi = 125
    marocco.pll_freq = 125e6

    marocco.backend = PyMarocco.Hardware
    marocco.calib_backend = PyMarocco.XML
    marocco.defects.path = marocco.calib_path = "/wang/data/calibration/brainscales/default-2017-09-26-1"
    marocco.defects.backend = Defects.XML
    marocco.default_wafer = C.Wafer(int(os.environ.get("WAFER", 21)))
    marocco.param_trafo.use_big_capacitors = True
    marocco.input_placement.consider_firing_rate(True)
    marocco.input_placement.bandwidth_utilization(0.8)
    return marocco

def set_sthal_params(wafer, gmax, gmax_div):
    """
    synaptic strength:
    gmax: 0 - 1023, strongest: 1023
    gmax_div: 1 - 15, strongest: 1
    """

    # for all HICANNs in use
    for hicann in wafer.getAllocatedHicannCoordinates():

        fgs = wafer[hicann].floating_gates

        # set parameters influencing the synaptic strength
        for block in C.iter_all(C.FGBlockOnHICANN):
            fgs.setShared(block, HICANN.shared_parameter.V_gmax0, gmax)
            fgs.setShared(block, HICANN.shared_parameter.V_gmax1, gmax)
            fgs.setShared(block, HICANN.shared_parameter.V_gmax2, gmax)
            fgs.setShared(block, HICANN.shared_parameter.V_gmax3, gmax)

        for driver in C.iter_all(C.SynapseDriverOnHICANN):
            for row in C.iter_all(C.RowOnSynapseDriver):
                wafer[hicann].synapses[driver][row].set_gmax_div(
                    C.left, gmax_div)
                wafer[hicann].synapses[driver][row].set_gmax_div(
                    C.right, gmax_div)

        # don't change values below
        for ii in xrange(fgs.getNoProgrammingPasses()):
            cfg = fgs.getFGConfig(C.Enum(ii))
            cfg.fg_biasn = 0
            cfg.fg_bias = 0
            fgs.setFGConfig(C.Enum(ii), cfg)

        for block in C.iter_all(C.FGBlockOnHICANN):
            fgs.setShared(block, HICANN.shared_parameter.V_dllres, 275)
            fgs.setShared(block, HICANN.shared_parameter.V_ccas, 800)

def create_wafer_node(node):
    kind = node["type"]
    if (kind == "population"):
        neuron = node["neuron_type"]
        neuron.pop("type", None) # Remove the 'type' which which is not allowed
        return pynn.Population(node["num_neurons"], pynn.IF_cond_exp, {
            'cm': neuron["cm"],
            'tau_m': neuron["tau_m"],
            'tau_syn_E': neuron["tau_syn_E"],
            'tau_refrac': neuron["tau_refrac"],
            'v_thresh': neuron["v_thresh"],
            'v_rest': neuron["v_rest"],
            'v_reset': neuron["v_reset"],
            'i_offset': neuron["i_offset"]
        })
    elif (kind == "spike_source_array"):
        return pynn.Population(1, pynn.SpikeSourceArray, {'spike_times': node["spike_times"]}, label= 'input')
    elif (kind == "input"):
        print "not yet supported"
        pass
    elif (kind == "output"):
        return node
    elif (kind == "spike_source_poisson"):
        print "Not supported"
        pass
    else:
        assert False

def create_wafer_edge(nodes, edge):
    projection_type = edge["projection_type"]["kind"]
    if ("type" in nodes[edge["output"]["id"]] and nodes[edge["output"]["id"]]["type"] == "output"):
        print("Not wiring output")
    elif  (projection_type == "all_to_all"):
        # only support static connectivity for now
        assert(edge['projection_target']['kind'] == 'static')
        connector = pynn.AllToAllConnector(weights=edge["projection_type"]["weight"])
        target_effect = edge['projection_target']['effect']
        pynn.Projection(nodes[edge["input"]["id"]],
                        nodes[edge["output"]["id"]],
                        connector,
                        target=str(target_effect))
    else:
        print "not yet supported"

def spikes_to_json(spikes):
    spiking_neurons = defaultdict(list)
    for spike in spikes:
        spiking_neurons[int(spike[0])].append(spike[1])

    return json.dumps(spiking_neurons.values(), separators=(',', ':'))

def execute(model):
    # Create and setup runtime
    marocco = instrument_marocco()
    runtime = Runtime(marocco.default_wafer)
    pynn.setup(marocco=marocco, marocco_runtime=runtime)

    # Build network
    net = model["network"]
    block = net["blocks"][0] # only support one block
    nodes = {}
    recordingPopulation = ""
    stimulus = ""
    for node in block["nodes"]:
        n = create_wafer_node(node)
        nodes[node["id"]] = n
        if node["type"] == "population":
            recordingPopulation = n
        elif node["type"] == "spike_source_array":
            stimulus = n

    for proj in block["edges"]:
        create_wafer_edge(nodes, proj)

    # Setup recording
    recordingPopulation.record()
    hicann = C.HICANNOnWafer(C.Enum(297))
    marocco.manual_placement.on_hicann(stimulus, hicann)
    marocco.manual_placement.on_hicann(recordingPopulation, hicann)

    # Run mapping
    marocco.skip_mapping = False
    marocco.backend = PyMarocco.None
    pynn.reset()
    pynn.run(model["simulation_time"])

    # Set hardware wafer parameters
    set_sthal_params(runtime.wafer(), gmax=1023, gmax_div=1)

    marocco.skip_mapping = True
    marocco.backend = PyMarocco.Hardware
    # Full configuration during first step
    marocco.hicann_configurator = PyMarocco.ParallelHICANNv4Configurator

    pynn.run(model["simulation_time"])
    print(spikes_to_json(recordingPopulation.getSpikes()))
    pynn.reset()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='wafer pynn executor')
    args = parser.parse_args()
    conf = json.load(sys.stdin)
    execute(conf)
