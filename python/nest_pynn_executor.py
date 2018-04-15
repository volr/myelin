import pyNN.nest as pynn

import json
import sys
import argparse

import logging
logging.basicConfig()

recordingPopulation = ""

def create_edge(nodes, edge, outputs = {}):
    global recordingPopulation
    projection_type = edge["projection_type"]["kind"]
    if ((not isinstance(nodes[edge["output"]["id"]], pynn.Population)) and nodes[edge["output"]["id"]]["type"] == "output"):
        recordingPopulation = nodes[edge["input"]["id"]]
    elif (projection_type == "all_to_all"):
        assert(edge['projection_target']['kind'] == 'static') # only support static connectivity for now
        target = edge['projection_target']['effect']
        projection = pynn.Projection(nodes[edge["input"]["id"]],
                        nodes[edge["output"]["id"]],
                        method=pynn.AllToAllConnector(),
                        target=target)
        projection.setWeights(edge["projection_type"]["weight"])
    else:
        print "not yet supported"

def create_node(node):
    kind = node["type"]
    if (kind == "population"):
        neuron = node["neuron_type"]
        return pynn.Population(node["num_neurons"],
                                pynn.IF_curr_alpha,
                                cellparams={k:v for k,v in node["neuron_type"].items() if k not in ["type"]})
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

def execute(conf):
    global recordingPopulation
    # NEST specific stuff
    pynn.setup()

    net = conf["network"]
    blocks = net["blocks"]
    # only support one block for now
    b = blocks[0]

    nodes = {}
    outputs = {}

    for node in b["nodes"]:
        nodes[node["id"]] = create_node(node)
    for proj in b["edges"]:
        create_edge(nodes, proj, outputs)

    recordingPopulation.record()
    recordingPopulation.record_v()

    pynn.run(conf["simulation_time"])

    recordingPopulation.printSpikes("spikes.ras")
    recordingPopulation.print_v("network.v")

    pynn.end()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='nest pynn executor')
    args = parser.parse_args()
    conf = json.load(sys.stdin)
    assert conf["execution_target"]["kind"] == "nest"
    execute(conf)
