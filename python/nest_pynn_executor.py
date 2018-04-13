import pyNN.nest as pynn

import json
import sys
import argparse

recordingPopulation = ""

def create_edge(nodes, edge, outputs = {}):
    global recordingPopulation
    projection_type = edge["projection_type"]["kind"]
    if (nodes[edge["output"]["id"]]["type"] == "output"):
        # create a file to record into
        recordingPopulation = nodes[edge["input"]["id"]]
    elif (projection_type == "all_to_all"):
        projection = pynn.Projection(nodes[edge["input"]["id"]],
                        nodes[edge["output"]["id"]],
                        method=pynn.AllToAllConnector(),
                        target='excitatory')
        projection.setWeights(1.0)
    else:
        print "not yet supported"

def create_node(node):
    kind = node["type"]
    if (kind == "population"):
        neuron = node["neuron_type"]
        return pynn.Population(node["num_neurons"], 
                                pynn.IF_curr_alpha, 
                                cellparams={k:v for k,v in d.items() if k not in ["type"]})
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

    print "----------------------"

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

