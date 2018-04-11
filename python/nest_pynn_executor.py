import nest
import pyNN.nest as pynn

import json
import sys
import argparse


def create_edge(nodes, edge):
    projection_type = edge["projection_type"]["kind"]
    if (projection_type == "all_to_all"):
        pynn.Projection(nodes[edge["input"]["id"]],
                        nodes[edge["output"]["id"]],
                        pynn.AllToAllConnector(),
                        StaticSynapse(weight = edge["projection_type"]["weight"]))
    else:
        print "not yet supported"
            
    

def create_node(node):
    kind = node["kind"]
    if (kind == "population"):
        neuron = node["neuron_type"]
        nrn = pynn.IF_curr_exp(
            cm = neuron["cm"],
            tau_m = neuron["tau_m"],
            tau_syn_E = neuron["tau_syn_E"],
            tau_refrac = neuron["tau_refrac"],
            v_thresh = neuron["v_thresh"],
            v_rest = neuron["v_rest"],
            v_reset = neuron["v_reset"],
            i_offset = neuron["i_offset"]
        )
        nrn.initialize(v=0.0)
        return pynn.Population(node["num_neurons"], nrn)
    elif (kind == "spike_source_array"):
        source_array = pynn.SpikeSourceArray(spike_times = node["spike_times"])
        return pynn.Population(1, source_array)
    elif (kind == "input"):
        print "not yet supported"
        pass
    elif (kind == "output"):
        print "Not yet supported"
        pass
    elif (kind == "spike_source_poisson"):
        print "Not supported"
        pass
    else:
        assert False

def execute(conf):
    nest.SetKernelStatus({"dict_miss_is_error": False})
    # NEST specific stuff
    pynn.setup()
    
    net = conf["network"]
    blocks = net["blocks"]
    # only support one block for now
    b = blocks[0]

    for node in b["nodes"]:
        nodes[node["id"]] = create_node(node)              

    print "----------------------"

    for proj in b["edges"]:
        create_edge(nodes, proj)

    pynn.run(conf["simulation_time"])

    
    
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='nest pynn executor')
    args = parser.parse_args()
    conf = json.load(sys.stdin)
    assert conf["execution_target"]["kind"] == "nest"
    execute(conf)
