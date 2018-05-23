
import pyNN.nest as pynn

from collections import defaultdict
import argparse
import json
import sys
import addict

from pyNN.utility import init_logging
init_logging("logfile", debug=False)

def create_edge(nodes, edge):
    """ 
    
    """
    projection_type = edge.projection_type.kind
    
    if projection_type == "all_to_all" and isinstance(nodes[edge.output.id], pynn.Population):
        assert(edge.projection_target.kind == 'static') # only support static connectivity for now
        target = edge.projection_target.effect
        projection = pynn.Projection(
            nodes[edge.input.id],
            nodes[edge.output.id],
            method=pynn.AllToAllConnector(),
            target=target)
        weight = edge.projection_type.weight
        if edge.projection_target.effect == 'inhibitory' and weight > 0:
            weight = weight * -1

        projection.setWeights(weight)
    else:
        print("not yet supported")

def create_population(node):
    """

    """
    neuron = node.neuron_type
    neuron_model = None

    def cellparams(neuron):
        {k:v for k,v in neuron.items() if k not in ["type"]}

    if neuron.type == 'IFCurrentAlpha':        
        neuron_model = pynn.IF_curr_alpha()
    elif neuron.type == 'IFCondAlpha':
        neuron_model = pynn.IF_cond_alpha()
    elif neuron.type == 'IFSpikey':
        assert False # not supported
    elif neuron.type == 'IFCurrExp':
        neuron_model = pynn.IF_curr_exp()
    elif neuron.type == 'IFCondExp' :
        neuron_model = pynn.IF_cond_exp()
    elif neuron.type == 'Izhikevich':
        assert False # not supported
    elif neuron.type == 'EIFCondExp':
        neuron_model = pynn.EIF_cond_exp_isfa_ista()
    elif neuron.type == 'EIFCondAlpha':
        neuron_model = pynn.EIF_cond_alpha_isfa_ista()
    elif neuron.type == 'HHCondExp':
        neuron_model = pynn.HH_cond_exp()
    else:
        assert False # unreachable

    return pynn.Population(node.num_neurons, neuron_model, cellparams=cellparams(neuron))

def create_node(node):
    """

    """
    kind = node.type
    if (kind == "population"):
        return create_population(node)
    elif (kind == "spike_source_array"):
        return pynn.Population(1, pynn.SpikeSourceArray, {'spike_times': node.spike_times}, label= 'input')
    elif (kind == "input"):
        print("not yet supported")
        pass
    elif (kind == "output"):
        return node
    elif (kind == "spike_source_poisson"):
        print("Not supported")
        pass
    else:
        assert False

def spikes_to_json(spikes):
    """

    """
    spiking_neurons = defaultdict(list)
    for spike in spikes:
        spiking_neurons[int(spike[0])].append(spike[1])

    return spiking_neurons.values()

def execute(conf):
    # NEST specific stuff
    pynn.setup()

    net = conf.network
    blocks = net.blocks
    # only support one block for now
    b = blocks[0]

    nodes = {}
    outputs = {}

    for node in b.nodes:
        nodes[node.id] = create_node(node)
        if "record_spikes" in node and node.record_spikes:
            outputs[node.label] = nodes[node.id]
    for proj in b.edges:
        create_edge(nodes, proj)

    # Start recordings
    for label in outputs:
        outputs[label].record()

    pynn.run(conf.simulation_time)

    # Collect spikes
    for label in outputs:
        outputs[label] = spikes_to_json(outputs[label].getSpikes())
    pynn.end()

    return json.dumps(outputs, separators=(',', ':'))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='nest pynn executor')
    args = parser.parse_args()
    conf = addict.Dict(json.load(sys.stdin))
    assert conf.execution_target.kind == "nest"
    result = execute(conf)
    print(result)
