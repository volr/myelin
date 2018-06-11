import pyNN.nest as pynn
import pyNN

from collections import defaultdict
import argparse
import json
import sys
import addict

from pyNN.utility import init_logging
init_logging("logfile", debug=False)


def create_edge(nodes, edge):
    """Create a pyNN edge.

    Args:
        nodes (addict.Dict): Graph nodes that have been created so far.
        edge (addict.Dict): pyNN edge to be created.

    Returns:
        The pyNN projection that was created.
    """

    if not isinstance(nodes[edge.output.id], pynn.Population):
        return None

    projection_type = edge.projection_type.kind
    connector = None
    projection = None
    # TODO(Christian): pyNN does not support a lot of synapse types natively
    synapse_type = None

    if projection_type == 'all_to_all':
        connector = pynn.AllToAllConnector()
    elif projection_type == 'one_to_one':
        connector = pynn.OneToOneConnector()
    elif projection_type == 'fixed_number_post':
        connector = pynn.FixedNumberPostConnector(n=edge.projection_type.n)
    elif projection_type == 'fixed_number_pre':
        connector = pynn.FixedNumberPostConnector(n=edge.projection_type.n)
    elif projection_type == 'fixed_probability':
        connector = pynn.FixedProbabilityConnector(
            p_connect=edge.projection_type.probability)
    elif projection_type == 'from_list':
        connector = pynn.FromListConnector(
            conn_list=edge.projection_type.weights)
    else:
        assert False  # unreachable

    # only support static connectivity for now
    assert(edge.projection_target.kind == 'static')
    target = edge.projection_target.effect

    # pyNN version 0.7 and 0.9 have slightly different APIs
    if pyNN.__version__ == '0.7.6':
        projection = pynn.Projection(
            nodes[edge.input.id],
            nodes[edge.output.id],
            method=connector,
            target=target
        )
    elif pyNN.__version__ == '0.9.2':
        projection = pynn.Projection(
            nodes[edge.input.id],
            nodes[edge.output.id],
            connector=connector
        )
    else:
        assert False

    # TODO(Christian): Refactor this!
    weight = edge.projection_type.weight
    if edge.projection_target.effect == 'inhibitory' and weight > 0:
        weight = weight * -1

    projection.setWeights(weight)

    return projection


def create_population(node):
    """Create a pyNN population.

    Args:
        node (addict.Dict): Parameters of the population to be created.

    Returns:
        The pyNN Population that was created.
    """
    neuron = node.neuron_type
    neuron_model = None

    def cellparams(neuron):
        return {k: v for k, v in neuron.items() if k not in ['type']}

    # TODO(Christian): This will only work in pyNN 0.9.2 because in 0.7.x
    # the cellparams are passed separately
    if neuron.type == 'IFCurrentAlpha':
        neuron_model = pynn.IF_curr_alpha(**cellparams(neuron))
    elif neuron.type == 'IFCondAlpha':
        neuron_model = pynn.IF_cond_alpha(**cellparams(neuron))
    elif neuron.type == 'IFSpikey':
        assert False  # not supported
    elif neuron.type == 'IFCurrExp':
        neuron_model = pynn.IF_curr_exp(**cellparams(neuron))
    elif neuron.type == 'IFCondExp':
        neuron_model = pynn.IF_cond_exp(**cellparams(neuron))
    elif neuron.type == 'Izhikevich':
        assert False  # not supported
    elif neuron.type == 'EIFCondExp':
        neuron_model = pynn.EIF_cond_exp_isfa_ista(**cellparams(neuron))
    elif neuron.type == 'EIFCondAlpha':
        neuron_model = pynn.EIF_cond_alpha_isfa_ista(**cellparams(neuron))
    elif neuron.type == 'HHCondExp':
        neuron_model = pynn.HH_cond_exp(**cellparams(neuron))
    elif neuron.type == 'SpikeSourceArray':
        assert False
    elif neuron.type == 'SpikeSourcePoisson':
        assert False
    elif neuron.type == 'SpikeSourceInhGamma':
        assert False
    else:
        assert False  # unreachable

    if pyNN.__version__ == '0.7.6':
        return pynn.Population(node.num_neurons, neuron_model, cellparams=cellparams(neuron))
    elif pyNN.__version__ == '0.9.2':
        return pynn.Population(node.num_neurons, neuron_model)


def create_node(node):
    """Create a node in the pyNN graph.

    Args:
        node (addict.Dict): Parameters for node to be created.

    Returns:
        Node that has been created.
    """
    kind = node.type
    if (kind == "population"):
        return create_population(node)
    elif (kind == "spike_source_array"):
        return pynn.Population(1, pynn.SpikeSourceArray, {'spike_times': node.spike_times}, label='input')
    elif (kind == "input"):
        assert False
    elif (kind == "output"):
        return node
    elif (kind == "spike_source_poisson"):
        assert False
    else:
        assert False


def spikes_to_json(spikes):
    """Convert spikes to json format.

    Args:
        spikes: Spikes as an array of tuples.
    """
    spiking_neurons = defaultdict(list)
    for spike in spikes:
        spiking_neurons[int(spike[0])].append(spike[1])

    return spiking_neurons.values()


def execute(conf):
    """Execute a pyNN experiment.

    Args:
        conf (addict.Dict): The configuration object of the whole experiment.

    Returns:
        Outputs that have been produced by the execution.
    """
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
