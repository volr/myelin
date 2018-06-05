import nest

import json
import argparse
import addict

import sys


def create_edge(nodes, edge):
    """Create a nest edge.

    Args:
        nodes (addict.Dict): Nodes in the network.
        edge (addict.Dict): Edge to be created.

    Returns:
        Nest edge created.
    """
    projection_type = edge.projection_type.kind
    source = nodes[edge.input.id]
    target = nodes[edge.output.id]
    return nest.Connect(source, target, projection_type)


def create_node(node):
    """Create a nest node.

    Args:
        node (addict.Dict): Node parameters of node to be created.

    Returns:
        Nest node created.
    """


    # Converting pynn node names to NEST
    # See https://github.com/NeuralEnsemble/PyNN/blob/master/pyNN/nest/standardmodels/cells.py
    if node.type == 'output':
        return nest.Create('spike_detector', params={'withtime': True, 'withgid': True})

    if node.type == 'spike_source_array':
        # https://github.com/nest/nest-simulator/blob/master/models/spike_generator.cpp
        return nest.Create('spike_generator', params=node.parameters)

    if node.neuron_type.type == 'IFCondExp':
        node.neuron_type.type = "iaf_cond_exp"

    return nest.Create(node['neuron_type']['type'], node.num_neurons, params=node.parameters)


def execute(conf):
    """Execute a given nest configuration.

    Args:
        conf (addict.Dict): Network configuration to be executed.
    """
    blocks = conf.network.blocks
    block = blocks[0]
    nodes = {}
    simtime = 100.0
    print(nest.Models)

    nest.ResetKernel()
    for node in block.nodes:
        nodes[node.id] = create_node(node)

    for edge in block.edges:
        create_edge(nodes, edge)

    nest.Simulate(simtime)
