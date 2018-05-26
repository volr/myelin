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
    return nest.Create(node.type, node.num_neurons, params=node.parameters)


def execute(conf):
    """Execute a given nest configuration.

    Args:
        conf (addict.Dict): Network configuration to be executed.
    """
    blocks = conf.network.blocks
    block = blocks[0]
    nodes = {}
    simtime = 100.0

    nest.ResetKernel()
    for node in block.nodes:
        nodes[node.id] = create_node(node)

    for edge in block.edges:
        create_edge(nodes, edge)

    nest.Simulate(simtime)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='nest executor')
    args = parser.parse_args()
    conf = addict.Dict(json.load(sys.stdin))
    execute(conf)
