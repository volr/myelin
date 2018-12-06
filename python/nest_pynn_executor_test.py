import nest_pynn_executor as ne
import addict


def neuron_params(id):
    return addict.Dict({
        'type': 'population',
        'num_neurons': 100,
        'neuron_type': {
            'type': 'IFCondAlpha',
        },
        'id': id,
    })


def edge_params(input_id, output_id):
    return addict.Dict({
        'input': {
            'id': input_id,
        },
        'output': {
            'id': output_id,
        },
        'projection_type': {
            'kind': 'all_to_all',
            'weight': 1.0,
        },
        'projection_target': {
            'kind': 'static',
        },
    })


def create_node_test():
    ne.create_node(neuron_params(0))


def create_edge_test():
    nodes = {}
    nodes[0] = ne.create_node(neuron_params(0))
    nodes[1] = ne.create_node(neuron_params(1))
    ne.create_edge(nodes, edge_params(0, 1))
