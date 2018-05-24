import nest_executor as ne
import addict
import nose

def neuron_params(id):
    return addict.Dict({
        'type': 'aeif_cond_alpha',
        'num_neurons': 100,
        'id': id
    })

def edge_params(input, output):
    return addict.Dict({
        'input': {
            'type': 'aeif_cond_alpha',
            'num_neurons': 100,
            'id': 1
        },
        'output': {
            'type': 'aeif_cond_alpha',
            'num_neurons': 100,
            'id': 2
        },
        'projection_type': {
            'kind': 'all_to_all'
        }
    })

def create_node_test():
    node = ne.create_node(neuron_params(1))
    assert True

def create_edge_test():
    nodes = {}
    edges = {}
    
    nodes[1] = ne.create_node(neuron_params(1))
    nodes[2] = ne.create_node(neuron_params(2))
    edges = ne.create_edge(nodes, edge_params(nodes[1], nodes[2]))

    assert True