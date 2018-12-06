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
            'id': 1
        },
        'output': {
            'id': 2,
        },
        'projection_type': {
            'kind': 'all_to_all'
        }
    })

def create_node_test():
    ne.create_node(neuron_params(1))

def create_edge_test():
    nodes = {}
    nodes[1] = ne.create_node(neuron_params(1))
    nodes[2] = ne.create_node(neuron_params(2))
    ne.create_edge(nodes, edge_params(nodes[1], nodes[2]))

def execute_test():
    test_case = addict.Dict({
        'network': {
            'blocks': [
                {
                    'nodes': [
                        {
                            'id': 0,
                            'type': 'iaf_psc_alpha',
                            'num_neurons': 1,
                            'parameters': {
                                'I_e': 376.0,
                            }
                        },
                        {
                            'id': 1,
                            'type': 'multimeter',
                            'num_neurons': 1,
                            'parameters': {
                                'withtime':True, 
                                'record_from': ['V_m']
                            }
                        },
                        {
                            'id': 2,
                            'type': 'spike_detector',
                            'num_neurons': 1,
                            'parameters': {
                                'withgid': True, 
                                'withtime': True,
                            },
                        },
                    ],
                    'edges': [
                        {
                            'input': {
                                'id': 1,
                            },
                            'output': {
                                'id': 0,
                            },
                            'projection_type': {
                                'kind': 'all_to_all',
                            },
                        },
                        {
                            'input': {
                                'id': 0,
                            },
                            'output': {
                                'id': 2,
                            },
                            'projection_type': {
                                'kind': 'all_to_all',
                            },
                        },
                    ]
                }
            ]
        }
    })
    ne.execute(test_case)
