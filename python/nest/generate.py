import nest

node_key_filters = [
    'recordables',
    'type_id',
    'elementsize',
    'vp',
    'synaptic_elements',
    'supports_precise_spikes',
    'instantiations',
    'available',
    'node_uses_wfr',
    'thread_local_id',
    'MAXERR',
    'HMIN',
    'local',
    'frozen',
    'element_type',
    'thread',
    'model',
    'archiver_length',
    'global_id',
    'capacity',
    'has_connections',
    'is_refractory',
    'linear_summation',
    'consistent_integration',
    'Interpol_Order',
    'events',
]

synapse_key_filters = [
    'sizeof',
    'label',
    'synapse_model',
    'weight_recorder'
]

def to_camel_case(str):
    components = str.split('_')
    return ''.join(x.title() for x in components)


class Node:
    def __init__(self,name, params):
        self.name = name
        self.params = params


    def type_declaration(self):
        types = {k:type(v).__name__.capitalize() for k,v in self.params.items() }
        typedecls = []
        for k,v in types.items():
            typedecls.append('_{0} :: {1}'.format(k.lower(), v))
        return '{0} {{\n        {1}\n    }}    '.format(to_camel_case(self.name), ',\n        '.join(typedecls))

    def default_declaration(self):
        defaultdecls = []

        for k,v in self.params.items():
            defaultdecls.append('_{0} = {1}'.format(k.lower(), v))

        return '{0} = {1} {{\n    {2}\n}}'.format(self.name, to_camel_case(self.name), ',\n    '.join(defaultdecls))

    def __repr__(self):
        return "{0} {1}".format(to_camel_case(self.name), self.params)
        

def discover_nodes(node_type):
    nodes = nest.Models(mtype = 'nodes')
    result = []

    def params(defaults):
        return {k:v for k,v in defaults.items() if k not in node_key_filters}

    for node in nodes:
        if (node == 'sli_neuron'):
            continue
        defaults = nest.GetDefaults(node)
        if (node_type == ''):
            print(node)            
        if (defaults['element_type'] == node_type):
            # print(node)
            # print(params(defaults))
            node = Node(node, params(defaults))
            result.append(node)
            # print(node.default_declaration())
    return result

def discover_synapses():
    synapses = nest.Models(mtype = 'synapses')
    result = []

    def params(defaults):
        return {k:v for k,v in defaults.items() if k not in synapse_key_filters}

    for synapse in synapses:
        defaults = nest.GetDefaults(synapse)
        # print(synapse)
        # print(params(defaults))
        result.append(Node(synapse, params(defaults)))

    return result

if __name__ == '__main__':

    nodes = discover_nodes('neuron') + discover_nodes('recorder') + discover_nodes('stimulator')

    res = []
    defaults = []
    for node in nodes:
        res.append(node.type_declaration())
        defaults.append(node.default_declaration())

    print("data Node =\n    {0}".format('\n    | '.join(res)))
    print('\n'.join(defaults))

    print("-------------------------")
                      
    synapses = discover_synapses()

    res = []
    defaults = []
    for synapse in synapses:
        res.append(synapse.type_declaration())
        defaults.append(synapse.default_declaration())

    print("data Synapse =\n    {0}".format('\n    | '.join(res)))
    print('\n'.join(defaults))
