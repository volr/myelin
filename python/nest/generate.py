import nest

# Keys that are filtered out because they are read-only or
# not understood.
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
    'consistent_integration',  # TODO(Christian): Whats this?
    'Interpol_Order',
    'events',
]

synapse_key_filters = [
    'sizeof',
    'label',  # TODO(Christian): Shouldn't be filtered probably
    'synapse_model',
    'weight_recorder'
]

scalar_values = [
    'gsl_error_tol'
]


def to_camel_case(str):
    components = str.split('_')
    return ''.join(x.title() for x in components)


class Node:
    """Represents one synapse / node that has been discovered
       using the pyNEST api. Only used to generate a corresponding
       Haskell default value and type declaration.

       TODO:
       - Need to make sure that the names that get converted
         to lowercase are remembered to generate a mapping between
         Haskell names and python names
    """

    def __init__(self, name, params, recordables=set()):
        self.name = name
        self.params = params
        self.recordables = recordables

    def haskell_type_declaration(self):
        """Generate a string containing a Haskell type declaration."""
        types = {k: type(v).__name__.capitalize()
                 for k, v in self.params.items()}
        typedecls = []
        for k, v in types.items():
            if v == 'Str':
                v = 'String'
            typedecls.append('_{0} :: {1}'.format(k.lower(), v))
        return '{0} {{\n        {1}\n    }}    '.format(to_camel_case(self.name), ',\n        '.join(typedecls))

    def haskell_default_declaration(self):
        """Generate a string containing a Haskell value of the defaults for this node."""
        defaultdecls = []

        for k, v in self.params.items():
            haskell_value = None
            if type(v).__name__ == 'dict':
                pass
            elif type(v).__name__ == 'tuple':
                haskell_value = v
            elif type(v).__name__ == 'ndarray':
                haskell_value = list(v)
            elif type(v).__name__ == 'str':
                haskell_value = '\"{}\"'.format(v)
            else:
                haskell_value = v
            defaultdecls.append('_{0} = {1}'.format(k.lower(), haskell_value))

        return '{0} = {1} {{\n    {2}\n}}'.format(self.name, to_camel_case(self.name), ',\n    '.join(defaultdecls))

    def __repr__(self):
        return "{0} {1}".format(to_camel_case(self.name), self.params)


def discover_nodes(node_type):
    nodes = nest.Models(mtype='nodes')
    result = []

    def params(defaults):
        return {k: v for k, v in defaults.items() if k not in node_key_filters}

    for node in nodes:
        if (node == 'sli_neuron'):
            continue
        defaults = nest.GetDefaults(node)
        if (node_type == ''):
            print(node)
        if (defaults['element_type'] == node_type):
            recordables = []
            try:
                recordables = defaults['recordables']
            except:
                pass
            result.append(Node(node, params(defaults),
                               recordables=recordables))
    return result


def discover_synapses():
    synapses = nest.Models(mtype='synapses')
    result = []

    def params(defaults):
        return {k: v for k, v in defaults.items() if k not in synapse_key_filters}

    for synapse in synapses:
        defaults = nest.GetDefaults(synapse)
        result.append(Node(synapse, params(defaults)))

    return result


if __name__ == '__main__':
    # separate calls to have the nodes in order (as oposed to alphabetical)
    nodes = discover_nodes(
        'neuron') + discover_nodes('recorder') + discover_nodes('stimulator')

    res = []
    defaults = []
    for node in nodes:
        res.append(node.haskell_type_declaration())
        defaults.append(node.haskell_default_declaration())

    print("data Node =\n    {0}".format('\n    | '.join(res)))
    print('\n'.join(defaults))

    synapses = discover_synapses()

    res = []
    defaults = []
    for synapse in synapses:
        res.append(synapse.haskell_type_declaration())
        defaults.append(synapse.haskell_default_declaration())

    print("data Synapse =\n    {0}".format('\n    | '.join(res)))
    print('\n'.join(defaults))
