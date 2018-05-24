import nest

def snake_to_camel_case(string):
   first, *rest = string.split('_')
   return first + ''.join(word.capitalize() for word in rest)

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
]

def discover(node_type):
    nodes = nest.Models(mtype = 'nodes')
    result = []

    def params(node):
        {k:v for k,v in node.items() if k not in node_key_filters}

    for node in nodes:
        if (node == 'sli_neuron'):
            continue
        defaults = nest.GetDefaults(node)
        if (node_type == ''):
            print(node)            
        if (defaults['element_type'] == node_type):
            print(node)
            print(params(node))
            result.append(node)
    return result

if __name__ == '__main__':
    print('-------- neurons -----------')
    neurons = discover('neuron')
    print('-------- recorders ---------')
    recorders = discover('recorder')
    print('-------- stimulators -------')
    stimulators = discover('stimulator')

