import nest



def discover(node_type):
    nodes = nest.Models(mtype = 'nodes')
    for node in nodes:
        if (node == 'sli_neuron'):
            continue
        defaults = nest.GetDefaults(node)
        if (node_type == ''):
            print(node)
        if (defaults['element_type'] == node_type):        
            print(node)

if __name__ == '__main__':
    print('-------- neurons -----------')
    discover('neuron')
    print('-------- recorders ---------')
    discover('recorder')
    print('-------- stimulators -------')
    discover('stimulator')

