import spynnaker8 as pynn

def create_edge():
    if projection_type == "all_to_all" and isinstance(nodes[edge["output"]["id"]], pynn.Population):
        assert(edge['projection_target']['kind'] == 'static') # only support static connectivity for now
        target = edge['projection_target']['effect']
        projection = pynn.Projection(nodes[edge["input"]["id"]],
                                     nodes[edge["output"]["id"]],
                                     method=pynn.AllToAllConnector(),
                                     target=target)
        weight = edge["projection_type"]["weight"]
        if edge['projection_target']['effect'] == 'inhibitory' and weight > 0:
            weight = weight * -1            
        projection.setWeights(weight)
    else:
        print "not yet supported"

def create_node(node):
    kind = node["type"]
    if (kind == "population"):
        neuron = node["neuron_type"]
        neuron.pop("e_rev_E", None) # Remove the 'e_rev_E' and 'e_rev_I' who're is not allowed
        neuron.pop("e_rev_I", None)
        return pynn.Population(node["num_neurons"],
                                pynn.IF_curr_alpha,
                                cellparams={k:v for k,v in node["neuron_type"].items() if k not in ["type"]})
    elif (kind == "spike_source_array"):
        return pynn.Population(1, pynn.SpikeSourceArray, {'spike_times': node["spike_times"]}, label= 'input')
    elif (kind == "input"):
        print "not yet supported"
        pass
    elif (kind == "output"):
        return node
    elif (kind == "spike_source_poisson"):
        print "Not supported"
        pass
    else:
        assert False


def execute(conf):
    pynn.setup()

    net = conf["network"]
    blocks = net["blocks"]
    # only support one block for now
    b = blocks[0]

    nodes = {}
    outputs = {}

    for node in b["nodes"]:
        nodes[node["id"]] = create_node(node)
        if "record_spikes" in node and node["record_spikes"]:
            outputs[node["label"]] = nodes[node["id"]]
    for proj in b["edges"]:
        create_edge(nodes, proj)

    # Start recordings
    for label in outputs:
        outputs[label].record()

    pynn.run(conf["simulation_time"])

    # Collect spikes
    for label in outputs:
        outputs[label] = spikes_to_json(outputs[label].getSpikes())
    pynn.end()

    return json.dumps(outputs, separators=(',', ':'))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='spinnaker pynn executor')
    args = parser.parse_args()
    conf = json.load(sys.stdin)
    assert conf["execution_target"]["kind"] == "nest"
    result = execute(conf)
    print(result)
        
