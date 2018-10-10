import numpy as np
import pynn_utils
import spikeprop

class Model(object):
    """A model of a supervised neural network experiment"""

    def __init__(self, pynn, configuration):
        self.pynn = pynn
        self.pynn_nodes = {}
        self.learning_nodes = {}
        self.edges = []

        for node_config in configuration.nodes:
            node = pynn_utils.create_node(pynn, node_config)
            self.pynn_nodes[node_config.id] = node
            self.learning_nodes[node_config.id] = LearningNode(node)
        for proj in configuration.edges:
            edge = pynn_utils.create_edge(pynn, self.pynn_nodes, proj)
            self.edges.append(edge)
            self.learning_nodes[proj.input.id].connect(self.learning_nodes[proj.output.id], edge)
        assert len(configuration.inputs) == 1
        self.output_node = self.learning_nodes[configuration.outputs[0].id]
        self.output_size = configuration.outputs[0].num_neurons

        # Create input poisson sources
        self.input_node = self.learning_nodes[configuration.inputs[0].id]
        self.input_size = configuration.inputs[0].num_neurons

        self.input_population = pynn.Population(self.input_size,
                pynn.SpikeSourcePoisson, {})

    def set_input(self, poisson_rates):
        assert len(poisson_rates) == self.input.size
        self.input_population

    def train(self, data):
        pass
    
    def predict(self, data):
        pass

    def simulate(self, time):
        # Start recordings
        #for label in outputs:
        #    outputs[label].record()
        self.pynn.run(time)

        # Collect spikes
        #for label in outputs:
        #    outputs[label] = spikes_to_json(outputs[label].getSpikes())
        self.pynn.end()

        #return json.dumps(outputs, separators=(',', ':'))

class LearningNode(object):
    """A node that can learn in the neural network, backed by a PyNN
    population."""

    def __init__(self, node):
        self.edges_in = []
        self.edges_out = []
        self.node = node 
        node.record('spikes')

    def connect(self, to, edge):
        self.edges_out.append(edge)
        to.edges_in.append(edge)

    def backprop(self, errors, is_output = True):
        if is_output:
            assert len(edges_out) == 0

        output = self.get_spikes(self.node)
        print(output)

    def get_spikes(self, node):
        return node.get_data()

    def set_weights(self, index, weights):
        pass
