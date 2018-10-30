import numpy as np
import pynn_utils
import spikeprop

class Model(object):
    """A model of a supervised neural network experiment"""

    def __init__(self, inputs, outputs):
        # Ensure that pynn is set
        assert pynn != None, "Please assign PyNN backend"

        # Only allow one input and output
        assert len(outputs) == 1, "Only one input supported for now"
        assert len(inputs) == 1, "Only one output supported for now"
        self.output_node = outputs[0]
        self.output_size = inputs[0] 

        # Create input Poisson sources
        self.input_node = self.learning_nodes[configuration.inputs[0].id]
        self.input_size = configuration.inputs[0].num_neurons

        self.input_source = pynn.Population(self.input_size,
                pynn.SpikeSourcePoisson(rate = 1.0), label = 'input')
        self.input_projection = pynn.Projection(self.input_source, self.input_node.node,
                pynn.OneToOneConnector(), pynn.StaticSynapse(weight = 1.0))
        self.pynn_nodes['input'] = self.input_source

    def set_input(self, poisson_rates):
        assert len(poisson_rates) == self.input_size

    def train(self, data):
        pass
    
    def predict(self, data, time):
        """Predicts an output by simulating the model with the given input
        Poisson rates"""
        spikes = []
        for rates in data:
            assert self.input_source.size == len(rates)
            self.input_source.set(rate=rates)
            self.simulate(time)
            spikes.append(self.output_node.spikes)
        return spikes

    def simulate(self, time):
        # Start recording
        for _, node in self.pynn_nodes.items():
            node.record('spikes')

        self.pynn.run(time)

        # Collect spikes
        for _, node in self.learning_nodes.items():
            node.store_spikes()

        self.pynn.end()
        return self

class LearningNode(object):
    """A node that can learn in the neural network, backed by a PyNN
    population."""

    def __init__(self, node):
        self.edges_in = []
        self.edges_out = []
        self.node = node 
        self.spikes = None

    def connect(self, to, edge):
        self.edges_out.append(edge)
        to.edges_in.append(edge)

    def backprop(self, errors, is_output = True):
        if is_output:
            assert len(edges_out) == 0

        output = self.get_spikes(self.node)

    def store_spikes(self):
        segments = self.node.getSpikes().segments
        assert len(segments) == 1
        self.spikes = segments[0].spiketrains        

    def set_weights(self, index, weights):
        pass
