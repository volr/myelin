
from collections import defaultdict
import argparse
import json
import sys
import addict
import sys

# Import nest 
import pyNN.nest as pynn

from pyNN.utility import init_logging
init_logging("logfile", debug=False)

# Import model utils
import pynn_model

def spikes_to_json(spikes):
    """Convert spikes to json format.

    Args:
        spikes: Spikes as an array of tuples.
    """
    spiking_neurons = defaultdict(list)
    for spike in spikes:
        spiking_neurons[int(spike[0])].append(spike[1])

    return spiking_neurons.values()


def execute(conf, train, test):
    """Execute a supervised pyNN experiment.

    Args:
        conf (addict.Dict): The configuration object of the whole experiment.
        train: Training data
        test: Testing data

    Returns:
        Outputs that have been produced by the execution.
    """
    pynn.setup()

    net = conf.network
    blocks = net.blocks
    # only support one block for now
    b = blocks[0]
    model = pynn_model.Model(b)
    model.train(train)
    model.predict(test)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='nest pynn executor')
    parser.add_argument("-i", "--input", metavar="input-file",
                        type=str, default=sys.stdin,
                        help="Input file for experiment JSON. Defaults to stdin")
    parser.add_argument("-d", "--data", metavar="data-file",
                        type=str, default=None,
                        help="Input file for data in JSON. Should be formatted"+
                              "as a tuple of training and testing each containing" +
                              "a tuple of data and labels.")
    args = parser.parse_args()

    with open(args.input) as input_file:
        conf = addict.Dict(json.load(input_file))
        assert conf.execution_target.kind == "nest"

    with open(args.data) as data_file:
        data = json.load(data_file)
        train, test = data

    result = execute(conf, train, test)
    print(result)
