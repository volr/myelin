
from io import open
import json
import sys
import addict
import numpy as np

from argparse import ArgumentParser
from past.builtins import execfile
from future.utils import raise_

def execute_target(target, version, experiment, train, test):
    """Executes an experiment (object) on given target (e. g. nest) with a
       given version (e. g. 2.14.0)"""
    path = target + "_" + version
    executor = __import__(path, fromlist = [version])
    executor.execute(experiment, train, test)

def read_data(file):
    with open(file, 'r') as f:
        (train, test) = json.load(f)
        (train, test) = (np.array(train), np.array(test))
        (x_train, y_train) = (train[:,0], train[:,1])
        (x_test, y_test)   = (test[:,0], test[:,1])
        return ((x_train, y_train), (x_test, y_test))

def read_experiment(input):
    """Reads the content of an experiment as JSON from a given file. If the
       input is `None`, the experiment is read from stdin"""
    if input == None:
        source = "/dev/stdin"
    else:
        source = input

    with open(source, 'r') as file:
        data = json.load(file)
        return addict.Dict(data)

if __name__ == '__main__':
    """Parses and executes the Myelin model JSON input using a given target"""
    parser = ArgumentParser(description="Execute a myelin model on a given target")
    parser.add_argument("target", type=str,
                        help="The target to execute the model on, e. g. 'nest', 'nest2.14', 'brainscales' etc.")
    parser.add_argument("-v", "--version", metavar="target-version",
                        type=str, default="latest",
                        help="Exact version of the target to use. Defaults to 'latest'")
    parser.add_argument("-i", "--input", metavar="input-file",
                        type=str, default=None,
                        help="Input file for experiment JSON. Defaults to stdin")
    parser.add_argument("-d", "--data", metavar="data-file",
                        type=str, default=None,
                        help="Input file for data in JSON. Should be formatted"+
                              "as a tuple of training and testing each containing" +
                              "a tuple of data and labels.")

    args = parser.parse_args()
    # Reads and extracts experiment as an object
    experiment = read_experiment(args.input)
    (train, test) = read_data(args.data)
    # Attempt to execute experiment
    try:
        target = args.target
        version = args.version
        execute_target(target, version, experiment, train, test)
    except Exception as e:
        raise_(Exception, "Error running target '{}' in version '{}': {}".format(target, version, e), sys.exc_info()[2])
