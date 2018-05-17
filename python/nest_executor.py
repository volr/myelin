import nest
import nest.raster_plot
import matplotlib.pyplot as plt

import json
import argparse
import addict

import sys

def create_edge(nodes, edge):
    projection_type = edge.projection_type.kind
    source = nodes[edge.input.id]
    target = nodes[edge.output.id]
    
    if (projection_type == 'all_to_all'):
        nest.Connect(source, target, conn_spec = 'all_to_all')
        return
    if (projection_type == 'one_to_one'):
        nest.Connect(source, target, conn_spec = 'one_to_one')
        return
    
    assert("projection type is not yet supported")


def create_node(node):
    if node.type == 'population':
        return nest.Create('gif_psc_exp', node.num_neurons)
    if node.type == 'spike_source_array':
        return nest.Create('poisson_generator')
    if node.type == 'output':
        return nest.Create('spike_detector')


def execute(conf):
    blocks = conf.network.blocks
    block = blocks[0]

    nodes = {}

    for node in block.nodes:
        nodes[node.id] = create_node(node)

    for edge in block.edges:
        create_edge(nodes, edge)

    # Hello World example for how a nest simulation
    # is run in the simplest case
    nest.ResetKernel()
    dt = 0.1
    simtime = 2000.0
    
    neuron_params = {"C_m": 83.1,
                     "g_L": 3.7,
                     "E_L": -67.0,
                     "Delta_V": 1.4,
                     "V_T_star": -39.6,
                     "t_ref": 4.0,
                     "V_reset": -36.7,
                     "lambda_0": 1.0,
                     "q_stc": [56.7, -6.9],
                     "tau_stc": [57.8, 218.2],
                     "q_sfa": [11.7, 1.8],
                     "tau_sfa": [53.8, 640.0],
                     "tau_syn_ex": 10.0,
    }
    

    N_ex = 100  # size of the population
    p_ex = 0.3  # connection probability inside the population
    w_ex = 30.0  # synaptic weights inside the population (pA)
    

    N_noise = 50  # size of Poisson group
    rate_noise = 10.0  # firing rate of Poisson neurons (Hz)
    w_noise = 20.0  # synaptic weights from Poisson to population neurons (pA)
    nest.SetKernelStatus({"resolution": dt})
    population = nest.Create("gif_psc_exp", N_ex, params=neuron_params)
    noise = nest.Create("poisson_generator", N_noise, params={'rate': rate_noise})
    spike_det = nest.Create("spike_detector")


    nest.Connect(
        population, population, {'rule': 'pairwise_bernoulli', 'p': p_ex},
        syn_spec={"weight": w_ex}
    )

    nest.Connect(noise, population, 'all_to_all', syn_spec={"weight": w_noise})
    nest.Connect(population, spike_det)
    nest.Simulate(simtime)
    
    nest.raster_plot.from_device(spike_det, hist=True)
    plt.title('Population dynamics')
    plt.show()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description = 'nest executor')
    args = parser.parse_args()
    conf = addict.Dict(json.load(sys.stdin))
    execute(conf)

