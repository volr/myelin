import json
import sys
import argparse

"""

pynn.setup(mapping_offset = cfg.mapping_offset)

i = 0
for pop in cfg.populations:
    population[i] = pynn.Population(dims = pop.dimensions, 
                                    cellclass = pynn.IF_facets_hardware1(parameters = pop.parameters), 
                                    label = pop.label)
    i = i + 1

# All possible connector types are:
#
# connector = pynn.AllToAllConnector(allow_self_connections = True, weights = 1.0)
# connector = pynn.FixedNumberPostConnector(n = 10, allow_self_connections = True, weights = 1.0)
# connector = pynn.FixedNumberPreConnector(n = 10, allow_self_connections = True, weights = 1.0)
# connector = pynn.FromListConnector(conn_list = [(pre, post, weight, None), (...), ...])
# connector = pynn.OneToOneConnector(weights = 1.0, delays = None)

# All possible spike sources are:
# 
# sources = pynn.SpikeSourceArray(parameters = {'spike_times': [1, 10, 20]})
# sources = pynn.SpikeSourcePoisson(parameters = {'duration': 1000., 'rate': 1.0, 'start': 0.0})

# Record from a specific population
# 
# pop.record() 

# pynn.minExcWeight
# pynn.maxExcWeight


for proj in cfg.projections:    
    pynn.Projection(
        presynaptic_population = populations[proj.presynaptic_population.idx],
        postsynaptic_population = populations[proj.postsynaptic_population.idx],
        method = proj.connector,
        target = proj.target
    )

pynn.run(simtime = cfg.simtime)

"""

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='spikey pynn executor')
    args = parser.parse_args()
    conf = json.load(sys.stdin)
    print conf
    # print("Hello World.")
