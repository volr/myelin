import numpy as np

from elephant.spike_train_generation import homogeneous_poisson_process
from quantities import Hz, s, ms

def square(xs):
    return xs*xs

def function_task(f = square, min = -10, max = 10, step = 0.5):
    """
    """
    xs = np.arange(start = min, stop = max, step = step)
    ys = f(xs)
    return xs, ys

def transform_to_log_domain_and_shift(ys):
    """Transform a data series into the log domain
    by shifting it up by its minimum value and 
    taking the log.
    """
    min_y = np.min(ys)
    return np.log(1 + ys + min_y)

def network_input(ys, i, j):
    """
    """
    return ys[i:j]

def generate_spiketrain_list(ys):
    return [
        homogeneous_poisson_process(rate=10.0*y*Hz, t_start=0.0*s, t_stop=100.0*s)
        for y in ys]


def debug_spike_train(spiketrain_list, i, j):
    import matplotlib.pyplot as plt

    # plot horizontal bars and shade region between
    # x_low and x_high

    t = spiketrain_list[0].rescale(ms)
    y_low = i * np.ones_like(t)
    y_high = j * np.ones_like(t)
    plt.fill_between(t, y_low, y_high)

    for i, spiketrain in enumerate(spiketrain_list):
        t = spiketrain.rescale(ms)
        plt.plot(t, i * np.ones_like(t), 'k.', markersize=2)

    plt.axis('tight')
    plt.xlim(0, 1000)
    plt.xlabel('Time (ms)', fontsize=16)
    plt.ylabel('Spike Train Index', fontsize=16)
    plt.gca().tick_params(axis='both', which='major', labelsize=14)
    plt.show()

# http://elephant.readthedocs.io/en/latest/tutorial.html
# use to generate input
# and then use in spike_source_array

#debug_spike_train(spiketrain_list = generate_spiketrain_list(function_task()[1]), i=1, j=3)