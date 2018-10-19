import math

t_m = 0.05
t_s = 0.002

def weight_delta_output(output, expected, weights, spikes):
    """Calculates the weight delta from a connection based on the weights and spikes"""
    denominator = 0
    for weights, spike in zip(weight_list, spikes):
        denominator += weights * spike_response_derivative(output - spike)
    if denominator == 0:
        denominator = 0.1

    return (expected - output) / denominator

def weight_delta_hidden(output, errors, prev_weights, curr_weights, prev_spikes,
        curr_spikes):
    numerator = 0
    denominator = 0
    for i in range(len(errors)):
        numerator += errors[i] * curr_weights[i] * spike_response_derivative(curr_spikes[i] - output)
        denominator += prev_weights[i] * spike_response_desivative(output -
            prev_spikes[i])
    return numerator / denominator

def error_function_sq(actual, expected):
    """Error function for spike rates.
    
    Args:
      actual ([int]): The actual number of spikes per neuron
      expected ([int]): The expected number of spikes per neuron
    """
    errors = [math.pow(actual - expected, 2) for actual, expected in
        zip(outputs, expectations)]
    return sum(errors) / 2

def output_potential(time, weights, spikes_list):
    """Calculates the numeric output potential of a single neuron at a given
    point in time"""
    total = 0
    for weight, spike_times in zip(weights, input_spike_list):
        max_time = 0 if len(spike_times) == 0 else max(spike_times)
        total += weight * spike_response(time - max_time)
    return total

def spike_response(interval):
    """Calculates the numerical spike response in the given interval"""
    if interval <= 0:
        return 0

    return math.exp(-interval / t_m) - math.exp(-interval / t_s)

def spike_response_derivative(interval):
    """Calculate the numerical derivative of the spike response in the
    given interval"""
    if interval <= 0:
        return 0
    
    return math.exp(-interval/t_s) / t_s - math.exp(-interval / t_m) / t_m
