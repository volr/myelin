import nest

import onedim_line as o

rate = 1000.0  # generator rate in spikes/s
start = 100.0  # start of simulation relative to trial start, in ms
stop = 500.0  # end of simulation relative to trial start, in ms

trial_duration = 1000.0  # trial duration, in ms
num_trials = 5      # number of trials to perform

nest.ResetKernel()
pg = nest.Create('poisson_generator',
                 params={'rate': rate,
                         'start': start,
                         'stop': stop}
                 )

sd = nest.Create('spike_detector')

nest.Connect(pg, sd)

for n in range(num_trials):
    nest.SetStatus(pg, {'origin': nest.GetKernelStatus()['time']})
    nest.Simulate(trial_duration)