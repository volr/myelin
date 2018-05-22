module Myelin.Examples where

import Myelin.SNN


net :: Monad m => SNN () m
net = do
    input <- spikeSourceArray [1, 2, 3, 5]
    a <- population 5 if_current_exponential_default "a" True
    b <- population 10 if_current_exponential_default "b" False
    c <- population 5 if_current_exponential_default "c" False
    projection (AllToAll 1.0 False) (Static Excitatory) input a
    projection (AllToAll 1.0 False) (Static Excitatory) a b
    projection (AllToAll 1.0 False) (Static Excitatory) b c
    projection (AllToAll 1.0 False) (Static Excitatory) c a
    output <- fileOutput "out.txt"
    projection (AllToAll 1.0 False) (Static Inhibitory) c output

izhikevich :: Monad m => SNN () m
izhikevich = do
    spike_source <- spikeSourceArray [10.0 .. 51]
    neurons <- population 3 izhikevich_default "neurons" False
    output <- fileOutput "out.txt"
    projection (OneToOne 3.0) (Static Excitatory) spike_source neurons
    return ()

net2 :: Monad m => SNN () m
net2 = do
    input <- spikeSourceArray spikeTimes
    cells <- population 20 if_cond_alpha_default "cells" True
    projection (FixedProbability 0.5) (Static Excitatory) input cells
    where spikeTimes = undefined
                            
