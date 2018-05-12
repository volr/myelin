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