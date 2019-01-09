{-# LANGUAGE OverloadedLists #-}
module Myelin.Examples where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Identity

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 as B

import Myelin.SNN
import Myelin.Neuron
import Myelin.Model

net :: Monad m => SNN () m
net = do
    input <- spikeSourceArray [1 .. 100]

    a <- population "a" 5 if_current_exponential Input
    b <- population "b" 10 if_current_exponential Hidden
    c <- population "c" 5 if_current_exponential Output
    
    projection (Static Excitatory (AllToAll (Constant 1.0))) [input] [a]
    projection (Static Excitatory (AllToAll (Constant 1.0))) [a] [b]
    projection (Static Excitatory (AllToAll (Constant 1.0))) [b] [c]
    projection (Static Excitatory (AllToAll (Constant 1.0))) [c] [a]

net2 :: Monad m => SNN () m
net2 = do
    spike_source <- spikeSourceArray [10 .. 51]
    neurons <- population "neurons" 3 izhikevich Input
    projection (Static Excitatory (OneToOne (Constant 3.0))) [spike_source] [neurons]
    return ()
       
netTest :: SNN () Identity
netTest = net

toTask :: SNN () Identity -> ExecutionTarget -> Double -> Task
toTask model target runtime =
  Task {
      _executionTarget = target,
      _simulationTime = runtime,
      _network = network
      }
  where (_, network) = runState model initialNetwork

exampleTask =
    Task {
        _executionTarget = Nest 0.1 0.5,
        _simulationTime = 100.0,
        _network = network
        }
    where (_, network) = runState netTest initialNetwork
