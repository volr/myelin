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

net :: Monad m => SNN () m
net = do
    input <- spikeSourceArray [1 .. 100]
    

    a <- population 5 (if_current_exponential & cm .~ [0.1, 0.3, 0.4, 0.5, 0.6]) "a" True
    b <- population 10 if_current_exponential "b" False
    c <- population 5 if_current_exponential "c" False
    
    projection (AllToAll 1.0 False) (Static Excitatory) input a
    projection (AllToAll 1.0 False) (Static Excitatory) a b
    projection (AllToAll 1.0 False) (Static Excitatory) b c
    projection (AllToAll 1.0 False) (Static Excitatory) c a
    
    output <- fileOutput "out.txt"
    projection (AllToAll 1.0 False) (Static Inhibitory) c output

net2 :: Monad m => SNN () m
net2 = do
    spike_source <- spikeSourceArray [10 .. 51]
    neurons <- population 3 izhikevich "neurons" False
    output <- fileOutput "out.txt"
    projection (OneToOne 3.0) (Static Excitatory) spike_source neurons
    return ()
       
netTest :: SNN () Identity
netTest = net

toTask :: SNN () Identity -> ExecutionTarget -> Double -> Task
toTask model target runtime =
  Task {
      _executionTarget = target,
      _simulationTime = runtime,
      _network = Network {
          _blocks = [block]
      }
  }
  where (_, block) = runState model initialBlockState

taskToJSON :: Task -> String
taskToJSON = B.unpack . encodePretty . toJSON

exampleTask =
    Task {
        _executionTarget = Nest 0.1 0.5,
        _simulationTime = 100.0,
        _network = Network {
            _blocks = [block]
        }
    }
    where (_, block) = runState netTest initialBlockState
