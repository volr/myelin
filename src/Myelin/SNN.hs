{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Myelin.SNN where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Lens hiding ((.=), (*~))
import Control.Monad
import Control.Monad.Trans.Identity

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.TH

import Data.ByteString.Lazy.Char8 as B
import Data.Monoid
import Data.Text
import Data.Traversable

import Numeric.LinearAlgebra

import GHC.Generics

import Myelin.Model
import Myelin.Neuron

-- Builder

-- | The backend targets supported by Myelin
data ExecutionTarget =
    Nest {
        _minTimestep :: Float,
        _maxTimestep :: Float
    }
    | BrainScaleS {
        _wafer :: Int, -- ^ wafer to run the experiment on
        _hicann :: Int -- ^ hicann chip to use
    } -- ^ first generation (wafer) brainscales system
    -- TODO: Add support for other platforms
    -- | Spikey {
    --     _mappingOffset :: Int -- 0..192 (really only 0 and 192 are sensible)
    -- }
    -- | SpiNNaker -- ^ SpiNNaker neuromorphic platform
    -- | BrainScaleS2 -- ^ second generation brainscales system
    deriving (Eq, Show)

{--
An execution task specifies all information needed to execute a SNN
on a specific target.
--}
data Task = Task {
    _executionTarget :: ExecutionTarget, -- ^ which neuromorphic hardware or simulator to run on
    _network :: Network, -- ^ network that should be implemented
    _simulationTime :: Double -- ^ simulation time in milliseconds
} deriving (Eq, Show)

-- | A spiking neural network described as a list of inputs, nodes, edges and
-- outputs. Also keeps a counter to uniquely label items in the network.
data Network = Network {
    _nextId :: Int,
    _inputs :: [Node],
    _nodes :: [Node],
    _edges :: [Edge],
    _outputs :: [Node]
} deriving (Eq, Show)

-- | Information about the population in the context of data injection/retrieval
data PopulationVisibility = Input | Output | Hidden

makeLenses ''Network

type SNN a m = StateT Network m a

-- | The initial empty network
initialNetwork = Network 0 [] [] [] []

newId :: Monad m => SNN Int m
newId = do
    l <- use nextId
    nextId += 1
    return l

-- | Creates a spike source array from a list of spike times in milliseconds
spikeSourceArray :: Monad m => [Float] -> SNN Node m
spikeSourceArray spikeTimes = do
    id <- newId
    let spikeSource = SpikeSourceArray spikeTimes id
    inputs <>= [spikeSource]
    return spikeSource

-- | Creates a spike source from a poisson distribution 
spikeSourcePoisson :: Monad m => 
  Float -> -- ^ The poisson rate
  Integer -> -- ^ The start time in milliseconds
  SNN Node m
spikeSourcePoisson rate start = do
    id <- newId
    let spikeSource = SpikeSourcePoisson rate start id
    inputs <>= [spikeSource]
    return spikeSource

-- | Creates a population of neurons, defined by 'Myelin.Neuron.NeuronType'
population :: Monad m =>
    String -- ^ label of the population (used for printing)
    -> Integer -- ^ size of the population
    -> NeuronType -- ^ type of neuron
    -> PopulationVisibility -- ^ Input, output or hidden population
    -> SNN Node m
population label i typ visibility = do
    l <- newId
    let pop = Population i typ label l 
    case visibility of
      Input -> inputs <>= [pop]
      Output -> outputs <>= [pop]
      Hidden -> nodes <>= [pop]
    return pop

-- | Projects two 'Node's together by projecting the first node to the second
-- node, as prescribed by the 'ProjectionEffect' 
projection :: Monad m => ProjectionEffect -> [Node] -> [Node] -> SNN () m
projection proj [p0] [p1] = edges <>= [DenseProjection proj p0 p1]
projection proj [p0, p1] [p2] = edges <>= [MergeProjection proj (p0, p1) p2]
projection proj [p0] [p1, p2] = edges <>= [ReplicateProjection proj p0 (p1, p2)]
