{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Myelin.Model where

import Control.Lens hiding ((.=), (*~))
import Data.Aeson

import Myelin.Neuron

{-- TODO: Implement units
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.UnitNames
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional.SIUnits


-- TODO(Christian): Move Units somewhere else
type Siemens = Unit Metric DElectricConductance Float
type Volt = Unit Metric DElectricPotential Float
type Farrad = Unit Metric DCapacitance Float
type Seconds = Unit Metric DTime Float
type Ampere = Unit Metric DElectricCurrent Float
-}

type Label = String

-- | A Node is a vertex in the connectivity graph.
data Node = Population {
        _numNeurons :: Integer, -- ^ number of neurons in the population
        _neuronType :: NeuronType,
        _label :: Label, -- ^ human readable label for the population
        _id :: Int -- ^ internal identifier to keep track of the population
    }
    | SpikeSourceArray {
        _spikeTimes :: [Float],
        _id :: Int
    }
    | SpikeSourcePoisson {
        _rate :: Float,
        _start :: Integer,
        _id :: Int
    }
    deriving (Eq, Show)

makeLenses ''Node
makePrisms ''Node

type Delay = Float -- TODO: This is platform specific
type Probability = Float -- TODO: Very unsophisticated representation

-- | Weight types that describes weights for a projection
data Weights 
  = Constant Float
  | GaussianRandom 
    { mean :: Float
    , scale :: Float
    }
  -- TODO: Add matrix
  -- | Matrix L
  deriving (Eq, Show)

makeLenses ''Weights
makePrisms ''Weights

-- | Types of connections in a projection from one neuron population to another
data ConnectionType
  = AllToAll
    { _weights :: Weights
    }
  | OneToOne
    { _weights :: Weights
    }
  deriving (Eq, Show)

makeLenses ''ConnectionType
makePrisms ''ConnectionType

-- | Determines the effect of a synapse to be excitatory of inhibitory
data SynapseEffect 
  = Inhibitory
  | Excitatory 
  deriving (Eq, Show)

makeLenses '' SynapseEffect
makePrisms '' SynapseEffect

-- | Effect of projections that determines the method with which 'Node's connect
data ProjectionEffect
  = Static SynapseEffect ConnectionType
-- | Dynamic .. TODO (Jens)
  deriving (Eq, Show)

makeLenses ''ProjectionEffect
makePrisms ''ProjectionEffect

-- | An edge between two units in the network with a given 'ProjectionEffect'
data Edge
  = DenseProjection
    { _effect :: ProjectionEffect
    , _input :: Node
    , _output :: Node
    }
  | MergeProjection
    { _effect :: ProjectionEffect
    , _inputs :: (Node, Node)
    , _output :: Node
    }
  | ReplicateProjection
    { _effect :: ProjectionEffect
    , _input :: Node
    , _outputs :: (Node, Node)
    }
  deriving (Eq, Show)

