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
        _id :: Int, -- ^ internal identifier to keep track of the popultion
        _record_spikes :: Bool -- ^ whether to record spikes from this population
    }
    | Input {
        _fileName :: String,
        _id :: Int
    }
    | Output {
        _fileName :: String,
        _id :: Int
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


instance ToJSON Node where
    toJSON Population{..} = object [
        "type" .= ("population" :: String),
        "num_neurons" .= _numNeurons,
        "neuron_type" .= _neuronType,
        "label" .= _label,
        "id" .= _id,
        "record_spikes" .= _record_spikes
        ]
    toJSON Input{..} = object [
            "type" .= ("input" :: String),
            "file_name" .= _fileName,
            "id" .= _id
        ]
    toJSON Output{..} = object [
            "type" .= ("output" :: String),
            "file_name" .= _fileName,
            "id" .= _id
        ]
    toJSON SpikeSourceArray{..} = object [
            "type" .= ("spike_source_array" :: String),
            "spike_times" .= _spikeTimes,
            "id" .= _id
        ]
    toJSON SpikeSourcePoisson{..} = object [
            "type" .= ("spike_source_poisson" :: String),
            "rate" .= _rate,
            "start" .= _start,
            "id" .= _id
        ]

instance FromJSON Node where
    parseJSON = withObject "node" $ \o -> do
        typ :: String <- o .: "type"
        case typ of
            "population" ->
                Population <$>
                    o .: "num_neurons" <*>
                    o .: "neuron_type" <*>
                    o .: "label" <*>
                    o .: "id" <*>
                    o .: "record_spikes"
            "input" ->
                Input <$>
                    o .: "file_name" <*>
                    o .: "id"
            "output" ->
                Output <$>
                    o .: "file_name" <*>
                    o .: "id"
            "spike_source_poisson" ->
                SpikeSourcePoisson <$>
                    o .: "rate" <*>
                    o .: "start" <*>
                    o .: "id"
            "spike_source_array" ->
                SpikeSourceArray <$>
                    o .: "spike_times" <*>
                    o .: "id"

type Weight = Float -- TODO: This is platform specific
type Delay = Float -- TODO: This is platform specific
type Probability = Float -- TODO: Very unsophisticated representation


data ProjectionType =
    AllToAll {
        _weight :: Weight,
        _allow_self_connections :: Bool -- TODO
    }
    | OneToOne {
        _weight :: Weight
    }
    | FixedNumberPost {
        _n :: Int,
        _weight :: Weight,
        _allow_self_connections :: Bool
    }
    | FixedNumberPre {
        _n :: Int,
        _weight :: Weight,
        _allow_self_connections :: Bool
    }
    | FromList {
        _weights :: [(Int, Int, Weight)]
    }
    | FixedProbability {
        _probability :: Probability
    }
    deriving (Eq, Show)

makeLenses ''ProjectionType
makePrisms ''ProjectionType

instance FromJSON ProjectionType where
    parseJSON = withObject "projection_type" $ \o -> do
        kind :: String <- o .: "kind"
        case kind of
            "all_to_all" -> AllToAll <$> o .: "weight" <*> o .: "allow_self_connections"
            "one_to_one" -> OneToOne <$> o .: "weight"
            "fixed_number_pre" -> FixedNumberPre <$> o .: "n" <*> o .: "weight" <*> o .: "allow_self_connections"
            "fixed_number_post" -> FixedNumberPost <$> o .: "n" <*> o .: "weight" <*> o .: "allow_self_connections"
            "from_list" -> FromList <$> o .: "weights"
            "fixed_probability" -> FixedProbability <$> o .: "probability"

instance ToJSON ProjectionType where
    toJSON AllToAll{..} = object [
            "kind" .= ("all_to_all" :: String),
            "weight" .= _weight,
            "allow_self_connections" .= _allow_self_connections
        ]
    toJSON OneToOne{..} = object [
            "kind" .= ("one_to_one" :: String),
            "weight" .= _weight
        ]
    toJSON FixedNumberPost{..} = object [
            "kind" .= ("fixed_number_post" :: String),
            "n" .= _n,
            "weight" .= _weight,
            "allow_self_connections" .= _allow_self_connections
        ]
    toJSON FixedNumberPre{..} = object [
            "kind" .= ("fixed_number_pre" :: String),
            "n"  .= _n,
            "weight" .= _weight,
            "allow_self_connections" .= _allow_self_connections
        ]
    toJSON FromList{..} = object [
            "kind" .= ("from_list" :: String),
            "weights" .= _weights
        ]
    toJSON FixedProbability{..} = object [
            "kind" .= ("fixed_probability" :: String),
            "probability" .= _probability
        ]

data SynapseEffect = Inhibitory | Excitatory deriving (Eq, Show)

data ProjectionTarget =
    Static SynapseEffect
    deriving (Eq, Show)

instance ToJSON ProjectionTarget where
    toJSON (Static Excitatory) = object [
            "kind" .= ("static" :: String),
            "effect" .= ("excitatory" :: String)
        ]
    toJSON (Static Inhibitory) = object [
            "kind" .= ("static" :: String),
            "effect" .= ("inhibitory" :: String)
        ]

instance FromJSON ProjectionTarget where
    parseJSON = withObject "projection_target" $ \o -> do
            kind :: String <- o .: "kind"
            effect :: String <- o .: "effect"
            case effect of
                "excitatory" -> return (Static Excitatory)
                "inhibitory" -> return (Static Inhibitory)

data Edge =
      Projection {
          _projectionType :: ProjectionType,
          _projectionTarget :: ProjectionTarget,
          _input :: Node,
          _output :: Node
      } deriving (Eq, Show)

instance ToJSON Edge where
    toJSON Projection {..} = object [
                "type" .= ("projection" :: String),
                "projection_type" .= _projectionType,
                "projection_target" .= _projectionTarget,
                "input" .= _input,
                "output" .= _output
            ]

instance FromJSON Edge where
    parseJSON = withObject "edge" $ \o -> do
        typ :: String <- o .: "type"
        case typ of
            "projection" ->
                Projection <$>
                    o .: "projection_type" <*>
                    o .: "projection_target" <*>
                    o .: "input" <*>
                    o .: "output"
