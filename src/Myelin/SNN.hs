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
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Printing
import Data.Monoid
import Data.Text
import Data.Traversable

import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.UnitNames
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional.SIUnits

import Numeric.LinearAlgebra

import GHC.Generics

type Label = String

-- TODO(Christian): Move Units somewhere else
type Siemens = Unit Metric DElectricConductance Float
type Volt = Unit Metric DElectricPotential Float
type Farrad = Unit Metric DCapacitance Float
type Seconds = Unit Metric DTime Float
type Ampere = Unit Metric DElectricCurrent Float


{--| NeuronType corresponds to the different point neuron
types that are constructible in pynn. Many of them
are back end specific.

Things that can be improved:

- Currently none of the parameters have any units,
  instead their units are indicated by their names

  - tau_* are time constants
  - v_* are voltages
  - i_* are currents
  - cm is a capacitance

- There is no restriction to "biological" ranges of
  parameters in place and no check of consistency

- In the case of the Heidelberg Hardware system additionally
  every neuron on the chip has analog parameter variation,
  and finite (much coarser than floating point) precision
  in parameter adjustment.

TODO:

HH_cond_exp
GIF_cond_exp

SpikeSourceGamma
SpikeSourceArray
SpikeSourcePoisson
SpikeSourcePoissonRefractory
--}
data NeuronType =
    IFCurrentAlpha {
        _tau_m :: Float, -- ^ membrane time constant
        _tau_refrac :: Float, -- ^ refractory time
        _v_thresh :: Float, -- ^ threshhold potential
        _tau_syn_E :: Float, -- ^ excitatory synaptic time constant
        _v_rest :: Float, -- ^ resting potential
        _cm :: Float, -- ^ membrane capactitance
        _v_reset :: Float, -- ^ reset potential
        _tau_syn_I :: Float, -- ^ inhibitory synaptic time constant
        _i_offset :: Float -- ^ offset current
    }
    | IFCondAlpha {
        _v_rest :: Float, -- ^ resting membrane potential
        _cm :: Float, -- ^ capacity of the membrane
        _tau_m :: Float, -- ^ membrane time constant
        _tau_refrac :: Float, -- ^ duration of refractory period
        _tau_syn_E :: Float, -- ^ rise time of the excitatory synaptic alpha function
        _tau_syn_I :: Float, -- ^ rise time of the inhibitory synaptic alpha function
        _e_rev_E :: Float, -- ^ reversal potential for excitatory input
        _e_rev_I :: Float, -- ^ reversal potential for inhibitory input
        _v_thresh :: Float, -- ^ spike threshold
        _v_reset :: Float, -- ^ reset potential after a spike
        _i_offset :: Float -- ^ offset current
    }
    | IFSpikey {
        _e_rev_I :: Float, -- ^ excitatory reversal current
        _g_leak :: Float, -- ^ leak conductance
        _tau_refrac :: Float, -- ^ refractory time
        _v_reset :: Float, -- ^ reset potential
        _v_rest :: Float, -- ^ resting potential
        _v_thresh :: Float -- ^ threshhold potential
    }
    | IFCurrExp {
        _cm :: Float, -- ^ membrane capacitance
        _tau_m :: Float, -- ^ membrane time constant
        _tau_syn_E :: Float, -- ^ excitatory synaptic time constant
        _tau_syn_I :: Float, -- ^ inhibitory synaptic time constant
        _tau_refrac :: Float, -- ^ refractory time
        _v_thresh :: Float, -- ^ threshhold voltage
        _v_rest :: Float, -- ^ resting potential
        _v_reset :: Float, -- ^ reset potential
        _i_offset :: Float -- ^ offset current
    }
    | IFCondExp {
        _v_rest :: Float, -- ^ resting potential
        _cm :: Float, -- ^ membrane capacitance
        _tau_m :: Float, -- ^ membrane time constant
        _tau_refrac :: Float, -- ^ refractory time
        _tau_syn_E :: Float, -- ^ excitatory synaptic time constant
        _tau_syn_I :: Float, -- ^ inhibitory synaptic time constant
        _e_rev_E :: Float, -- ^ excitatory reversal potential
        _e_rev_I :: Float, -- ^ inhibitory reversal potential
        _v_thresh :: Float, -- ^ spike initiation threshold
        _v_reset :: Float, -- ^ reset value for membrane potential after a spike
        _i_offset :: Float -- ^ offset current
    }
    | Izhikevich {
        _a :: Float,
        _i_offset :: Float,
        _c :: Float,
        _d :: Float,
        _b :: Float
    }
    | EIFCondExp {
        _v_reset :: Float,
        _i_offset :: Float,
        _tau_w :: Float,
        _tau_syn_I :: Float,
        _e_rev_E :: Float,
        _v_rest :: Float,
        _cm :: Float,
        _tau_syn_E :: Float,
        _tau_m :: Float,
        _a :: Float,
        _delta_T :: Float,
        _v_thresh :: Float,
        _b :: Float,
        _v_spike :: Float,
        _e_rev_I :: Float,
        _tau_refrac :: Float
    }
    | EIFCondAlpha {
        _cm :: Float, -- ^ capacity of the membrane
        _tau_refrac :: Float, -- ^ duration of the refractory period
        _v_spike :: Float, -- ^ spike detection threshold
        _v_reset :: Float, -- ^ reset value for membrane potential after a spike
        _v_rest :: Float, -- ^ resting membrane potential (Leak reversal potential)
        _tau_m :: Float, -- ^ membrane time constant
        _i_offset :: Float, -- ^ offset current
        _a :: Float, -- ^ subthreshold adaptation conductance
        _b :: Float, -- ^ spike-triggered adaptation
        _delta_T :: Float, -- ^ slope factor
        _tau_w :: Float, -- ^ adaptation time constant
        _v_thresh :: Float, -- ^ spike initiation threshold
        _e_rev_E :: Float, -- ^ excitatory reversal potential
        _tau_syn_E :: Float, -- ^ rise time of excitatory synaptic conductance (alpha function)
        _e_rev_I :: Float, -- ^ inhibitory reversal potential
        _tau_syn_I :: Float -- ^ rise time of the inhibitory synaptic conductance (alpha function)
    }
    | HHCondExp {
        _cm :: Float, -- ^ capacity of the membrane
        _e_rev_E :: Float, -- ^ excitatory reversal potential
        _e_rev_I :: Float, -- ^ inhibitory reversal potential
        _e_rev_K :: Float,
        _e_rev_Na :: Float,
        _e_rev_leak :: Float,
        _g_leak :: Float,
        _gbar_K :: Float,
        _gbar_Na :: Float,
        _i_offset :: Float,
        _tau_syn_E :: Float,
        _tau_syn_I :: Float,
        _v_offset :: Float
    } deriving (Eq, Show)


makeLenses ''NeuronType
makePrisms ''NeuronType

-- ^  Defaults taken from http://neuralensemble.org/docs/PyNN/standardmodels.html
if_cond_exp :: NeuronType
if_cond_exp = IFCondExp {
    _v_rest = -65.0,
    _cm = 1.0,
    _tau_m = 20.0,
    _tau_refrac = 0.0,
    _tau_syn_E = 5.0,
    _tau_syn_I = 5.0,
    _e_rev_E = 0.0,
    _e_rev_I = -70.0,
    _v_thresh = -50.0,
    _v_reset = -65.0,
    _i_offset = 0.0
}

if_current_alpha :: NeuronType
if_current_alpha = IFCurrentAlpha {
    _tau_m = 20.0,
    _tau_refrac = 0.1,
    _v_thresh = -50.0,
    _tau_syn_E =  0.5,
    _tau_syn_I = 0.5,
    _v_rest = -65.0,
    _cm = 1.0,
    _v_reset = -65.0,
    _i_offset = 0.0
}

if_cond_alpha :: NeuronType
if_cond_alpha = IFCondAlpha {
    _v_rest = -65.0,
    _cm = 1.0,
    _tau_m = 20.0,
    _tau_refrac = 0.0,
    _tau_syn_E = 5.0,
    _tau_syn_I = 5.0,
    _e_rev_E = 0.0,
    _e_rev_I = -70.0,
    _v_thresh = -50.0,
    _v_reset = -65.0,
    _i_offset = 0.0
}

if_spikey :: NeuronType
if_spikey = IFSpikey {
    _e_rev_I = -80.0,
    _g_leak = 20.0,
    _tau_refrac = 1.0,
    _v_reset = -80.0,
    _v_rest = -75.0,
    _v_thresh = -55.0
}

if_current_exponential :: NeuronType
if_current_exponential = IFCurrExp {
    _cm = 1.0,
    _tau_m = 20.0,
    _tau_syn_E = 5.0,
    _tau_syn_I = 5.0,
    _tau_refrac = 0.1,
    _v_thresh = -50.0,
    _v_rest = -65.0,
    _v_reset = -65.0,
    _i_offset = 0.0
}

izhikevich :: NeuronType
izhikevich = Izhikevich {
    _a = 0.02,
    _i_offset = 0.0,
    _c = -65.0,
    _d = 2.0,
    _b = 0.2
}

eif_cond_exp :: NeuronType
eif_cond_exp = EIFCondExp {
    _v_reset = -70.6,
    _i_offset = 0.0,
    _tau_w = 144.0,
    _tau_syn_I = 5.0,
    _e_rev_E = 0.0,
    _v_rest = -70.6,
    _cm = 0.281,
    _tau_syn_E = 5.0,
    _tau_m = 9.3667,
    _a = 4.0,
    _delta_T = 2.0,
    _v_thresh = -50.4,
    _b = 0.0805,
    _v_spike = -40.0,
    _e_rev_I = -80.0,
    _tau_refrac = 0.1
}

eif_cond_alpha :: NeuronType
eif_cond_alpha = EIFCondAlpha {
    _v_reset = -70.6,
    _i_offset = 0.0,
    _tau_w = 144.0,
    _tau_syn_I = 5.0,
    _e_rev_E = 0.0,
    _v_rest = -70.6,
    _cm = 0.281,
    _tau_syn_E = 5.0,
    _tau_m = 9.3667,
    _a = 4.0,
    _delta_T = 2.0,
    _v_thresh = -50.4,
    _b = 0.0805,
    _v_spike = -40.0,
    _e_rev_I = -80.0,
    _tau_refrac = 0.1
}

instance ToJSON NeuronType where
    toJSON IFCondExp {..} = object [
            "type" .= ("IFCondExp" :: String),
            "tau_m" .= _tau_m,
            "tau_refrac" .= _tau_refrac,
            "v_thresh" .= _v_thresh,
            "tau_syn_E" .= _tau_syn_E,
            "v_rest" .= _v_rest,
            "cm" .= _cm,
            "v_reset" .= _v_reset,
            "tau_syn_I" .= _tau_syn_I,
            "i_offset" .= _i_offset,
            "e_rev_E" .= _e_rev_E,
            "e_rev_I" .= _e_rev_I
        ]
    toJSON IFCurrentAlpha {..} = object [
            "type" .= ("IFCurrentAlpha" :: String),
            "tau_m" .= _tau_m,
            "tau_refrac" .= _tau_refrac,
            "v_thresh" .= _v_thresh,
            "tau_syn_E" .= _tau_syn_E,
            "v_rest" .= _v_rest,
            "cm" .= _cm,
            "v_reset" .= _v_reset,
            "tau_syn_I" .= _tau_syn_I,
            "i_offset" .= _i_offset
        ]
    toJSON IFCondAlpha {..} = object [
            "type" .= ("IFCondAlpha" :: String),
            "v_rest" .= _v_rest,
            "cm" .= _cm,
            "tau_m" .= _tau_m,
            "tau_refrac" .= _tau_refrac,
            "tau_syn_E" .= _tau_syn_E,
            "tau_syn_I" .= _tau_syn_I,
            "e_rev_E" .= _e_rev_E,
            "e_rev_I" .= _e_rev_I,
            "v_thresh" .= _v_thresh,
            "v_reset" .= _v_reset,
            "i_offset" .= _i_offset
        ]
    toJSON IFSpikey {..} = object [
            "type" .= ("IFSpikey" :: String),
            "e_rev_I" .= _e_rev_I,
            "g_leak" .= _g_leak,
            "tau_refrac" .= _tau_refrac,
            "v_reset" .= _v_reset,
            "v_rest" .= _v_rest,
            "v_thresh" .= _v_thresh
        ]
    toJSON IFCurrExp {..} = object [
            "type" .= ("IFCurrentExp" :: String),
            "cm" .= _cm,
            "tau_m" .= _tau_m,
            "tau_syn_E" .= _tau_syn_E,
            "tau_syn_I" .= _tau_syn_I,
            "tau_refrac" .= _tau_refrac,
            "v_thresh" .= _v_thresh,
            "v_rest" .= _v_rest,
            "v_reset" .= _v_reset,
            "i_offset" .= _i_offset
        ]
    toJSON Izhikevich {..} = object [
            "type" .= ("Izhikevich" :: String),
            "a" .= _a,
            "i_offset" .= _i_offset,
            "c" .= _c,
            "d" .= _d,
            "b" .= _b
        ]
    toJSON EIFCondExp {..} = object [
            "type" .= ("EIFCondExp" :: String),
            "v_reset" .= _v_reset,
            "i_offset" .= _i_offset,
            "tau_w" .= _tau_w,
            "tau_syn_I" .= _tau_syn_I,
            "e_rev_E" .= _e_rev_E,
            "v_rest" .= _v_rest,
            "cm" .= _cm,
            "tau_syn_E" .= _tau_syn_E,
            "tau_m" .= _tau_m,
            "a" .= _a,
            "delta_T" .= _delta_T,
            "v_thresh" .= _v_thresh,
            "b" .= _b,
            "v_spike" .= _v_spike,
            "e_rev_I" .= _e_rev_I,
            "tau_refrac" .= _tau_refrac
        ]
    toJSON EIFCondAlpha {..} = object [
            "type" .= ("EIFCondAlpha" :: String),
            "cm" .= _cm,
            "tau_refrac" .= _tau_refrac,
            "v_spike" .= _v_spike,
            "v_reset" .= _v_reset,
            "v_rest" .= _v_rest,
            "tau_m" .= _tau_m,
            "i_offset" .= _i_offset,
            "a" .= _a,
            "b" .= _b,
            "delta_T" .= _delta_T,
            "tau_w" .= _tau_w,
            "v_thresh" .= _v_thresh,
            "e_rev_E" .= _e_rev_E,
            "tau_syn_E" .= _tau_syn_E,
            "e_rev_I" .= _e_rev_I,
            "tau_syn_I" .= _tau_syn_I
        ]

instance FromJSON NeuronType where
    parseJSON = withObject "neuron" $ \o -> do
        typ :: String <- o .: "type"
        case typ of
            "IFCondExp" -> IFCondExp <$>
                o .: "v_rest" <*>
                o .: "cm" <*>
                o .: "tau_m" <*>
                o .: "tau_refrac" <*>
                o .: "tau_syn_E" <*>
                o .: "tau_syn_I" <*>
                o .: "e_rev_E" <*>
                o .: "e_rev_I" <*>
                o .: "v_thresh" <*>
                o .: "v_reset" <*>
                o .: "i_offset"
            "IFCurrentAlpha" -> IFCurrentAlpha <$>
                o .: "tau_m" <*>
                o .: "tau_refrac" <*>
                o .: "v_thresh" <*>
                o .: "tau_syn_E" <*>
                o .: "v_rest" <*>
                o .: "cm" <*>
                o .: "v_reset" <*>
                o .: "tau_syn_I" <*>
                o .: "i_offset"
            "IFCondAlpha" -> IFCondAlpha <$>
                o .: "v_rest" <*>
                o .: "cm" <*>
                o .: "tau_m" <*>
                o .: "tau_refrac" <*>
                o .: "tau_syn_E" <*>
                o .: "tau_syn_I" <*>
                o .: "e_rev_E" <*>
                o .: "e_rev_I" <*>
                o .: "v_thresh" <*>
                o .: "v_reset" <*>
                o .: "i_offset"
            "IFSpikey" -> IFSpikey <$>
                o .: "e_rev_I" <*>
                o .: "g_leak" <*>
                o .: "tau_refrac" <*>
                o .: "v_reset" <*>
                o .: "v_rest" <*>
                o .: "v_thresh"
            "IFCurrExp" -> IFCurrExp <$>
                o .: "cm" <*>
                o .: "tau_m" <*>
                o .: "tau_syn_E" <*>
                o .: "tau_syn_I" <*>
                o .: "tau_refrac" <*>
                o .: "v_thresh" <*>
                o .: "v_rest" <*>
                o .: "v_reset" <*>
                o .: "i_offset"
            "Izhikevich" -> Izhikevich <$>
                o .: "a" <*>
                o .: "i_offset"<*>
                o .: "c" <*>
                o .: "d" <*>
                o .: "b"
            "EIFCondExp" -> EIFCondExp <$>
                o .: "v_reset" <*>
                o .: "i_offset" <*>
                o .: "tau_w" <*>
                o .: "tau_syn_I" <*>
                o .: "e_rev_E" <*>
                o .: "v_rest" <*>
                o .: "cm" <*>
                o .: "tau_syn_E" <*>
                o .: "tau_m" <*>
                o .: "a" <*>
                o .: "delta_T" <*>
                o .: "v_thresh" <*>
                o .: "b" <*>
                o .: "v_spike" <*>
                o .: "e_rev_I" <*>
                o .: "tau_refrac"
            "EIFCondAlpha" -> EIFCondAlpha <$>
                o .: "cm" <*>
                o .: "tau_refrac" <*>
                o .: "v_spike" <*>
                o .: "v_reset" <*>
                o .: "v_rest" <*>
                o .: "tau_m" <*>
                o .: "i_offset" <*>
                o .: "a" <*>
                o .: "b" <*>
                o .: "delta_T" <*>
                o .: "tau_w" <*>
                o .: "v_thresh" <*>
                o .: "e_rev_E" <*>
                o .: "tau_syn_E" <*>
                o .: "e_rev_I" <*>
                o .: "tau_syn_I"


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
-- Builder

data ExecutionTarget =
    Nest {
        _minTimestep :: Float,
        _maxTimestep :: Float
    }
    | BrainScaleS {
        _wafer :: Int, -- ^ wafer to run the experiment on
        _hicann :: Int -- ^ hicann chip to use
    } -- ^ first generation (wafer) brainscales system
    | Spikey {
        _mappingOffset :: Int -- 0..192 (really only 0 and 192 are sensible)
    }
    | SpiNNaker -- ^ SpiNNaker neuromorphic platform
    | BrainScaleS2 -- ^ second generation brainscales system
    deriving (Eq, Show)

instance ToJSON ExecutionTarget where
    toJSON Nest{..} = object [ "kind" .= ("nest" :: String)]
    toJSON BrainScaleS{..} = object
      [ "kind" .= ("brainscales" :: String)
      , "wafer" .= _wafer
      , "hicann" .= _hicann
      ]
    toJSON SpiNNaker = object [ "kind" .= ("spinnaker" :: String) ]

instance FromJSON ExecutionTarget where
    parseJSON = withObject "execution_target" $ \o -> do
        kind :: String <- o .: "kind"
        case kind of
            "brainscales" -> BrainScaleS <$>
                        o .: "wafer" <*>
                        o .: "hicann"
            "nest" -> Nest <$>
                        o .: "min_timestep" <*>
                        o .: "max_timestep"
            "spinnaker" -> return SpiNNaker
            _ -> error "target not supported yet"

{--
An execution task specifies all information needed to execute a SNN
on a specific target.
--}
data Task = Task {
    _executionTarget :: ExecutionTarget, -- ^ which neuromorphic hardware or simulator to run on
    _network :: Network, -- ^ network that should be implemented
    _simulationTime :: Double -- TODO: Might be simulator specific, also has no unit
} deriving (Eq, Show)

instance ToJSON Task where
    toJSON Task {..} = object [
            "execution_target" .= _executionTarget,
            "network" .= _network,
            "simulation_time" .= _simulationTime
        ]

instance FromJSON Task where
    parseJSON = withObject "task" $ \o ->
        Task <$> o .: "execution_target" <*>
                 o .: "network" <*>
                 o .: "simulation_time"

data Network = Network {
    _blocks :: [BlockState]
} deriving (Eq, Show)

instance ToJSON Network where
    toJSON Network {..} = object [
            "blocks" .= _blocks
        ]

instance FromJSON Network where
    parseJSON = withObject "network" $ \o ->
        Network <$> o .: "blocks"

data BlockState = BlockState {
    _nextId :: Int,
    _inputs :: [Node],
    _nodes :: [Node],
    _edges :: [Edge],
    _outputs :: [Node]
} deriving (Eq, Show)

instance ToJSON BlockState where
    toJSON BlockState {..} = object [
            "next_id" .= _nextId,
            "nodes" .= _nodes,
            "edges" .= _edges,
            "inputs" .= _inputs,
            "outputs" .= _outputs
        ]

instance FromJSON BlockState where
    parseJSON = withObject "block_state" $ \o ->
        BlockState <$>
        o .: "next_id" <*>
        o .: "nodes" <*>
        o .: "edges" <*>
        o .: "inputs" <*>
        o .: "outputs"

makeLenses ''BlockState

type SNN a m = StateT BlockState m a

initialBlockState = BlockState 0 [] [] [] []

newId :: Monad m => SNN Int m
newId = do
    l <- use nextId
    nextId += 1
    return l

spikeSourceArray :: Monad m => [Float] -> SNN Node m
spikeSourceArray spikeTimes = do
    id <- newId
    let spikeSource = SpikeSourceArray spikeTimes id
    nodes <>= [spikeSource]
    return spikeSource

spikeSourcePoisson :: Monad m => Float -> Integer -> SNN Node m
spikeSourcePoisson rate start = do
    id <- newId
    let spikeSource = SpikeSourcePoisson rate start id
    nodes <>= [spikeSource]
    return spikeSource

population :: Monad m =>
    String -- ^ label of the population (used for printing)
    -> Integer -- ^ size of the population
    -> NeuronType -- ^ type of neuron
    -> SNN Node m
population  label i typ = do
    l <- newId
    let pop = Population i typ label l True
    nodes <>= [pop]
    return pop

projection :: Monad m => ProjectionType -> ProjectionTarget -> Node -> Node -> SNN () m
projection proj target p0 p1 = edges <>= [Projection proj target p0 p1]

fileInput :: Monad m => String -> SNN Node m
fileInput filename = do
    i <- newId
    let input = Input filename i
    nodes <>= [input]
    return input

fileOutput :: Monad m => String -> SNN Node m
fileOutput filename = do
    i <- newId
    let output = Output filename i
    nodes <>= [output]
    return output
