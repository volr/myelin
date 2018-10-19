{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Myelin.Neuron where

import Control.Lens hiding ((.=), (*~))
import Data.Aeson
import Numeric.LinearAlgebra

{--| NeuronType corresponds to the different point neuron
types that are constructible in SNN. Many of them
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
        _tau_m :: Vector Float, -- ^ membrane time constant
        _tau_refrac :: Vector Float, -- ^ refractory time
        _v_thresh :: Vector Float, -- ^ threshhold potential
        _tau_syn_E :: Vector Float, -- ^ excitatory synaptic time constant
        _v_rest :: Vector Float, -- ^ resting potential
        _cm :: Vector Float, -- ^ membrane capactitance
        _v_reset :: Vector Float, -- ^ reset potential
        _tau_syn_I :: Vector Float, -- ^ inhibitory synaptic time constant
        _i_offset :: Vector Float -- ^ offset current
    }
    | IFCondAlpha {
        _v_rest :: Vector Float, -- ^ resting membrane potential
        _cm :: Vector Float, -- ^ capacity of the membrane
        _tau_m :: Vector Float, -- ^ membrane time constant
        _tau_refrac :: Vector Float, -- ^ duration of refractory period
        _tau_syn_E :: Vector Float, -- ^ rise time of the excitatory synaptic alpha function
        _tau_syn_I :: Vector Float, -- ^ rise time of the inhibitory synaptic alpha function
        _e_rev_E :: Vector Float, -- ^ reversal potential for excitatory input
        _e_rev_I :: Vector Float, -- ^ reversal potential for inhibitory input
        _v_thresh :: Vector Float, -- ^ spike threshold
        _v_reset :: Vector Float, -- ^ reset potential after a spike
        _i_offset :: Vector Float -- ^ offset current
    }
    | IFSpikey {
        _e_rev_I :: Vector Float, -- ^ excitatory reversal current
        _g_leak :: Vector Float, -- ^ leak conductance
        _tau_refrac :: Vector Float, -- ^ refractory time
        _v_reset :: Vector Float, -- ^ reset potential
        _v_rest :: Vector Float, -- ^ resting potential
        _v_thresh :: Vector Float -- ^ threshhold potential
    }
    | IFCurrExp {
        _cm :: Vector Float, -- ^ membrane capacitance
        _tau_m :: Vector Float, -- ^ membrane time constant
        _tau_syn_E :: Vector Float, -- ^ excitatory synaptic time constant
        _tau_syn_I :: Vector Float, -- ^ inhibitory synaptic time constant
        _tau_refrac :: Vector Float, -- ^ refractory time
        _v_thresh :: Vector Float, -- ^ threshhold voltage
        _v_rest :: Vector Float, -- ^ resting potential
        _v_reset :: Vector Float, -- ^ reset potential
        _i_offset :: Vector Float -- ^ offset current
    }
    | IFCondExp {
        _v_rest :: Vector Float, -- ^ resting potential
        _cm :: Vector Float, -- ^ membrane capacitance
        _tau_m :: Vector Float, -- ^ membrane time constant
        _tau_refrac :: Vector Float, -- ^ refractory time
        _tau_syn_E :: Vector Float, -- ^ excitatory synaptic time constant
        _tau_syn_I :: Vector Float, -- ^ inhibitory synaptic time constant
        _e_rev_E :: Vector Float, -- ^ excitatory reversal potential
        _e_rev_I :: Vector Float, -- ^ inhibitory reversal potential
        _v_thresh :: Vector Float, -- ^ spike initiation threshold
        _v_reset :: Vector Float, -- ^ reset value for membrane potential after a spike
        _i_offset :: Vector Float -- ^ offset current
    }
    | Izhikevich {
        _a :: Vector Float,
        _i_offset :: Vector Float,
        _c :: Vector Float,
        _d :: Vector Float,
        _b :: Vector Float
    }
    | EIFCondExp {
        _v_reset :: Vector Float,
        _i_offset :: Vector Float,
        _tau_w :: Vector Float,
        _tau_syn_I :: Vector Float,
        _e_rev_E :: Vector Float,
        _v_rest :: Vector Float,
        _cm :: Vector Float,
        _tau_syn_E :: Vector Float,
        _tau_m :: Vector Float,
        _a :: Vector Float,
        _delta_T :: Vector Float,
        _v_thresh :: Vector Float,
        _b :: Vector Float,
        _v_spike :: Vector Float,
        _e_rev_I :: Vector Float,
        _tau_refrac :: Vector Float
    }
    | EIFCondAlpha {
        _cm :: Vector Float, -- ^ capacity of the membrane
        _tau_refrac :: Vector Float, -- ^ duration of the refractory period
        _v_spike :: Vector Float, -- ^ spike detection threshold
        _v_reset :: Vector Float, -- ^ reset value for membrane potential after a spike
        _v_rest :: Vector Float, -- ^ resting membrane potential (Leak reversal potential)
        _tau_m :: Vector Float, -- ^ membrane time constant
        _i_offset :: Vector Float, -- ^ offset current
        _a :: Vector Float, -- ^ subthreshold adaptation conductance
        _b :: Vector Float, -- ^ spike-triggered adaptation
        _delta_T :: Vector Float, -- ^ slope factor
        _tau_w :: Vector Float, -- ^ adaptation time constant
        _v_thresh :: Vector Float, -- ^ spike initiation threshold
        _e_rev_E :: Vector Float, -- ^ excitatory reversal potential
        _tau_syn_E :: Vector Float, -- ^ rise time of excitatory synaptic conductance (alpha function)
        _e_rev_I :: Vector Float, -- ^ inhibitory reversal potential
        _tau_syn_I :: Vector Float -- ^ rise time of the inhibitory synaptic conductance (alpha function)
    }
    | HHCondExp {
        _cm :: Vector Float, -- ^ capacity of the membrane
        _e_rev_E :: Vector Float, -- ^ excitatory reversal potential
        _e_rev_I :: Vector Float, -- ^ inhibitory reversal potential
        _e_rev_K :: Vector Float,
        _e_rev_Na :: Vector Float,
        _e_rev_leak :: Vector Float,
        _g_leak :: Vector Float,
        _gbar_K :: Vector Float,
        _gbar_Na :: Vector Float,
        _i_offset :: Vector Float,
        _tau_syn_E :: Vector Float,
        _tau_syn_I :: Vector Float,
        _v_offset :: Vector Float
    } deriving (Eq, Show)

makeLenses ''NeuronType
makePrisms ''NeuronType

-- ^  Defaults taken from http://neuralensemble.org/docs/PyNN/standardmodels.html
if_cond_exp :: NeuronType
if_cond_exp = IFCondExp {
    _v_rest = [-65.0],
    _cm = [1.0],
    _tau_m = [20.0],
    _tau_refrac = [0.0],
    _tau_syn_E = [5.0],
    _tau_syn_I = [5.0],
    _e_rev_E = [0.0],
    _e_rev_I = [-70.0],
    _v_thresh = [-50.0],
    _v_reset = [-65.0],
    _i_offset = [0.0]
}

if_current_alpha :: NeuronType
if_current_alpha = IFCurrentAlpha {
    _tau_m = [20.0],
    _tau_refrac = [0.1],
    _v_thresh = [-50.0],
    _tau_syn_E = [ 0.5],
    _tau_syn_I = [0.5],
    _v_rest = [-65.0],
    _cm = [1.0],
    _v_reset = [-65.0],
    _i_offset = [0.0]
}

if_cond_alpha :: NeuronType
if_cond_alpha = IFCondAlpha {
    _v_rest = [-65.0],
    _cm = [1.0],
    _tau_m = [20.0],
    _tau_refrac = [0.0],
    _tau_syn_E = [5.0],
    _tau_syn_I = [5.0],
    _e_rev_E = [0.0],
    _e_rev_I = [-70.0],
    _v_thresh = [-50.0],
    _v_reset = [-65.0],
    _i_offset = [0.0]
}

if_spikey :: NeuronType
if_spikey = IFSpikey {
    _e_rev_I = [-80.0],
    _g_leak = [20.0],
    _tau_refrac = [1.0],
    _v_reset = [-80.0],
    _v_rest = [-75.0],
    _v_thresh = [-55.0]
}

if_current_exponential :: NeuronType
if_current_exponential = IFCurrExp {
    _cm = [1.0],
    _tau_m = [20.0],
    _tau_syn_E = [5.0],
    _tau_syn_I = [5.0],
    _tau_refrac = [0.1],
    _v_thresh = [-50.0],
    _v_rest = [-65.0],
    _v_reset = [-65.0],
    _i_offset = [0.0]
}

izhikevich :: NeuronType
izhikevich = Izhikevich {
    _a = [0.02],
    _i_offset = [0.0],
    _c = [-65.0],
    _d = [2.0],
    _b = [0.2]
}

eif_cond_exp :: NeuronType
eif_cond_exp = EIFCondExp {
    _v_reset = [-70.6],
    _i_offset = [0.0],
    _tau_w = [144.0],
    _tau_syn_I = [5.0],
    _e_rev_E = [0.0],
    _v_rest = [-70.6],
    _cm = [0.281],
    _tau_syn_E = [5.0],
    _tau_m = [9.3667],
    _a = [4.0],
    _delta_T = [2.0],
    _v_thresh = [-50.4],
    _b = [0.0805],
    _v_spike = [-40.0],
    _e_rev_I = [-80.0],
    _tau_refrac = [0.1]
}

eif_cond_alpha :: NeuronType
eif_cond_alpha = EIFCondAlpha {
    _v_reset = [-70.6],
    _i_offset = [0.0],
    _tau_w = [144.0],
    _tau_syn_I = [5.0],
    _e_rev_E = [0.0],
    _v_rest = [-70.6],
    _cm = [0.281],
    _tau_syn_E = [5.0],
    _tau_m = [9.3667],
    _a = [4.0],
    _delta_T = [2.0],
    _v_thresh = [-50.4],
    _b = [0.0805],
    _v_spike = [-40.0],
    _e_rev_I = [-80.0],
    _tau_refrac = [0.1]
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


