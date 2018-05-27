{-# LANGUAGE TemplateHaskell, OverloadedLists #-}
module Myelin.PyNN.Types.Node where

import Data.Aeson.TH
import Control.Lens
import Numeric.LinearAlgebra

-- | pyNN population node
data Node =
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
        _i_offset :: Vector Float, -- ^ offset current
        _c :: Vector Float,
        _d :: Vector Float,
        _b :: Vector Float
    }
    | EIFCondExp {
        _v_reset :: Vector Float, -- ^ reset potential
        _i_offset :: Vector Float, -- ^ offset current
        _tau_w :: Vector Float,
        _tau_syn_I :: Vector Float, -- ^ inhibitory synaptic time constant
        _e_rev_E :: Vector Float,
        _v_rest :: Vector Float, -- ^ resting potential
        _cm :: Vector Float, -- ^ capactivie memory
        _tau_syn_E :: Vector Float, -- ^ excitatory synaptic time constant
        _tau_m :: Vector Float, -- ^ membrane timeconstant
        _a :: Vector Float,
        _delta_T :: Vector Float,
        _v_thresh :: Vector Float, -- ^ threshhold potential
        _b :: Vector Float,
        _v_spike :: Vector Float,
        _e_rev_I :: Vector Float,
        _tau_refrac :: Vector Float -- ^ refractory time constant
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
    } 
    | SpikeSourcePoisson {
        _start :: Float, 
        _duration :: Float,
        _rate :: Float
    }
    | SpikeSourceArray {
        _spike_times :: Vector Float
    }
    | SpikeSourceInhGamma {
        _a :: Vector Float,
        _tbins :: Vector Float,
        _duration :: Float,
        _b :: Vector Float,
        _start :: Float
    }
    deriving (Eq, Show, Ord, Read)

-- |  Defaults taken from http://neuralensemble.org/docs/PyNN/standardmodels.html

-- | default parameters for the integrate and fire conductance based neuron with exponential term
if_cond_exp :: Node
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

if_current_alpha :: Node
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

if_cond_alpha :: Node
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

if_spikey :: Node
if_spikey = IFSpikey {
    _e_rev_I = [-80.0],
    _g_leak = [20.0],
    _tau_refrac = [1.0],
    _v_reset = [-80.0],
    _v_rest = [-75.0],
    _v_thresh = [-55.0]
}

if_current_exponential :: Node
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

izhikevich :: Node
izhikevich = Izhikevich {
    _a = [0.02],
    _i_offset = [0.0],
    _c = [-65.0],
    _d = [2.0],
    _b = [0.2]
}

eif_cond_exp :: Node
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

eif_cond_alpha :: Node
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

spike_source_poisson = SpikeSourcePoisson {
    _start = 0.0,
    _duration = 100.0,
    _rate = 0.1
}

spike_source_array = SpikeSourceArray {
    _spike_times = [],
}

deriveJSON defaultOptions ''Node
makeLenses ''Node
makePrisms ''Node