{-# LANGUAGE TemplateHaskell #-}
module Myelin.Nest.Types.MultiCompartment where

import Control.Lens
import Data.Aeson.TH

-- | parameters that can be passed to a multi compartment model
data Parameters = Parameters {
    _c_m :: Float,
    _e_ex :: Float,
    _e_in :: Float,
    _e_L :: Float,
    _g_L :: Float,
    _i_e :: Float,
    _tau_syn_ex :: Float,
    _tau_syn_in :: Float,
    _v_m :: Float
} deriving (Show, Read, Eq, Ord)

defaults = Parameters {
    _c_m = 150.0, 
    _e_ex = 0.0, 
    _e_in = -85.0, 
    _e_L = -70.0, 
    _g_L = 10.0, 
    _i_e = 0.0,
    _tau_syn_ex = 0.5, 
    _tau_syn_in = 2.0, 
    _v_m = -70.0
}

deriveJSON defaultOptions ''Parameters
makeLenses ''Parameters