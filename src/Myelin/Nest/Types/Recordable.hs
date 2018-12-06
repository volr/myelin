module Myelin.Nest.Types.Recordable where

-- | Where to store the recording
data RecordTarget =
    Memory
    | File
    | UI 
    deriving (Read, Show, Eq, Ord)

-- | Recordable values of nest nodes
data Recordable =
      G_in
    | G_ex    
    | V_m
    | V_th
    | V_th_v
    | H
    | S
    | W
    | G_1
    | E_sfa
    | I_syn_ex
    | I_syn_in
    | I_syn_1
    | Inact_n
    | U_m
    | Act_h
    | Act_m
    | Rate
    | Noise
    | NoiseRate
    | NEvents
    | G_Ahp
    | G_AMPA
    | G_GABA_A
    | G_GABA_B
    | G_NMDA
    | G_rr
    | I_h
    | I_KNa
    | I_NaP
    | I_T
    | Theta
    deriving (Read, Show, Eq, Ord)
