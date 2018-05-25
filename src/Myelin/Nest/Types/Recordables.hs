module Myelin.Nest.Types.Recordables where

-- | Where to store the recording
data RecordTarget =
    Memory
    | File
    | UI 

-- | Recordable values of nest nodes
data Recordable =
      G_in
    | G_ex    
    | V_m
    | W
    | G_1
    | I_syn_ex
    | I_syn_in
    | Inact_n
    | U_m
    | Act_h
    | Rate
    | Noise
    | NoiseRate
    | NEvents
    | G_Ahp
    | G_AMPA
    | G_GABA_A
    | G_GABA_B
    | G_NMDA
    | I_h
    | I_KNa
    | I_NaP
    | I_T
    | Theta

-- TODO(Christian): Mapping between nodes and recordable parameters

{-

Recordable parameters

(<SLILiteral: g_ex>, <SLILiteral: g_in>, <SLILiteral: V_m>, <SLILiteral: w>)
(<SLILiteral: g_ex>, <SLILiteral: g_in>, <SLILiteral: V_m>, <SLILiteral: w>)
(<SLILiteral: V_m>, <SLILiteral: w>, <SLILiteral: g_1>)
(<SLILiteral: V_m>, <SLILiteral: w>, <SLILiteral: g_1>)
(<SLILiteral: g_ex>, <SLILiteral: g_in>, <SLILiteral: V_m>, <SLILiteral: w>)
(<SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: V_m>, <SLILiteral: w>)
(<SLILiteral: V_m>, <SLILiteral: w>)
(<SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: V_m>, <SLILiteral: w>)
(<SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: V_m>, <SLILiteral: V_th>, <SLILiteral: V_th_v>)
(<SLILiteral: h>, <SLILiteral: S>)
(<SLILiteral: noise>, <SLILiteral: rate>)
(<SLILiteral: E_sfa>, <SLILiteral: g_ex>, <SLILiteral: g_in>, <SLILiteral: I_stc>, <SLILiteral: V_m>)
(<SLILiteral: E_sfa>, <SLILiteral: I_stc>, <SLILiteral: V_m>)
(<SLILiteral: E_sfa>, <SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: mean>, <SLILiteral: n_events>, <SLILiteral: V_m>)
(<SLILiteral: E_sfa>, <SLILiteral: I_stc>, <SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: V_m>)
(<SLILiteral: E_sfa>, <SLILiteral: I_stc>, <SLILiteral: V_m>)
(<SLILiteral: h>, <SLILiteral: S>)
(<SLILiteral: Act_h>, <SLILiteral: Act_m>, <SLILiteral: g_ex>, <SLILiteral: g_in>, <SLILiteral: Inact_n>, <SLILiteral: V_m>)
(<SLILiteral: Act_h>, <SLILiteral: Act_m>, <SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: Inact_n>, <SLILiteral: V_m>)
(<SLILiteral: Act_h>, <SLILiteral: Act_m>, <SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: Inact_n>, <SLILiteral: Inact_p>, <SLILiteral: V_m>)
(<SLILiteral: g_AMPA>, <SLILiteral: g_GABA_A>, <SLILiteral: g_GABA_B>, <SLILiteral: g_NMDA>, <SLILiteral: I_h>, <SLILiteral: I_KNa>, <SLILiteral: I_NaP>, <SLILiteral: I_T>, <SLILiteral: theta>, <SLILiteral: V_m>)
(<SLILiteral: V_m>,)
(<SLILiteral: g_ahp>, <SLILiteral: g_ex>, <SLILiteral: g_in>, <SLILiteral: I_ahp>, <SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: V_m>)
(<SLILiteral: g_ex>, <SLILiteral: g_in>, <SLILiteral: t_ref_remaining>, <SLILiteral: V_m>)
(<SLILiteral: t_ref_remaining>, <SLILiteral: V_m.s>, <SLILiteral: g_ex.s>, <SLILiteral: g_in.s>, <SLILiteral: V_m.p>, <SLILiteral: g_ex.p>, <SLILiteral: g_in.p>, <SLILiteral: V_m.d>, <SLILiteral: g_ex.d>, <SLILiteral: g_in.d>)
(<SLILiteral: g_ex>, <SLILiteral: g_in>, <SLILiteral: V_m>)
(<SLILiteral: g_ex>, <SLILiteral: g_in>, <SLILiteral: g_rr>, <SLILiteral: g_sfa>, <SLILiteral: V_m>)
(<SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: V_m>, <SLILiteral: weighted_spikes_ex>, <SLILiteral: weighted_spikes_in>)
(<SLILiteral: V_m>,)
(<SLILiteral: I_syn>, <SLILiteral: V_m>, <SLILiteral: I_syn_1>)
(<SLILiteral: V_m>,)
(<SLILiteral: V_m>,)
(<SLILiteral: V_m>,)
(<SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: V_m>, <SLILiteral: weighted_spikes_ex>, <SLILiteral: weighted_spikes_in>)
(<SLILiteral: I_syn>, <SLILiteral: V_m>, <SLILiteral: I_syn_1>)
(<SLILiteral: V_m>,)
(<SLILiteral: I_syn>, <SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: V_m>)
(<SLILiteral: I_syn_ex>, <SLILiteral: I_syn_in>, <SLILiteral: V_m>)
(<SLILiteral: U_m>, <SLILiteral: V_m>)
(<SLILiteral: noise>, <SLILiteral: rate>)
(<SLILiteral: noise>, <SLILiteral: noisy_rate>, <SLILiteral: rate>)
(<SLILiteral: V_m>, <SLILiteral: V_th>)
(<SLILiteral: h>, <SLILiteral: S>)
(<SLILiteral: n_events>, <SLILiteral: V_m>)
(<SLILiteral: E_sfa>, <SLILiteral: V_m>)
(<SLILiteral: rate>,)
(<SLILiteral: rate>,)
(<SLILiteral: rate>,)
(<SLILiteral: rate>,)
(<SLILiteral: rate>,)
(<SLILiteral: rate>,)
(<SLILiteral: rate>,)
(<SLILiteral: noise>, <SLILiteral: rate>)
(<SLILiteral: noise>, <SLILiteral: rate>)
(<SLILiteral: noise>, <SLILiteral: rate>)
(<SLILiteral: noise>, <SLILiteral: noisy_rate>, <SLILiteral: rate>)
(<SLILiteral: noise>, <SLILiteral: rate>)
(<SLILiteral: noise>, <SLILiteral: noisy_rate>, <SLILiteral: rate>)
(<SLILiteral: I>,)
(<SLILiteral: I>,)
(<SLILiteral: I>,)
(<SLILiteral: rate>,)
(<SLILiteral: rate>,)
(<SLILiteral: I>,)
-}