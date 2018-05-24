module Myelin.NEST(run) where

import Data.Aeson
import System.Process
import GHC.IO.Handle
import Data.ByteString.Lazy as B

import Myelin.SNN


{-

List of NEST nodes that are supported (in master):

-------- neurons -----------
aeif_cond_alpha
aeif_cond_alpha_RK5
aeif_cond_alpha_multisynapse
aeif_cond_beta_multisynapse
aeif_cond_exp
aeif_psc_alpha
aeif_psc_delta
aeif_psc_exp
amat2_psc_exp
erfc_neuron
gauss_rate_ipn
gif_cond_exp
gif_cond_exp_multisynapse
gif_pop_psc_exp
gif_psc_exp
gif_psc_exp_multisynapse
ginzburg_neuron
hh_cond_exp_traub
hh_psc_alpha
hh_psc_alpha_gap
ht_neuron
iaf_chs_2007
iaf_chxk_2008
iaf_cond_alpha
iaf_cond_alpha_mc
iaf_cond_exp
iaf_cond_exp_sfa_rr
iaf_psc_alpha
iaf_psc_alpha_canon
iaf_psc_alpha_multisynapse
iaf_psc_alpha_presc
iaf_psc_delta
iaf_psc_delta_canon
iaf_psc_exp
iaf_psc_exp_multisynapse
iaf_psc_exp_ps
iaf_psc_exp_ps_lossless
iaf_tum_2000
izhikevich
lin_rate_ipn
lin_rate_opn
mat2_psc_exp
mcculloch_pitts_neuron
parrot_neuron
parrot_neuron_ps
pp_pop_psc_delta
pp_psc_delta
rate_transformer_gauss
rate_transformer_lin
rate_transformer_sigmoid
rate_transformer_sigmoid_gg_1998
rate_transformer_tanh
rate_transformer_threshold_lin
siegert_neuron
sigmoid_rate_gg_1998_ipn
sigmoid_rate_ipn
tanh_rate_ipn
tanh_rate_opn
threshold_lin_rate_ipn
threshold_lin_rate_opn
-------- recorders ---------
correlation_detector
correlomatrix_detector
correlospinmatrix_detector
multimeter
spike_detector
spin_detector
voltmeter
weight_recorder
-------- stimulators -------
ac_generator
dc_generator
gamma_sup_generator
inhomogeneous_poisson_generator
mip_generator
noise_generator
poisson_generator
poisson_generator_ps
ppd_sup_generator
pulsepacket_generator
sinusoidal_gamma_generator
sinusoidal_poisson_generator
spike_dilutor
spike_generator
step_current_generator

List of NEST synapse models that are supported (in master):

bernoulli_synapse
bernoulli_synapse_lbl
cont_delay_synapse
cont_delay_synapse_hpc
cont_delay_synapse_lbl
diffusion_connection
diffusion_connection_lbl
gap_junction
gap_junction_lbl
ht_synapse
ht_synapse_hpc
ht_synapse_lbl
quantal_stp_synapse
quantal_stp_synapse_hpc
quantal_stp_synapse_lbl
rate_connection_delayed
rate_connection_delayed_lbl
rate_connection_instantaneous
rate_connection_instantaneous_lbl
static_synapse
static_synapse_hom_w
static_synapse_hom_w_hpc
static_synapse_hom_w_lbl
static_synapse_hpc
static_synapse_lbl
stdp_dopamine_synapse
stdp_dopamine_synapse_hpc
stdp_dopamine_synapse_lbl
stdp_facetshw_synapse_hom
stdp_facetshw_synapse_hom_hpc
stdp_facetshw_synapse_hom_lbl
stdp_pl_synapse_hom
stdp_pl_synapse_hom_hpc
stdp_pl_synapse_hom_lbl
stdp_synapse
stdp_synapse_hom
stdp_synapse_hom_hpc
stdp_synapse_hom_lbl
stdp_synapse_hpc
stdp_synapse_lbl
stdp_triplet_synapse
stdp_triplet_synapse_hpc
stdp_triplet_synapse_lbl
tsodyks2_synapse
tsodyks2_synapse_hpc
tsodyks2_synapse_lbl
tsodyks_synapse
tsodyks_synapse_hom
tsodyks_synapse_hom_hpc
tsodyks_synapse_hom_lbl
tsodyks_synapse_hpc
tsodyks_synapse_lbl
vogels_sprekeler_synapse
vogels_sprekeler_synapse_hpc
vogels_sprekeler_synapse_lbl

-}




run :: Task -> IO ByteString
run task = do
    let jsonDescription = encode $ toJSON task
    (Just scriptHandle, Just runOut, _, _) <- createProcess (proc "python" ["python/nest_pynn_executor.py"]) { std_in = CreatePipe, std_out = CreatePipe}
    B.hPutStr scriptHandle jsonDescription
    B.hGetContents runOut

