module Myelin.Nest.Example where 

import Control.Lens

{-
experiment =
    ex neuron_type
    where 
        neuron_type = iaf_psc_alpha 
        ex neuron_type = do
            a <- create  $ neuron_type 100 & beta_ca .~ 0.2
                                    & c_m .~ 2.0
            b <- create $ neuron_type 20 & beta_ca .~ 0.5
                                   & c_m .~ 0.4
            connect a b $ static_synapse & delay .~ 0.2
                                     & weight .~ 0.9
-}