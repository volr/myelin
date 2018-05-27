module Myelin.Nest.Example where 

import Control.Lens

{-
ex = do
    a <- create $ iaf_psc_alpha & beta_ca .~ 0.2
                                & c_m .~ 2.0
    b <- create $ iaf_psc_alpha & beta_ca .~ 0.5
                                & c_m .~ 0.4
    connect a b $ static_synapse & delay .~ 0.2
                                 & weight .~ 0.9
-}