module Myelin.PyNN.Types.Recordable where

import Myelin.PyNN.Types.Node    

data Recordable =
    Spikes
    | V
    | G_r
    | G_s
    | Gsyn_exc
    | Gsyn_inh
    deriving (Eq, Show, Ord, Read)

-- | returns a list of recordable parameters for a given node
recordable :: Node -> [Recordable]
recordable = undefined
