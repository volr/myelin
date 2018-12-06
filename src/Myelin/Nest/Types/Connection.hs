module Myelin.Nest.Types.Connection where

-- | Connection types supported by Nest
data Connection = 
      AllToAll
    | FixedIndegree
    | OneToOne
    | FixedOutdegree
    | PairwiseBernoulli
    deriving (Read, Show, Eq, Ord)
