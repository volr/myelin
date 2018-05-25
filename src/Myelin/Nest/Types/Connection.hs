module Myelin.Nest.Types.Connection where

data Connection = 
      AllToAll
    | FixedIndegree
    | OneToOne
    | FixedOutdegree
    | PairwiseBernoulli
