{-# LANGUAGE TemplateHaskell #-}
module Myelin.PyNN.Types.Synapse where

import Control.Lens
import Data.Aeson.TH
import Numeric.LinearAlgebra

type ScalarWeight = Float
type Probability = Float

{--|

ProjectionType corresponds to the different connectors available
in pynn. Some of the connectors are backend specific.

AllToAll,
FixedProbability,
DistanceDependentProbability,
FixedNumberPre,
FixedNumberPost,
OneToOne,
SmallWorld,
FromList,
FromFile

Not all have them been defined below. In addition there
are backend specific connectors. 

--}
data ProjectionType =
    AllToAll {
        _weight :: Matrix Float
    }
    | OneToOne {
        _weight :: Matrix Float
    }
    | FixedNumberPost {
        _n :: Int,
        _weight ::  Matrix Float
    }
    | FixedNumberPre {
        _n :: Int,
        _weight ::  Matrix Float
    }
    | FromList {
        _weights :: [(Int, Int, Float)]
    }
    | FixedProbability {
        _probability :: Probability
    }
    deriving (Eq, Show)

--deriveJSON defaultOptions ''ProjectionType
makeLenses ''ProjectionType
makePrisms ''ProjectionType

