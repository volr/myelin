{-# LANGUAGE TemplateHaskell #-}
module Myelin.PyNN.Types.Synapse where

import Control.Lens
import Data.Aeson.TH
import Numeric.LinearAlgebra

type ScalarWeight = Float
type Weight = Matrix Float
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
        _weight :: Weight
    }
    | OneToOne {
        _weight :: Weight
    }
    | FixedNumberPost {
        _n :: Int,
        _weight :: Weight
    }
    | FixedNumberPre {
        _n :: Int,
        _weight :: Weight
    }
    | FromList {
        _weights :: [(Int, Int, ScalarWeight)]
    }
    | FixedProbability {
        _probability :: Probability
    }
    deriving (Eq, Show)

deriveJSON defaultOptions ''ProjectionType
makeLenses ''ProjectionType
makePrisms ''ProjectionType

