module Myelin.Nest.Types.Distributions where

-- | Distributions supported as arguments for synapse weights and other paramters in Nest
data Distribution =
    Normal {
        _mu :: Float,
        _sigma :: Float
    }
    | LogNormal {
        _mu :: Float,
        _sigma :: Float
    }
    | Uniform {
        _low :: Float,
        _high :: Float
    }
    | UniformIntegral {
        _lower :: Int,
        _upper :: Int
    }
    | Binomial {
    -- n, p
    }
    | Exponential {
        _lambda :: Float
    }
    | Gamma {
    -- order, scale
    }
    | Poisson {
        _lambda :: Float
    } deriving (Read, Show, Eq, Ord)
