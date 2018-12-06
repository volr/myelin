module Myelin.PyNN.Types.ExecutionTarget where

-- | The targets of PyNN
data ExecutionTarget =
    Nest {
        _minTimestep :: Float,
        _maxTimestep :: Float
    }
    | BrainScaleS {
        _wafer :: Int, -- ^ wafer to run the experiment on
        _hicann :: Int -- ^ hicann chip to use
    } -- ^ first generation (wafer) brainscales system
    | Spikey {
        _mappingOffset :: Int -- ^  0..192 (really only 0 and 192 are sensible)
    } 
    | SpiNNaker -- ^ SpiNNaker neuromorphic platform
    | BrainScaleS2 -- ^ second generation brainscales system
    deriving (Eq, Show)
