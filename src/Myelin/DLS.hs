{-# LANGUAGE RecordWildCards #-}
module Myelin.DLS where

import Myelin.SNN

data Location = Location Int

data HardwareConfig = HardwareConfig {
    _neurons :: [HardwareNeuron]
}

data HardwareNeuron = HardwareNeuron {
    _loc :: Location
}

numHardwareNeurons = 32 -- hardcoded for now

mapBlockToHardware :: BlockState -> Maybe HardwareConfig
mapBlockToHardware BlockState{..} = undefined

placeNeuronPopulations :: BlockState -> Int
placeNeuronPopulations BlockState{..} = undefined
    where 
        numberOfNeurons = sum $ map neuronCount _nodes
        neuronCount Population{..} = _numNeurons
        neuronCount _ = 0

mapEdgesToWeightMatrix :: BlockState -> Int
mapEdgesToWeightMatrix BlockState{..} = undefined


