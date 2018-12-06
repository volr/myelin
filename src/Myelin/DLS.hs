{-# LANGUAGE RecordWildCards #-}
module Myelin.DLS where

import Myelin.SNN
import Myelin.Model

data Location = Location Int

data HardwareConfig = HardwareConfig {
    _neurons :: [HardwareNeuron]
}

data HardwareNeuron = HardwareNeuron {
    _loc :: Location
}

numHardwareNeurons = 32 -- hardcoded for now

mapNetworkToHardware :: Network -> Maybe HardwareConfig
mapNetworkToHardware Network{..} = undefined

placeNeuronPopulations :: Network -> Int
placeNeuronPopulations Network{..} = undefined
    where 
        numberOfNeurons = sum $ map neuronCount _nodes
        neuronCount Population{..} = _numNeurons
        neuronCount _ = 0

mapEdgesToWeightMatrix :: Network -> Int
mapEdgesToWeightMatrix Network{..} = undefined


