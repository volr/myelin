{-# LANGUAGE TemplateHaskell #-}
module Myelin.PyNN.Types.Task where

import Control.Lens    
import Data.Aeson.TH    

data Network = Network

deriveJSON defaultOptions ''Network

data ExecutionTarget = ExecutionTarget

deriveJSON defaultOptions ''ExecutionTarget


-- | An execution task specifies all information needed to execute a SNN 
-- on a specific target.
data Task a = Task {
    _executionTarget :: ExecutionTarget, -- ^ which neuromorphic hardware or simulator to run on
    _network :: Network, -- ^ network that should be implemented
    _backendParameters :: a -- ^ backend specific parameters
}

deriveJSON defaultOptions ''Task
makeLenses ''Task
makePrisms ''Task