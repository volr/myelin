{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Myelin.PyNN.PyNN where

import Prelude hiding (id)

import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Lens
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import Data.Text (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.RawString.QQ

import Myelin.Model
import Myelin.Neuron
import Myelin.SNN

data PyNNPreamble
  = PyNNPreamble 
    { pyNNImport :: String
    , configuration :: String
    }
    deriving (Eq, Show)

data PyNNModel = PyNNModel 
  { _declarations :: [String]
  , _pyNNNodes :: Map.Map String String -- ^ Nodes indexed by their Python variables
  , _pyNNProjections :: Map.Map String String -- ^ Projections between nodes indexed by their python variables
  }
  deriving (Eq, Show)

makeLenses ''PyNNModel

-- | An empty initial model
emptyPyNNModel = PyNNModel [] Map.empty Map.empty

type PyNNState = ExceptT String (State PyNNModel)

translate :: Network -> PyNNPreamble -> Either String String
translate network (PyNNPreamble {..}) = 
  let (result, (PyNNModel {..})) = runState (runExceptT $ translate' network) emptyPyNNModel
  in  case result of
        Left error -> Left $ "Failed to parse to Python: " ++ error
	Right _ ->
          let nodeLines = unlines $ Map.elems $ _pyNNNodes
              projectionLines = unlines $ Map.elems $ _pyNNProjections
              declarationLines = unlines $ _declarations
              preambleLines = pyNNImport ++ "\n" ++ configuration
          in Right $ unlines [nodeLines, projectionLines, declarationLines, preambleLines]
 
translate' :: Network -> PyNNState ()
translate' _ = return ()

-- | The Python PyNN preamble for import statements
pyNNPreamble :: PyNNPreamble -> String
pyNNPreamble PyNNPreamble {..} = [i|
import addict
import numpy
import #{pyNNImport} as pynn

import pynn_utils as pu

#{configuration}
|]

pyNNNode :: Node -> PyNNState String
pyNNNode node = case node of
  Population {..} ->
    nodeVariable _id $ pyNNLearningNode $ pyNNPopulation _neuronType

--   SpikeSourceArray { .. } -> 
--   SpikeSourcePoisson { .. } -> 

-- | Converts the code of a regular PyNN node into a learning node
pyNNLearningNode :: String -> String
pyNNLearningNode population = "pu.LearningNode(" ++ population ++ ")"

-- | Returns the code for a PyNN population
pyNNPopulation :: NeuronType -> String
pyNNPopulation neuronType = 
  let params = BS.unpack $ encode neuronType
  in  case neuronType of
        IFCondExp { .. } -> "pynn.IF_cond_exp(" ++ params ++ ")"

-- | Creates and stores an Edge as a PyNN projection
pyNNEdge :: Edge -> PyNNState String
pyNNEdge Projection { .. } = do
  let connector = pyNNConnector _effect
--  let dynamics = pyNNDynamics projectionDynamics
  let nodeRef1 = nodeReference $ _input ^. id
  let nodeRef2 = nodeReference $ _output ^. id
  let code = "pynn.Projection(" ++ nodeRef1 ++", " ++ nodeRef2 ++ ", connector = " ++ connector ++ ")"
  edgeRef <- projectionVariable code
  declarations <>= [edgeRef ++ ".set(weight=numpy.random.normal)", nodeRef1 ++ ".connect(" ++ nodeRef2 ++ ")"]
  return edgeRef

pyNNConnector :: ProjectionEffect -> String
pyNNConnector (Static Excitatory (AllToAll _))  = "pynn.AllToAllConnector()"

-- pyNNDynamics :: ProjectionDynamics -> String
-- pyNNDynamics Static Excitatory = 

nodeReference :: Int -> String
nodeReference nodeId = "node" ++ (show nodeId)

nodeVariable :: Int -> String -> PyNNState String
nodeVariable nodeId code = do
  let name = "node" ++ (show nodeId)
  let variable = pythonVariable name code
  pyNNNodes %= (Map.insert name variable)
  return name

projectionVariable :: String -> PyNNState String
projectionVariable code = do
  projections <- use pyNNProjections
  let name = "proj" ++ (show $ Map.size projections)
  let variable = pythonVariable name code
  pyNNProjections %= (Map.insert name variable)
  return name

pythonVariable :: String -> String -> String
pythonVariable name value = name ++ " = " ++ value

