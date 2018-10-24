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
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import qualified Data.ByteString.Lazy.Char8 as BS

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
emptyPyNNModel :: PyNNModel
emptyPyNNModel = PyNNModel [] Map.empty Map.empty

type PyNNState = ExceptT String (State PyNNModel)

translate :: Network -> PyNNPreamble -> Either String String
translate network (PyNNPreamble {..}) = 
  let (result, (PyNNModel {..})) = runState (runExceptT $ translate' network) emptyPyNNModel
  in  case result of
        Left translationError -> Left $ "Failed to translate SNN model to Python: " ++ translationError
        Right _ ->
          let nodeLines = unlines $ Map.elems $ _pyNNNodes
              projectionLines = unlines $ Map.elems $ _pyNNProjections
              declarationLines = unlines $ _declarations
              preambleLines = pyNNImport ++ "\n" ++ configuration
          in Right $ unlines [nodeLines, projectionLines, declarationLines, preambleLines]
 
translate' :: Network -> PyNNState ()
translate' Network {..} = do
  inputStrings <- mapM pyNNNode _inputs
  _ <- mapM pyNNNode _nodes
  outputStrings <- mapM pyNNNode _outputs
  _ <- mapM pyNNEdge _edges
  let inputList = "[" ++ (intercalate "," inputStrings) ++ "]"
  let outputList = "[" ++ (intercalate "," outputStrings) ++ "]"
  let model = "pu.Model(" ++ inputList ++ "," ++ outputList ++ ")"
  declarations <>= [model]
  return ()

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
  Population {..} -> do
    pop <- pyNNPopulation _neuronType
    nodeVariable _id $ pyNNLearningNode pop
  _ -> throwError $ "Unknown node type " ++ (show node)

--   SpikeSourceArray { .. } -> 
--   SpikeSourcePoisson { .. } -> 

-- | Converts the code of a regular PyNN node into a learning node
pyNNLearningNode :: String -> String
pyNNLearningNode pop = "pu.LearningNode(" ++ pop ++ ")"

-- | Returns the code for a PyNN population
pyNNPopulation :: NeuronType -> PyNNState String
pyNNPopulation tpe = 
  let params = BS.unpack $ encode tpe
  in  case tpe of
        IFCondExp { .. } -> return $ "pynn.IF_cond_exp(" ++ params ++ ")"
        _ -> throwError $ "Unknown neuron type " ++ (show tpe)

-- | Creates and stores an Edge as a PyNN projection
pyNNEdge :: Edge -> PyNNState String
pyNNEdge Projection { .. } = do
  PyNNProjection {..} <- pyNNProjection _effect
--  let dynamics = pyNNDynamics projectionDynamics
  let nodeRef1 = nodeReference $ _input ^. id
  let nodeRef2 = nodeReference $ _output ^. id
  let code = "pynn.Projection(" ++ nodeRef1 ++", " ++ nodeRef2 ++ ", connector = " ++ connector ++ ")"
  edgeRef <- projectionVariable code
  declarations <>= [edgeRef ++ ".set(weight=" ++ weight ++ ")", nodeRef1 ++ ".connect(" ++ nodeRef2 ++ ")"]
  return edgeRef

data PyNNProjection = PyNNProjection 
  { connector :: String
  , weight :: String
  }

-- | Create a PyNN Connector definition
pyNNProjection :: ProjectionEffect -> PyNNState PyNNProjection
pyNNProjection (Static Excitatory (AllToAll weight)) = do
  let connector = "pynn.AllToAllConnector()"
  w <- pyNNWeight weight
  return $ PyNNProjection connector w 
pyNNProjection p = throwError $ "Unknown projection effect" ++ (show p)

-- | Converts a weight to a PyNN weight setting
pyNNWeight :: Weights -> PyNNState String
pyNNWeight (Constant n) = return (show n)
pyNNWeight (GaussianRandom mean scale) = return [i|numpy.random.normal(#{mean}, #{scale})|]

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

