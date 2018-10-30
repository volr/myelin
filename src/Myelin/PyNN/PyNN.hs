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

data PyNNPreample
  = PyNNPreample 
    { pyNNImport :: String
    , configuration :: String
    }
    deriving (Eq, Show)

data PyNNModel = PyNNModel 
  { _declarations :: [String]
  , _pyNNPopulations :: Map.Map String String -- ^ Nodes indexed by their Python variables
  , _pyNNProjections :: Map.Map String String -- ^ Projections between nodes indexed by their python variables
  }
  deriving (Eq, Show)

makeLenses ''PyNNModel

-- | An empty initial model
emptyPyNNModel :: PyNNModel
emptyPyNNModel = PyNNModel [] Map.empty Map.empty

type PyNNState = ExceptT String (State PyNNModel)

-- | Attempts to translate a network into a Python code string
translate :: Network -> PyNNPreample -> Either String String
translate network preample = 
  let (result, (PyNNModel {..})) = runState (runExceptT $ translate' network) emptyPyNNModel
  in  case result of
        Left translationError -> Left $ "Failed to translate SNN model to Python: " ++ translationError
        Right _ ->
          let populationLines = unlines $ Map.elems $ _pyNNPopulations
              projectionLines = unlines $ Map.elems $ _pyNNProjections
              declarationLines = unlines $ _declarations
              preampleLines = pyNNPreample preample 
          in Right $ concat [preampleLines, populationLines, projectionLines, declarationLines]
 
translate' :: Network -> PyNNState ()
translate' Network {..} = do
  inputStrings <- mapM pyNNNode _inputs
  _ <- mapM pyNNNode _nodes
  outputStrings <- mapM pyNNNode _outputs
  _ <- mapM pyNNEdge _edges
  let inputList = "[" ++ (intercalate "," inputStrings) ++ "]"
  let outputList = "[" ++ (intercalate "," outputStrings) ++ "]"
  let model = "model = pm.Model(" ++ inputList ++ "," ++ outputList ++ ")"
  declarations <>= [model]

-- | The Python PyNN preamble for import statements
pyNNPreample :: PyNNPreample -> String
pyNNPreample PyNNPreample {..} = [i|import numpy
import #{pyNNImport} as pynn
import pynn_utils as pu
import pynn_model as pm

#{configuration}

|]

pyNNNode :: Node -> PyNNState String
pyNNNode node = case node of
  Population {..} -> do
    pop <- pyNNPopulation _neuronType _numNeurons _id
    pyNNLearningNode pop _id
  _ -> throwError $ "Unknown node type " ++ (show node)

--   SpikeSourceArray { .. } -> 
--  SpikeSourcePoisson { .. } -> do 

-- | Converts the code of a regular PyNN node into a learning node
pyNNLearningNode :: String -> Int -> PyNNState String
pyNNLearningNode pop id = do
    let nodeName = nodeReference id
    let code = "pm.LearningNode(" ++ pop ++ ")"
    declarations <>= [pythonVariable nodeName code]
    return nodeName

-- | Statefully encodes a pyNNPopulation, returning the variable name for that
--   population
pyNNPopulation :: NeuronType -> Integer -> Int -> PyNNState String
pyNNPopulation tpe numNeurons id = 
  case pyNNPopulationString tpe numNeurons id of
    Right codeString -> populationVariable id codeString
    Left errorString -> throwError errorString 
 
-- | Encodes a PyNN population as a string without state
pyNNPopulationString :: NeuronType -> Integer -> Int -> Either String String
pyNNPopulationString tpe numNeurons id =
  let params = BS.unpack $ encode tpe
  in  case tpe of
        IFCondExp { .. } -> Right $ "pynn.Population(" ++ (show numNeurons) ++ ", pynn.IF_Cond_Exp(" ++ params ++ "))"
        _ -> Left $ "Unknown neuron type " ++ (show tpe)

-- | Creates and stores an Edge as a PyNN projection
pyNNEdge :: Edge -> PyNNState String
pyNNEdge Projection { .. } = do
  PyNNProjection {..} <- pyNNProjection _effect
  let popRef1 = populationReference $ _input ^. id
  let popRef2 = populationReference $ _output ^. id
  let code = "pynn.Projection(" ++ popRef1 ++", " ++ popRef2 ++ ", connector = " ++ connector ++ ")"
  edgeRef <- projectionVariable code
  let nodeRef1 = nodeReference $ _input ^. id
  let nodeRef2 = nodeReference $ _output ^. id
  declarations <>= [edgeRef ++ ".set(weight=" ++ weight ++ ")", nodeRef1 ++ ".connect_to(" ++ nodeRef2 ++ ")"]
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

nodeReference :: Int -> String
nodeReference id = "node" ++ (show id)

populationReference :: Int -> String
populationReference nodeId = "p" ++ (show nodeId)

populationVariable :: Int -> String -> PyNNState String
populationVariable popId code = do
  let name = "p" ++ (show popId)
  let variable = pythonVariable name code
  pyNNPopulations %= (Map.insert name variable)
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

