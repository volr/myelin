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
  , _layers :: Map.Map String String -- ^ Layers between the nodes
  , _populations :: Map.Map String String -- ^ Nodes indexed by their Python variables
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
          let populationLines = unlines $ Map.elems _populations
              layerLines = unlines $ Map.elems _layers
              declarationLines = unlines $ _declarations
              preampleLines = pyNNPreample preample 
          in Right $ concat [preampleLines, populationLines, layerLines, declarationLines]
 
translate' :: Network -> PyNNState ()
translate' Network {..} = do
  inputStrings <- mapM pyNNNode _inputs
  _ <- mapM pyNNNode _nodes
  outputStrings <- mapM pyNNNode _outputs
  _ <- mapM pyNNEdge _edges
  layers <- fmap _layers get
  let layersString = intercalate ", " $ Map.keys layers
  let model = "model = v.Model(pynn, " ++ layersString ++ ")"
  declarations <>= [model]

-- | The Python PyNN preamble for import statements
pyNNPreample :: PyNNPreample -> String
pyNNPreample PyNNPreample {..} = [i|import numpy
import #{pyNNImport} as pynn
import volrpynn as v

#{configuration}

|]

pyNNNode :: Node -> PyNNState String
pyNNNode node = case node of
  Population {..} -> do
    pyNNPopulation _neuronType _numNeurons _id
  _ -> throwError $ "Unknown node type " ++ (show node)

--   SpikeSourceArray { .. } -> 
--  SpikeSourcePoisson { .. } -> do 

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
        IFCondExp { .. } -> Right $ "pynn.Population(" ++ (show numNeurons) ++ ", pynn.IF_cond_exp(**" ++ params ++ "))"
        _ -> Left $ "Unknown neuron type " ++ (show tpe)

-- | Creates and stores an Edge as a PyNN projection
pyNNEdge :: Edge -> PyNNState String
pyNNEdge Projection { .. } = do
  PyNNProjection {..} <- pyNNProjection _effect
  let popRef1 = populationReference $ _input ^. id
  let popRef2 = populationReference $ _output ^. id
  let code = "v." ++ layerType ++ "(pynn, " ++ popRef1 ++", " ++ popRef2 ++ ", weights = " ++ weight ++")"
  layerVariable code

data PyNNProjection = PyNNProjection 
  { layerType :: String
  , weight :: String
  }

-- | Create a PyNN Connector definition
pyNNProjection :: ProjectionEffect -> PyNNState PyNNProjection
pyNNProjection (Static Excitatory (AllToAll weight)) = do
  w <- pyNNWeight weight
  return $ PyNNProjection  "Dense" w
pyNNProjection p = throwError $ "Unknown projection effect" ++ (show p)

-- | Converts a weight to a PyNN weight setting
pyNNWeight :: Weights -> PyNNState String
pyNNWeight (Constant n) = return (show n)
pyNNWeight (GaussianRandom mean scale) = return [i|numpy.random.normal(#{mean}, #{scale})|]

populationReference :: Int -> String
populationReference nodeId = "p" ++ (show nodeId)

populationVariable :: Int -> String -> PyNNState String
populationVariable popId code = do
  let name = "p" ++ (show popId)
  let variable = pythonVariable name code
  populations %= (Map.insert name variable)
  return name

layerVariable :: String -> PyNNState String
layerVariable code = do
  projections <- use layers
  let name = "layer" ++ (show $ Map.size projections)
  let variable = pythonVariable name code
  layers %= (Map.insert name variable)
  return name

pythonVariable :: String -> String -> String
pythonVariable name value = name ++ " = " ++ value

