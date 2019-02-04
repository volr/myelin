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
import Numeric.LinearAlgebra
import Text.Regex as Regex

import Myelin.Model
import Myelin.Neuron
import Myelin.SNN

data PyNNPreample
  = PyNNPreample 
    { configuration :: String
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
translate :: Task -> PyNNPreample -> Either String String
translate (Task target network simulationTime) preample = 
  let (result, (PyNNModel {..})) = runState (runExceptT $ translate' network) emptyPyNNModel
  in  case result of
        Left translationError -> Left $ "Failed to translate SNN model to Python: " ++ translationError
        Right _ ->
          let populationLines = unlines $ Map.elems _populations
              layerLines = unlines $ Map.elems _layers
              declarationLines = unlines $ _declarations
              preampleLines = pyNNPreample target preample 
          in Right $ concat [preampleLines, populationLines, layerLines, declarationLines, training]
    where training = [i|
optimiser = v.GradientDescentOptimiser(0.1, simulation_time=#{simulationTime})
if __name__ == "__main__":
    v.Main(model).train(optimiser)
|]
 
translate' :: Network -> PyNNState ()
translate' Network {..} = do
  _ <- mapM pyNNNode _inputs
  _ <- mapM pyNNNode _nodes
  outputStrings <- mapM pyNNNode _outputs
  _ <- mapM pyNNEdge _edges
  layers <- fmap _layers get
  let decodeLayer = "l_decode = v.Decode(" ++ populationReference ((last $ _outputs) ^.id) ++ ")"
  let layersString = intercalate ", " $ Map.keys layers
  let model = "model = v.Model(" ++ layersString ++ ", l_decode)"
  declarations <>= [decodeLayer, model]

-- | The Python PyNN preamble for import statements
pyNNPreample :: ExecutionTarget -> PyNNPreample -> String
pyNNPreample target (PyNNPreample {..}) = let 
 pyNNTarget = 
   case target of
      Nest { .. } -> "nest"
 in [i|import numpy as np
import volrpynn.#{pyNNTarget} as v
import pyNN.#{pyNNTarget} as pynn

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
 
typeRegex = Regex.mkRegex "\"type\":\"\\w*\","

-- | Encodes a PyNN population as a string without state
pyNNPopulationString :: NeuronType -> Integer -> Int -> Either String String
pyNNPopulationString tpe numNeurons id =
  let params = BS.unpack $ encode tpe
      stripType = Regex.subRegex typeRegex params ""
  in  case tpe of
        IFCondExp { .. } -> Right $ "pynn.Population(" ++ (show numNeurons) ++ ", pynn.IF_cond_exp(**" ++ stripType ++ "))"
        _ -> Left $ "Unknown neuron type " ++ (show tpe)

-- | Creates and stores an Edge as a PyNN projection
pyNNEdge :: Edge -> PyNNState String
pyNNEdge projection = do
  (PyNNProjection {..}, inputs, outputs) <- pyNNProjection projection
  let popRef1 = concatReferences inputs
  let popRef2 = concatReferences outputs
  let code = "v." ++ layerType ++ "(" ++ popRef1 ++", " ++ popRef2 ++ (weightString weight) ++ (biasString bias) ++ ")"
  layerVariable code
    where 
      concatReferences :: [Node] -> String
      concatReferences nodes = 
        case length nodes of
          1 -> populationReference (nodes !! 0 ^. id)
          _ -> let strings = intercalate ", " $ map populationReference $ map (\node -> node ^. id) nodes
               in "(" ++ strings ++ ")"
      weightString (Just weight) =  ", weights=" ++ weight
      weightString Nothing = ""
      biasString (Just bias) = ", biases=" ++ bias
      biasString Nothing = ""

data PyNNProjection = PyNNProjection 
  { bias :: Maybe String
  , layerType :: String
  , weight :: Maybe String
  }

-- | Create a PyNN Connector definition
pyNNProjection :: Edge-> PyNNState (PyNNProjection, [Node], [Node])
pyNNProjection (DenseProjection (Static Excitatory (AllToAll bias weight)) nodeIn nodeOut) = do
  b <- pyNNBias bias (_numNeurons nodeOut)
  w <- pyNNWeight weight (_numNeurons nodeIn) (_numNeurons nodeOut)
  return $ (PyNNProjection (Just b) "Dense" (Just w), [nodeIn], [nodeOut])
pyNNProjection (MergeProjection _ (nodeIn1, nodeIn2) nodeOut) = do
  let input1Size = (_numNeurons nodeIn1) 
  let input2Size = (_numNeurons nodeIn2) 
  return $ input1Size + input2Size
  return $ (PyNNProjection Nothing "Merge" Nothing, [nodeIn1, nodeIn2], [nodeOut])
pyNNProjection (ReplicateProjection (Static Excitatory (AllToAll bias weight)) nodeIn (nodeOut1, nodeOut2)) = do
  let inputSize = _numNeurons nodeIn
  let output1Size = _numNeurons nodeOut1
  let output2Size = _numNeurons nodeOut2
  outputSize <- if (output1Size == output2Size)
    then return $ output1Size
    else throwError $ "Replicaten connections require two outputs of the same size, found " ++ (show (output1Size, output2Size))
  w1 <- pyNNWeight weight inputSize output1Size
  w2 <- pyNNWeight weight inputSize output1Size
  let w = [i|(#{w1}, #{w2})|]
  b <- pyNNBias bias output1Size
  return $ (PyNNProjection (Just b) "Replicate" (Just w), [nodeIn], [nodeOut1, nodeOut2])
pyNNProjection p = throwError $ "Unknown projection effect" ++ (show p)

-- | Converts biases to a Python bias setting
pyNNBias :: Biases -> Integer -> PyNNState String
pyNNBias (BiasGenerator (Constant n)) _ = return (show n)
pyNNBias (BiasGenerator (GaussianRandom mean scale)) toSize 
  = return [i|np.random.normal(#{mean}, #{scale}, (#{toSize}))|]
pyNNBias (Biases list) toSize = return [i|np.array(#{show list})|]

-- | Converts weights to a PyNN weight setting
pyNNWeight :: Weights -> Integer -> Integer -> PyNNState String
pyNNWeight (WeightGenerator (Constant n)) _ _ = return (show n)
pyNNWeight (WeightGenerator (GaussianRandom mean scale)) fromSize toSize 
  = return [i|np.random.normal(#{mean}, #{scale}, (#{fromSize}, #{toSize}))|]
pyNNWeight (Weights matrix) fromSize toSize = 
  let (rows, columns) = join bimap toInteger $ size matrix
      str = show $ toLists matrix
  in  if (rows == toSize && columns == fromSize) 
      then return [i|np.array(#{str})|]
      else throwError $ [i|Expected sizes (#{toSize},#{fromSize}), but got (#{rows},#{columns})|]

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

