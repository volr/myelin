{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Myelin.PyNN.PyNN where

import Prelude hiding (id)

import Control.Lens
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Text (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.RawString.QQ
import NeatInterpolation (text)

import Myelin.Model
import Myelin.Neuron
import Myelin.SNN

data PyNNPreample
  = PyNNPreample 
    { pyNNImport :: String
    , instrumentation :: String
    }

-- | A variable declaration in Python
type PythonDeclaration = (String, String)

-- translate :: Network -> PyNNPreample -> String
-- translate network preample = 
--   
-- 
-- translate' :: Network -> String
-- translate 

-- | The Python PyNN preample for import statements
pyNNPreample :: PyNNPreample -> String
pyNNPreample PyNNPreample {..} = unpack $ [text|
import addict
import numpy
import ${pack pyNNImport} as pynn

${pack instrumentation}
|]

pyNNNode :: Node -> String
pyNNNode node = case node of
  Population { .. } -> 
    let (_, code) = nodeVariable (node ^. id) $ pyNNPopulation _neuronType
    in  code
--   SpikeSourceArray { .. } -> 
--   SpikeSourcePoisson { .. } -> 

pyNNPopulation :: NeuronType -> String
pyNNPopulation neuronType = 
  let params = BS.unpack $ encode neuronType
  in  case neuronType of
        IFCondExp { .. } -> "pynn.IF_cond_exp(" ++ params ++ ")"

pyNNEdge :: Edge -> String
pyNNEdge Projection { .. } =
  let connector = pyNNConnector _projectionType
--  let dynamics = pyNNDynamics projectionDynamics
      nodeRef1 = nodeReference $ _input ^. id
      nodeRef2 = nodeReference $ _output ^. id
      code = "pynn.Projection(" ++ nodeRef1 ++", " ++ nodeRef2 ++ ", connector = " ++ connector ++ ")"
      (_, definition) = projectionVariable 0 code
  in  definition

pyNNConnector :: ProjectionType -> String
pyNNConnector (AllToAll _ _)  = "pynn.AllToAllConnector()"

-- pyNNDynamics :: ProjectionDynamics -> String
-- pyNNDynamics Static Excitatory = 

nodeReference :: Int -> String
nodeReference id = "node" ++ (show id)

nodeVariable :: Int -> String -> PythonDeclaration
nodeVariable id code = pythonVariable ("node" ++ (show id)) code

projectionVariable :: Int -> String -> PythonDeclaration
projectionVariable id code = pythonVariable ("proj" ++ (show id)) code

pythonVariable :: String -> String -> PythonDeclaration
pythonVariable name value = (name, name ++ " = " ++ value)

