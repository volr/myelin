{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Myelin.SNN where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Lens hiding ((.=))
import Control.Monad

{-}
import Data.Data (Typeable, Data)
import GHC.Generics (Generic)
-}
import Data.Aeson
import Data.Monoid

type Label = String

data NeuronType =
    IFCurrentAlpha {
        tau_m :: Float,
        tau_refrac :: Float,
        v_thresh :: Float,
        tau_syn_E :: Float,
        v_rest :: Float,
        cm :: Float,
        v_reset :: Float,
        tau_syn_I :: Float,
        i_offset :: Float
    } deriving (Eq, Show) -- , Typeable, Data, Generic)

if_current_alpha_default :: NeuronType
if_current_alpha_default = IFCurrentAlpha { 
    tau_m = 0.2,
    tau_refrac =  0.3,
    v_thresh = -65.0,
    tau_syn_E =  0.3,
    v_rest = -70.0,
    cm = 1.0,
    v_reset = -75.0,
    tau_syn_I = 0.9,
    i_offset = 5
}

instance ToJSON NeuronType where
    toJSON IFCurrentAlpha {..} = object [
            "type" .= ("IFCurrentAlpha" :: String),
            "tau_m" .= tau_m,
            "tau_refrac" .= tau_refrac,
            "v_thresh" .= v_thresh,
            "tau_syn_E" .= tau_syn_E,
            "v_rest" .= v_rest,
            "cm" .= cm,
            "v_reset" .= v_reset,
            "tau_syn_I" .= tau_syn_I,
            "i_offset" .= i_offset
        ]
    toJSON Other = object [ "type" .= ("Other" :: String)]

instance FromJSON NeuronType where
    parseJSON = withObject "neuron" $ \o -> do
        typ :: String <- o .: "type"
        tau_m <- o .: "tau_m"
        tau_refrac <- o .: "tau_refrac"
        v_thresh <- o .: "v_thresh"
        tau_syn_E <- o .: "tau_syn_E"
        v_rest <- o .: "v_rest"
        cm <- o .: "cm"
        v_reset <- o .: "v_reset"
        tau_syn_I <- o .: "tau_syn_I"
        i_offset <- o .: "i_offset"
        if (typ == "IFCurrentAlpha") then return IFCurrentAlpha{..} else return Other


data Node = Population {
        _numNeurons :: Integer,
        _neuronType :: NeuronType,
        _label :: Label,
        _id :: Int
    }
    | Input {
        _fileName :: String,
        _id :: Int
    }
    | Output {
        _fileName :: String,
        _id :: Int
    }
    deriving (Eq, Show) -- , Typeable, Data, Generic)

instance ToJSON Node where
    toJSON Population {..} = object [
        "type" .= ("population" :: String),
        "num_neurons" .= _numNeurons,
        "neuron_type" .= toJSON _neuronType,
        "label" .= _label,
        "id" .= _id
        ]
    
    toJSON Input {..} = object [
        "type" .= ("input" :: String),
        "file_name" .= _fileName,
        "id" .= _id,
    ]

    toJSON Output {..} = object [
        "type" .= ("output" :: String)
        "file_name" .= _filenName
        "id" .= _id,
    ]


instance FromJSON Node where
    parseJSON = withObject "node" $ \o -> do
        typ :: String <- o .: "type"
        case typ of
            "population" ->
                Population <$>
                    o .: "num_neurons" <*>
                    o .: "neuron_type" <*>
                    o .: "label" <*>
                    o .: "id"
            "input" ->
                Input <$>
                    o .: "file_name" <*>
                    o .: "id"
            "output" ->
                Output <$>
                    o .: "file_name" <*>
                    o .: "id"

data ProjectionType =
      AllToAll
    | OneToOne
    deriving (Eq, Show)

data Edge = 
      Projection { 
          projection_type :: ProjectionType,
          input :: Node,
          output :: Node
      } deriving (Eq, Show)

     
instance ToJSON Edge where
    toJSON (Projection {..}) = withObject "edge" $ \o -> object [
        "type" .= ("projection" :: String),
        "projection_type" .= _projection_type,
        "input" .= _input,
        "output" .= _output
    ]

instance FromJSON Edge where
    parseJSON = withObject "edge" $ \o ->
        typ :: String <- o .: "type"
        case typ of
            "projection" -> 
                Projection <$>
                    o .: "projection_type" <*>
                    o .: "input" <*>
                    o .: "output"

-- Builder

data BlockState = BlockState {
    _nextId :: Int,
    _nodes :: [Node],
    _edges :: [Edge]
    } deriving (Eq, Show) -- , Typeable, Data, Generic)

makeLenses ''BlockState

type SNN a = StateT BlockState IO a

initialBlockState = BlockState 0 [] []

population :: Integer -> NeuronType -> String -> SNN Node
population i typ label = do
    l <- use nextId
    nextId += 1
    let pop = Population i typ label l
    nodes <>= [pop]
    return pop

projection :: ProjectionType -> Node -> Node -> SNN ()
projection proj p0 p1 = edges <>= [Projection proj p0 p1]

fileInput :: String -> SNN Node
fileInput filename = do
    i <- use nextId
    nextId += 1
    let input = Input filename i
    nodes <>= [input]
    return input

fileOutput :: String -> SNN Node
fileOutput filename = do
    i <- use nextId
    nextId += 1
    let output = Output filename i
    nodes <>= [output]
    return output

net :: SNN ()
net = do
    input <- fileInput "in.txt"
    output <- fileOutput "out.txt"
    a <- population 10 if_current_alpha_default "a"
    b <- population 20 if_current_alpha_default "b"
    c <- population 20 if_current_alpha_default "b"
    projection AllToAll input a
    projection AllToAll a b
    projection AllToAll b c
    projection AllToAll c a
    projection AllToAll c output

netTest = do
    b <- execStateT net initialBlockState
    print b
    return ()







