{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Myelin.SNN where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Trans.Identity

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 as B
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Printing
import Data.Monoid
import Data.Text
import Data.Traversable

type Label = String

{--| 

NeuronType corresponds to the different point neuron
types that are constructible in pynn. Many of them
are back end specific.

Things that can be improved:

- Currently none of the parameters have any units,
  instead their units are indicated by their names

  - tau_* are time constants
  - v_* are voltages
  - i_* are currents
  - cm is a capacitance

- There is no restriction to "biological" ranges of
  parameters in place and no check of consistency

- In the case of the Heidelberg Hardware system additionally
  every neuron on the chip has analog parameter variation,
  and finite (much coarser than floating point) precision
  in parameter adjustment.

--}
data NeuronType =
    IFCurrentAlpha {
        tau_m :: Float, -- ^ Membrane time constant  
        tau_refrac :: Float, -- ^ Refractory time
        v_thresh :: Float, -- ^ Threshhold potential
        tau_syn_E :: Float, -- ^ Excitatory synaptic time constant
        v_rest :: Float, -- ^ Resting potential
        cm :: Float, -- ^ Membrane capactitance
        v_reset :: Float, -- ^ Reset potential 
        tau_syn_I :: Float, -- ^ Inhibitory synaptic time constant
        i_offset :: Float -- ^ Offset current
    }
    | IFSpikey {
        e_rev_I :: Float, -- ^ Excitatory reversal current
        g_leak :: Float, -- ^ Leak conductance
        tau_refrac :: Float, -- ^ Refractory time
        v_reset :: Float, -- ^ Reset potential
        v_rest :: Float, -- ^ Resting potential
        v_thresh :: Float -- ^ Threshhold potential
    }
    | IFCurrExp {
        cm :: Float,
        tau_m :: Float,
        tau_syn_E :: Float,
        tau_syn_I :: Float,
        tau_refrac :: Float,
        v_thresh :: Float,
        v_rest :: Float,
        v_reset :: Float,
        i_offset :: Float
    }
    | IFCondExp
      { v_rest :: Float
      , cm :: Float
      , tau_m :: Float
      , tau_refrac :: Float
      , tau_syn_E :: Float
      , tau_syn_I :: Float
      , e_rev_E :: Float
      , e_rev_I :: Float
      , v_thresh :: Float
      , v_reset :: Float
      , i_offset :: Float
      }
    deriving (Eq, Show) -- , Typeable, Data, Generic)

-- Defaults pulled from http://neuralensemble.org/docs/PyNN/standardmodels.html
if_cond_exp_default :: NeuronType
if_cond_exp_default = IFCondExp
  { v_rest = -65.0
  , cm = 1.0
  , tau_m = 20.0
  , tau_refrac = 0.0
  , tau_syn_E = 5.0
  , tau_syn_I = 5.0
  , e_rev_E = 0.0
  , e_rev_I = -70.0
  , v_thresh = -50.0
  , v_reset = -65.0
  , i_offset = 0.0
  }

if_current_alpha_default :: NeuronType
if_current_alpha_default = IFCurrentAlpha {
    tau_m = 20.0,
    tau_refrac = 0.0,
    v_thresh = -65.0,
    tau_syn_E =  5.0,
    tau_syn_I = 5.0,
    v_rest = -65.0,
    cm = 1.0,
    v_reset = -65.0,
    i_offset = 0.0
}

if_spikey_default :: NeuronType
if_spikey_default = IFSpikey {
    e_rev_I = -80.0,
    g_leak = 20.0,
    tau_refrac = 1.0,
    v_reset = -80.0,
    v_rest = -75.0,
    v_thresh = -55.0
}

if_current_exponential_default :: NeuronType
if_current_exponential_default = IFCurrExp {
    cm = 1.0,
    tau_m = 10.0,
    tau_syn_E = 5.0,
    tau_syn_I = 5.0,
    tau_refrac = 2.0,
    v_thresh = -50.0,
    v_rest = -65.0,
    v_reset = 0.0,
    i_offset = 0.0
}

instance ToJSON NeuronType where
    toJSON IFCondExp {..} = object [
            "type" .= ("IFCondExp" :: String),
            "tau_m" .= tau_m,
            "tau_refrac" .= tau_refrac,
            "v_thresh" .= v_thresh,
            "tau_syn_E" .= tau_syn_E,
            "v_rest" .= v_rest,
            "cm" .= cm,
            "v_reset" .= v_reset,
            "tau_syn_I" .= tau_syn_I,
            "i_offset" .= i_offset,
            "e_rev_E" .= e_rev_E,
            "e_rev_I" .= e_rev_I
        ]
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
    toJSON IFSpikey {..} = object [
            "type" .= ("IFSpikey" :: String),
            "e_rev_I" .= e_rev_I,
            "g_leak" .= g_leak,
            "tau_refrac" .= tau_refrac,
            "v_reset" .= v_reset,
            "v_rest" .= v_rest,
            "v_thresh" .= v_thresh
        ]
    toJSON IFCurrExp {..} = object [
            "type" .= ("IFCurrentExp" :: String),
            "cm" .= cm,
            "tau_m" .= tau_m,
            "tau_syn_E" .= tau_syn_E,
            "tau_syn_I" .= tau_syn_I,
            "tau_refrac" .= tau_refrac,
            "v_thresh" .= v_thresh,
            "v_rest" .= v_rest,
            "v_reset" .= v_reset,
            "i_offset" .= i_offset
        ]

instance FromJSON NeuronType where
    parseJSON = withObject "neuron" $ \o -> do
        typ :: String <- o .: "type"
        case typ of
            "IFCondExp" -> IFCondExp <$>
                o .: "v_rest" <*>
                o .: "cm" <*>
                o .: "tau_m" <*>
                o .: "tau_refrac" <*>
                o .: "tau_syn_E" <*>
                o .: "tau_syn_I" <*>
                o .: "e_rev_E" <*>
                o .: "e_rev_I" <*>
                o .: "v_thresh" <*>
                o .: "v_reset" <*>
                o .: "i_offset"
            "IFCurrentAlpha" -> IFCurrentAlpha <$>
                o .: "tau_m" <*>
                o .: "tau_refrac" <*>
                o .: "v_thresh" <*>
                o .: "tau_syn_E" <*>
                o .: "v_rest" <*>
                o .: "cm" <*>
                o .: "v_reset" <*>
                o .: "tau_syn_I" <*>
                o .: "i_offset"
            "IFSpikey" -> IFSpikey <$>
                o .: "e_rev_I" <*>
                o .: "g_leak" <*>
                o .: "tau_refrac" <*>
                o .: "v_reset" <*>
                o .: "v_rest" <*>
                o .: "v_thresh"
            "IFCurrExp" -> IFCurrExp <$>
                o .: "cm" <*>
                o .:"tau_m" <*>
                o .: "tau_syn_E" <*>
                o .: "tau_syn_I" <*>
                o .: "tau_refrac" <*>
                o .: "v_thresh" <*>
                o .: "v_rest" <*>
                o .: "v_reset" <*>
                o .: "i_offset"
{-- | 

Nodes correspond to the 

--}
data Node = Population {
        _numNeurons :: Integer,
        _neuronType :: NeuronType,
        _label :: Label,
        _id :: Int,
        _record_spikes :: Bool
    }
    | Input {
        _fileName :: String,
        _id :: Int
    }
    | Output {
        _fileName :: String,
        _id :: Int
    }
    | SpikeSourceArray {
        _spikeTimes :: [Float],
        _id :: Int
    }
    | SpikeSourcePoisson {
        _rate :: Float,
        _start :: Integer,
        _id :: Int
    }
    deriving (Eq, Show) -- , Typeable, Data, Generic)

instance ToJSON Node where
    toJSON Population{..} = object [
        "type" .= ("population" :: String),
        "num_neurons" .= _numNeurons,
        "neuron_type" .= _neuronType,
        "label" .= _label,
        "id" .= _id,
        "record_spikes" .= _record_spikes
        ]
    toJSON Input{..} = object [
            "type" .= ("input" :: String),
            "file_name" .= _fileName,
            "id" .= _id
        ]
    toJSON Output{..} = object [
            "type" .= ("output" :: String),
            "file_name" .= _fileName,
            "id" .= _id
        ]
    toJSON SpikeSourceArray{..} = object [
            "type" .= ("spike_source_array" :: String),
            "spike_times" .= _spikeTimes,
            "id" .= _id
        ]
    toJSON SpikeSourcePoisson{..} = object [
            "type" .= ("spike_source_poisson" :: String),
            "rate" .= _rate,
            "start" .= _start,
            "id" .= _id
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
                    o .: "id" <*>
                    o .: "record_spikes"
            "input" ->
                Input <$>
                    o .: "file_name" <*>
                    o .: "id"
            "output" ->
                Output <$>
                    o .: "file_name" <*>
                    o .: "id"
            "spike_source_poisson" ->
                SpikeSourcePoisson <$>
                    o .: "rate" <*>
                    o .: "start" <*>
                    o .: "id"
            "spike_source_array" ->
                SpikeSourceArray <$>
                    o .: "spike_times" <*>
                    o .: "id"

type Weight = Float -- TODO: This is platform specific

{--

ProjectionType corresponds to the different connectors available
in pynn. Some of the connectors are backend specific.

AllToAll,
FixedProbability,
DistanceDependentProbability,
FixedNumberPre,
FixedNumberPost,
OneToOne,
SmallWorld,
FromList,
FromFile

Not all have them been defined below. In addition there
are backend specific connectors. 

--}
data ProjectionType =
    AllToAll {
        _weight :: Weight,
        _allow_self_connections :: Bool
    }
    | OneToOne {
        _weight :: Weight
    }
    | FixedNumberPost {
        _n :: Int,
        _weight :: Weight,
        _allow_self_connections :: Bool
    }
    | FixedNumberPre {
        _n :: Int,
        _weight :: Weight,
        _allow_self_connections :: Bool
    }
    | FromList {
        _weights :: [(Int, Int, Weight)]
    }
    deriving (Eq, Show)

instance FromJSON ProjectionType where
    parseJSON = withObject "projection_type" $ \o -> do
        kind :: String <- o .: "kind"
        case kind of
            "all_to_all" -> AllToAll <$> o .: "weight" <*> o .: "allow_self_connections"
            "one_to_one" -> OneToOne <$> o .: "weight"
            "fixed_number_pre" -> FixedNumberPre <$> o .: "n" <*> o .: "weight" <*> o .: "allow_self_connections"
            "fixed_number_post" -> FixedNumberPost <$> o .: "n" <*> o .: "weight" <*> o .: "allow_self_connections"
            "from_list" -> FromList <$> o .: "weights"

instance ToJSON ProjectionType where
    toJSON AllToAll{..} = object [
            "kind" .= ("all_to_all" :: String),
            "weight" .= _weight,
            "allow_self_connections" .= _allow_self_connections
        ]
    toJSON OneToOne{..} = object [
            "kind" .= ("one_to_one" :: String),
            "weight" .= _weight
        ]
    toJSON FixedNumberPost{..} = object [
            "kind" .= ("fixed_number_post" :: String),
            "n" .= _n,
            "weight" .= _weight,
            "allow_self_connections" .= _allow_self_connections
        ]
    toJSON FixedNumberPre{..} = object [
            "kind" .= ("fixed_number_pre" :: String),
            "n"  .= _n,
            "weight" .= _weight,
            "allow_self_connections" .= _allow_self_connections
        ]
    toJSON FromList{..} = object [
            "kind" .= ("from_list" :: String),
            "weights" .= _weights
        ]

data SynapseEffect = Inhibitory | Excitatory deriving (Eq, Show)

data ProjectionTarget =
    Static SynapseEffect
    deriving (Eq, Show)

instance ToJSON ProjectionTarget where
    toJSON (Static Excitatory) = object [
            "kind" .= ("static" :: String),
            "effect" .= ("excitatory" :: String)
        ]
    toJSON (Static Inhibitory) = object [
            "kind" .= ("static" :: String),
            "effect" .= ("inhibitory" :: String)
        ]

instance FromJSON ProjectionTarget where
    parseJSON = withObject "projection_target" $ \o -> do
            kind :: String <- o .: "kind"
            effect :: String <- o .: "effect"
            case effect of
                "excitatory" -> return (Static Excitatory)
                "inhibitory" -> return (Static Inhibitory)

data Edge =
      Projection {
          _projectionType :: ProjectionType,
          _projectionTarget :: ProjectionTarget,
          _input :: Node,
          _output :: Node
      } deriving (Eq, Show)

instance ToJSON Edge where
    toJSON Projection {..} = object [
                "type" .= ("projection" :: String),
                "projection_type" .= _projectionType,
                "projection_target" .= _projectionTarget,
                "input" .= _input,
                "output" .= _output
            ]

instance FromJSON Edge where
    parseJSON = withObject "edge" $ \o -> do
        typ :: String <- o .: "type"
        case typ of
            "projection" ->
                Projection <$>
                    o .: "projection_type" <*>
                    o .: "projection_target" <*>
                    o .: "input" <*>
                    o .: "output"
-- Builder

data ExecutionTarget =
    Nest {
        _minTimestep :: Float,
        _maxTimestep :: Float
    }
    | BrainScaleS {
        _wafer :: Int,
        _hicann :: Int
    }
    | Spikey {
        _mappingOffset :: Int -- 0..192 (really only 0 and 192 are sensible)
    }
    | SpiNNaker
    deriving (Eq, Show)

instance ToJSON ExecutionTarget where
    toJSON Nest{..} = object [ "kind" .= ("nest" :: String)]
    toJSON BrainScaleS{..} = object
      [ "kind" .= ("brainscales" :: String)
      , "wafer" .= _wafer
      , "hicann" .= _hicann
      ]

instance FromJSON ExecutionTarget where
    parseJSON = withObject "execution_target" $ \o -> do
        kind :: String <- o .: "kind"
        case kind of
            "brainscales" -> BrainScaleS <$>
                        o .: "wafer" <*>
                        o .: "hicann"
            "nest" -> Nest <$>
                        o .: "min_timestep" <*>
                        o .: "max_timestep"
            _ -> error "target not supported yet"

{--
An execution task specifies all informationn needed to execute a SNN 
on a specific target.
--}
data Task = Task {
    _executionTarget :: ExecutionTarget, -- ^ Which neuromorphic hardware or simulator to run on.
    _network :: Network, -- ^ Network that should be implemented.
    _simulationTime :: Double -- TODO: Might be simulator specific, also has no unit
} deriving (Eq, Show)

instance ToJSON Task where
    toJSON Task {..} = object [
            "execution_target" .= _executionTarget,
            "network" .= _network,
            "simulation_time" .= _simulationTime
        ]

instance FromJSON Task where
    parseJSON = withObject "task" $ \o ->
        Task <$> o .: "execution_target" <*>
                 o .: "network" <*>
                 o .: "simulation_time"

data Network = Network {
    _blocks :: [BlockState]
} deriving (Eq, Show)

instance ToJSON Network where
    toJSON Network {..} = object [
            "blocks" .= _blocks
        ]

instance FromJSON Network where
    parseJSON = withObject "network" $ \o ->
        Network <$> o .: "blocks"

data BlockState = BlockState {
    _nextId :: Int,
    _inputs :: [Node],
    _nodes :: [Node],
    _edges :: [Edge],
    _outputs :: [Node]
} deriving (Eq, Show) -- , Typeable, Data, Generic)

instance ToJSON BlockState where
    toJSON BlockState {..} = object [
            "next_id" .= _nextId,
            "nodes" .= _nodes,
            "edges" .= _edges,
            "inputs" .= _inputs,
            "outputs" .= _outputs
        ]

instance FromJSON BlockState where
    parseJSON = withObject "block_state" $ \o ->
        BlockState <$>
        o .: "next_id" <*>
        o .: "nodes" <*>
        o .: "edges" <*>
        o .: "inputs" <*>
        o .: "outputs"

makeLenses ''BlockState

type SNN a m = StateT BlockState m a

initialBlockState = BlockState 0 [] [] [] []

newId :: Monad m => SNN Int m
newId = do
    l <- use nextId
    nextId += 1
    return l

spikeSourceArray :: Monad m => [Float] -> SNN Node m
spikeSourceArray spikeTimes = do
    id <- newId
    let spikeSource = SpikeSourceArray spikeTimes id
    nodes <>= [spikeSource]
    return spikeSource

spikeSourcePoisson :: Monad m => Float -> Integer -> SNN Node m
spikeSourcePoisson rate start = do
    id <- newId
    let spikeSource = SpikeSourcePoisson rate start id
    nodes <>= [spikeSource]
    return spikeSource

population :: Monad m => Integer -> NeuronType -> String -> Bool -> SNN Node m
population i typ label recordSpikes = do
    l <- newId
    let pop = Population i typ label l recordSpikes
    nodes <>= [pop]
    return pop

projection :: Monad m => ProjectionType -> ProjectionTarget -> Node -> Node -> SNN () m
projection proj target p0 p1 = edges <>= [Projection proj target p0 p1]

fileInput :: Monad m => String -> SNN Node m
fileInput filename = do
    i <- newId
    let input = Input filename i
    nodes <>= [input]
    return input

fileOutput :: Monad m => String -> SNN Node m
fileOutput filename = do
    i <- newId
    let output = Output filename i
    nodes <>= [output]
    return output

-- Example API use


net :: Monad m => SNN () m
net = do
    input <- spikeSourceArray [1, 2, 3, 5]
    a <- population 10 if_current_exponential_default "a" False
    b <- population 20 if_current_exponential_default "b" False -- TODO: Labels should be checked for doublication
    c <- population 20 if_current_exponential_default "c" False
    -- TODO: This is kind of ugly now
    projection (AllToAll 1.0 False) (Static Excitatory) input a
    projection (AllToAll 1.0 False) (Static Excitatory) a b
    projection (AllToAll 1.0 False) (Static Excitatory) b c
    projection (AllToAll 1.0 False) (Static Excitatory) c a
    -- TODO: inhibitory projection target is not really meaningful
    -- for output/input
    output <- fileOutput "out.txt"
    projection (AllToAll 1.0 False) (Static Inhibitory) c output

netTest :: SNN () Identity
netTest = net

toTask :: SNN () Identity -> ExecutionTarget -> Double -> Task
toTask model target runtime =
  Task {
      _executionTarget = target,
      _simulationTime = runtime,
      _network = Network {
          _blocks = [block]
      }
  }
  where (_, block) = runState model initialBlockState

taskToJSON :: Task -> String
taskToJSON = B.unpack . encodePretty . toJSON

exampleTask =
    Task {
        _executionTarget = Nest 0.1 0.5,
        _simulationTime = 100.0,
        _network = Network {
            _blocks = [block]
        }
    }
    where (_, block) = runState netTest initialBlockState
