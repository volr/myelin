{-# LANGUAGE QuasiQuotes #-}
module Myelin.PyNN.PyNNSpec where

import Control.Lens
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Either
import qualified Data.Map.Strict as Map
import Data.String.Interpolate

import Myelin.Model
import Myelin.Neuron
import Myelin.PyNN.PyNN
import Myelin.SNN

import Test.Hspec

main :: IO ()
main = hspec spec

eval :: PyNNState a -> Either String a
eval s = evalState (runExceptT s) emptyPyNNModel

exec :: PyNNState a -> PyNNModel
exec s = execState (runExceptT s) emptyPyNNModel

spec :: Spec
spec = do
  describe "PyNN backend" $ do
    it "can return the reference for a node" $ do
      let tpe = if_cond_exp
      let pop = Population 2 tpe "p0" 0 
      let expected = "p0"
      eval (pyNNNode pop) `shouldBe` Right expected 
    it "can translate an IF_exp_cond population to Python" $ do
      let tpe = if_cond_exp
      let dict = unpack $ encode tpe
      let pop = Population 2 tpe "p0" 0
      let expectedPopulation = "p0 = pynn.Population(2, pynn.IF_Cond_Exp(**" ++ dict ++ "))"
      let actualModel = exec (pyNNNode pop) 
      actualModel ^. populations `shouldBe` Map.singleton "p0" expectedPopulation
    it "can return the reference for a layer" $ do
      let effect = Static Excitatory (AllToAll (Constant 1.0))
      let nodeIn = SpikeSourcePoisson 0.1 0 0
      let nodeOut = Population 2 if_cond_exp "p1" 1
      let edge = Projection effect nodeIn nodeOut
      let expected = "layer0"
      eval (pyNNEdge edge) `shouldBe` Right expected  
    it "can translate an edge to Python" $ do
      let effect = Static Excitatory (AllToAll (Constant 1.0))
      let nodeIn = SpikeSourcePoisson 0.1 0 0
      let nodeOut = Population 2 if_cond_exp "p1" 1 
      let edge = Projection effect nodeIn nodeOut
      let expected = "layer0 = v.Dense(pynn, p0, p1, weights = 1.0)"
      exec (pyNNEdge edge) ^. layers `shouldBe` (Map.singleton "layer0" expected)
    it "can connect edges to learning nodes with constant weights" $ do
      let effect = Static Excitatory (AllToAll (Constant 12.0))
      let nodeIn = SpikeSourcePoisson 0.1 0 0
      let nodeOut = Population 2 if_cond_exp "p1" 1 
      let edge = Projection effect nodeIn nodeOut
      let expected = "layer0 = v.Dense(pynn, p0, p1, weights = 12.0)"
      exec (pyNNEdge edge) ^. layers `shouldBe` Map.singleton "layer0" expected 
    it "can correctly translate two nodes into a dictionary" $ do
      let node1 = Population 1 if_cond_exp "p0" 0
      let node2 = Population 2 if_cond_exp "p1" 1 
      let network = Network 0 [node1] [node2] [] []
      let code1 = "p0 = " ++ (fromRight "" (pyNNPopulationString if_cond_exp 1 0))
      let code2 = "p1 = " ++ (fromRight "" (pyNNPopulationString if_cond_exp 2 1))
      let expected = Map.fromList [("p0", code1), ("p1", code2)]
      exec (translate' network) ^. populations `shouldBe` expected
    it "can create a model with two inputs and two outputs" $ do
      let effect1 = Static Excitatory (AllToAll (GaussianRandom 1 2))
      let effect2 = Static Excitatory (AllToAll (Constant 1.0))
      let nodeIn1 = Population 1 if_cond_exp "p0" 0
      let nodeIn2 = Population 2 if_cond_exp "p1" 1
      let nodeOut1 = Population 3 if_cond_exp "p2" 2
      let nodeOut2 = Population 4 if_cond_exp "p3" 3
      let edge1 = Projection effect1 nodeIn1 nodeOut1
      let edge2 = Projection effect2 nodeIn2 nodeOut2
      let expectedDeclarations = ["model = v.Model(pynn, layer0, layer1)"]
      let network = Network 0 [nodeIn1, nodeIn2] [] [edge1, edge2] [nodeOut1, nodeOut2]
      let model = exec (translate' network)
      model ^. declarations `shouldBe` expectedDeclarations
    it "can translate SNN nodes and edges to a PyNN model" $ do
      let input = Population 2 if_cond_exp "p1" 0
      let hidden = Population 4 if_cond_exp "p2" 1
      let output = Population 3 if_cond_exp "p3" 2
      let effect = Static Excitatory (AllToAll (Constant 1))
      let edges = [ Projection effect input hidden, Projection effect hidden output ]
      let network = Network 0 [input] [hidden] edges [output]
      let expectedDeclarations = ["model = v.Model(pynn, layer0, layer1)"]
      let model = exec (translate' network)
      model ^. declarations `shouldBe` expectedDeclarations
      Map.size (model ^. populations) `shouldBe` 3
      Map.size (model ^. layers) `shouldBe` 2
    it "can translate a whole SNN model to a Python script" $ do
      let input = Population 2 if_cond_exp "p0" 0
      let hidden = Population 4 if_cond_exp "p1" 1
      let output = Population 3 if_cond_exp "p2" 2
      let dict = unpack $ encode if_cond_exp
      let effect1 = Static Excitatory (AllToAll (Constant 1))
      let effect2 = Static Excitatory (AllToAll (GaussianRandom 2 1))
      let edges = [ Projection effect1 input hidden, Projection effect2 hidden output ]
      let network = Network 0 [input] [hidden] edges [output]
      let preample = PyNNPreample "pyNN.nest" "# some config"
      let code = [i|import numpy
import pyNN.nest as pynn
import volrpynn as v

# some config

p0 = pynn.Population(2, pynn.IF_Cond_Exp(**#{dict}))
p1 = pynn.Population(4, pynn.IF_Cond_Exp(**#{dict}))
p2 = pynn.Population(3, pynn.IF_Cond_Exp(**#{dict}))
layer0 = v.Dense(pynn, p0, p1, weights = 1.0)
layer1 = v.Dense(pynn, p1, p2, weights = numpy.random.normal(2.0, 1.0))
model = v.Model(pynn, layer0, layer1)
|]
      translate network preample `shouldBe` Right code
