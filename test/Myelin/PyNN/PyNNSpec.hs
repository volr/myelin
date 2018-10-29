{-# LANGUAGE QuasiQuotes #-}
module Myelin.PyNN.PyNNSpec where

import Control.Lens
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString.Lazy.Char8
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
      let expected = "node0"
      eval (pyNNNode pop) `shouldBe` Right expected 
    it "can translate an IF_exp_cond population to Python" $ do
      let tpe = if_cond_exp
      let dict = unpack $ encode tpe
      let pop = Population 2 tpe "p0" 0
      let expectedPopulation = "p0 = pynn.Population(2, pynn.IF_Cond_Exp(" ++ dict ++ "))"
      let expectedLearning = "node0 = pm.LearningNode(p0)"
      let actualModel = exec (pyNNNode pop) 
      actualModel ^. pyNNPopulations `shouldBe` Map.singleton "p0" expectedPopulation
      actualModel ^. declarations `shouldBe` [expectedLearning]
    it "can return the reference for an edge" $ do
      let effect = Static Excitatory (AllToAll (Constant 1.0))
      let nodeIn = SpikeSourcePoisson 0.1 0 0
      let nodeOut = Population 2 if_cond_exp "p1" 1
      let edge = Projection effect nodeIn nodeOut
      let expected = "proj0"
      eval (pyNNEdge edge) `shouldBe` Right expected  
    it "can translate an edge to Python" $ do
      let effect = Static Excitatory (AllToAll (Constant 1.0))
      let nodeIn = SpikeSourcePoisson 0.1 0 0
      let nodeOut = Population 2 if_cond_exp "p1" 1 
      let edge = Projection effect nodeIn nodeOut
      let expected = "proj0 = pynn.Projection(p0, p1, connector = pynn.AllToAllConnector())"
      exec (pyNNEdge edge) ^. pyNNProjections `shouldBe` (Map.singleton "proj0" expected)
    it "can connect edges to learning nodes with constant weights" $ do
      let effect = Static Excitatory (AllToAll (Constant 1.0))
      let nodeIn = SpikeSourcePoisson 0.1 0 0
      let nodeOut = Population 2 if_cond_exp "p1" 1 
      let edge = Projection effect nodeIn nodeOut
      let expected = ["proj0.set(weight=1.0)", "node0.connect_to(node1)"]
      exec (pyNNEdge edge) ^. declarations `shouldBe` expected 
    it "can connect edges to learning nodes with random weights" $ do
      let effect = Static Excitatory (AllToAll (GaussianRandom 1 2))
      let nodeIn = SpikeSourcePoisson 0.1 0 0
      let nodeOut = Population 2 if_cond_exp "p1" 1 
      let edge = Projection effect nodeIn nodeOut
      let expected = ["proj0.set(weight=numpy.random.normal(1.0, 2.0))", "node0.connect_to(node1)"]
      exec (pyNNEdge edge) ^. declarations `shouldBe` expected 
    it "can translate SNN nodes and edges to a PyNN model" $ do
      let input = Population 2 if_cond_exp "p1" 0
      let hidden = Population 4 if_cond_exp "p2" 1
      let output = Population 3 if_cond_exp "p3" 2
      let effect = Static Excitatory (AllToAll (Constant 1))
      let edges = [ Projection effect input hidden, Projection effect hidden output ]
      let network = Network 0 [input] [hidden] edges [output]
      let expectedDeclarations = ["node0 = pm.LearningNode(p0)", "node1 = pm.LearningNode(p1)", "node2 = pm.LearningNode(p2)", "proj0.set(weight=1.0)", "node0.connect_to(node1)", "proj1.set(weight=1.0)", "node1.connect_to(node2)", "model = pm.Model([node0],[node2])"]
      let model = exec (translate' network)
      model ^. declarations `shouldBe` expectedDeclarations
      Map.size (model ^. pyNNPopulations) `shouldBe` 3
      Map.size (model ^. pyNNProjections) `shouldBe` 2
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
import pynn_utils as pu
import pynn_model as pm

# some config

p0 = pynn.Population(2, pynn.IF_Cond_Exp(#{dict}))
p1 = pynn.Population(4, pynn.IF_Cond_Exp(#{dict}))
p2 = pynn.Population(3, pynn.IF_Cond_Exp(#{dict}))
proj0 = pynn.Projection(p0, p1, connector = pynn.AllToAllConnector())
proj1 = pynn.Projection(p1, p2, connector = pynn.AllToAllConnector())
node0 = pm.LearningNode(p0)
node1 = pm.LearningNode(p1)
node2 = pm.LearningNode(p2)
proj0.set(weight=1.0)
node0.connect_to(node1)
proj1.set(weight=numpy.random.normal(2.0, 1.0))
node1.connect_to(node2)
model = pm.Model([node0],[node2])
|]
      translate network preample `shouldBe` Right code
