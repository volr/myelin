{-# LANGUAGE QuasiQuotes #-}
module Myelin.PyNN.PyNNSpec where

import Control.Lens
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString.Lazy.Char8
import qualified Data.Map.Strict as Map

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
      let expected = "node0 = pu.LearningNode(pynn.IF_cond_exp(" ++ dict ++ "))"
      let newModel = emptyPyNNModel & pyNNNodes .~ (Map.singleton "node0" expected)
      exec (pyNNNode pop) `shouldBe` newModel
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
      let expected = "proj0 = pynn.Projection(node0, node1, connector = pynn.AllToAllConnector())"
      exec (pyNNEdge edge) ^. pyNNProjections `shouldBe` (Map.singleton "proj0" expected)
    it "can connect edges to learning nodes with constant weights" $ do
      let effect = Static Excitatory (AllToAll (Constant 1.0))
      let nodeIn = SpikeSourcePoisson 0.1 0 0
      let nodeOut = Population 2 if_cond_exp "p1" 1 
      let edge = Projection effect nodeIn nodeOut
      let expected = ["proj0.set(weight=1.0)", "node0.connect(node1)"]
      exec (pyNNEdge edge) ^. declarations `shouldBe` expected 
    it "can connect edges to learning nodes with random weights" $ do
      let effect = Static Excitatory (AllToAll (GaussianRandom 1 2))
      let nodeIn = SpikeSourcePoisson 0.1 0 0
      let nodeOut = Population 2 if_cond_exp "p1" 1 
      let edge = Projection effect nodeIn nodeOut
      let expected = ["proj0.set(weight=numpy.random.normal(1.0, 2.0))", "node0.connect(node1)"]
      exec (pyNNEdge edge) ^. declarations `shouldBe` expected 
    it "can translate SNN nodes and edges to a PyNN model" $ do
      let input = Population 2 if_cond_exp "p1" 0
      let hidden = Population 4 if_cond_exp "p2" 1
      let output = Population 3 if_cond_exp "p3" 2
      let effect = Static Excitatory (AllToAll (Constant 1))
      let edges = [ Projection effect input hidden, Projection effect hidden output ]
      let network = Network 0 [input] [hidden] edges [output]
      let expectedDeclarations = ["proj0.set(weight=1.0)", "node0.connect(node1)", "proj1.set(weight=1.0)", "node1.connect(node2)", "pu.Model([node0],[node2])"]
      let model = exec (translate' network)
      model ^. declarations `shouldBe` expectedDeclarations
      Map.size (model ^. pyNNNodes) `shouldBe` 3
      Map.size (model ^. pyNNProjections) `shouldBe` 2
   -- it "can translate a whole SNN model to a Python script" $ do
   --   let input = Population 2 if_cond_exp "p3" 0
   --   let hidden = Population 4 if_cond_exp "p2" 1
   --   let output = Population 3 if_cond_exp "p3" 2
   --   let effect = Static Excitatory (AllToAll (Constant 1))
   --   let edges = [ Projection effect input hidden, Projection effect hidden output ]
   --   let network = Network 0 [input] [hidden] edges [output]
   --   exec (translate network) `shouldBe` expected
    --it "can translate a complete SNN model with preample" $ do
