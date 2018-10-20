module Myelin.PyNN.PyNNSpec where

import Data.Aeson
import Data.ByteString.Lazy.Char8

import Myelin.Model
import Myelin.Neuron
import Myelin.PyNN.PyNN
import Myelin.SNN

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PyNN backend" $ do
    it "can translate an IF_exp_cond population to Python" $ do
      let tpe = if_cond_exp
      let dict = unpack $ encode tpe
      let pop = Population 2 tpe "p0" 0 True
      pyNNNode pop `shouldBe` "node0 = pynn.IF_cond_exp(" ++ dict ++ ")"
    it "can translate an edge to Python" $ do
      let tpe = AllToAll 1 False
      let target = Static Excitatory
      let nodeIn = SpikeSourcePoisson 0.1 0 0
      let nodeOut = Population 2 if_cond_exp "p1" 1 True
      let edge = Projection tpe target nodeIn nodeOut
      pyNNEdge edge `shouldBe` "proj0 = pynn.Projection(node0, node1, connector = pynn.AllToAllConnector())"
