{-# LANGUAGE QuasiQuotes #-}
module Myelin.PyNN.SpiNNakerSpec where

import Control.Lens
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Either
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import Numeric.LinearAlgebra
import Text.Regex as Regex

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
    it "can translate an entire SNN model to a SpiNNaker script" $ do
      let input = Population 2 if_cond_exp "p0" 0
      let hidden = Population 4 if_cond_exp "p1" 1
      let output = Population 3 if_cond_exp "p2" 2
      let dictWithType = unpack $ encode if_cond_exp
      let dict = Regex.subRegex typeRegex dictWithType ""
      let effect1 = Static Excitatory (AllToAll (BiasGenerator (Constant 0)) (WeightGenerator (Constant 1)))
      let effect2 = Static Excitatory (AllToAll (Biases [0.0, 1.1, 2.2]) (WeightGenerator (GaussianRandom 2 1)))
      let edges = [ DenseProjection effect1 input hidden, DenseProjection effect2 hidden output ]
      let network = Network 0 [input] [hidden] edges [output]
      let task = Task SpiNNaker network 50
      let preample = PyNNPreample "# some config"
      let code = [i|import numpy as np
import volrpynn.spiNNaker as v
import pyNN.spiNNaker as pynn

# some config

p0 = pynn.Population(2, pynn.IF_cond_exp(**#{dict}))
p1 = pynn.Population(4, pynn.IF_cond_exp(**#{dict}))
p2 = pynn.Population(3, pynn.IF_cond_exp(**#{dict}))
layer0 = v.Dense(p0, p1, weights=1.0, biases=0.0)
layer1 = v.Dense(p1, p2, weights=np.random.normal(2.0, 1.0, (4, 3)), biases=np.array([0.0,1.1,2.2]))
l_decode = v.Decode(p2)
model = v.Model(layer0, layer1, l_decode)

optimiser = v.GradientDescentOptimiser(0.1, simulation_time=50.0)
if __name__ == "__main__":
    v.Main(model).train(optimiser)
|]
      translate task preample `shouldBe` Right code