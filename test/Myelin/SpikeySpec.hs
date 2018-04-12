module Myelin.SpikeySpec where

import Myelin.SNN
import Myelin.Spikey

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The Spikey evaluator should" $ do

    it "can compile to a python script" $ do
      python <- run exampleTask
      (length python) > 0 `shouldBe` True

    it "can convert a task to JSON" $ do
      let json = taskToJSON exampleTask
      (length json) > 0 `shouldBe` True
