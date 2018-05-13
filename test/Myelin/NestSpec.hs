{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Myelin.NestSpec where

import Myelin.SNN
import Myelin.NEST
import Data.Foldable
import Data.ByteString.Lazy (ByteString)

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The Nest evaluator should" $ do
    --it "compile to a python script" $ do
    --  python :: ByteString <- run exampleTask
    --  python `shouldBe` ""
    it "convert a task to JSON" $ do
      let json = taskToJSON exampleTask
      (length json) > 0 `shouldBe` True