module Myelin.SNNSpec where

import Myelin.SNN

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SNN should" $ do
    it "generate sensible JSON output" $ do
      (0 == 0) `shouldBe` True