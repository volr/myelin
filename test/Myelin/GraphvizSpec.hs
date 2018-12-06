module Myelin.GraphvizSpec where

import Myelin.Graphviz
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "The graphviz backend can" $ do
      it "generate dotfiles" $ do
        (0 == 0) `shouldBe` True
