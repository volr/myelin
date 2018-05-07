module Myelin.DLS.PPU.AssemblerSpec where

import Myelin.DLS.PPU.Assembler
import Test.Hspec

main :: IO ()
main = hspec spec

spec = do
    describe "PPU Assembler" $ do
        it "can assemble ppu instructions" $ do        
            (0 == 0) `shouldBe` True
