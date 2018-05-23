module Myelin.DLS.PPU.AssemblerSpec where

import Myelin.DLS.PPU.Assembler
import Data.Word
import Data.Monoid

import Test.Hspec

main :: IO ()
main = hspec spec


asm :: [Inst]
asm =
    [
      addc R0 R1 R2,
      add R1 R2 R3
    ]

bin :: [Word32]
bin = map encode asm

spec = do
    describe "PPU Assembler" $ do
        it "can assemble ppu instructions" $ do        
            bin `shouldBe` [42076191,1115885663]
