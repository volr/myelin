{-# LANGUAGE OverloadedStrings #-}
module Myelin.DLS.PPU.AssemblerSpec where

import Myelin.DLS.PPU.Assembler
import Data.Word
import Data.Monoid
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder

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

str :: ByteString
str = toLazyByteString $ assembleInstructions asm


spec = do
    describe "PPU Assembler" $ do
        it "can assemble ppu instructions" $ do        
            bin `shouldBe` [42076191,1115885663]
        it "can assemble ppu instructions into bytestrings" $ do
            str `shouldBe` "addc 0, 1, 2\nadd 1, 2, 3\n"
