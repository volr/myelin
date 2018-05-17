{-# LANGUAGE OverloadedLists #-}
module Myelin.DLS.PPU.Assembler.MonadSpec where

import Myelin.DLS.PPU.Assembler.Monad
import Myelin.DLS.PPU.Assembler as A
import Test.Hspec
import Control.Monad.Trans.State
import Control.Monad.Trans.Identity 
import Data.Functor.Identity

import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = hspec spec

spec = describe "PPU Assembler Monad" $ do
        it "allocates and deallocates registers" $
            test0 `shouldBe` (Set.delete A.R1 [A.R0 ..])
        it "allocates and deallocates vector registers" $
            test1 `shouldBe` (Set.delete A.VR1 [A.VR0 ..])
        it "allocates many vector registers" $
            test3 `shouldBe` [A.VR12 ..]
        it "passes trivial test" $       
            (0 == 0) `shouldBe` True
            
test0 = _freeRegisters finalAsmState
    where 
        (_, finalAsmState) = runState asm initialAsmState
        asm :: Asm () Identity
        asm = do 
                v <- allocateRegister
                v' <- allocateRegister
                releaseRegister v

test1 = _freeVectorRegisters finalAsmState
    where 
        (_, finalAsmState) = runState asm initialAsmState
        asm :: Asm () Identity
        asm = do 
                v <- allocateVectorRegister
                v' <- allocateVectorRegister
                releaseVectorRegister v

test3 = _freeVectorRegisters finalAsmState
    where 
        (_, finalAsmState) = runState asm initialAsmState
        asm :: Asm () Identity
        asm = do 
            allocateVectorRegisters 12
            return ()
