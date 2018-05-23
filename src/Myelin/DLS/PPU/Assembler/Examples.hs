{-# LANGUAGE OverloadedLists #-}
module Myelin.DLS.PPU.Assembler.Examples where

import Myelin.DLS.PPU.Assembler.Monad as M
import Myelin.DLS.PPU.Assembler as A

ex1 :: Monad m => Asm A.Register m
ex1 = do
    a <- allocateRegister
    b <- allocateRegister
    M.add a b

ex2 :: Monad m => Asm A.VectorRegister m
ex2 = do
    a <- allocateVectorRegister
    b <- allocateVectorRegister
    M.fxvaddbm a b A.Fxv_cond_null

ex :: Monad m => Asm () m
ex = do
    r0 <- allocateRegister
    r1 <- allocateRegister
    r2 <- allocateRegister
    r3 <- block [r0, r1] [] $ \[r1, r2] [] -> do
        return r1
    return ()

