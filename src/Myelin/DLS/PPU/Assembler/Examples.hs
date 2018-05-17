{-# LANGUAGE OverloadedLists #-}
module Myelin.DLS.PPU.Assembler.Examples where

import Myelin.DLS.PPU.Assembler.Monad as M
import Myelin.DLS.PPU.Assembler as A

ex1 :: Monad m => Asm A.Register m
ex1 = do
    a <- allocateRegister
    b <- allocateRegister
    M.add a b False False

ex2 :: Monad m => Asm A.VectorRegister m
ex2 = do
    a <- allocateVectorRegister
    b <- allocateVectorRegister
    M.fxvaddbm a b A.Fxv_cond_null

writeWeights weight_base index w = do
    M.fxvshb_ w (-1) A.Fxv_cond_null
    M.fxvoutx w weight_base index

readWeights weight_base index = do
    w <- M.fxvinx weight_base index
    M.fxvshb_ w 1 A.Fxv_cond_null
    return w

-- Operations on correlation measurements
readCorrelation base row ca_offset = do
    co <- M.fxvinx base row
    M.fxvshb_ co (-1) A.Fxv_cond_null
    M.fxvsubbfs_ co ca_offset A.Fxv_cond_null
    return co

readAntiCorrelation base row ac_offset = do
    ac <- M.fxvinx base row
    M.fxvshb_ ac (-1) A.Fxv_cond_null
    M.fxvsubbfs_ ac ac_offset A.Fxv_cond_null
    return ac

resetCorrelation :: Monad m => A.VectorRegister -> A.Register -> A.Register -> Asm () m
resetCorrelation = M.fxvoutx

readSynapseCorrelations select ca_base ac_base row ca_offset ac_offset = do
    co <- readCorrelation ca_base row ca_offset
    ac <- readAntiCorrelation ac_base row ac_offset
    resetCorrelation select ca_base row
    return (co,ac)

-- ^ This example show how one would implement an STDP update rule using the
--   monadic assembler api. 
stdpRaw select ca_base ac_base weight_base ca_offset ac_offset factors decay_factors zeros index = do
    (co, ac) <- readSynapseCorrelations select ca_base ac_base index ca_offset ac_offset
    sum <- M.fxvaddbfs co ac A.Fxv_cond_null
    dw <- M.fxvmulbfs sum factors A.Fxv_cond_null
    w <- readWeights weight_base index
    -- apply weight decay
    decay <- M.fxvmulbfs w decay_factors A.Fxv_cond_null
    M.fxvsubbfs_ w decay A.Fxv_cond_null
    -- stdp update
    M.fxvaddbfs_ w dw A.Fxv_cond_null
    r <- M.fxvsel w zeros A.Fxv_cond_gt
    -- save shifted weights
    writeWeights weight_base index r
    return ()

ex :: Monad m => Asm () m
ex = do
    r0 <- allocateRegister
    r1 <- allocateRegister
    r2 <- allocateRegister
    r3 <- block [r0, r1] [] $ \[r1, r2] [] -> do
        return r1
    return ()

