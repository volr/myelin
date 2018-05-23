{-# LANGUAGE OverloadedLists #-}
module Myelin.DLS.PPU.Plasticity where

import Control.Monad
import Data.Word

import Myelin.DLS.PPU.Assembler as A
import Myelin.DLS.PPU.Assembler.Monad as M

-- ^ write synaptic  weights in the specified row
writeWeights :: Monad m => 
    A.Register -- ^ register holding the base address of the synapse array weight store
    -> A.Register  -- ^ register holding the row to be accessed
    -> A.VectorRegister  -- ^ weight vector to store
    -> Asm () m
writeWeights weight_base index w = do
    M.fxvshb_ w (-1) A.Fxv_cond_null
    M.fxvoutx w weight_base index

-- ^ read the weights of the specified row
readWeights :: Monad m =>
    A.Register -- ^ register holding the base address of the synapse array weight store
    -> A.Register -- ^ register holding the row to be accessed
    -> Asm A.VectorRegister m
readWeights weight_base row = do
    w <- M.fxvinx weight_base row
    M.fxvshb_ w 1 A.Fxv_cond_null
    return w

-- ^ read the correlation for the specified row
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

-- ^ implementation of one row update stdp using the monadic assembler api
stdpRow select ca_base ac_base weight_base ca_offset ac_offset factors decay_factors zeros index = do
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

-- ^ apply stdp update (hardcoded number of rows)
stdpUpdate :: Monad m => 
       A.VectorRegister 
    -> A.Register 
    -> A.Register 
    -> A.Register 
    -> A.VectorRegister
    -> A.VectorRegister
    -> A.VectorRegister
    -> A.VectorRegister
    -> A.VectorRegister
    -> Asm () m
stdpUpdate select ca_base ac_base weight_base ca_offset ac_offset factors decay_factors zeros = do
    row <- M.allocateRegister
    forM_ ([0..31] :: [Word32]) $ \index -> do
        M.xor_ row row False -- zero row register
        M.addi row index -- load immediate
        stdpRow select ca_base ac_base weight_base ca_offset ac_offset factors decay_factors zeros row
        retainVectorRegisters [select, factors, decay_factors, zeros]

