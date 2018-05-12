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

readCorrelation = undefined
readAntiCorrelation = undefined

writeWeights = undefined
readWeights = undefined

stdp row co_offset ac_offset = do
    co <- (readCorrelation row) - co_offset
    ac <- (readAntiCorrelation row) - ac_offset
    return ()
    -- dw <- scale * (co + ac)
    -- w <- readWeights row
    -- writeWeights row (w + dw)


{--
    // Load causal and acausal measurement
    "fxvinx 1, %[ca_base], %[index]\n"
    "fxvinx 2, %[ac_base], %[index]\n"
    "fxvshb 1, 1, -1\n"
    "fxvshb 2, 2, -1\n"
    // Reset correlation measurement
    "fxvoutx %[select], %[ca_base], %[index]\n"
    // Subtract offsets
    "fxvsubbfs 1, 1, %[ca_offset]\n"
    "fxvsubbfs 2, 2, %[ac_offset]\n"
    // Add arg1 and arg2 and scale by factor
    "fxvaddbfs 3, 1, 2\n"
    "fxvmulbfs 3, 3, %[factors]\n"
    // Load the shifted weights
    "fxvinx 4, %[w_base], %[index]\n"
    "fxvshb 4, 4, 1\n"
    // Apply decay
    "fxvmulbfs 5, 4, %[decay_factors]\n"
    "fxvsubbfs 4, 4, 5\n"
    // Add stdp update to the weights
    "fxvaddbfs 4, 4, 3\n"
    // Set to zero if the result is smaller than 0
    "fxvcmpb 4\n"
    "fxvsel 4, 4, %[zeros], 2\n"
    // Save shifted weights
    "fxvshb 4, 4, -1\n"
    "fxvoutx 4, %[w_base], %[index]"
    : /* no output */
    : [index] "r" (index),
      [ca_base] "r" (dls_causal_base),
      [ac_base] "r" (dls_acausal_base),
      [ca_offset] "kv" (ca_offsets[index]),
      [ac_offset] "kv" (ac_offsets[index]),
      [select] "kv" (select),
      [w_base] "r" (dls_weight_base),
      [zeros] "kv" (zeros),
      [factors] "kv" (inh_factors),
      [decay_factors] "kv" (decay_factors)
    : "kv1", "kv2", "kv3", "kv4", "kv5");
--}