{-# LANGUAGE GADTs, RebindableSyntax #-}
module Myelin.DLS.PPU.IR where

import Control.Lens
import Data.Word
import Myelin.DLS.PPU.Assembler.Monad as M
import Myelin.DLS.PPU.Assembler as A
import Prelude

type IRLabel = Int

data Value =
      V Word32
    | L IRLabel
    deriving (Show)

data IR where
    Nop :: IR -> IR
    Identity :: IR -> IR
    Add :: IR -> IR -> IR
    Sub :: IR -> IR -> IR
    Mul :: IR -> IR -> IR
    Div :: IR -> IR -> IR
    Mod :: IR -> IR -> IR 
    Neg :: IR -> IR
    BitAnd :: IR -> IR -> IR
    BitOr :: IR -> IR -> IR
    BitXor :: IR -> IR -> IR
    Shl :: IR -> IR -> IR
    SShr :: IR -> IR -> IR
    ZShr :: IR -> IR -> IR
    RotR :: IR -> IR -> IR
    RotL :: IR -> IR -> IR
    Equal :: IR -> IR -> IR
    NotEqual :: IR -> IR -> IR
    LessThan :: IR -> IR -> IR
    GreaterThan :: IR -> IR -> IR
    LessEqual :: IR -> IR -> IR
    GreaterEqual :: IR -> IR -> IR
    Abs :: IR -> IR
    Signum :: IR -> IR
    Select :: IR -> IR -> IR -> IR
    Load :: IR -> IR
    Store :: IR -> IR
    CCall :: IR -> IR -- ^ call a c function
    Upsilon :: IR -> IR
    Phi :: IR
    Branch :: IR
    Jump :: IR -> IR
    Return :: IR -> IR
    -- 
    Value :: Value -> IR
    deriving (Show)

instance Num IR where
    (+) = Add
    (*) = Mul
    (-) (Value (V 0)) b = Neg b
    (-) a b = Sub a b
    abs = Abs
    signum = Signum
    fromInteger = Value . V . fromIntegral

exIR = (12 + 2) * 2 :: IR
exAsm :: Monad m => M.Asm A.Register m
exAsm = genAsm exIR

genBinOp :: Monad m => (A.Register -> A.Register -> Bool -> Bool -> M.Asm A.Register m) 
         -> IR 
         -> IR 
         -> M.Asm A.Register m
genBinOp op a b = do
    a' <- genAsm a
    b' <- genAsm b
    op a' b' False False

genLoadImmediate :: Monad m => Word32 -> M.Asm A.Register m
genLoadImmediate i = do
    r <- allocateRegister
    M.lbz r i
    return r

genAsm :: Monad m => IR -> M.Asm A.Register m
genAsm (Value (V i)) = genLoadImmediate i
genAsm (Value (L i)) = undefined
genAsm (Add a b) = genBinOp M.add a b
-- genAsm (Sub a b) = genBinOp M.sub a b
genAsm (Mul a b) = genBinOp M.mullw a b
genAsm (Div a b) = genBinOp M.divw a b
genAsm (Mod a b) = undefined
genAsm (Neg a) = undefined
genAsm (BitAnd a b) = undefined
genAsm (BitOr a b) = undefined
genAsm (BitXor a b) = undefined
genAsm (Shl a b) = undefined
genAsm (Select a b c) = do
    a' <- genAsm a 
    b' <- genAsm b
    c' <- genAsm c 
    undefined

