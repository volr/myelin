{-# LANGUAGE GADTs, RebindableSyntax, DataKinds, NoImplicitPrelude #-}
module Myelin.DLS.PPU.IR where

import Prelude hiding ((<))
import Control.Lens
import Data.Word
import Myelin.DLS.PPU.Assembler.Monad as M
import Myelin.DLS.PPU.Assembler as A
{- 
type IRLabel = Int

data ScalarType = Address | Word | Halfword | Byte | Immediate
data ScalarArithmetic = Signed | Unsigned
data VectorArithmetic = FractionalSaturating | Modulo
data VectorType = VectorType ScalarType VectorArithmetic

type FXVHFS = VectorType Halfword FractionalSaturating
fxvhfs = VectorType Halfword FractionalSaturating
type FXVHM = VectorType Halfword Modulo
fxvhm = VectorType Halfword Modulo
type FXVBFS = VectorType Byte FractionalSaturating
fxvbfs = VectorType Byte FractionalSaturating
type FXVBM = VectorType Byte Modulo
fxvbm = VectorType Byte Modulo

data I a = I IR

data IR where
    Nop :: Type -> IR -> IR
    Identity :: Type -> IR -> IR
    Add :: Type -> IR -> IR -> IR
    Sub :: Type -> IR -> IR -> IR
    Mul :: Type -> IR -> IR -> IR
    Div :: Type -> IR -> IR -> IR
    Mod :: Type -> IR -> IR -> IR 
    Neg :: Type -> IR -> IR
    BitAnd :: Type -> IR -> IR -> IR
    BitOr :: Type -> IR -> IR -> IR
    BitXor :: Type -> IR -> IR -> IR
    Shl :: Type -> IR -> IR -> IR
    SShr :: Type -> IR -> IR -> IR
    ZShr :: Type -> IR -> IR -> IR
    RotR :: Type -> IR -> IR -> IR
    RotL :: Type -> IR -> IR -> IR
    Equal :: Type -> IR -> IR -> IR
    NotEqual :: Type -> IR -> IR -> IR
    LessThan :: Type -> IR -> IR -> IR
    GreaterThan :: Type -> IR -> IR -> IR
    LessEqual :: Type -> IR -> IR -> IR
    GreaterEqual :: Type -> IR -> IR -> IR
    Abs :: Type -> IR -> IR
    Signum :: Type -> IR -> IR
    Select :: Type -> IR -> IR -> IR -> IR
    Load :: Type -> IR -> IR
    Store :: Type -> IR -> IR
    CCall :: Type -> IR -> IR -- ^ call a c function
    Upsilon :: Type -> IR -> IR
    Phi :: Type -> IR
    Branch :: Type -> IR
    Jump :: Type -> IR -> IR
    Return :: Type -> IR -> IR
    -- 
    Splat :: Type -> IR -> IR
    -- 
    Value :: Value -> IR
    deriving (Show)

instance Num (I FXVHFS) where
    (+) = Add fxvhfs
    (*) = Mul fxvhfs
    (-) = Sub fxvhfs
    abs = Abs fxvhfs
    signum = Abs fxvhs
    fromInteger i = Splat fxvhfs $ Value . V . fromIntegral

instance Num (I FXVBFS) where
    (+) = Add fxvbfs
    (*) = Mul fxvbfs
    (-) = Sub fxvbfs
    abs = Abs fxvbfs
    signum = Abs fxvbfs
    fromInteger i = Splat fxvbfs $ Value . V . fromIntegral

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
-}