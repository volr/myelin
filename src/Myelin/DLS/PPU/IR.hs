{-# LANGUAGE GADTs #-}
module Myelin.DLS.PPU.IR where

import Data.Word
import Myelin.DLS.PPU.Assembler.Monad as M
import Myelin.DLS.PPU.Assembler as A

data Value =
      V Integer
    | L Int
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
    Select :: IR
    Load :: IR -> IR
    Store :: IR -> IR
    CCall :: IR -> IR
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
    fromInteger i = Value (V i)

genBinOp :: (A.Register -> A.Register -> Bool -> Bool -> M.Asm A.Register) -> IR -> IR -> M.Asm A.Register
genBinOp op a b = do
    a' <- genAsm a
    b' <- genAsm b
    op a' b' False False

genAsm :: IR -> M.Asm A.Register
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
