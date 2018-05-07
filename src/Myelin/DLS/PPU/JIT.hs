{-# LANGUAGE TemplateHaskell #-}

module Myelin.DLS.PPU.JIT where

import Control.Lens hiding ((.=))
import Myelin.DLS.PPU.IR as I hiding (gen)
import Myelin.DLS.PPU.Assembler as A    

data RegType = Scalar | Vector deriving (Eq, Show)
data Reg = R0
    | R1
    | R2
    | R3
    | R4 
    | R5
    | R6
    | R7
    | R8 deriving (Eq, Show)

data RegState = RegState {
    _saved :: [Reg],
    _used :: [Reg],
    _temporary :: [Reg],
    _free :: [Reg]
} deriving (Eq, Show)

makeLenses ''RegState

data JITState = JITState {
    _scalar :: RegState,
    _vector :: RegState
} deriving (Eq, Show)

makeLenses ''JITState
{- 
initialJITState = JITState {
    _scalar = RegState [] [] [] (map R [0..31]),
    _vector = RegState [] [] [] (map R [0..31])
}
                        
genBinop :: Monad m => (IR -> IR -> m a) -> IR -> IR -> m a
genBinop op a b = do
    a' <- gen a
    b' <- gen b
    op a' b'

genUnop :: Monad m => (IR -> m a) -> IR -> m a
genUnop op a = do
    a' <- gen a
    op a'

gen = undefined
-}