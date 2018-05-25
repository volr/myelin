{-# LANGUAGE RecordWildCards, TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module Myelin.DLS.Omnibus where

import Control.Applicative
import Control.Lens

import Data.Word
import Data.Bits
import Data.String

import Myelin.DLS.Regfile

data OmnibusHierachy =
    Split {
        _select :: Int,
        _high :: OmnibusHierachy,
        _low :: OmnibusHierachy
    }
    | RegisterTarget {
        _name :: String,
        _numEntries :: Int,
        _registersFields :: [Register]
    }
    | Delay {
        _input :: OmnibusHierachy,
        _output :: OmnibusHierachy
    }
    | Connect {
        _input :: OmnibusHierachy,
        _output :: OmnibusHierachy
    }
    | Arb {
        a :: OmnibusHierachy,
        b :: OmnibusHierachy
    }
    | SRam {
        _name :: String,
        _size :: Int
    }
    | MainMemory {
        _size :: Int
    }
    | Master {
        _name :: String
    }
    | Slave {
        _name :: String,
        _createInterface :: Bool,
        _byteen :: Bool
    }
    | Terminator
    deriving (Show, Read)

a |> b = b a



data Address =
      Shift Word32 Int
    | Or Address Address
    | Base Word32

evalAddress (Shift a b) = a `shiftL` b
evalAddress (Or a b) =
    let a' = evalAddress a in
    let b' = evalAddress b in
        a' .|. b'
evalAddress (Base a) = a

computeSlaveAddresses :: Address -> OmnibusHierachy -> [Address]
computeSlaveAddresses address Split{..}  = 
    let rhs = computeSlaveAddresses (Or address (Shift 1 _select)) _high in
    let lhs = computeSlaveAddresses (Or address (Shift 0 _select)) _low in
    rhs ++ lhs
computeSlaveAddresses address Terminator = []
computeSlaveAddresses address Connect{..} = computeSlaveAddresses address _output
computeSlaveAddresses address Delay{..} = computeSlaveAddresses address _output
computeSlaveAddresses address _ = [address]

