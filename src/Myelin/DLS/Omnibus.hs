{-# LANGUAGE RecordWildCards #-}
module Myelin.DLS.Omnibus where

import Control.Applicative
import Data.Word
import Data.Bits

data Error =
    WidthError { _expectedWidth :: Int, _actualWidth :: Int }

data Access = R | W | RW deriving (Show, Read)

data RegField =
    Padding Int
    | Field {
        _fieldName :: String,
        _fieldWidth :: Int,
        _fieldDoc :: Maybe String
    } deriving (Show, Read)

data Register = Register {
    _registerName :: String,
    _registerWidth :: Int,
    _access :: Access,
    _fields :: [RegField]
} deriving (Show, Read)

regExample :: Register
regExample = Register "configuration" 32 RW [
    Padding 3,
    Field "reg_en" 3 Nothing,
    Field "cap_en" 8 Nothing,
    Field "cap" 9 (Just "documentation")
    ]

checkRegister :: Register -> Maybe Error
checkRegister Register{..} =
    if actualWidth == expectedWidth then
        Nothing
    else Just $ WidthError expectedWidth actualWidth
    where
        expectedWidth = _registerWidth
        actualWidth = foldr (\field acc -> case field of
                            Padding p -> acc + p
                            Field _ w _ -> acc + w
                        ) 0 _fields

data OmnibusHierachy =
    Split {
        _select :: Int,
        _high :: OmnibusHierachy,
        _low :: OmnibusHierachy
    }
    | RegisterTarget {
        _name :: String,
        _numEntries :: Int,
        _registers :: [Register]
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

