{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Myelin.DLS.Omnibus where

import Control.Applicative
import Control.Lens

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
        _fieldDescription :: String
    } deriving (Show, Read)
makeLenses ''RegField

field :: String -> RegField
field name = Field {
        _fieldName = name,
        _fieldWidth = 0,
        _fieldDescription = ""
    }

data Register = Register {
    _registerName :: String,
    _registerWidth :: Int,
    _access :: Access,
    _registerDescription:: String,
    _fields :: [RegField]
} deriving (Show, Read)
makeLenses ''Register


register :: String -> Register
register name = Register {
        _registerName = name,
        _registerWidth = 32,
        _access = RW,
        _registerDescription = "",
        _fields = []
    }

data RegisterFile = RegisterFile {
    _registerFileName :: String,
    _registerFileDescription :: String,
    _registers :: [Register]
} deriving (Show, Read)
makeLenses ''RegisterFile


registerFile :: String -> RegisterFile
registerFile name = RegisterFile {
        _registerFileName = name,
        _registerFileDescription = "",
        _registers = []
    }

exRegisterFile = registerFile "test_rf" 
    & registerFileDescription .~ "My example RegisterFile"
    & registers .~ [
        register "test_register" 
        & registerDescription .~ "Test register"
        & registerWidth .~ 16
        & access .~ RW
        & fields .~ [
            field "test_field"
            & fieldWidth .~ 8
            & fieldDescription .~ "Test Field One"
        ,   field "test_field_2"
            & fieldWidth .~ 8
            & fieldDescription .~ "Test Field Two"
        ]
        ,
        register "test_register_2" 
        & registerDescription .~ "Test register 2"
        & registerWidth .~ 16
        & access .~ RW
        & fields .~ [
            field "test_field"
            & fieldWidth .~ 8
            & fieldDescription .~ "Test Field One"
        ,   field "test_field_2"
            & fieldWidth .~ 8
            & fieldDescription .~ "Test Field Two"
        ]
    ]

regExample :: Register
regExample = Register "configuration" 32 RW "This is an example register"
    [
    Padding 3,
    Field "reg_en" 3 "",
    Field "cap_en" 8 "",
    Field "cap" 9 ""
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

