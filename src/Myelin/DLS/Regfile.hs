{-# LANGUAGE RecordWildCards, TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module Myelin.DLS.Regfile where

import Control.Lens
import Data.String    

data Error =
    WidthError { _expectedWidth :: Int, _actualWidth :: Int }

data Access = R | W | RW | RWC deriving (Show, Read)

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

instance IsString RegField where
    fromString = field

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

instance IsString Register where
    fromString = register

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

instance IsString RegisterFile where
    fromString = registerFile

exRegisterFile = "test_register_file" 
    & registerFileDescription .~ "My example RegisterFile"
    & registers .~ [
        "test_register" 
        & registerDescription .~ "Test register"
        & registerWidth .~ 16
        & access .~ RW
        & fields .~ [
            "test_field" & fieldWidth .~ 8 & fieldDescription .~ "Test Field One"
        ,   "test_field_2" & fieldWidth .~ 8 & fieldDescription .~ "Test Field Two"
        ]
        ,
        "test_register_2" 
        & registerDescription .~ "Test register 2"
        & registerWidth .~ 16
        & access .~ RW
        & fields .~ [
            "test_field" & fieldWidth .~ 8 & fieldDescription .~ "Test Field One"
        ,   "test_field_2" & fieldWidth .~ 8 & fieldDescription .~ "Test Field Two"
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