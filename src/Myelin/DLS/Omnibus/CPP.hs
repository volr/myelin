module Myelin.DLS.Omnibus.CPP where

data PrimType = 
    UInt32
    | UInt16
    | UInt64

data Field = 
    Padding Int
    | Field String Int 
    
data Type = 
    Struct {
        packed :: Bool,
        fields :: [Field]
    }
    | Bitfield {
        typ :: PrimType,
        fields :: [Field]
    }
    | Array {
        size :: Int
    }
    | Union {
    }

data TypeDeclaration = Decl String Type

bitfieldEx = Bitfield UInt32 [
        Padding 2,
        Field "name" 3
    ]

{-

union {




}


-}