{-# LANGUAGE DeriveFunctor, PatternGuards #-}
module Myelin.DLS.UNI where

import Data.Word
import Data.Bits
import Data.Monoid
import Data.ByteString.Builder

import Control.Applicative

-- very preliminary type definitions
type OmnibusValue = Word32
type OmnibusAddress = Word32
type Time = Word64

data Term a =
    Seq [Term a]
  | Par [Term a]
  | P a
  deriving (Eq, Show, Functor)

instance Monoid (Term a) where
  mappend (Seq a) (Seq b) = Seq (a <> b)
  mappend (Seq a) b = Seq (a <> [b])
  mappend a (Seq b) = Seq ([a] <> b)
  mappend a b = Seq [a,b]
  mempty = Seq []

pmappend :: Term a -> Term a -> Term a
pmappend (Par a) (Par b) = Par (a <> b)
pmappend a (Par b) = Par ([a] <> b)
pmappend (Par a) b = Par (a <> [b])
pmappend a b = Par [a,b]

parallel :: [Term a] -> Term a
parallel = Par

sequential :: [Term a] -> Term a
sequential = Seq

infixl 7 <|>
(<|>) = pmappend

data Primitive = 
      WaitFor7 !Word8 -- ^
    | WaitFor16 !Word16 -- ^
    | WaitFor32 !Word32 -- ^
    | WaitUntil !Word64 -- ^
    | SetTime !Word64 -- ^ 
    | Fire !Word32 !Word8 -- ^ send several spike events to the chip
    | FireOne !Word8 -- ^ send one spike event to the chip
    | Write !OmnibusAddress !OmnibusValue -- ^ write to the specified omnibus address
    | Read !OmnibusAddress -- ^ read from the specified omnibus address 
    | Halt -- ^ halt program execution
    | RecStart -- ^ start recording chip answers (spikes and read responses)
    | RecStop -- ^ stop recortding chip answers (spikes and read responses)
    deriving (Eq, Show)

waitFor :: Time -> Term Primitive
waitFor t | t < 2^7 = P (WaitFor7 (fromIntegral t))
waitFor t | t < 2^16 = P (WaitFor16 (fromIntegral t))
waitFor t | t < 2^32 = P (WaitFor32 (fromIntegral t))
waitFor t = error "Time in cycles out of bound, use waitUntil"

waitUntil :: Time -> Term Primitive
waitUntil = P . WaitUntil

setTime :: Time -> Term Primitive
setTime = P . SetTime

fire :: Word32 -> Word8 -> Term Primitive
fire mask address = P (Fire mask address) 

fireOne :: Word8 -> Term Primitive
fireOne address = P (FireOne address)

write :: OmnibusAddress -> OmnibusValue -> Term Primitive
write address value = P (Write address value)

read :: OmnibusAddress -> Term Primitive
read address = P (Read address)

halt :: Term Primitive
halt = P Halt

recStart :: Term Primitive
recStart = P RecStart

recStop :: Term Primitive
recStop = P RecStop

-- ^ binary encoding of UNI
opcode :: Primitive -> Word8
opcode (SetTime _) = 0x0
opcode Halt = 0x0e
opcode (Read _) = 0x0b
opcode (Write _ _) = 0x0a
opcode (WaitUntil _) = 0x01
opcode (WaitFor32 _) = 0x05
opcode (WaitFor16 _) = 0x04
opcode (WaitFor7 _) = 0x80
opcode RecStart = 0x0c
opcode RecStop = 0x0d
opcode (Fire _ _) = 0x0f
opcode (FireOne _) = 0x40

-- ^ use a bytestring builder to encode
encodePrimitive :: Primitive -> Builder
encodePrimitive op@(SetTime t) = word8 (opcode op) <> word64LE t
encodePrimitive op@(WaitFor7 t) = word8 (opcode op .|. t) 
encodePrimitive op@(WaitFor16 t) = word8 (opcode op) <> word16LE t
encodePrimitive op@(WaitFor32 t) = word8 (opcode op) <> word32LE t
encodePrimitive op@(WaitUntil t) = word8 (opcode op) <> word64LE t
encodePrimitive op@(FireOne a) = word8 (opcode op .|. (0x4f .&. a))
encodePrimitive op@(Fire mask address) = word8 (opcode op) <> word32LE mask <> word8 address
encodePrimitive op@(Write address value) = word8 (opcode op) <> word32LE address <> word32LE value
encodePrimitive op@(Read address) = word8 (opcode op) <> word32LE address
encodePrimitive Halt = word8 (opcode Halt) 
encodePrimitive RecStart = word8 (opcode RecStart)
encodePrimitive RecStop = word8 (opcode RecStop)

