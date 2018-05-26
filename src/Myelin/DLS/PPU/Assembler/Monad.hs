{-# LANGUAGE TemplateHaskell, OverloadedLists #-}
module Myelin.DLS.PPU.Assembler.Monad where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe
import Data.Word
import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set

import Myelin.DLS.PPU.Assembler as A

data Label = Label {
    _id :: Int
} deriving (Eq, Show)

-- | State of the Assembler Monad
data AsmState = AsmState {
    _labels :: [Label], -- ^ labels generated so far (currently not used)
    _instructions :: [A.Inst], -- ^ instructions assembled so far
    _freeRegisters :: Set Register, -- ^ registers not in use
    _temporaryRegisters :: Set Register, -- ^ registers that were temporarily allocated
    _usedRegisters :: Set Register, -- ^ registers in use
    _freeVectorRegisters :: Set VectorRegister, -- ^ vector registers not in use
    _temporaryVectorRegisters :: Set VectorRegister, -- ^ vector registers that were temporarily allocated
    _usedVectorRegisters :: Set VectorRegister, -- ^ vector registers in use
    _registerLabels :: [(Label, Register)] -- ^ labels for registers (currently not used)
} deriving (Eq, Show)

initialAsmState = AsmState [] [] [A.R0 .. A.R31] [] [] [A.VR0 .. A.VR31] [] [] []

type Asm a m = StateT AsmState m a

makeLenses ''AsmState

-- | append a list of instructions
asm :: Monad m => [Inst] -> Asm () m
asm inst = instructions <>= inst

-- | Register Management functions
 
-- | allocate one scalar register
allocateRegister :: Monad m => Asm Register m
allocateRegister = do
    fr <- use freeRegisters
    let f = fromJust $ Set.lookupMin fr
    freeRegisters .= Set.deleteMin fr
    usedRegisters <>= [f]
    return f

-- | allocate a new list of registers
allocateRegisters :: Monad m => Int -> Asm [Register] m
allocateRegisters n = mapM (\_ -> allocateRegister) [1..n]

-- | release a scalar register
releaseRegister :: Monad m => Register -> Asm () m
releaseRegister r = do
    used <- use usedRegisters
    usedRegisters .= Set.delete r used
    freeRegisters <>= [r]

-- | release a set of registers
releaseRegisters :: Monad m => Set Register -> Asm () m
releaseRegisters r = do
    used <- use usedRegisters
    usedRegisters .= Set.difference used r
    freeRegisters <>= r

-- TODO: This does not actually handle errors...
allocateTemporaryRegister :: Monad m => Asm Register m
allocateTemporaryRegister = do
    fr <- use freeRegisters
    let f = fromJust $ Set.lookupMin fr
    freeRegisters .= Set.deleteMin fr
    temporaryRegisters <>= [f]
    return f

releaseTemporaryRegister :: Monad m => Register -> Asm () m
releaseTemporaryRegister r = do
    temp <- use temporaryRegisters
    temporaryRegisters .= Set.delete r temp
    freeRegisters <>= [r]

allocateVectorRegister :: Monad m => Asm VectorRegister m
allocateVectorRegister = do
    fr <- use freeVectorRegisters
    let f = fromJust $ Set.lookupMin fr
    freeVectorRegisters .= Set.deleteMin fr
    usedVectorRegisters <>= [f]
    return f

allocateVectorRegisters :: Monad m => Int -> Asm [VectorRegister] m
allocateVectorRegisters n = mapM (\_ -> allocateVectorRegister) [1..n]

releaseVectorRegister :: Monad m => VectorRegister -> Asm () m
releaseVectorRegister r = do
    used <- use usedVectorRegisters
    usedVectorRegisters .= Set.delete r used
    freeVectorRegisters <>= [r]

releaseVectorRegisters :: Monad m => Set VectorRegister -> Asm () m
releaseVectorRegisters r = do
    used <- use usedVectorRegisters
    usedVectorRegisters .= Set.difference used r
    freeVectorRegisters <>= r

allocateTemporaryVectorRegister :: Monad m => Asm VectorRegister m
allocateTemporaryVectorRegister = do
    fr <- use freeVectorRegisters
    let f = fromJust $ Set.lookupMin fr
    freeVectorRegisters .= Set.deleteMin fr
    temporaryVectorRegisters <>= [f]
    return f

releaseTemporaryVectorRegister :: Monad m => VectorRegister -> Asm () m
releaseTemporaryVectorRegister r = do
    temp <- use temporaryVectorRegisters
    temporaryVectorRegisters .= Set.delete r temp
    freeVectorRegisters <>= [r]

retainVectorRegisters :: Monad m => Set VectorRegister -> Asm () m
retainVectorRegisters retainedVectorRegisters =
    releaseVectorRegisters releasedVectorRegisters
    where releasedVectorRegisters = Set.difference [A.VR0 .. A.VR31] retainedVectorRegisters

-- ^ A new block of assembler instructions
block :: Monad m => Set Register -> Set VectorRegister -> ([Register] -> [VectorRegister] -> Asm a m) -> Asm a m
block retainedRegisters retainedVectorRegisters action = do
    releaseVectorRegisters releasedVectorRegisters
    releaseRegisters releasedRegisters
    r <- Set.elems <$> use usedRegisters
    vr <- Set.elems <$> use usedVectorRegisters
    action r vr
    where
        releasedRegisters = Set.difference [A.R0 .. A.R31] retainedRegisters
        releasedVectorRegisters = Set.difference [A.VR0 .. A.VR31] retainedVectorRegisters

------------------------------------------------------------------------------
-- Function Prologue and Epilogue generation
------------------------------------------------------------------------------

-- | create a stack frame of n words according to the powerpc-eabi convention
createStackFrame :: Monad m =>
    Word32 -- ^ size of stack frame (in words) to be created
    -> Asm () m
createStackFrame n = asm
    [
        A.stwu A.R1 A.R1 (-n), -- stwu r1, -N(r1)
        A.mflr A.R0, -- mflr r0
        A.stw A.R0 A.R1 (n+4) -- stw r0, N+4(r1)
    ]

-- | remove a stack frame of n words according to the powerpc-eabi convention
removeStackFrame :: Monad m =>
    Word32 -- ^ size of stack frame (in words) to be removed
    -> Asm () m
removeStackFrame n = asm
    [
        A.lwz A.R0 A.R1 (n+4), -- lwz    r0, N+4(r1)
        A.addi A.R1 A.R1 n, -- addi   r1, r1, N
        A.mtlr A.R0 -- mtlr r0
        -- A.blr -- blr
    ]

restoreRegisters :: Monad m => [A.Register] -> Asm () m
restoreRegisters regs = do
    undefined
    undefined

storeRegisters :: Monad m => [A.Register] -> Asm () m
storeRegisters regs = do
    -- stw    r31, N-4(r1)
    -- stw    r30, N-8(r1)
    -- stw    r29, N-12(r1)
    undefined
    undefined

functionPrologue :: Monad m => Asm () m
functionPrologue = do
    createStackFrame 64
    storeRegisters [A.R14 .. A.R31]

functionEpilogue :: Monad m => Asm () m
functionEpilogue = do
    restoreRegisters [A.R14 .. A.R31]
    removeStackFrame 64

------------------------------------------------------------------------------
-- Functions for generating instructions of a specific form
------------------------------------------------------------------------------

iop :: Monad m => (Word32 -> Bool -> Bool -> A.Inst)
    -> Word32
    -> Bool
    -> Bool
    -> Asm () m
iop op li aa lk = instructions <>= [op li aa lk]

bop :: Monad m => (Register -> Register -> Word32 -> Bool -> Bool -> A.Inst)
    -> Register
    -> Register
    -> Word32
    -> Bool
    -> Bool
    -> Asm () m
bop op bo bi bd aa lk = instructions <>= [inst]
    where inst = op bo bi bd aa lk

dop :: Monad m => (Register -> Register -> Word32 -> A.Inst)
    -> Register
    -> Word32
    -> Asm Register m
dop op ra d = do
    rt <- allocateRegister
    instructions <>= [op rt ra d]
    return rt

dop_ :: Monad m => (Register -> Register -> Word32 -> A.Inst)
    -> Register
    -> Word32
    -> Asm () m
dop_ op ra d = instructions <>= [op ra ra d]

xoop :: Monad m => (Register -> Register -> Register -> A.Inst)
    -> Register
    -> Register
    -> Asm Register m
xoop op ra rb = do
    rt <- allocateRegister
    instructions <>= [op rt ra rb]
    return rt

xoop_ :: Monad m => (Register -> Register -> Register -> A.Inst)
    -> Register
    -> Register
    -> Asm () m
xoop_ op ra rb = instructions <>= [op ra ra rb]

xop :: Monad m => (Register -> Register -> Register -> A.Inst)
    -> Register
    -> Register
    -> Asm Register m
xop op ra rb = do
    rt <- allocateRegister
    instructions <>= [op rt ra rb]
    return rt

xop_ :: Monad m => (Register -> Register -> Register -> A.Inst)
    -> Register
    -> Register
    -> Asm () m
xop_ op ra rb = instructions <>= [op ra ra rb]

-- mop :: Register -> Register -> Register -> Register -> Register -> Bool -> Asm ()

xfxop :: Monad m => Register
      -> SpecialPurposeRegister
      -> Asm () m
xfxop = undefined

xlop :: Monad m => (Register -> Register -> Register -> Bool -> A.Inst)
    -> Register
    -> Register
    -> Register
    -> Bool
    -> Asm () m
xlop op bt ba bb lk = instructions <>= [op bt ba bb lk]

fxvop' :: Monad m => (VectorRegister -> VectorRegister -> VectorRegister -> A.Fxv_cond -> A.Inst)
    -> VectorRegister
    -> VectorRegister
    -> A.Fxv_cond
    -> Asm VectorRegister m
fxvop' op ra rb cond = do
    rt <- allocateVectorRegister
    instructions <>= [op rt ra rb cond]
    return rt

fxvop'_ :: Monad m => (VectorRegister -> VectorRegister -> VectorRegister -> A.Fxv_cond -> A.Inst)
       -> VectorRegister
       -> VectorRegister
       -> A.Fxv_cond
       -> Asm () m
fxvop'_ op ra rb cond = instructions <>= [op ra ra rb cond]

fxvop :: Monad m => (VectorRegister -> VectorRegister -> VectorRegister -> A.Inst)
    -> VectorRegister
    -> VectorRegister
    -> Asm VectorRegister m
fxvop op ra rb = do
    rt <- allocateVectorRegister
    instructions <>= [op rt ra rb]
    return rt

fxvop_ :: Monad m => (VectorRegister -> VectorRegister -> VectorRegister -> A.Inst)
       -> VectorRegister
       -> VectorRegister
       -> Asm () m
fxvop_ op ra rb = instructions <>= [op ra ra rb]

fxviop :: Monad m => (VectorRegister -> VectorRegister -> Int8 -> A.Fxv_cond -> A.Inst)
    -> VectorRegister
    -> Int8
    -> A.Fxv_cond
    -> Asm VectorRegister m
fxviop op ra imm cond = do
    rt <- allocateVectorRegister
    instructions <>= [op rt ra imm cond]
    return rt

fxviop_ :: Monad m => (VectorRegister -> VectorRegister -> Int8 -> A.Fxv_cond -> A.Inst)
    -> VectorRegister
    -> Int8
    -> A.Fxv_cond
    -> Asm () m
fxviop_ op ra imm cond = instructions <>= [op ra ra imm cond]

fxvsop :: Monad m => (VectorRegister -> Register -> Register -> A.Inst)
    -> Register
    -> Register
    -> Asm VectorRegister m
fxvsop op ra rb = do
    rt <- allocateVectorRegister
    instructions <>= [op rt ra rb]
    return rt

-- D Instruction Format
twi         :: Monad m => Register -> Word32 -> Asm Register m
mulli       :: Monad m => Register -> Word32 -> Asm Register m
subfic      :: Monad m => Register -> Word32 -> Asm Register m
syncmpi_rec :: Monad m => Register -> Word32 -> Asm Register m
cmpli       :: Monad m => Register -> Word32 -> Asm Register m
cmpi        :: Monad m => Register -> Word32 -> Asm Register m
addic       :: Monad m => Register -> Word32 -> Asm Register m
addic_rec   :: Monad m => Register -> Word32 -> Asm Register m
addi        :: Monad m => Register -> Word32 -> Asm Register m
addis       :: Monad m => Register -> Word32 -> Asm Register m
twi         = dop A.twi
mulli       = dop A.mulli
subfic      = dop A.subfic
syncmpi_rec = dop A.syncmpi_rec
cmpli       = dop A.cmpli
cmpi        = dop A.cmpi
addic       = dop A.addic
addic_rec   = dop A.addic_rec
addi        = dop A.addi
addis       = dop A.addis
--
bc :: Monad m => Register -> Register -> Word32 -> Bool -> Bool -> Asm () m
bc = bop A.bc
branch :: Monad m => Word32 -> Bool -> Bool -> Asm () m
branch = iop A.branch
-- D Instruction Format
rlwimi :: Monad m => Register -> Word32 -> Asm Register m
rlwinm :: Monad m => Register -> Word32 -> Asm Register m
rlwnm  :: Monad m => Register -> Word32 -> Asm Register m
ori    :: Monad m => Register -> Word32 -> Asm Register m
oris   :: Monad m => Register -> Word32 -> Asm Register m
xori   :: Monad m => Register -> Word32 -> Asm Register m
xoris  :: Monad m => Register -> Word32 -> Asm Register m
andi   :: Monad m => Register -> Word32 -> Asm Register m
andis  :: Monad m => Register -> Word32 -> Asm Register m
lwz    :: Monad m => Register -> Word32 -> Asm Register m
lwzu   :: Monad m => Register -> Word32 -> Asm Register m
lbz    :: Monad m => Register -> Word32 -> Asm Register m
lbzu   :: Monad m => Register -> Word32 -> Asm Register m
stw    :: Monad m => Register -> Word32 -> Asm Register m
stwu   :: Monad m => Register -> Word32 -> Asm Register m
stb    :: Monad m => Register -> Word32 -> Asm Register m
stbu   :: Monad m => Register -> Word32 -> Asm Register m
lhz    :: Monad m => Register -> Word32 -> Asm Register m
lhzu   :: Monad m => Register -> Word32 -> Asm Register m
lha    :: Monad m => Register -> Word32 -> Asm Register m
lhau   :: Monad m => Register -> Word32 -> Asm Register m
sth    :: Monad m => Register -> Word32 -> Asm Register m
sthu   :: Monad m => Register -> Word32 -> Asm Register m
lmw    :: Monad m => Register -> Word32 -> Asm Register m
stmw   :: Monad m => Register -> Word32 -> Asm Register m
rlwimi = dop A.rlwimi
rlwinm = dop A.rlwinm
rlwnm  = dop A.rlwnm
ori    = dop A.ori
oris   = dop A.oris
xori   = dop A.xori
xoris  = dop A.xoris
andi   = dop A.andi
andis  = dop A.andis
lwz    = dop A.lwz
lwzu   = dop A.lwzu
lbz    = dop A.lbz
lbzu   = dop A.lbzu
stw    = dop A.stw
stwu   = dop A.stwu
stb    = dop A.stb
stbu   = dop A.stbu
lhz    = dop A.lhz
lhzu   = dop A.lhzu
lha    = dop A.lha
lhau   = dop A.lhau
sth    = dop A.sth
sthu   = dop A.sthu
lmw    = dop A.lmw
stmw   = dop A.stmw
rlwimi_ :: Monad m => Register -> Word32 -> Asm () m
rlwinm_ :: Monad m => Register -> Word32 -> Asm () m
rlwnm_  :: Monad m => Register -> Word32 -> Asm () m
ori_    :: Monad m => Register -> Word32 -> Asm () m
oris_   :: Monad m => Register -> Word32 -> Asm () m
xori_   :: Monad m => Register -> Word32 -> Asm () m
xoris_  :: Monad m => Register -> Word32 -> Asm () m
andi_   :: Monad m => Register -> Word32 -> Asm () m
andis_  :: Monad m => Register -> Word32 -> Asm () m
lwz_    :: Monad m => Register -> Word32 -> Asm () m
lwzu_   :: Monad m => Register -> Word32 -> Asm () m
lbz_    :: Monad m => Register -> Word32 -> Asm () m
lbzu_   :: Monad m => Register -> Word32 -> Asm () m
stw_    :: Monad m => Register -> Word32 -> Asm () m
stwu_   :: Monad m => Register -> Word32 -> Asm () m
stb_    :: Monad m => Register -> Word32 -> Asm () m
stbu_   :: Monad m => Register -> Word32 -> Asm () m
lhz_    :: Monad m => Register -> Word32 -> Asm () m
lhzu_   :: Monad m => Register -> Word32 -> Asm () m
lha_    :: Monad m => Register -> Word32 -> Asm () m
lhau_   :: Monad m => Register -> Word32 -> Asm () m
sth_    :: Monad m => Register -> Word32 -> Asm () m
sthu_   :: Monad m => Register -> Word32 -> Asm () m
lmw_    :: Monad m => Register -> Word32 -> Asm () m
stmw_   :: Monad m => Register -> Word32 -> Asm () m
rlwimi_ = dop_ A.rlwimi
rlwinm_ = dop_ A.rlwinm
rlwnm_  = dop_ A.rlwnm
ori_    = dop_ A.ori
oris_   = dop_ A.oris
xori_   = dop_ A.xori
xoris_  = dop_ A.xoris
andi_   = dop_ A.andi
andis_  = dop_ A.andis
lwz_    = dop_ A.lwz
lwzu_   = dop_ A.lwzu
lbz_    = dop_ A.lbz
lbzu_   = dop_ A.lbzu
stw_    = dop_ A.stw
stwu_   = dop_ A.stwu
stb_    = dop_ A.stb
stbu_   = dop_ A.stbu
lhz_    = dop_ A.lhz
lhzu_   = dop_ A.lhzu
lha_    = dop_ A.lha
lhau_   = dop_ A.lhau
sth_    = dop_ A.sth
sthu_   = dop_ A.sthu
lmw_    = dop_ A.lmw
stmw_   = dop_ A.stmw

-- X Instruction Format
cmp      :: Monad m => Register -> Register -> Asm Register m
cmp'     :: Monad m => Register -> Register -> Asm Register m
tw       :: Monad m => Register -> Register -> Asm Register m
tw'      :: Monad m => Register -> Register -> Asm Register m
lwzx     :: Monad m => Register -> Register -> Asm Register m
lwzx'    :: Monad m => Register -> Register -> Asm Register m
slw      :: Monad m => Register -> Register -> Asm Register m
slw'     :: Monad m => Register -> Register -> Asm Register m
cntlzw   :: Monad m => Register -> Register -> Asm Register m
cntlzw'  :: Monad m => Register -> Register -> Asm Register m
and      :: Monad m => Register -> Register -> Asm Register m
and'     :: Monad m => Register -> Register -> Asm Register m
cmpl     :: Monad m => Register -> Register -> Asm Register m
cmpl'    :: Monad m => Register -> Register -> Asm Register m
nvem     :: Monad m => Register -> Register -> Asm Register m
nvem'    :: Monad m => Register -> Register -> Asm Register m
nves     :: Monad m => Register -> Register -> Asm Register m
nves'    :: Monad m => Register -> Register -> Asm Register m
nvemtl   :: Monad m => Register -> Register -> Asm Register m
nvemtl'  :: Monad m => Register -> Register -> Asm Register m
lwzux    :: Monad m => Register -> Register -> Asm Register m
lwzux'   :: Monad m => Register -> Register -> Asm Register m
andc     :: Monad m => Register -> Register -> Asm Register m
andc'    :: Monad m => Register -> Register -> Asm Register m
wait     :: Monad m => Register -> Register -> Asm Register m
wait'    :: Monad m => Register -> Register -> Asm Register m
mfmsr    :: Monad m => Register -> Register -> Asm Register m
mfmsr'   :: Monad m => Register -> Register -> Asm Register m
lbzx     :: Monad m => Register -> Register -> Asm Register m
lbzx'    :: Monad m => Register -> Register -> Asm Register m
lbzux    :: Monad m => Register -> Register -> Asm Register m
lbzux'   :: Monad m => Register -> Register -> Asm Register m
popcb    :: Monad m => Register -> Register -> Asm Register m
popcb'   :: Monad m => Register -> Register -> Asm Register m
nor      :: Monad m => Register -> Register -> Asm Register m
nor'     :: Monad m => Register -> Register -> Asm Register m
mtmsr    :: Monad m => Register -> Register -> Asm Register m
mtmsr'   :: Monad m => Register -> Register -> Asm Register m
stwx     :: Monad m => Register -> Register -> Asm Register m
stwx'    :: Monad m => Register -> Register -> Asm Register m
prtyw    :: Monad m => Register -> Register -> Asm Register m
prtyw'   :: Monad m => Register -> Register -> Asm Register m
stwux    :: Monad m => Register -> Register -> Asm Register m
stwux'   :: Monad m => Register -> Register -> Asm Register m
stbx     :: Monad m => Register -> Register -> Asm Register m
stbx'    :: Monad m => Register -> Register -> Asm Register m
stbux    :: Monad m => Register -> Register -> Asm Register m
stbux'   :: Monad m => Register -> Register -> Asm Register m
lhzx     :: Monad m => Register -> Register -> Asm Register m
lhzx'    :: Monad m => Register -> Register -> Asm Register m
eqv      :: Monad m => Register -> Register -> Asm Register m
eqv'     :: Monad m => Register -> Register -> Asm Register m
eciwx    :: Monad m => Register -> Register -> Asm Register m
eciwx'   :: Monad m => Register -> Register -> Asm Register m
lhzux    :: Monad m => Register -> Register -> Asm Register m
lhzux'   :: Monad m => Register -> Register -> Asm Register m
xor      :: Monad m => Register -> Register -> Asm Register m
xor'     :: Monad m => Register -> Register -> Asm Register m
lhax     :: Monad m => Register -> Register -> Asm Register m
lhax'    :: Monad m => Register -> Register -> Asm Register m
lhaux    :: Monad m => Register -> Register -> Asm Register m
lhaux'   :: Monad m => Register -> Register -> Asm Register m
sthx     :: Monad m => Register -> Register -> Asm Register m
sthx'    :: Monad m => Register -> Register -> Asm Register m
orc      :: Monad m => Register -> Register -> Asm Register m
orc'     :: Monad m => Register -> Register -> Asm Register m
ecowx    :: Monad m => Register -> Register -> Asm Register m
ecowx'   :: Monad m => Register -> Register -> Asm Register m
sthux    :: Monad m => Register -> Register -> Asm Register m
sthux'   :: Monad m => Register -> Register -> Asm Register m
or       :: Monad m => Register -> Register -> Asm Register m
or'      :: Monad m => Register -> Register -> Asm Register m
nand     :: Monad m => Register -> Register -> Asm Register m
nand'    :: Monad m => Register -> Register -> Asm Register m
srw      :: Monad m => Register -> Register -> Asm Register m
srw'     :: Monad m => Register -> Register -> Asm Register m
sync     :: Monad m => Register -> Register -> Asm Register m
sync'    :: Monad m => Register -> Register -> Asm Register m
synm     :: Monad m => Register -> Register -> Asm Register m
synm'    :: Monad m => Register -> Register -> Asm Register m
syns     :: Monad m => Register -> Register -> Asm Register m
syns'    :: Monad m => Register -> Register -> Asm Register m
synmtl   :: Monad m => Register -> Register -> Asm Register m
synmtl'  :: Monad m => Register -> Register -> Asm Register m
synmtvr  :: Monad m => Register -> Register -> Asm Register m
synmtvr' :: Monad m => Register -> Register -> Asm Register m
synmfvr  :: Monad m => Register -> Register -> Asm Register m
synmfvr' :: Monad m => Register -> Register -> Asm Register m
synmtp   :: Monad m => Register -> Register -> Asm Register m
synmtp'  :: Monad m => Register -> Register -> Asm Register m
synmfp   :: Monad m => Register -> Register -> Asm Register m
synmfp'  :: Monad m => Register -> Register -> Asm Register m
synmvvr  :: Monad m => Register -> Register -> Asm Register m
synmvvr' :: Monad m => Register -> Register -> Asm Register m
synops   :: Monad m => Register -> Register -> Asm Register m
synops'  :: Monad m => Register -> Register -> Asm Register m
synswp   :: Monad m => Register -> Register -> Asm Register m
synswp'  :: Monad m => Register -> Register -> Asm Register m
sraw     :: Monad m => Register -> Register -> Asm Register m
sraw'    :: Monad m => Register -> Register -> Asm Register m
srawi    :: Monad m => Register -> Register -> Asm Register m
srawi'   :: Monad m => Register -> Register -> Asm Register m
extsh    :: Monad m => Register -> Register -> Asm Register m
extsh'   :: Monad m => Register -> Register -> Asm Register m
extsb    :: Monad m => Register -> Register -> Asm Register m
extsb'   :: Monad m => Register -> Register -> Asm Register m
cmp      = xop A.cmp
cmp'     = xop A.cmp'
tw       = xop A.tw
tw'      = xop A.tw'
lwzx     = xop A.lwzx
lwzx'    = xop A.lwzx'
slw      = xop A.slw
slw'     = xop A.slw'
cntlzw   = xop A.cntlzw
cntlzw'  = xop A.cntlzw'
and      = xop A.and
and'     = xop A.and'
cmpl     = xop A.cmpl
cmpl'    = xop A.cmpl'
nvem     = xop A.nvem
nvem'    = xop A.nvem'
nves     = xop A.nves
nves'    = xop A.nves'
nvemtl   = xop A.nvemtl
nvemtl'  = xop A.nvemtl'
lwzux    = xop A.lwzux
lwzux'   = xop A.lwzux'
andc     = xop A.andc
andc'    = xop A.andc'
wait     = xop A.wait
wait'    = xop A.wait'
mfmsr    = xop A.mfmsr
mfmsr'   = xop A.mfmsr'
lbzx     = xop A.lbzx
lbzx'    = xop A.lbzx'
lbzux    = xop A.lbzux
lbzux'   = xop A.lbzux'
popcb    = xop A.popcb
popcb'   = xop A.popcb'
nor      = xop A.nor
nor'     = xop A.nor'
mtmsr    = xop A.mtmsr
mtmsr'   = xop A.mtmsr'
stwx     = xop A.stwx
stwx'    = xop A.stwx'
prtyw    = xop A.prtyw
prtyw'   = xop A.prtyw'
stwux    = xop A.stwux
stwux'   = xop A.stwux'
stbx     = xop A.stbx
stbx'    = xop A.stbx'
stbux    = xop A.stbux
stbux'   = xop A.stbux'
lhzx     = xop A.lhzx
lhzx'    = xop A.lhzx'
eqv      = xop A.eqv
eqv'     = xop A.eqv'
eciwx    = xop A.eciwx
eciwx'   = xop A.eciwx'
lhzux    = xop A.lhzux
lhzux'   = xop A.lhzux'
xor      = xop A.xor
xor'     = xop A.xor'
lhax     = xop A.lhax
lhax'    = xop A.lhax'
lhaux    = xop A.lhaux
lhaux'   = xop A.lhaux'
sthx     = xop A.sthx
sthx'    = xop A.sthx'
orc      = xop A.orc
orc'     = xop A.orc'
ecowx    = xop A.ecowx
ecowx'   = xop A.ecowx'
sthux    = xop A.sthux
sthux'   = xop A.sthux'
or       = xop A.or
or'      = xop A.or'
nand     = xop A.nand
nand'    = xop A.nand'
srw      = xop A.srw
srw'     = xop A.srw'
sync     = xop A.sync
sync'    = xop A.sync'
synm     = xop A.synm
synm'    = xop A.synm'
syns     = xop A.syns
syns'    = xop A.syns'
synmtl   = xop A.synmtl
synmtl'  = xop A.synmtl'
synmtvr  = xop A.synmtvr
synmtvr' = xop A.synmtvr'
synmfvr  = xop A.synmfvr
synmfvr' = xop A.synmfvr'
synmtp   = xop A.synmtp
synmtp'  = xop A.synmtp'
synmfp   = xop A.synmfp
synmfp'  = xop A.synmfp'
synmvvr  = xop A.synmvvr
synmvvr' = xop A.synmvvr'
synops   = xop A.synops
synops'  = xop A.synops'
synswp   = xop A.synswp
synswp'  = xop A.synswp'
sraw     = xop A.sraw
sraw'    = xop A.sraw'
srawi    = xop A.srawi
srawi'   = xop A.srawi'
extsh    = xop A.extsh
extsh'   = xop A.extsh'
extsb    = xop A.extsb
extsb'   = xop A.extsb'
cmp_      :: Monad m => Register -> Register -> Asm () m
cmp'_     :: Monad m => Register -> Register -> Asm () m
tw_       :: Monad m => Register -> Register -> Asm () m
tw'_      :: Monad m => Register -> Register -> Asm () m
lwzx_     :: Monad m => Register -> Register -> Asm () m
lwzx'_    :: Monad m => Register -> Register -> Asm () m
slw_      :: Monad m => Register -> Register -> Asm () m
slw'_     :: Monad m => Register -> Register -> Asm () m
cntlzw_   :: Monad m => Register -> Register -> Asm () m
cntlzw'_  :: Monad m => Register -> Register -> Asm () m
and_      :: Monad m => Register -> Register -> Asm () m
and'_     :: Monad m => Register -> Register -> Asm () m
cmpl_     :: Monad m => Register -> Register -> Asm () m
cmpl'_    :: Monad m => Register -> Register -> Asm () m
nvem_     :: Monad m => Register -> Register -> Asm () m
nvem'_    :: Monad m => Register -> Register -> Asm () m
nves_     :: Monad m => Register -> Register -> Asm () m
nves'_    :: Monad m => Register -> Register -> Asm () m
nvemtl_   :: Monad m => Register -> Register -> Asm () m
nvemtl'_  :: Monad m => Register -> Register -> Asm () m
lwzux_    :: Monad m => Register -> Register -> Asm () m
lwzux'_   :: Monad m => Register -> Register -> Asm () m
andc_     :: Monad m => Register -> Register -> Asm () m
andc'_    :: Monad m => Register -> Register -> Asm () m
wait_     :: Monad m => Register -> Register -> Asm () m
wait'_    :: Monad m => Register -> Register -> Asm () m
mfmsr_    :: Monad m => Register -> Register -> Asm () m
mfmsr'_   :: Monad m => Register -> Register -> Asm () m
lbzx_     :: Monad m => Register -> Register -> Asm () m
lbzx'_    :: Monad m => Register -> Register -> Asm () m
lbzux_    :: Monad m => Register -> Register -> Asm () m
lbzux'_   :: Monad m => Register -> Register -> Asm () m
popcb_    :: Monad m => Register -> Register -> Asm () m
popcb'_   :: Monad m => Register -> Register -> Asm () m
nor_      :: Monad m => Register -> Register -> Asm () m
nor'_     :: Monad m => Register -> Register -> Asm () m
mtmsr_    :: Monad m => Register -> Register -> Asm () m
mtmsr'_   :: Monad m => Register -> Register -> Asm () m
stwx_     :: Monad m => Register -> Register -> Asm () m
stwx'_    :: Monad m => Register -> Register -> Asm () m
prtyw_    :: Monad m => Register -> Register -> Asm () m
prtyw'_   :: Monad m => Register -> Register -> Asm () m
stwux_    :: Monad m => Register -> Register -> Asm () m
stwux'_   :: Monad m => Register -> Register -> Asm () m
stbx_     :: Monad m => Register -> Register -> Asm () m
stbx'_    :: Monad m => Register -> Register -> Asm () m
stbux_    :: Monad m => Register -> Register -> Asm () m
stbux'_   :: Monad m => Register -> Register -> Asm () m
lhzx_     :: Monad m => Register -> Register -> Asm () m
lhzx'_    :: Monad m => Register -> Register -> Asm () m
eqv_      :: Monad m => Register -> Register -> Asm () m
eqv'_     :: Monad m => Register -> Register -> Asm () m
eciwx_    :: Monad m => Register -> Register -> Asm () m
eciwx'_   :: Monad m => Register -> Register -> Asm () m
lhzux_    :: Monad m => Register -> Register -> Asm () m
lhzux'_   :: Monad m => Register -> Register -> Asm () m
xor_      :: Monad m => Register -> Register -> Asm () m
xor'_     :: Monad m => Register -> Register -> Asm () m
lhax_     :: Monad m => Register -> Register -> Asm () m
lhax'_    :: Monad m => Register -> Register -> Asm () m
lhaux_    :: Monad m => Register -> Register -> Asm () m
lhaux'_   :: Monad m => Register -> Register -> Asm () m
sthx_     :: Monad m => Register -> Register -> Asm () m
sthx'_    :: Monad m => Register -> Register -> Asm () m
orc_      :: Monad m => Register -> Register -> Asm () m
orc'_     :: Monad m => Register -> Register -> Asm () m
ecowx_    :: Monad m => Register -> Register -> Asm () m
ecowx'_   :: Monad m => Register -> Register -> Asm () m
sthux_    :: Monad m => Register -> Register -> Asm () m
sthux'_   :: Monad m => Register -> Register -> Asm () m
or_       :: Monad m => Register -> Register -> Asm () m
or'_      :: Monad m => Register -> Register -> Asm () m
nand_     :: Monad m => Register -> Register -> Asm () m
nand'_    :: Monad m => Register -> Register -> Asm () m
srw_      :: Monad m => Register -> Register -> Asm () m
srw'_     :: Monad m => Register -> Register -> Asm () m
sync_     :: Monad m => Register -> Register -> Asm () m
sync'_    :: Monad m => Register -> Register -> Asm () m
synm_     :: Monad m => Register -> Register -> Asm () m
synm'_    :: Monad m => Register -> Register -> Asm () m
syns_     :: Monad m => Register -> Register -> Asm () m
syns'_    :: Monad m => Register -> Register -> Asm () m
synmtl_   :: Monad m => Register -> Register -> Asm () m
synmtl'_  :: Monad m => Register -> Register -> Asm () m
synmtvr_  :: Monad m => Register -> Register -> Asm () m
synmtvr'_ :: Monad m => Register -> Register -> Asm () m
synmfvr_  :: Monad m => Register -> Register -> Asm () m
synmfvr'_ :: Monad m => Register -> Register -> Asm () m
synmtp_   :: Monad m => Register -> Register -> Asm () m
synmtp'_  :: Monad m => Register -> Register -> Asm () m
synmfp_   :: Monad m => Register -> Register -> Asm () m
synmfp'_  :: Monad m => Register -> Register -> Asm () m
synmvvr_  :: Monad m => Register -> Register -> Asm () m
synmvvr'_ :: Monad m => Register -> Register -> Asm () m
synops_   :: Monad m => Register -> Register -> Asm () m
synops'_  :: Monad m => Register -> Register -> Asm () m
synswp_   :: Monad m => Register -> Register -> Asm () m
synswp'_  :: Monad m => Register -> Register -> Asm () m
sraw_     :: Monad m => Register -> Register -> Asm () m
sraw'_    :: Monad m => Register -> Register -> Asm () m
srawi_    :: Monad m => Register -> Register -> Asm () m
srawi'_   :: Monad m => Register -> Register -> Asm () m
extsh_    :: Monad m => Register -> Register -> Asm () m
extsh'_   :: Monad m => Register -> Register -> Asm () m
extsb_    :: Monad m => Register -> Register -> Asm () m
extsb'_   :: Monad m => Register -> Register -> Asm () m
cmp_      = xop_ A.cmp
cmp'_     = xop_ A.cmp'
tw_       = xop_ A.tw
tw'_      = xop_ A.tw'
lwzx_     = xop_ A.lwzx
lwzx'_    = xop_ A.lwzx'
slw_      = xop_ A.slw
slw'_     = xop_ A.slw'
cntlzw_   = xop_ A.cntlzw
cntlzw'_  = xop_ A.cntlzw'
and_      = xop_ A.and
and'_     = xop_ A.and'
cmpl_     = xop_ A.cmpl
cmpl'_    = xop_ A.cmpl'
nvem_     = xop_ A.nvem
nvem'_    = xop_ A.nvem'
nves_     = xop_ A.nves
nves'_    = xop_ A.nves'
nvemtl_   = xop_ A.nvemtl
nvemtl'_  = xop_ A.nvemtl'
lwzux_    = xop_ A.lwzux
lwzux'_   = xop_ A.lwzux'
andc_     = xop_ A.andc
andc'_    = xop_ A.andc'
wait_     = xop_ A.wait
wait'_    = xop_ A.wait'
mfmsr_    = xop_ A.mfmsr
mfmsr'_   = xop_ A.mfmsr'
lbzx_     = xop_ A.lbzx
lbzx'_    = xop_ A.lbzx'
lbzux_    = xop_ A.lbzux
lbzux'_   = xop_ A.lbzux'
popcb_    = xop_ A.popcb
popcb'_   = xop_ A.popcb'
nor_      = xop_ A.nor
nor'_     = xop_ A.nor'
mtmsr_    = xop_ A.mtmsr
mtmsr'_   = xop_ A.mtmsr'
stwx_     = xop_ A.stwx
stwx'_    = xop_ A.stwx'
prtyw_    = xop_ A.prtyw
prtyw'_   = xop_ A.prtyw'
stwux_    = xop_ A.stwux
stwux'_   = xop_ A.stwux'
stbx_     = xop_ A.stbx
stbx'_    = xop_ A.stbx'
stbux_    = xop_ A.stbux
stbux'_   = xop_ A.stbux'
lhzx_     = xop_ A.lhzx
lhzx'_    = xop_ A.lhzx'
eqv_      = xop_ A.eqv
eqv'_     = xop_ A.eqv'
eciwx_    = xop_ A.eciwx
eciwx'_   = xop_ A.eciwx'
lhzux_    = xop_ A.lhzux
lhzux'_   = xop_ A.lhzux'
xor_      = xop_ A.xor
xor'_     = xop_ A.xor'
lhax_     = xop_ A.lhax
lhax'_    = xop_ A.lhax'
lhaux_    = xop_ A.lhaux
lhaux'_   = xop_ A.lhaux'
sthx_     = xop_ A.sthx
sthx'_    = xop_ A.sthx'
orc_      = xop_ A.orc
orc'_     = xop_ A.orc'
ecowx_    = xop_ A.ecowx
ecowx'_   = xop_ A.ecowx'
sthux_    = xop_ A.sthux
sthux'_   = xop_ A.sthux'
or_       = xop_ A.or
or'_      = xop_ A.or'
nand_     = xop_ A.nand
nand'_    = xop_ A.nand'
srw_      = xop_ A.srw
srw'_     = xop_ A.srw'
sync_     = xop_ A.sync
sync'_    = xop_ A.sync'
synm_     = xop_ A.synm
synm'_    = xop_ A.synm'
syns_     = xop_ A.syns
syns'_    = xop_ A.syns'
synmtl_   = xop_ A.synmtl
synmtl'_  = xop_ A.synmtl'
synmtvr_  = xop_ A.synmtvr
synmtvr'_ = xop_ A.synmtvr'
synmfvr_  = xop_ A.synmfvr
synmfvr'_ = xop_ A.synmfvr'
synmtp_   = xop_ A.synmtp
synmtp'_  = xop_ A.synmtp'
synmfp_   = xop_ A.synmfp
synmfp'_  = xop_ A.synmfp'
synmvvr_  = xop_ A.synmvvr
synmvvr'_ = xop_ A.synmvvr'
synops_   = xop_ A.synops
synops'_  = xop_ A.synops'
synswp_   = xop_ A.synswp
synswp'_  = xop_ A.synswp'
sraw_     = xop_ A.sraw
sraw'_    = xop_ A.sraw'
srawi_    = xop_ A.srawi
srawi'_   = xop_ A.srawi'
extsh_    = xop_ A.extsh
extsh'_   = xop_ A.extsh'
extsb_    = xop_ A.extsb
extsb'_   = xop_ A.extsb'

-- XO Instruction Format
subfc    :: Monad m => Register -> Register -> Asm Register m
subfc'   :: Monad m => Register -> Register -> Asm Register m
subfco   :: Monad m => Register -> Register -> Asm Register m
subfco'  :: Monad m => Register -> Register -> Asm Register m
addc     :: Monad m => Register -> Register -> Asm Register m
addc'    :: Monad m => Register -> Register -> Asm Register m
addco    :: Monad m => Register -> Register -> Asm Register m
addco'   :: Monad m => Register -> Register -> Asm Register m
mulhwu   :: Monad m => Register -> Register -> Asm Register m
mulhwu'  :: Monad m => Register -> Register -> Asm Register m
mulhwuo  :: Monad m => Register -> Register -> Asm Register m
mulhwuo' :: Monad m => Register -> Register -> Asm Register m
subf     :: Monad m => Register -> Register -> Asm Register m
subf'    :: Monad m => Register -> Register -> Asm Register m
subfo    :: Monad m => Register -> Register -> Asm Register m
subfo'   :: Monad m => Register -> Register -> Asm Register m
mulhw    :: Monad m => Register -> Register -> Asm Register m
mulhw'   :: Monad m => Register -> Register -> Asm Register m
mulhwo   :: Monad m => Register -> Register -> Asm Register m
mulhwo'  :: Monad m => Register -> Register -> Asm Register m
neg      :: Monad m => Register -> Register -> Asm Register m
neg'     :: Monad m => Register -> Register -> Asm Register m
nego     :: Monad m => Register -> Register -> Asm Register m
nego'    :: Monad m => Register -> Register -> Asm Register m
subfe    :: Monad m => Register -> Register -> Asm Register m
subfe'   :: Monad m => Register -> Register -> Asm Register m
subfeo   :: Monad m => Register -> Register -> Asm Register m
subfeo'  :: Monad m => Register -> Register -> Asm Register m
adde     :: Monad m => Register -> Register -> Asm Register m
adde'    :: Monad m => Register -> Register -> Asm Register m
addeo    :: Monad m => Register -> Register -> Asm Register m
addeo'   :: Monad m => Register -> Register -> Asm Register m
subfze   :: Monad m => Register -> Register -> Asm Register m
subfze'  :: Monad m => Register -> Register -> Asm Register m
subfzeo  :: Monad m => Register -> Register -> Asm Register m
subfzeo' :: Monad m => Register -> Register -> Asm Register m
addze    :: Monad m => Register -> Register -> Asm Register m
addze'   :: Monad m => Register -> Register -> Asm Register m
addzeo   :: Monad m => Register -> Register -> Asm Register m
addzeo'  :: Monad m => Register -> Register -> Asm Register m
subfme   :: Monad m => Register -> Register -> Asm Register m
subfme'  :: Monad m => Register -> Register -> Asm Register m
subfmeo  :: Monad m => Register -> Register -> Asm Register m
subfmeo' :: Monad m => Register -> Register -> Asm Register m
addme    :: Monad m => Register -> Register -> Asm Register m
addme'   :: Monad m => Register -> Register -> Asm Register m
addmeo   :: Monad m => Register -> Register -> Asm Register m
addmeo'  :: Monad m => Register -> Register -> Asm Register m
mullw    :: Monad m => Register -> Register -> Asm Register m
mullw'   :: Monad m => Register -> Register -> Asm Register m
mullwo   :: Monad m => Register -> Register -> Asm Register m
mullwo'  :: Monad m => Register -> Register -> Asm Register m
add      :: Monad m => Register -> Register -> Asm Register m
add'     :: Monad m => Register -> Register -> Asm Register m
addo     :: Monad m => Register -> Register -> Asm Register m
addo'    :: Monad m => Register -> Register -> Asm Register m
divwu    :: Monad m => Register -> Register -> Asm Register m
divwu'   :: Monad m => Register -> Register -> Asm Register m
divwuo   :: Monad m => Register -> Register -> Asm Register m
divwuo'  :: Monad m => Register -> Register -> Asm Register m
divw     :: Monad m => Register -> Register -> Asm Register m
divw'    :: Monad m => Register -> Register -> Asm Register m
divwo    :: Monad m => Register -> Register -> Asm Register m
divwo'   :: Monad m => Register -> Register -> Asm Register m
subfc    = xoop A.subfc
subfc'   = xoop A.subfc'
subfco   = xoop A.subfco
subfco'  = xoop A.subfco'
addc     = xoop A.addc
addc'    = xoop A.addc'
addco    = xoop A.addco
addco'   = xoop A.addco'
mulhwu   = xoop A.mulhwu
mulhwu'  = xoop A.mulhwu'
mulhwuo  = xoop A.mulhwuo
mulhwuo' = xoop A.mulhwuo'
subf     = xoop A.subf
subf'    = xoop A.subf'
subfo    = xoop A.subfo
subfo'   = xoop A.subfo'
mulhw    = xoop A.mulhw
mulhw'   = xoop A.mulhw'
mulhwo   = xoop A.mulhwo
mulhwo'  = xoop A.mulhwo'
neg      = xoop A.neg
neg'     = xoop A.neg'
nego     = xoop A.nego
nego'    = xoop A.nego'
subfe    = xoop A.subfe
subfe'   = xoop A.subfe'
subfeo   = xoop A.subfeo
subfeo'  = xoop A.subfeo'
adde     = xoop A.adde
adde'    = xoop A.adde'
addeo    = xoop A.addeo
addeo'   = xoop A.addeo'
subfze   = xoop A.subfze
subfze'  = xoop A.subfze'
subfzeo  = xoop A.subfzeo
subfzeo' = xoop A.subfzeo'
addze    = xoop A.addze
addze'   = xoop A.addze'
addzeo   = xoop A.addzeo
addzeo'  = xoop A.addzeo'
subfme   = xoop A.subfme
subfme'  = xoop A.subfme'
subfmeo  = xoop A.subfmeo
subfmeo' = xoop A.subfmeo'
addme    = xoop A.addme
addme'   = xoop A.addme'
addmeo   = xoop A.addmeo
addmeo'  = xoop A.addmeo'
mullw    = xoop A.mullw
mullw'   = xoop A.mullw'
mullwo   = xoop A.mullwo
mullwo'  = xoop A.mullwo'
add      = xoop A.add
add'     = xoop A.add'
addo     = xoop A.addo
addo'    = xoop A.addo'
divwu    = xoop A.divwu
divwu'   = xoop A.divwu'
divwuo   = xoop A.divwuo
divwuo'  = xoop A.divwuo'
divw     = xoop A.divw
divw'    = xoop A.divw'
divwo    = xoop A.divwo
divwo'   = xoop A.divwo'
subfc_    :: Monad m => Register -> Register -> Asm () m
subfc'_   :: Monad m => Register -> Register -> Asm () m
subfco_   :: Monad m => Register -> Register -> Asm () m
subfco'_  :: Monad m => Register -> Register -> Asm () m
addc_     :: Monad m => Register -> Register -> Asm () m
addc'_    :: Monad m => Register -> Register -> Asm () m
addco_    :: Monad m => Register -> Register -> Asm () m
addco'_   :: Monad m => Register -> Register -> Asm () m
mulhwu_   :: Monad m => Register -> Register -> Asm () m
mulhwu'_  :: Monad m => Register -> Register -> Asm () m
mulhwuo_  :: Monad m => Register -> Register -> Asm () m
mulhwuo'_ :: Monad m => Register -> Register -> Asm () m
subf_     :: Monad m => Register -> Register -> Asm () m
subf'_    :: Monad m => Register -> Register -> Asm () m
subfo_    :: Monad m => Register -> Register -> Asm () m
subfo'_   :: Monad m => Register -> Register -> Asm () m
mulhw_    :: Monad m => Register -> Register -> Asm () m
mulhw'_   :: Monad m => Register -> Register -> Asm () m
mulhwo_   :: Monad m => Register -> Register -> Asm () m
mulhwo'_  :: Monad m => Register -> Register -> Asm () m
neg_      :: Monad m => Register -> Register -> Asm () m
neg'_     :: Monad m => Register -> Register -> Asm () m
nego_     :: Monad m => Register -> Register -> Asm () m
nego'_    :: Monad m => Register -> Register -> Asm () m
subfe_    :: Monad m => Register -> Register -> Asm () m
subfe'_   :: Monad m => Register -> Register -> Asm () m
subfeo_   :: Monad m => Register -> Register -> Asm () m
subfeo'_  :: Monad m => Register -> Register -> Asm () m
adde_     :: Monad m => Register -> Register -> Asm () m
adde'_    :: Monad m => Register -> Register -> Asm () m
addeo_    :: Monad m => Register -> Register -> Asm () m
addeo'_   :: Monad m => Register -> Register -> Asm () m
subfze_   :: Monad m => Register -> Register -> Asm () m
subfze'_  :: Monad m => Register -> Register -> Asm () m
subfzeo_  :: Monad m => Register -> Register -> Asm () m
subfzeo'_ :: Monad m => Register -> Register -> Asm () m
addze_    :: Monad m => Register -> Register -> Asm () m
addze'_   :: Monad m => Register -> Register -> Asm () m
addzeo_   :: Monad m => Register -> Register -> Asm () m
addzeo'_  :: Monad m => Register -> Register -> Asm () m
subfme_   :: Monad m => Register -> Register -> Asm () m
subfme'_  :: Monad m => Register -> Register -> Asm () m
subfmeo_  :: Monad m => Register -> Register -> Asm () m
subfmeo'_ :: Monad m => Register -> Register -> Asm () m
addme_    :: Monad m => Register -> Register -> Asm () m
addme'_   :: Monad m => Register -> Register -> Asm () m
addmeo_   :: Monad m => Register -> Register -> Asm () m
addmeo'_  :: Monad m => Register -> Register -> Asm () m
mullw_    :: Monad m => Register -> Register -> Asm () m
mullw'_   :: Monad m => Register -> Register -> Asm () m
mullwo_   :: Monad m => Register -> Register -> Asm () m
mullwo'_  :: Monad m => Register -> Register -> Asm () m
add_      :: Monad m => Register -> Register -> Asm () m
add'_     :: Monad m => Register -> Register -> Asm () m
addo_     :: Monad m => Register -> Register -> Asm () m
addo'_    :: Monad m => Register -> Register -> Asm () m
divwu_    :: Monad m => Register -> Register -> Asm () m
divwu'_   :: Monad m => Register -> Register -> Asm () m
divwuo_   :: Monad m => Register -> Register -> Asm () m
divwuo'_  :: Monad m => Register -> Register -> Asm () m
divw_     :: Monad m => Register -> Register -> Asm () m
divw'_    :: Monad m => Register -> Register -> Asm () m
divwo_    :: Monad m => Register -> Register -> Asm () m
divwo'_   :: Monad m => Register -> Register -> Asm () m
subfc_  = xoop_ A.subfc
subfc'_  = xoop_ A.subfc'
subfco_  = xoop_ A.subfco
subfco'_  = xoop_ A.subfco'
addc_   = xoop_ A.addc
addc'_   = xoop_ A.addc
addco_   = xoop_ A.addc
addco'_   = xoop_ A.addc
mulhwu_ = xoop_ A.mulhwu
mulhwu'_ = xoop_ A.mulhwu
mulhwuo_ = xoop_ A.mulhwu
mulhwuo'_ = xoop_ A.mulhwu
subf_   = xoop_ A.subf
subf'_   = xoop_ A.subf
subfo_   = xoop_ A.subf
subfo'_   = xoop_ A.subf
mulhw_  = xoop_ A.mulhw
mulhw'_  = xoop_ A.mulhw
mulhwo_  = xoop_ A.mulhw
mulhwo'_  = xoop_ A.mulhw
neg_    = xoop_ A.neg
neg'_    = xoop_ A.neg
nego_    = xoop_ A.neg
nego'_    = xoop_ A.neg
subfe_  = xoop_ A.subfe
subfe'_  = xoop_ A.subfe
subfeo_  = xoop_ A.subfe
subfeo'_  = xoop_ A.subfe
adde_   = xoop_ A.adde
adde'_   = xoop_ A.adde
addeo_   = xoop_ A.adde
addeo'_   = xoop_ A.adde
subfze_ = xoop_ A.subfze
subfze'_ = xoop_ A.subfze
subfzeo_ = xoop_ A.subfze
subfzeo'_ = xoop_ A.subfze
addze_  = xoop_ A.addze
addze'_  = xoop_ A.addze
addzeo_  = xoop_ A.addze
addzeo'_  = xoop_ A.addze
subfme_ = xoop_ A.subfme
subfme'_ = xoop_ A.subfme
subfmeo_ = xoop_ A.subfme
subfmeo'_ = xoop_ A.subfme
addme_  = xoop_ A.addme
addme'_  = xoop_ A.addme
addmeo_  = xoop_ A.addme
addmeo'_  = xoop_ A.addme
mullw_  = xoop_ A.mullw
mullw'_  = xoop_ A.mullw
mullwo_  = xoop_ A.mullw
mullwo'_  = xoop_ A.mullw
add_    = xoop_ A.add
add'_    = xoop_ A.add
addo_    = xoop_ A.add
addo'_    = xoop_ A.add
divwu_  = xoop_ A.divwu
divwu'_  = xoop_ A.divwu
divwuo_  = xoop_ A.divwu
divwuo'_  = xoop_ A.divwu
divw_   = xoop_ A.divw
divw'_   = xoop_ A.divw
divwo_   = xoop_ A.divw
divwo'_   = xoop_ A.divw


-- XL Instruction Format
mcrf   :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
bclr   :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
crnor  :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
rfmci  :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
rfi    :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
rfci   :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
crandc :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
crxor  :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
crnand :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
creqv  :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
crand  :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
crorc  :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
cror   :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
bcctr  :: Monad m => Register -> Register -> Register -> Bool -> Asm () m
mcrf   = xlop A.mcrf
bclr   = xlop A.bclr
crnor  = xlop A.crnor
rfmci  = xlop A.rfmci
rfi    = xlop A.rfi
rfci   = xlop A.rfci
crandc = xlop A.crandc
crxor  = xlop A.crxor
crnand = xlop A.crnand
creqv  = xlop A.creqv
crand  = xlop A.crand
crorc  = xlop A.crorc
cror   = xlop A.cror
bcctr  = xlop A.bcctr

-- XFX Instruction Format
{-
mfocrf = xfxop A.mfocrf
mtocrf = xfxop A.mtocrf
mfspr  = xfxop A.mfspr
mtspr  = xfxop A.mtspr
-}

-- FXV/FXVS Instruction Format
fxvmahm        :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmahm'       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmabm        :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmabm'       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmtacb       :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmtacb'      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmtach       :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmtach'      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmahfs       :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmahfs'      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmabfs       :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmabfs'      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmtacbf      :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmtacbf'     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmtachf      :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmtachf'     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmatachm     :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmatachm'    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmatacbm     :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmatacbm'    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmatachfs    :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmatachfs'   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmatacbfs    :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmatacbfs'   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmulhm       :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmulhm'      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmulbm       :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmulbm'      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmulhfs      :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmulhfs'     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmulbfs      :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmulbfs'     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmultachm    :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmultachm'   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmultacbm    :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmultacbm'   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmultachfs   :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmultachfs'  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmultacbfs   :: Monad m => VectorRegister -> VectorRegister               -> Asm VectorRegister m
fxvmultacbfs'  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvpckbu      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvpckbl      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvupckbr     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvupckbl     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvcmph       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvcmpb       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsel        :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsubhm       :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvsubhm'      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsubbm       :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvsubbm'      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsubhfs      :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvsubhfs'     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsubbfs      :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvsubbfs'     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddactachm  :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddactachm' :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddactacb   :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddactacb'  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddactachf  :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddactachf' :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddactacbf  :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddactacbf' :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddachm     :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddachm'    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddacbm     :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddacbm'    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddachfs    :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddachfs'   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddacbfs    :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddacbfs'   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddtachm    :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddtachm'   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddtacb     :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddtacb'    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddhm       :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddhm'      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddbm       :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddbm'      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddhfs      :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddhfs'     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddbfs      :: Monad m => VectorRegister -> VectorRegister ->               Asm VectorRegister m
fxvaddbfs'     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvinx        :: Monad m => Register -> Register -> Asm VectorRegister m
fxvoutx       :: Monad m => VectorRegister -> Register -> Register -> Asm () m
fxvlax        :: Monad m => Register -> Register -> Asm VectorRegister m
fxvstax       :: Monad m => Register -> Register -> Asm VectorRegister m
fxvsplath     :: Monad m => Register             -> Asm VectorRegister m
fxvsplatb     :: Monad m => Register             -> Asm VectorRegister m
fxvshh        :: Monad m => VectorRegister -> Int8 -> A.Fxv_cond -> Asm VectorRegister m
fxvshb        :: Monad m => VectorRegister -> Int8 -> A.Fxv_cond -> Asm VectorRegister m
fxvmahm       = fxvop A.fxvmahm
fxvmahm'      = fxvop' A.fxvmahm'
fxvmabm       = fxvop A.fxvmabm
fxvmabm'      = fxvop' A.fxvmabm'
fxvmtacb      = fxvop A.fxvmtacb
fxvmtacb'     = fxvop' A.fxvmtacb'
fxvmtach      = fxvop A.fxvmtach
fxvmtach'     = fxvop' A.fxvmtach'
fxvmahfs      = fxvop A.fxvmahfs
fxvmahfs'     = fxvop' A.fxvmahfs'
fxvmabfs      = fxvop A.fxvmabfs
fxvmabfs'     = fxvop' A.fxvmabfs'
fxvmtacbf     = fxvop A.fxvmtacbf
fxvmtacbf'    = fxvop' A.fxvmtacbf'
fxvmtachf     = fxvop A.fxvmtachf
fxvmtachf'    = fxvop' A.fxvmtachf'
fxvmatachm    = fxvop A.fxvmatachm
fxvmatachm'   = fxvop' A.fxvmatachm'
fxvmatacbm    = fxvop A.fxvmatacbm
fxvmatacbm'   = fxvop' A.fxvmatacbm'
fxvmatachfs   = fxvop A.fxvmatachfs
fxvmatachfs'  = fxvop' A.fxvmatachfs'
fxvmatacbfs   = fxvop A.fxvmatacbfs
fxvmatacbfs'  = fxvop' A.fxvmatacbfs'
fxvmulhm      = fxvop A.fxvmulhm
fxvmulhm'     = fxvop' A.fxvmulhm'
fxvmulbm      = fxvop A.fxvmulbm
fxvmulbm'     = fxvop' A.fxvmulbm'
fxvmulhfs     = fxvop A.fxvmulhfs
fxvmulhfs'    = fxvop' A.fxvmulhfs'
fxvmulbfs     = fxvop A.fxvmulbfs
fxvmulbfs'    = fxvop' A.fxvmulbfs'
fxvmultachm   = fxvop A.fxvmultachm
fxvmultachm'  = fxvop' A.fxvmultachm'
fxvmultacbm   = fxvop A.fxvmultacbm
fxvmultacbm'  = fxvop' A.fxvmultacbm'
fxvmultachfs  = fxvop A.fxvmultachfs
fxvmultachfs' = fxvop' A.fxvmultachfs'
fxvmultacbfs  = fxvop A.fxvmultacbfs
fxvmultacbfs' = fxvop' A.fxvmultacbfs'
fxvpckbu      = fxvop' A.fxvpckbu
fxvpckbl      = fxvop' A.fxvpckbl
fxvupckbr     = fxvop' A.fxvupckbr
fxvupckbl     = fxvop' A.fxvupckbl
fxvcmph       = fxvop' A.fxvcmph
fxvcmpb       = fxvop' A.fxvcmpb
fxvsel        = fxvop' A.fxvsel
fxvsubhm       = fxvop A.fxvsubhm
fxvsubhm'      = fxvop' A.fxvsubhm'
fxvsubbm       = fxvop A.fxvsubbm
fxvsubbm'      = fxvop' A.fxvsubbm'
fxvsubhfs      = fxvop A.fxvsubhfs
fxvsubhfs'     = fxvop' A.fxvsubhfs'
fxvsubbfs      = fxvop A.fxvsubbfs
fxvsubbfs'     = fxvop' A.fxvsubbfs'
fxvaddactachm  = fxvop A.fxvaddactachm
fxvaddactachm' = fxvop' A.fxvaddactachm'
fxvaddactacb   = fxvop A.fxvaddactacb
fxvaddactacb'  = fxvop' A.fxvaddactacb'
fxvaddactachf  = fxvop A.fxvaddactachf
fxvaddactachf' = fxvop' A.fxvaddactachf'
fxvaddactacbf  = fxvop A.fxvaddactacbf
fxvaddactacbf' = fxvop' A.fxvaddactacbf'
fxvaddachm     = fxvop A.fxvaddachm
fxvaddachm'    = fxvop' A.fxvaddachm'
fxvaddacbm     = fxvop A.fxvaddacbm
fxvaddacbm'    = fxvop' A.fxvaddacbm'
fxvaddachfs    = fxvop A.fxvaddachfs
fxvaddachfs'   = fxvop' A.fxvaddachfs'
fxvaddacbfs    = fxvop A.fxvaddacbfs
fxvaddacbfs'   = fxvop' A.fxvaddacbfs'
fxvaddtachm    = fxvop A.fxvaddtachm
fxvaddtachm'   = fxvop' A.fxvaddtachm'
fxvaddtacb     = fxvop A.fxvaddtacb
fxvaddtacb'    = fxvop' A.fxvaddtacb'
fxvaddhm       = fxvop A.fxvaddhm
fxvaddhm'      = fxvop' A.fxvaddhm'
fxvaddbm       = fxvop A.fxvaddbm
fxvaddbm'      = fxvop' A.fxvaddbm'
fxvaddhfs      = fxvop A.fxvaddhfs
fxvaddhfs'     = fxvop' A.fxvaddhfs'
fxvaddbfs      = fxvop A.fxvaddbfs
fxvaddbfs'     = fxvop' A.fxvaddbfs'
-- scalar operations
fxvinx        = fxvsop A.fxvinx
fxvoutx vr ra rb = instructions <>= [A.fxvoutx vr ra rb]
fxvlax        = fxvsop A.fxvlax
fxvstax       = fxvsop A.fxvstax
fxvsplath ra  = fxvsop (\rt ra rb -> A.fxvsplath rt ra) ra R0
fxvsplatb ra  = fxvsop (\rt ra rb -> A.fxvsplatb rt ra) ra R0
fxvshh        = fxviop A.fxvshh
fxvshb        = fxviop A.fxvshb

fxvmahm_       :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmahm'_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmabm_       :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmabm'_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmtacb_      :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmtacb'_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmtach_      :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmtach'_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmahfs_      :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmahfs'_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmabfs_      :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmabfs'_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmtacbf_     :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmtacbf'_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmtachf_     :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmtachf'_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmatachm_    :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmatachm'_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmatacbm_    :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmatacbm'_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmatachfs_   :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmatachfs'_  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmatacbfs_   :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmatacbfs'_  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmulhm_      :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmulhm'_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmulbm_      :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmulbm'_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmulhfs_     :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmulhfs'_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmulbfs_     :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmulbfs'_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmultachm_   :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmultachm'_  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmultacbm_   :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmultacbm'_  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmultachfs_  :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmultachfs'_ :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmultacbfs_  :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvmultacbfs'_ :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvpckbu_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvpckbl_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvupckbr_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvupckbl_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvcmph_       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvcmpb_       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvsel_        :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvsubhm_       :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvsubhm'_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvsubbm_       :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvsubbm'_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvsubhfs_      :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvsubhfs'_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvsubbfs_      :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvsubbfs'_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddactachm_  :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddactachm'_ :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddactacb_   :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddactacb'_  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddactachf_  :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddactachf'_ :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddactacbf_  :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddactacbf'_ :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddachm_     :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddachm'_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddacbm_     :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddacbm'_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddachfs_    :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddachfs'_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddacbfs_    :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddacbfs'_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddtachm_    :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddtachm'_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddtacb_     :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddtacb'_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddhm_       :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddhm'_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddbm_       :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddbm'_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddhfs_      :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddhfs'_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddbfs_      :: Monad m => VectorRegister -> VectorRegister               -> Asm () m
fxvaddbfs'_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvshh_        :: Monad m => VectorRegister -> Int8 -> A.Fxv_cond -> Asm () m
fxvshb_        :: Monad m => VectorRegister -> Int8 -> A.Fxv_cond -> Asm () m
fxvmahm_       = fxvop_ A.fxvmahm
fxvmahm'_      = fxvop'_ A.fxvmahm'
fxvmabm_       = fxvop_ A.fxvmabm
fxvmabm'_      = fxvop'_ A.fxvmabm'
fxvmtacb_      = fxvop_ A.fxvmtacb
fxvmtacb'_     = fxvop'_ A.fxvmtacb'
fxvmtach_      = fxvop_ A.fxvmtach
fxvmtach'_     = fxvop'_ A.fxvmtach'
fxvmahfs_      = fxvop_ A.fxvmahfs
fxvmahfs'_     = fxvop'_ A.fxvmahfs'
fxvmabfs_      = fxvop_ A.fxvmabfs
fxvmabfs'_     = fxvop'_ A.fxvmabfs'
fxvmtacbf_     = fxvop_ A.fxvmtacbf
fxvmtacbf'_    = fxvop'_ A.fxvmtacbf'
fxvmtachf_     = fxvop_ A.fxvmtachf
fxvmtachf'_    = fxvop'_ A.fxvmtachf'
fxvmatachm_    = fxvop_ A.fxvmatachm
fxvmatachm'_   = fxvop'_ A.fxvmatachm'
fxvmatacbm_    = fxvop_ A.fxvmatacbm
fxvmatacbm'_   = fxvop'_ A.fxvmatacbm'
fxvmatachfs_   = fxvop_ A.fxvmatachfs
fxvmatachfs'_  = fxvop'_ A.fxvmatachfs'
fxvmatacbfs_   = fxvop_ A.fxvmatacbfs
fxvmatacbfs'_  = fxvop'_ A.fxvmatacbfs'
fxvmulhm_      = fxvop_ A.fxvmulhm
fxvmulhm'_     = fxvop'_ A.fxvmulhm'
fxvmulbm_      = fxvop_ A.fxvmulbm
fxvmulbm'_     = fxvop'_ A.fxvmulbm'
fxvmulhfs_     = fxvop_ A.fxvmulhfs
fxvmulhfs'_    = fxvop'_ A.fxvmulhfs'
fxvmulbfs_     = fxvop_ A.fxvmulbfs
fxvmulbfs'_    = fxvop'_ A.fxvmulbfs'
fxvmultachm_   = fxvop_ A.fxvmultachm
fxvmultachm'_  = fxvop'_ A.fxvmultachm'
fxvmultacbm_   = fxvop_ A.fxvmultacbm
fxvmultacbm'_  = fxvop'_ A.fxvmultacbm'
fxvmultachfs_  = fxvop_ A.fxvmultachfs
fxvmultachfs'_ = fxvop'_ A.fxvmultachfs'
fxvmultacbfs_  = fxvop_ A.fxvmultacbfs
fxvmultacbfs'_ = fxvop'_ A.fxvmultacbfs'
fxvpckbu_      = fxvop'_ A.fxvpckbu
fxvpckbl_      = fxvop'_ A.fxvpckbl
fxvupckbr_     = fxvop'_ A.fxvupckbr
fxvupckbl_     = fxvop'_ A.fxvupckbl
fxvcmph_       = fxvop'_ A.fxvcmph
fxvcmpb_       = fxvop'_ A.fxvcmpb
fxvsel_        = fxvop'_ A.fxvsel
fxvsubhm_       = fxvop_ A.fxvsubhm
fxvsubhm'_      = fxvop'_ A.fxvsubhm'
fxvsubbm_       = fxvop_ A.fxvsubbm
fxvsubbm'_      = fxvop'_ A.fxvsubbm'
fxvsubhfs_      = fxvop_ A.fxvsubhfs
fxvsubhfs'_     = fxvop'_ A.fxvsubhfs'
fxvsubbfs_      = fxvop_ A.fxvsubbfs
fxvsubbfs'_     = fxvop'_ A.fxvsubbfs'
fxvaddactachm_  = fxvop_ A.fxvaddactachm
fxvaddactachm'_ = fxvop'_ A.fxvaddactachm'
fxvaddactacb_   = fxvop_ A.fxvaddactacb
fxvaddactacb'_  = fxvop'_ A.fxvaddactacb'
fxvaddactachf_  = fxvop_ A.fxvaddactachf
fxvaddactachf'_ = fxvop'_ A.fxvaddactachf'
fxvaddactacbf_  = fxvop_ A.fxvaddactacbf
fxvaddactacbf'_ = fxvop'_ A.fxvaddactacbf'
fxvaddachm_     = fxvop_ A.fxvaddachm
fxvaddachm'_    = fxvop'_ A.fxvaddachm'
fxvaddacbm_     = fxvop_ A.fxvaddacbm
fxvaddacbm'_    = fxvop'_ A.fxvaddacbm'
fxvaddachfs_    = fxvop_ A.fxvaddachfs
fxvaddachfs'_   = fxvop'_ A.fxvaddachfs'
fxvaddacbfs_    = fxvop_ A.fxvaddacbfs
fxvaddacbfs'_   = fxvop'_ A.fxvaddacbfs'
fxvaddtachm_    = fxvop_ A.fxvaddtachm
fxvaddtachm'_   = fxvop'_ A.fxvaddtachm'
fxvaddtacb_     = fxvop_ A.fxvaddtacb
fxvaddtacb'_    = fxvop'_ A.fxvaddtacb'
fxvaddhm_       = fxvop_ A.fxvaddhm
fxvaddhm'_      = fxvop'_ A.fxvaddhm'
fxvaddbm_       = fxvop_ A.fxvaddbm
fxvaddbm'_      = fxvop'_ A.fxvaddbm'
fxvaddhfs_      = fxvop_ A.fxvaddhfs
fxvaddhfs'_     = fxvop'_ A.fxvaddhfs'
fxvaddbfs_      = fxvop_ A.fxvaddbfs
fxvaddbfs'_     = fxvop'_ A.fxvaddbfs'
fxvshh_        = fxviop_ A.fxvshh
fxvshb_        = fxviop_ A.fxvshb