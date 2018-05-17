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

data AsmState = AsmState {
    _labels :: [Label],
    _instructions :: [A.Inst],
    _freeRegisters :: Set Register, -- ^ registers not in use
    _temporaryRegisters :: Set Register, -- ^ registers that were temporarily allocated
    _usedRegisters :: Set Register, -- ^ registers in use
    _freeVectorRegisters :: Set VectorRegister, -- ^ vector registers not in use
    _temporaryVectorRegisters :: Set VectorRegister, -- ^ vector registers that were temporarily allocated
    _usedVectorRegisters :: Set VectorRegister, -- ^ vector registers in use
    _registerLabels :: [(Label, Register)]
} deriving (Eq, Show)

initialAsmState = AsmState [] [] [A.R0 .. A.R31] [] [] [A.VR0 .. A.VR31] [] [] []

type Asm a m = StateT AsmState m a

makeLenses ''AsmState

allocateRegister :: Monad m => Asm Register m
allocateRegister = do
    fr <- use freeRegisters
    let f = fromJust $ Set.lookupMin fr
    freeRegisters .= Set.deleteMin fr
    usedRegisters <>= [f]
    return f

releaseRegister :: Monad m => Register -> Asm () m
releaseRegister r = do
    used <- use usedRegisters
    usedRegisters .= Set.delete r used
    freeRegisters <>= [r]

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

releaseVectorRegister :: Monad m => VectorRegister -> Asm () m
releaseVectorRegister r = do
    used <- use usedVectorRegisters
    usedVectorRegisters .= Set.delete r used
    freeVectorRegisters <>= [r]

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

xoop :: Monad m => (Register -> Register -> Register -> Bool -> Bool -> A.Inst)
    -> Register 
    -> Register 
    -> Bool 
    -> Bool 
    -> Asm Register m
xoop op ra rb oe rc = do
    rt <- allocateRegister
    instructions <>= [op rt ra rb oe rc]
    return rt

xoop_ :: Monad m => (Register -> Register -> Register -> Bool -> Bool -> A.Inst)
    -> Register 
    -> Register 
    -> Bool 
    -> Bool 
    -> Asm () m
xoop_ op ra rb oe rc = instructions <>= [op ra ra rb oe rc]

xop :: Monad m => (Register -> Register -> Register -> Bool -> A.Inst)
    -> Register 
    -> Register 
    -> Bool 
    -> Asm Register m
xop op ra rb rc = do
    rt <- allocateRegister
    instructions <>= [op rt ra rb rc]
    return rt

xop_ :: Monad m => (Register -> Register -> Register -> Bool -> A.Inst)
    -> Register 
    -> Register 
    -> Bool 
    -> Asm () m
xop_ op ra rb rc = instructions <>= [op ra ra rb rc]

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

fxvop :: Monad m => (VectorRegister -> VectorRegister -> VectorRegister -> A.Fxv_cond -> A.Inst) 
    -> VectorRegister 
    -> VectorRegister 
    -> A.Fxv_cond 
    -> Asm VectorRegister m
fxvop op ra rb cond = do
    rt <- allocateVectorRegister
    instructions <>= [op rt ra rb cond] 
    return rt

fxvop_ :: Monad m => (VectorRegister -> VectorRegister -> VectorRegister -> A.Fxv_cond -> A.Inst) 
       -> VectorRegister 
       -> VectorRegister 
       -> A.Fxv_cond 
       -> Asm () m
fxvop_ op ra rb cond = instructions <>= [op ra ra rb cond]

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
cmp     :: Monad m => Register -> Register -> Bool -> Asm Register m
tw      :: Monad m => Register -> Register -> Bool -> Asm Register m
lwzx    :: Monad m => Register -> Register -> Bool -> Asm Register m
slw     :: Monad m => Register -> Register -> Bool -> Asm Register m
cntlzw  :: Monad m => Register -> Register -> Bool -> Asm Register m
and     :: Monad m => Register -> Register -> Bool -> Asm Register m
cmpl    :: Monad m => Register -> Register -> Bool -> Asm Register m
nvem    :: Monad m => Register -> Register -> Bool -> Asm Register m
nves    :: Monad m => Register -> Register -> Bool -> Asm Register m
nvemtl  :: Monad m => Register -> Register -> Bool -> Asm Register m
lwzux   :: Monad m => Register -> Register -> Bool -> Asm Register m
andc    :: Monad m => Register -> Register -> Bool -> Asm Register m
wait    :: Monad m => Register -> Register -> Bool -> Asm Register m
mfmsr   :: Monad m => Register -> Register -> Bool -> Asm Register m
lbzx    :: Monad m => Register -> Register -> Bool -> Asm Register m
lbzux   :: Monad m => Register -> Register -> Bool -> Asm Register m
popcb   :: Monad m => Register -> Register -> Bool -> Asm Register m
nor     :: Monad m => Register -> Register -> Bool -> Asm Register m
mtmsr   :: Monad m => Register -> Register -> Bool -> Asm Register m
stwx    :: Monad m => Register -> Register -> Bool -> Asm Register m
prtyw   :: Monad m => Register -> Register -> Bool -> Asm Register m
stwux   :: Monad m => Register -> Register -> Bool -> Asm Register m
stbx    :: Monad m => Register -> Register -> Bool -> Asm Register m
stbux   :: Monad m => Register -> Register -> Bool -> Asm Register m
lhzx    :: Monad m => Register -> Register -> Bool -> Asm Register m
eqv     :: Monad m => Register -> Register -> Bool -> Asm Register m
eciwx   :: Monad m => Register -> Register -> Bool -> Asm Register m
lhzux   :: Monad m => Register -> Register -> Bool -> Asm Register m
xor     :: Monad m => Register -> Register -> Bool -> Asm Register m
lhax    :: Monad m => Register -> Register -> Bool -> Asm Register m
lhaux   :: Monad m => Register -> Register -> Bool -> Asm Register m
sthx    :: Monad m => Register -> Register -> Bool -> Asm Register m
orc     :: Monad m => Register -> Register -> Bool -> Asm Register m
ecowx   :: Monad m => Register -> Register -> Bool -> Asm Register m
sthux   :: Monad m => Register -> Register -> Bool -> Asm Register m
or      :: Monad m => Register -> Register -> Bool -> Asm Register m
nand    :: Monad m => Register -> Register -> Bool -> Asm Register m
srw     :: Monad m => Register -> Register -> Bool -> Asm Register m
sync    :: Monad m => Register -> Register -> Bool -> Asm Register m
synm    :: Monad m => Register -> Register -> Bool -> Asm Register m
syns    :: Monad m => Register -> Register -> Bool -> Asm Register m
synmtl  :: Monad m => Register -> Register -> Bool -> Asm Register m
synmtvr :: Monad m => Register -> Register -> Bool -> Asm Register m
synmfvr :: Monad m => Register -> Register -> Bool -> Asm Register m
synmtp  :: Monad m => Register -> Register -> Bool -> Asm Register m
synmfp  :: Monad m => Register -> Register -> Bool -> Asm Register m
synmvvr :: Monad m => Register -> Register -> Bool -> Asm Register m
synops  :: Monad m => Register -> Register -> Bool -> Asm Register m
synswp  :: Monad m => Register -> Register -> Bool -> Asm Register m
sraw    :: Monad m => Register -> Register -> Bool -> Asm Register m
srawi   :: Monad m => Register -> Register -> Bool -> Asm Register m
extsh   :: Monad m => Register -> Register -> Bool -> Asm Register m
extsb   :: Monad m => Register -> Register -> Bool -> Asm Register m
cmp     = xop A.cmp     
tw      = xop A.tw      
lwzx    = xop A.lwzx    
slw     = xop A.slw     
cntlzw  = xop A.cntlzw  
and     = xop A.and     
cmpl    = xop A.cmpl    
nvem    = xop A.nvem    
nves    = xop A.nves    
nvemtl  = xop A.nvemtl  
lwzux   = xop A.lwzux   
andc    = xop A.andc    
wait    = xop A.wait    
mfmsr   = xop A.mfmsr   
lbzx    = xop A.lbzx    
lbzux   = xop A.lbzux   
popcb   = xop A.popcb   
nor     = xop A.nor     
mtmsr   = xop A.mtmsr   
stwx    = xop A.stwx    
prtyw   = xop A.prtyw   
stwux   = xop A.stwux   
stbx    = xop A.stbx    
stbux   = xop A.stbux   
lhzx    = xop A.lhzx    
eqv     = xop A.eqv     
eciwx   = xop A.eciwx   
lhzux   = xop A.lhzux   
xor     = xop A.xor     
lhax    = xop A.lhax    
lhaux   = xop A.lhaux   
sthx    = xop A.sthx    
orc     = xop A.orc     
ecowx   = xop A.ecowx   
sthux   = xop A.sthux   
or      = xop A.or      
nand    = xop A.nand    
srw     = xop A.srw     
sync    = xop A.sync    
synm    = xop A.synm    
syns    = xop A.syns    
synmtl  = xop A.synmtl  
synmtvr = xop A.synmtvr 
synmfvr = xop A.synmfvr 
synmtp  = xop A.synmtp  
synmfp  = xop A.synmfp  
synmvvr = xop A.synmvvr 
synops  = xop A.synops  
synswp  = xop A.synswp  
sraw    = xop A.sraw    
srawi   = xop A.srawi   
extsh   = xop A.extsh   
extsb   = xop A.extsb 
cmp_     :: Monad m => Register -> Register -> Bool -> Asm () m
tw_      :: Monad m => Register -> Register -> Bool -> Asm () m
lwzx_    :: Monad m => Register -> Register -> Bool -> Asm () m
slw_     :: Monad m => Register -> Register -> Bool -> Asm () m
cntlzw_  :: Monad m => Register -> Register -> Bool -> Asm () m
and_     :: Monad m => Register -> Register -> Bool -> Asm () m
cmpl_    :: Monad m => Register -> Register -> Bool -> Asm () m
nvem_    :: Monad m => Register -> Register -> Bool -> Asm () m
nves_    :: Monad m => Register -> Register -> Bool -> Asm () m
nvemtl_  :: Monad m => Register -> Register -> Bool -> Asm () m
lwzux_   :: Monad m => Register -> Register -> Bool -> Asm () m
andc_    :: Monad m => Register -> Register -> Bool -> Asm () m
wait_    :: Monad m => Register -> Register -> Bool -> Asm () m
mfmsr_   :: Monad m => Register -> Register -> Bool -> Asm () m
lbzx_    :: Monad m => Register -> Register -> Bool -> Asm () m
lbzux_   :: Monad m => Register -> Register -> Bool -> Asm () m
popcb_   :: Monad m => Register -> Register -> Bool -> Asm () m
nor_     :: Monad m => Register -> Register -> Bool -> Asm () m
mtmsr_   :: Monad m => Register -> Register -> Bool -> Asm () m
stwx_    :: Monad m => Register -> Register -> Bool -> Asm () m
prtyw_   :: Monad m => Register -> Register -> Bool -> Asm () m
stwux_   :: Monad m => Register -> Register -> Bool -> Asm () m
stbx_    :: Monad m => Register -> Register -> Bool -> Asm () m
stbux_   :: Monad m => Register -> Register -> Bool -> Asm () m
lhzx_    :: Monad m => Register -> Register -> Bool -> Asm () m
eqv_     :: Monad m => Register -> Register -> Bool -> Asm () m
eciwx_   :: Monad m => Register -> Register -> Bool -> Asm () m
lhzux_   :: Monad m => Register -> Register -> Bool -> Asm () m
xor_     :: Monad m => Register -> Register -> Bool -> Asm () m
lhax_    :: Monad m => Register -> Register -> Bool -> Asm () m
lhaux_   :: Monad m => Register -> Register -> Bool -> Asm () m
sthx_    :: Monad m => Register -> Register -> Bool -> Asm () m
orc_     :: Monad m => Register -> Register -> Bool -> Asm () m
ecowx_   :: Monad m => Register -> Register -> Bool -> Asm () m
sthux_   :: Monad m => Register -> Register -> Bool -> Asm () m
or_      :: Monad m => Register -> Register -> Bool -> Asm () m
nand_    :: Monad m => Register -> Register -> Bool -> Asm () m
srw_     :: Monad m => Register -> Register -> Bool -> Asm () m
sync_    :: Monad m => Register -> Register -> Bool -> Asm () m
synm_    :: Monad m => Register -> Register -> Bool -> Asm () m
syns_    :: Monad m => Register -> Register -> Bool -> Asm () m
synmtl_  :: Monad m => Register -> Register -> Bool -> Asm () m
synmtvr_ :: Monad m => Register -> Register -> Bool -> Asm () m
synmfvr_ :: Monad m => Register -> Register -> Bool -> Asm () m
synmtp_  :: Monad m => Register -> Register -> Bool -> Asm () m
synmfp_  :: Monad m => Register -> Register -> Bool -> Asm () m
synmvvr_ :: Monad m => Register -> Register -> Bool -> Asm () m
synops_  :: Monad m => Register -> Register -> Bool -> Asm () m
synswp_  :: Monad m => Register -> Register -> Bool -> Asm () m
sraw_    :: Monad m => Register -> Register -> Bool -> Asm () m
srawi_   :: Monad m => Register -> Register -> Bool -> Asm () m
extsh_   :: Monad m => Register -> Register -> Bool -> Asm () m
extsb_   :: Monad m => Register -> Register -> Bool -> Asm () m
cmp_     = xop_ A.cmp     
tw_      = xop_ A.tw      
lwzx_    = xop_ A.lwzx    
slw_     = xop_ A.slw     
cntlzw_  = xop_ A.cntlzw  
and_     = xop_ A.and     
cmpl_    = xop_ A.cmpl    
nvem_    = xop_ A.nvem    
nves_    = xop_ A.nves    
nvemtl_  = xop_ A.nvemtl  
lwzux_   = xop_ A.lwzux   
andc_    = xop_ A.andc    
wait_    = xop_ A.wait    
mfmsr_   = xop_ A.mfmsr   
lbzx_    = xop_ A.lbzx    
lbzux_   = xop_ A.lbzux   
popcb_   = xop_ A.popcb   
nor_     = xop_ A.nor     
mtmsr_   = xop_ A.mtmsr   
stwx_    = xop_ A.stwx    
prtyw_   = xop_ A.prtyw   
stwux_   = xop_ A.stwux   
stbx_    = xop_ A.stbx    
stbux_   = xop_ A.stbux   
lhzx_    = xop_ A.lhzx    
eqv_     = xop_ A.eqv     
eciwx_   = xop_ A.eciwx   
lhzux_   = xop_ A.lhzux   
xor_     = xop_ A.xor     
lhax_    = xop_ A.lhax    
lhaux_   = xop_ A.lhaux   
sthx_    = xop_ A.sthx    
orc_     = xop_ A.orc     
ecowx_   = xop_ A.ecowx   
sthux_   = xop_ A.sthux   
or_      = xop_ A.or      
nand_    = xop_ A.nand    
srw_     = xop_ A.srw     
sync_    = xop_ A.sync    
synm_    = xop_ A.synm    
syns_    = xop_ A.syns    
synmtl_  = xop_ A.synmtl  
synmtvr_ = xop_ A.synmtvr 
synmfvr_ = xop_ A.synmfvr 
synmtp_  = xop_ A.synmtp  
synmfp_  = xop_ A.synmfp  
synmvvr_ = xop_ A.synmvvr 
synops_  = xop_ A.synops  
synswp_  = xop_ A.synswp  
sraw_    = xop_ A.sraw    
srawi_   = xop_ A.srawi   
extsh_   = xop_ A.extsh   
extsb_   = xop_ A.extsb 

-- XO Instruction Format
subfc  :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
addc   :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
mulhwu :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
subf   :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
mulhw  :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
neg    :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
subfe  :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
adde   :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
subfze :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
addze  :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
subfme :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
addme  :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
mullw  :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
add    :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
divwu  :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
divw   :: Monad m => Register -> Register -> Bool -> Bool -> Asm Register m
subfc  = xoop A.subfc  
addc   = xoop A.addc   
mulhwu = xoop A.mulhwu 
subf   = xoop A.subf   
mulhw  = xoop A.mulhw  
neg    = xoop A.neg    
subfe  = xoop A.subfe  
adde   = xoop A.adde   
subfze = xoop A.subfze 
addze  = xoop A.addze  
subfme = xoop A.subfme 
addme  = xoop A.addme  
mullw  = xoop A.mullw  
add    = xoop A.add    
divwu  = xoop A.divwu  
divw   = xoop A.divw
subfc_  :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
addc_   :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
mulhwu_ :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
subf_   :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
mulhw_  :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
neg_    :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
subfe_  :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
adde_   :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
subfze_ :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
addze_  :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
subfme_ :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
addme_  :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
mullw_  :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
add_    :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
divwu_  :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
divw_   :: Monad m => Register -> Register -> Bool -> Bool -> Asm () m
subfc_  = xoop_ A.subfc  
addc_   = xoop_ A.addc   
mulhwu_ = xoop_ A.mulhwu 
subf_   = xoop_ A.subf   
mulhw_  = xoop_ A.mulhw  
neg_    = xoop_ A.neg    
subfe_  = xoop_ A.subfe  
adde_   = xoop_ A.adde   
subfze_ = xoop_ A.subfze 
addze_  = xoop_ A.addze  
subfme_ = xoop_ A.subfme 
addme_  = xoop_ A.addme  
mullw_  = xoop_ A.mullw  
add_    = xoop_ A.add    
divwu_  = xoop_ A.divwu  
divw_   = xoop_ A.divw


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
fxvmahm       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmabm       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmtacb      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmtach      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmahfs      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmabfs      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmtacbf     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmtachf     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmatachm    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmatacbm    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmatachfs   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmatacbfs   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmulhm      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmulbm      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmulhfs     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmulbfs     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmultachm   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmultacbm   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmultachfs  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvmultacbfs  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvpckbu      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvpckbl      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvupckbr     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvupckbl     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvcmph       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvcmpb       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsel        :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsubhm      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsubbm      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsubhfs     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsubbfs     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddactachm :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddactacb  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddactachf :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddactacbf :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddachm    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddacbm    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddachfs   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddacbfs   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddtachm   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddtacb    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddhm      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddbm      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddhfs     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvaddbfs     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvinx        :: Monad m => Register -> Register -> Asm VectorRegister m
fxvoutx       :: Monad m => VectorRegister -> Register -> Register -> Asm () m
fxvlax        :: Monad m => Register -> Register -> Asm VectorRegister m
fxvstax       :: Monad m => Register -> Register -> Asm VectorRegister m
fxvsplath     :: Monad m => Register             -> Asm VectorRegister m
fxvsplatb     :: Monad m => Register             -> Asm VectorRegister m
fxvshh        :: Monad m => VectorRegister -> Int8 -> A.Fxv_cond -> Asm VectorRegister m
fxvshb        :: Monad m => VectorRegister -> Int8 -> A.Fxv_cond -> Asm VectorRegister m
fxvmahm       = fxvop A.fxvmahm       
fxvmabm       = fxvop A.fxvmabm       
fxvmtacb      = fxvop A.fxvmtacb      
fxvmtach      = fxvop A.fxvmtach      
fxvmahfs      = fxvop A.fxvmahfs      
fxvmabfs      = fxvop A.fxvmabfs      
fxvmtacbf     = fxvop A.fxvmtacbf     
fxvmtachf     = fxvop A.fxvmtachf     
fxvmatachm    = fxvop A.fxvmatachm    
fxvmatacbm    = fxvop A.fxvmatacbm    
fxvmatachfs   = fxvop A.fxvmatachfs   
fxvmatacbfs   = fxvop A.fxvmatacbfs   
fxvmulhm      = fxvop A.fxvmulhm  
fxvmulbm      = fxvop A.fxvmulbm      
fxvmulhfs     = fxvop A.fxvmulhfs     
fxvmulbfs     = fxvop A.fxvmulbfs     
fxvmultachm   = fxvop A.fxvmultachm   
fxvmultacbm   = fxvop A.fxvmultacbm   
fxvmultachfs  = fxvop A.fxvmultachfs
fxvmultacbfs  = fxvop A.fxvmultacbfs
fxvpckbu      = fxvop A.fxvpckbu
fxvpckbl      = fxvop A.fxvpckbl      
fxvupckbr     = fxvop A.fxvupckbr     
fxvupckbl     = fxvop A.fxvupckbl     
fxvcmph       = fxvop A.fxvcmph       
fxvcmpb       = fxvop A.fxvcmpb               
fxvsel        = fxvop A.fxvsel        
fxvsubhm      = fxvop A.fxvsubhm      
fxvsubbm      = fxvop A.fxvsubbm      
fxvsubhfs     = fxvop A.fxvsubhfs     
fxvsubbfs     = fxvop A.fxvsubbfs     
fxvaddactachm = fxvop A.fxvaddactachm 
fxvaddactacb  = fxvop A.fxvaddactacb  
fxvaddactachf = fxvop A.fxvaddactachf 
fxvaddactacbf = fxvop A.fxvaddactacbf 
fxvaddachm    = fxvop A.fxvaddachm    
fxvaddacbm    = fxvop A.fxvaddacbm    
fxvaddachfs   = fxvop A.fxvaddachfs   
fxvaddacbfs   = fxvop A.fxvaddacbfs   
fxvaddtachm   = fxvop A.fxvaddtachm   
fxvaddtacb    = fxvop A.fxvaddtacb    
fxvaddhm      = fxvop A.fxvaddhm      
fxvaddbm      = fxvop A.fxvaddbm      
fxvaddhfs     = fxvop A.fxvaddhfs     
fxvaddbfs     = fxvop A.fxvaddbfs
-- scalar operations     
fxvinx        = fxvsop A.fxvinx
fxvoutx vr ra rb = instructions <>= [A.fxvoutx vr ra rb] 
fxvlax        = fxvsop A.fxvlax
fxvstax       = fxvsop A.fxvstax
fxvsplath ra  = fxvsop (\rt ra rb -> A.fxvsplath rt ra) ra R0
fxvsplatb ra  = fxvsop (\rt ra rb -> A.fxvsplatb rt ra) ra R0
fxvshh        = fxviop A.fxvshh        
fxvshb        = fxviop A.fxvshb

fxvmahm_       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmabm_       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmtacb_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmtach_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmahfs_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmabfs_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmtacbf_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmtachf_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmatachm_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmatacbm_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmatachfs_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmatacbfs_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmulhm_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmulbm_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmulhfs_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmulbfs_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmultachm_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmultacbm_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmultachfs_  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvmultacbfs_  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvpckbu_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvpckbl_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvupckbr_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvupckbl_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvcmph_       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvcmpb_       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvsel_        :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvsubhm_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvsubbm_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvsubhfs_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvsubbfs_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddactachm_ :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddactacb_  :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddactachf_ :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddactacbf_ :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddachm_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddacbm_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddachfs_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddacbfs_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddtachm_   :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddtacb_    :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddhm_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddbm_      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddhfs_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvaddbfs_     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm () m
fxvshh_        :: Monad m => VectorRegister -> Int8 -> A.Fxv_cond -> Asm () m
fxvshb_        :: Monad m => VectorRegister -> Int8 -> A.Fxv_cond -> Asm () m
fxvmahm_       = fxvop_ A.fxvmahm       
fxvmabm_       = fxvop_ A.fxvmabm       
fxvmtacb_      = fxvop_ A.fxvmtacb      
fxvmtach_      = fxvop_ A.fxvmtach      
fxvmahfs_      = fxvop_ A.fxvmahfs      
fxvmabfs_      = fxvop_ A.fxvmabfs      
fxvmtacbf_     = fxvop_ A.fxvmtacbf     
fxvmtachf_     = fxvop_ A.fxvmtachf     
fxvmatachm_    = fxvop_ A.fxvmatachm    
fxvmatacbm_    = fxvop_ A.fxvmatacbm    
fxvmatachfs_   = fxvop_ A.fxvmatachfs   
fxvmatacbfs_   = fxvop_ A.fxvmatacbfs   
fxvmulhm_      = fxvop_ A.fxvmulhm  
fxvmulbm_      = fxvop_ A.fxvmulbm      
fxvmulhfs_     = fxvop_ A.fxvmulhfs     
fxvmulbfs_     = fxvop_ A.fxvmulbfs     
fxvmultachm_   = fxvop_ A.fxvmultachm   
fxvmultacbm_   = fxvop_ A.fxvmultacbm   
fxvmultachfs_  = fxvop_ A.fxvmultachfs
fxvmultacbfs_  = fxvop_ A.fxvmultacbfs
fxvpckbu_      = fxvop_ A.fxvpckbu
fxvpckbl_      = fxvop_ A.fxvpckbl      
fxvupckbr_     = fxvop_ A.fxvupckbr     
fxvupckbl_     = fxvop_ A.fxvupckbl     
fxvcmph_       = fxvop_ A.fxvcmph       
fxvcmpb_       = fxvop_ A.fxvcmpb           
fxvsel_        = fxvop_ A.fxvsel        
fxvsubhm_      = fxvop_ A.fxvsubhm      
fxvsubbm_      = fxvop_ A.fxvsubbm      
fxvsubhfs_     = fxvop_ A.fxvsubhfs     
fxvsubbfs_     = fxvop_ A.fxvsubbfs     
fxvaddactachm_ = fxvop_ A.fxvaddactachm 
fxvaddactacb_  = fxvop_ A.fxvaddactacb  
fxvaddactachf_ = fxvop_ A.fxvaddactachf 
fxvaddactacbf_ = fxvop_ A.fxvaddactacbf 
fxvaddachm_    = fxvop_ A.fxvaddachm    
fxvaddacbm_    = fxvop_ A.fxvaddacbm    
fxvaddachfs_   = fxvop_ A.fxvaddachfs   
fxvaddacbfs_   = fxvop_ A.fxvaddacbfs   
fxvaddtachm_   = fxvop_ A.fxvaddtachm   
fxvaddtacb_    = fxvop_ A.fxvaddtacb    
fxvaddhm_      = fxvop_ A.fxvaddhm      
fxvaddbm_      = fxvop_ A.fxvaddbm      
fxvaddhfs_     = fxvop_ A.fxvaddhfs     
fxvaddbfs_     = fxvop_ A.fxvaddbfs
fxvshh_        = fxviop_ A.fxvshh
fxvshb_        = fxviop_ A.fxvshb