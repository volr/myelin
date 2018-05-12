{-# LANGUAGE TemplateHaskell, OverloadedLists #-}
module Myelin.DLS.PPU.Assembler.Monad where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set

import Myelin.DLS.PPU.Assembler as A

data Label = Label {
    _id :: Int
} deriving (Eq, Show)

data AsmState = AsmState {
    _labels :: [Label],
    _instructions :: [A.Inst],
    _freeRegisters :: Set Register,
    _temporaryRegisters :: Set Register,
    _usedRegisters :: Set Register,
    _freeVectorRegisters :: Set VectorRegister,
    _temporaryVectorRegisters :: Set VectorRegister,
    _usedVectorRegisters :: Set VectorRegister,
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

xop :: Monad m => (Register -> Register -> Register -> Bool -> A.Inst)
    -> Register 
    -> Register 
    -> Bool 
    -> Asm Register m
xop op ra rb rc = do
    rt <- allocateRegister
    instructions <>= [op rt ra rb rc]
    return rt

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
fxvinx        :: Monad m => Register -> Register -> Asm VectorRegister m
fxvpckbu      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvoutx       :: Monad m => Register -> Register -> Asm VectorRegister m
fxvpckbl      :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvsplath     :: Monad m => Register                                       -> Asm VectorRegister m
fxvsplatb     :: Monad m => Register                                       -> Asm VectorRegister m
fxvupckbr     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvupckbl     :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvcmph       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvcmpb       :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvshh        :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
fxvshb        :: Monad m => VectorRegister -> VectorRegister -> A.Fxv_cond -> Asm VectorRegister m
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
fxvlax        :: Monad m => Register -> Register -> Asm VectorRegister m
fxvstax       :: Monad m => Register -> Register -> Asm VectorRegister m
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
fxvinx        = fxvsop A.fxvinx
fxvpckbu      = fxvop A.fxvpckbu      
fxvoutx       = fxvsop A.fxvoutx 
fxvpckbl      = fxvop A.fxvpckbl      
fxvsplath ra  = fxvsop (\rt ra rb -> A.fxvsplath rt ra) ra R0
fxvsplatb ra  = fxvsop (\rt ra rb -> A.fxvsplatb rt ra) ra R0
fxvupckbr     = fxvop A.fxvupckbr     
fxvupckbl     = fxvop A.fxvupckbl     
fxvcmph       = fxvop A.fxvcmph       
fxvcmpb       = fxvop A.fxvcmpb       
fxvshh        = fxvop A.fxvshh        
fxvshb        = fxvop A.fxvshb        
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
fxvlax        = fxvsop A.fxvlax
fxvstax       = fxvsop A.fxvstax