{-# LANGUAGE TemplateHaskell #-}
module Myelin.DLS.PPU.Assembler.Monad where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Lens

import Data.Word

import Myelin.DLS.PPU.Assembler as A

data Label = Label {
    _id :: Int
}

data AsmState = AsmState {
    _labels :: [Label],
    _instructions :: [A.Inst],
    _freeRegisters :: [Register],
    _temporaryRegisters :: [Register],
    _usedRegisters :: [Register]
}

type Asm a = StateT AsmState IO a

makeLenses ''AsmState

allocateRegister :: Asm (Register)
allocateRegister = do
    fr <- use freeRegisters
    case fr of
        [] -> error "Error: No more registers to allocate."
        f:fs -> do
            freeRegisters .= fs
            usedRegisters <>= [f]
            return f

iop :: (Word32 -> Bool -> Bool -> A.Inst)
    -> Word32 
    -> Bool 
    -> Bool 
    -> Asm ()
iop op li aa lk = instructions <>= [op li aa lk]

bop :: (Register -> Register -> Word32 -> Bool -> Bool -> A.Inst) 
    -> Register 
    -> Register
    -> Word32 
    -> Bool 
    -> Bool 
    -> Asm ()
bop op bo bi bd aa lk = instructions <>= [inst]
    where inst = op bo bi bd aa lk

dop :: (Register -> Register -> Word32 -> A.Inst) 
    -> Register 
    -> Word32
    -> Asm Register
dop op ra d = do
    rt <- allocateRegister
    instructions <>= [op rt ra d]
    return rt

xoop :: (Register -> Register -> Register -> Bool -> Bool -> A.Inst)
     -> Register -> Register -> Bool -> Bool -> Asm Register
xoop op ra rb oe rc = do
    rt <- allocateRegister
    instructions <>= [op rt ra rb oe rc]
    return rt

xop :: (Register -> Register -> Register -> Bool -> A.Inst)
    -> Register -> Register -> Bool -> Asm Register
xop op ra rb rc = do
    rt <- allocateRegister
    instructions <>= [op rt ra rb rc]
    return rt

-- mop :: Register -> Register -> Register -> Register -> Register -> Bool -> Asm ()

xfxop :: Register -> SpecialPurposeRegister -> Asm ()
xfxop = undefined

xlop :: (Register -> Register -> Register -> Bool -> A.Inst) 
    -> Register 
    -> Register 
    -> Register 
    -> Bool 
    -> Asm ()
xlop op bt ba bb lk = instructions <>= [op bt ba bb lk]

fxvop :: (Register -> Register -> Register -> A.Fxv_cond -> A.Inst) 
    -> Register 
    -> Register 
    -> A.Fxv_cond 
    -> Asm Register
fxvop op ra rb cond = do
    rt <- allocateRegister
    instructions <>= [op rt ra rb cond] 
    return rt

-- D Instruction Format
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
bc = bop A.bc
branch = iop A.branch
-- D Instruction Format
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
-- FXV Instruction Format
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
fxvinx ra rb  = fxvop (\rt ra rb _ -> A.fxvinx rt ra rb) ra rb A.Fxv_cond_null      
fxvpckbu      = fxvop A.fxvpckbu      
fxvoutx ra rb = fxvop (\rt ra rb _ -> A.fxvoutx rt ra rb) ra rb A.Fxv_cond_null      
fxvpckbl      = fxvop A.fxvpckbl      
fxvsplath ra  = fxvop (\rt ra rb _ -> A.fxvsplath rt ra rb) ra (Register 0) A.Fxv_cond_null
fxvsplatb ra  = fxvop (\rt ra rb _ -> A.fxvsplatb rt ra rb) ra (Register 0) A.Fxv_cond_null
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
fxvlax ra rb  = fxvop (\rt ra rb _ -> A.fxvlax rt ra rb) ra rb A.Fxv_cond_null       
fxvstax ra rb = fxvop (\rt ra rb _ -> A.fxvstax rt ra rb) ra rb A.Fxv_cond_null