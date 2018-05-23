{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Myelin.DLS.PPU.Assembler where

import Data.Int
import Data.Word
import Data.Bits
import Data.Monoid
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as B

data ScalarType = Byte | Halfword
data Arithmetic = Saturating| Modulo

data Register = 
    R0
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    | R16
    | R17
    | R18
    | R19
    | R20
    | R21
    | R22 
    | R23
    | R24
    | R25
    | R26
    | R27
    | R28
    | R29
    | R30
    | R31
    deriving (Eq, Show, Enum, Ord)

encodeRegister :: Register -> Word32
encodeRegister = fromIntegral . fromEnum

data VectorRegister = 
    VR0
    | VR1
    | VR2
    | VR3
    | VR4
    | VR5
    | VR6
    | VR7
    | VR8
    | VR9
    | VR10
    | VR11
    | VR12
    | VR13
    | VR14
    | VR15
    | VR16
    | VR17
    | VR18
    | VR19
    | VR20
    | VR21
    | VR22 
    | VR23
    | VR24
    | VR25
    | VR26
    | VR27
    | VR28
    | VR29
    | VR30
    | VR31
    deriving (Eq, Show, Enum, Ord)

encodeVectorRegister :: VectorRegister -> Word32
encodeVectorRegister = fromIntegral . fromEnum

data SpecialPurposeRegister = SpecialPurposeRegister Word32 deriving (Eq, Show)
encodeSpecialPurposeRegister (SpecialPurposeRegister r) = r

data Opcd = 
    Op_null          
    | Op_twi         
    | Op_nve_xo      
    | Op_nvecmpi     
    | Op_syncmpi     
    | Op_mulli       
    | Op_subfic      
    | Op_syncmpi_rec 
    | Op_cmpli       
    | Op_cmpi        
    | Op_addic       
    | Op_addic_rec   
    | Op_addi        
    | Op_addis       
    | Op_bc          
    | Op_branch      
    | Op_bclr        
    | Op_rlwimi      
    | Op_rlwinm      
    | Op_rlwnm       
    | Op_ori         
    | Op_oris        
    | Op_xori        
    | Op_xoris       
    | Op_andi        
    | Op_andis       
    | Op_alu_xo      
    | Op_lwz         
    | Op_lwzu        
    | Op_lbz         
    | Op_lbzu        
    | Op_stw         
    | Op_stwu        
    | Op_stb         
    | Op_stbu        
    | Op_lhz         
    | Op_lhzu        
    | Op_lha         
    | Op_lhau        
    | Op_sth         
    | Op_sthu        
    | Op_lmw         
    | Op_stmw       
    deriving (Eq, Show) 

opcode Op_null        = 0
opcode Op_twi         = 3
opcode Op_nve_xo      = 4
opcode Op_nvecmpi     = 5
opcode Op_syncmpi     = 6
opcode Op_mulli       = 7
opcode Op_subfic      = 8
opcode Op_syncmpi_rec = 9
opcode Op_cmpli       = 10
opcode Op_cmpi        = 11
opcode Op_addic       = 12
opcode Op_addic_rec   = 13
opcode Op_addi        = 14
opcode Op_addis       = 15
opcode Op_bc          = 16
opcode Op_branch      = 18
opcode Op_bclr        = 19
opcode Op_rlwimi      = 20
opcode Op_rlwinm      = 21
opcode Op_rlwnm       = 23
opcode Op_ori         = 24
opcode Op_oris        = 25
opcode Op_xori        = 26
opcode Op_xoris       = 27
opcode Op_andi        = 28
opcode Op_andis       = 29
opcode Op_alu_xo      = 31
opcode Op_lwz         = 32
opcode Op_lwzu        = 33
opcode Op_lbz         = 34
opcode Op_lbzu        = 35
opcode Op_stw         = 36
opcode Op_stwu        = 37
opcode Op_stb         = 38
opcode Op_stbu        = 39
opcode Op_lhz         = 40
opcode Op_lhzu        = 41
opcode Op_lha         = 42
opcode Op_lhau        = 43
opcode Op_sth         = 44
opcode Op_sthu        = 45
opcode Op_lmw         = 46
opcode Op_stmw        = 47

asm_opcode Op_null        = "null" 
asm_opcode Op_twi         = "twi" 
asm_opcode Op_nve_xo      = "nve_xo" 
asm_opcode Op_nvecmpi     = "nvecmpi" 
asm_opcode Op_syncmpi     = "syncmpi" 
asm_opcode Op_mulli       = "mulli" 
asm_opcode Op_subfic      = "subfic" 
asm_opcode Op_syncmpi_rec = "syncmpi_rec" 
asm_opcode Op_cmpli       = "cmpli" 
asm_opcode Op_cmpi        = "cmpi" 
asm_opcode Op_addic       = "addic" 
asm_opcode Op_addic_rec   = "addic_rec" 
asm_opcode Op_addi        = "addi" 
asm_opcode Op_addis       = "addis" 
asm_opcode Op_bc          = "bc" 
asm_opcode Op_branch      = "branch" 
asm_opcode Op_bclr        = "bclr" 
asm_opcode Op_rlwimi      = "rlwimi" 
asm_opcode Op_rlwinm      = "rlwinm" 
asm_opcode Op_rlwnm       = "rlwnm" 
asm_opcode Op_ori         = "ori" 
asm_opcode Op_oris        = "oris" 
asm_opcode Op_xori        = "xori" 
asm_opcode Op_xoris       = "xoris" 
asm_opcode Op_andi        = "andi" 
asm_opcode Op_andis       = "andis" 
asm_opcode Op_alu_xo      = "alu_xo" 
asm_opcode Op_lwz         = "lwz" 
asm_opcode Op_lwzu        = "lwzu" 
asm_opcode Op_lbz         = "lbz" 
asm_opcode Op_lbzu        = "lbzu" 
asm_opcode Op_stw         = "stw" 
asm_opcode Op_stwu        = "stwu" 
asm_opcode Op_stb         = "stb" 
asm_opcode Op_stbu        = "stbu" 
asm_opcode Op_lhz         = "lhz" 
asm_opcode Op_lhzu        = "lhzu" 
asm_opcode Op_lha         = "lha" 
asm_opcode Op_lhau        = "lhau" 
asm_opcode Op_sth         = "sth" 
asm_opcode Op_sthu        = "sthu" 
asm_opcode Op_lmw         = "lmw" 
asm_opcode Op_stmw        = "stmw" 

twi         rt ra d = D Op_twi         rt ra d
mulli       rt ra d = D Op_mulli       rt ra d
subfic      rt ra d = D Op_subfic      rt ra d 
syncmpi_rec rt ra d = D Op_syncmpi_rec rt ra d 
cmpli       rt ra d = D Op_cmpli       rt ra d 
cmpi        rt ra d = D Op_cmpi        rt ra d 
addic       rt ra d = D Op_addic       rt ra d 
addic_rec   rt ra d = D Op_addic_rec   rt ra d 
addi        rt ra d = D Op_addi        rt ra d 
addis       rt ra d = D Op_addis       rt ra d 
--
bc bo bi bd aa lk = B Op_bc bo bi bd aa lk
branch li aa lk = I Op_branch li aa lk
-- 
rlwimi      rt ra d = D Op_rlwimi      rt ra d 
rlwinm      rt ra d = D Op_rlwinm      rt ra d 
rlwnm       rt ra d = D Op_rlwnm       rt ra d 
ori         rt ra d = D Op_ori         rt ra d 
oris        rt ra d = D Op_oris        rt ra d 
xori        rt ra d = D Op_xori        rt ra d 
xoris       rt ra d = D Op_xoris       rt ra d 
andi        rt ra d = D Op_andi        rt ra d 
andis       rt ra d = D Op_andis       rt ra d
lwz         rt ra d = D Op_lwz         rt ra d
lwzu        rt ra d = D Op_lwzu        rt ra d
lbz         rt ra d = D Op_lbz         rt ra d
lbzu        rt ra d = D Op_lbzu        rt ra d
stw         rt ra d = D Op_stw         rt ra d
stwu        rt ra d = D Op_stwu        rt ra d
stb         rt ra d = D Op_stb         rt ra d
stbu        rt ra d = D Op_stbu        rt ra d
lhz         rt ra d = D Op_lhz         rt ra d
lhzu        rt ra d = D Op_lhzu        rt ra d
lha         rt ra d = D Op_lha         rt ra d
lhau        rt ra d = D Op_lhau        rt ra d
sth         rt ra d = D Op_sth         rt ra d
sthu        rt ra d = D Op_sthu        rt ra d
lmw         rt ra d = D Op_lmw         rt ra d
stmw        rt ra d = D Op_stmw        rt ra d

data X_opcd = 
    Xop_cmp     
    | Xop_tw     
    | Xop_lwzx   
    | Xop_slw    
    | Xop_cntlzw 
    | Xop_and    
    | Xop_cmpl   
    | Xop_nvem   
    | Xop_nves   
    | Xop_nvemtl 
    | Xop_lwzux  
    | Xop_andc   
    | Xop_wait   
    | Xop_mfmsr  
    | Xop_lbzx   
    | Xop_lbzux  
    | Xop_popcb  
    | Xop_nor    
    | Xop_mtmsr  
    | Xop_stwx   
    | Xop_prtyw  
    | Xop_stwux  
    | Xop_stbx   
    | Xop_stbux  
    | Xop_lhzx   
    | Xop_eqv    
    | Xop_eciwx  
    | Xop_lhzux  
    | Xop_xor    
    | Xop_lhax   
    | Xop_lhaux  
    | Xop_sthx   
    | Xop_orc    
    | Xop_ecowx  
    | Xop_sthux  
    | Xop_or     
    | Xop_nand   
    | Xop_srw    
    | Xop_sync   
    | Xop_synm   
    | Xop_syns   
    | Xop_synmtl 
    | Xop_synmtvr
    | Xop_synmfvr
    | Xop_synmtp 
    | Xop_synmfp 
    | Xop_synmvvr
    | Xop_synops 
    | Xop_synswp 
    | Xop_sraw   
    | Xop_srawi  
    | Xop_extsh  
    | Xop_extsb 
    deriving (Eq, Show)

x_opcd :: X_opcd -> Word32
x_opcd Xop_cmp     =   0
x_opcd Xop_tw      =   4
x_opcd Xop_lwzx    =  23
x_opcd Xop_slw     =  24
x_opcd Xop_cntlzw  =  26
x_opcd Xop_and     =  28
x_opcd Xop_cmpl    =  32
x_opcd Xop_nvem    =  48
x_opcd Xop_nves    =  49
x_opcd Xop_nvemtl  =  50
x_opcd Xop_lwzux   =  55
x_opcd Xop_andc    =  60
x_opcd Xop_wait    =  62
x_opcd Xop_mfmsr   =  83
x_opcd Xop_lbzx    =  87
x_opcd Xop_lbzux   = 119
x_opcd Xop_popcb   = 122
x_opcd Xop_nor     = 124
x_opcd Xop_mtmsr   = 146
x_opcd Xop_stwx    = 151
x_opcd Xop_prtyw   = 154
x_opcd Xop_stwux   = 183
x_opcd Xop_stbx    = 215
x_opcd Xop_stbux   = 247
x_opcd Xop_lhzx    = 279
x_opcd Xop_eqv     = 284
x_opcd Xop_eciwx   = 310
x_opcd Xop_lhzux   = 311
x_opcd Xop_xor     = 316
x_opcd Xop_lhax    = 343
x_opcd Xop_lhaux   = 375
x_opcd Xop_sthx    = 407
x_opcd Xop_orc     = 412
x_opcd Xop_ecowx   = 438
x_opcd Xop_sthux   = 439
x_opcd Xop_or      = 444
x_opcd Xop_nand    = 476
x_opcd Xop_srw     = 536
x_opcd Xop_sync    = 598
x_opcd Xop_synm    = 648
x_opcd Xop_syns    = 649
x_opcd Xop_synmtl  = 650
x_opcd Xop_synmtvr = 651
x_opcd Xop_synmfvr = 652
x_opcd Xop_synmtp  = 653
x_opcd Xop_synmfp  = 654
x_opcd Xop_synmvvr = 655
x_opcd Xop_synops  = 656
x_opcd Xop_synswp  = 657
x_opcd Xop_sraw    = 792
x_opcd Xop_srawi   = 824
x_opcd Xop_extsh   = 922
x_opcd Xop_extsb   = 954

x_asm Xop_cmp     = "cmp" 
x_asm Xop_tw      = "tw"
x_asm Xop_lwzx    = "lwzx"
x_asm Xop_slw     = "slw"
x_asm Xop_cntlzw  = "cntlzw"
x_asm Xop_and     = "and"
x_asm Xop_cmpl    = "cmpl"
x_asm Xop_nvem    = "nvem"
x_asm Xop_nves    = "nves"
x_asm Xop_nvemtl  = "nvemtl"
x_asm Xop_lwzux   = "lwzux"
x_asm Xop_andc    = "andc"
x_asm Xop_wait    = "wait"
x_asm Xop_mfmsr   = "mfmsr"
x_asm Xop_lbzx    = "lbzx"
x_asm Xop_lbzux   = "lbzux"
x_asm Xop_popcb   = "popcb"
x_asm Xop_nor     = "nor"
x_asm Xop_mtmsr   = "mtmsr"
x_asm Xop_stwx    = "stwx"
x_asm Xop_prtyw   = "prtyw"
x_asm Xop_stwux   = "stwux"
x_asm Xop_stbx    = "stbx"
x_asm Xop_stbux   = "stbux"
x_asm Xop_lhzx    = "lhzx"
x_asm Xop_eqv     = "eqv"
x_asm Xop_eciwx   = "eciwx"
x_asm Xop_lhzux   = "lhzux"
x_asm Xop_xor     = "xor"
x_asm Xop_lhax    = "lhax"
x_asm Xop_lhaux   = "lhaux"
x_asm Xop_sthx    = "sthx"
x_asm Xop_orc     = "orc"
x_asm Xop_ecowx   = "ecowx"
x_asm Xop_sthux   = "sthux"
x_asm Xop_or      = "or"
x_asm Xop_nand    = "nand"
x_asm Xop_srw     = "srw"
x_asm Xop_sync    = "sync"
x_asm Xop_synm    = "synm"
x_asm Xop_syns    = "syns"
x_asm Xop_synmtl  = "synmtl"
x_asm Xop_synmtvr = "synmtvr"
x_asm Xop_synmfvr = "synmfvr"
x_asm Xop_synmtp  = "synmtp"
x_asm Xop_synmfp  = "synmfp"
x_asm Xop_synmvvr = "synmvvr"
x_asm Xop_synops  = "synops"
x_asm Xop_synswp  = "synswp"
x_asm Xop_sraw    = "sraw"
x_asm Xop_srawi   = "srawi"
x_asm Xop_extsh   = "extsh"
x_asm Xop_extsb   = "extsb"

-- c.f. p. 193-197
cmp     rt ra rb = X Op_alu_xo rt ra rb Xop_cmp     
tw      rt ra rb = X Op_alu_xo rt ra rb Xop_tw      
lwzx    rt ra rb = X Op_alu_xo rt ra rb Xop_lwzx    
slw     rt ra rb = X Op_alu_xo rt ra rb Xop_slw     
cntlzw  rt ra rb = X Op_alu_xo rt ra rb Xop_cntlzw  
and     rt ra rb = X Op_alu_xo rt ra rb Xop_and     
cmpl    rt ra rb = X Op_alu_xo rt ra rb Xop_cmpl    
nvem    rt ra rb = X Op_alu_xo rt ra rb Xop_nvem    
nves    rt ra rb = X Op_alu_xo rt ra rb Xop_nves    
nvemtl  rt ra rb = X Op_alu_xo rt ra rb Xop_nvemtl  
lwzux   rt ra rb = X Op_alu_xo rt ra rb Xop_lwzux   
andc    rt ra rb = X Op_alu_xo rt ra rb Xop_andc    
wait    rt ra rb = X Op_alu_xo rt ra rb Xop_wait    
mfmsr   rt ra rb = X Op_alu_xo rt ra rb Xop_mfmsr   
lbzx    rt ra rb = X Op_alu_xo rt ra rb Xop_lbzx    
lbzux   rt ra rb = X Op_alu_xo rt ra rb Xop_lbzux   
popcb   rt ra rb = X Op_alu_xo rt ra rb Xop_popcb   
nor     rt ra rb = X Op_alu_xo rt ra rb Xop_nor     
mtmsr   rt ra rb = X Op_alu_xo rt ra rb Xop_mtmsr   
stwx    rt ra rb = X Op_alu_xo rt ra rb Xop_stwx    
prtyw   rt ra rb = X Op_alu_xo rt ra rb Xop_prtyw   
stwux   rt ra rb = X Op_alu_xo rt ra rb Xop_stwux   
stbx    rt ra rb = X Op_alu_xo rt ra rb Xop_stbx    
stbux   rt ra rb = X Op_alu_xo rt ra rb Xop_stbux   
lhzx    rt ra rb = X Op_alu_xo rt ra rb Xop_lhzx    
eqv     rt ra rb = X Op_alu_xo rt ra rb Xop_eqv     
eciwx   rt ra rb = X Op_alu_xo rt ra rb Xop_eciwx   
lhzux   rt ra rb = X Op_alu_xo rt ra rb Xop_lhzux   
xor     rt ra rb = X Op_alu_xo rt ra rb Xop_xor     
lhax    rt ra rb = X Op_alu_xo rt ra rb Xop_lhax    
lhaux   rt ra rb = X Op_alu_xo rt ra rb Xop_lhaux   
sthx    rt ra rb = X Op_alu_xo rt ra rb Xop_sthx    
orc     rt ra rb = X Op_alu_xo rt ra rb Xop_orc     
ecowx   rt ra rb = X Op_alu_xo rt ra rb Xop_ecowx   
sthux   rt ra rb = X Op_alu_xo rt ra rb Xop_sthux   
or      rt ra rb = X Op_alu_xo rt ra rb Xop_or      
nand    rt ra rb = X Op_alu_xo rt ra rb Xop_nand    
srw     rt ra rb = X Op_alu_xo rt ra rb Xop_srw     
sync    rt ra rb = X Op_alu_xo rt ra rb Xop_sync    
synm    rt ra rb = X Op_alu_xo rt ra rb Xop_synm    
syns    rt ra rb = X Op_alu_xo rt ra rb Xop_syns    
synmtl  rt ra rb = X Op_alu_xo rt ra rb Xop_synmtl  
synmtvr rt ra rb = X Op_alu_xo rt ra rb Xop_synmtvr 
synmfvr rt ra rb = X Op_alu_xo rt ra rb Xop_synmfvr 
synmtp  rt ra rb = X Op_alu_xo rt ra rb Xop_synmtp  
synmfp  rt ra rb = X Op_alu_xo rt ra rb Xop_synmfp  
synmvvr rt ra rb = X Op_alu_xo rt ra rb Xop_synmvvr 
synops  rt ra rb = X Op_alu_xo rt ra rb Xop_synops  
synswp  rt ra rb = X Op_alu_xo rt ra rb Xop_synswp  
sraw    rt ra rb = X Op_alu_xo rt ra rb Xop_sraw    
srawi   rt ra rb = X Op_alu_xo rt ra rb Xop_srawi   
extsh   rt ra rb = X Op_alu_xo rt ra rb Xop_extsh   
extsb   rt ra rb = X Op_alu_xo rt ra rb Xop_extsb   

data Xo_opcd = 
    Xop_subfc   
    | Xop_addc  
    | Xop_mulhwu
    | Xop_subf  
    | Xop_mulhw 
    | Xop_neg   
    | Xop_subfe 
    | Xop_adde  
    | Xop_subfze
    | Xop_addze 
    | Xop_subfme
    | Xop_addme 
    | Xop_mullw 
    | Xop_add   
    | Xop_divwu 
    | Xop_divw 
    deriving (Eq, Show) 

xo_opcd :: Xo_opcd -> Word32
xo_opcd Xop_subfc  =   8
xo_opcd Xop_addc   =  10
xo_opcd Xop_mulhwu =  11
xo_opcd Xop_subf   =  40
xo_opcd Xop_mulhw  =  75
xo_opcd Xop_neg    = 104
xo_opcd Xop_subfe  = 136
xo_opcd Xop_adde   = 138
xo_opcd Xop_subfze = 200
xo_opcd Xop_addze  = 202
xo_opcd Xop_subfme = 232
xo_opcd Xop_addme  = 234
xo_opcd Xop_mullw  = 235
xo_opcd Xop_add    = 266
xo_opcd Xop_divwu  = 459
xo_opcd Xop_divw   = 491

xo_asm Xop_subfc  = "subfc"
xo_asm Xop_addc   = "addc"
xo_asm Xop_mulhwu = "mulhwu"
xo_asm Xop_subf   = "subf"
xo_asm Xop_mulhw  = "mulhw"
xo_asm Xop_neg    = "neg"
xo_asm Xop_subfe  = "subfe"
xo_asm Xop_adde   = "adde"
xo_asm Xop_subfze = "subfze"
xo_asm Xop_addze  = "addze"
xo_asm Xop_subfme = "subfme"
xo_asm Xop_addme  = "addme"
xo_asm Xop_mullw  = "mullw"
xo_asm Xop_add    = "add"
xo_asm Xop_divwu  = "divwu"
xo_asm Xop_divw   = "divw"

xo_asm_suffix oe rc = (if oe then "o" else "") <> (if rc then "'" else "")

subfc    rt ra rb = XO Op_alu_xo rt ra rb False Xop_subfc False
subfc'   rt ra rb = XO Op_alu_xo rt ra rb False Xop_subfc True
subfco   rt ra rb = XO Op_alu_xo rt ra rb True Xop_subfc False
subfco'  rt ra rb = XO Op_alu_xo rt ra rb True Xop_subfc True  
addc     rt ra rb = XO Op_alu_xo rt ra rb False Xop_addc False
addc'    rt ra rb = XO Op_alu_xo rt ra rb False Xop_addc True
addco    rt ra rb = XO Op_alu_xo rt ra rb True Xop_addc False
addco'   rt ra rb = XO Op_alu_xo rt ra rb True Xop_addc True   
mulhwu   rt ra rb = XO Op_alu_xo rt ra rb False Xop_mulhwu False
mulhwu'  rt ra rb = XO Op_alu_xo rt ra rb False Xop_mulhwu True
mulhwuo  rt ra rb = XO Op_alu_xo rt ra rb True Xop_mulhwu False
mulhwuo' rt ra rb = XO Op_alu_xo rt ra rb True Xop_mulhwu True 
subf     rt ra rb = XO Op_alu_xo rt ra rb False Xop_subf False
subf'    rt ra rb = XO Op_alu_xo rt ra rb False Xop_subf True
subfo    rt ra rb = XO Op_alu_xo rt ra rb True Xop_subf False
subfo'   rt ra rb = XO Op_alu_xo rt ra rb True Xop_subf True   
mulhw    rt ra rb = XO Op_alu_xo rt ra rb False Xop_mulhw False
mulhw'   rt ra rb = XO Op_alu_xo rt ra rb False Xop_mulhw True
mulhwo   rt ra rb = XO Op_alu_xo rt ra rb True Xop_mulhw False
mulhwo'  rt ra rb = XO Op_alu_xo rt ra rb True Xop_mulhw True  
neg      rt ra rb = XO Op_alu_xo rt ra rb False Xop_neg False
neg'     rt ra rb = XO Op_alu_xo rt ra rb False Xop_neg True
nego     rt ra rb = XO Op_alu_xo rt ra rb True Xop_neg False
nego'    rt ra rb = XO Op_alu_xo rt ra rb True Xop_neg True    
subfe    rt ra rb = XO Op_alu_xo rt ra rb False Xop_subfe False
subfe'   rt ra rb = XO Op_alu_xo rt ra rb False Xop_subfe True
subfeo   rt ra rb = XO Op_alu_xo rt ra rb True Xop_subfe False
subfeo'  rt ra rb = XO Op_alu_xo rt ra rb True Xop_subfe True  
adde     rt ra rb = XO Op_alu_xo rt ra rb False Xop_adde False
adde'    rt ra rb = XO Op_alu_xo rt ra rb False Xop_adde True
addeo    rt ra rb = XO Op_alu_xo rt ra rb True Xop_adde False
addeo'   rt ra rb = XO Op_alu_xo rt ra rb True Xop_adde True   
subfze   rt ra rb = XO Op_alu_xo rt ra rb False Xop_subfze False
subfze'  rt ra rb = XO Op_alu_xo rt ra rb False Xop_subfze True
subfzeo  rt ra rb = XO Op_alu_xo rt ra rb True Xop_subfze False
subfzeo' rt ra rb = XO Op_alu_xo rt ra rb True Xop_subfze True 
addze    rt ra rb = XO Op_alu_xo rt ra rb False Xop_addze False
addze'   rt ra rb = XO Op_alu_xo rt ra rb False Xop_addze True
addzeo   rt ra rb = XO Op_alu_xo rt ra rb True Xop_addze False
addzeo'  rt ra rb = XO Op_alu_xo rt ra rb True Xop_addze True  
subfme   rt ra rb = XO Op_alu_xo rt ra rb False Xop_subfme False
subfme'  rt ra rb = XO Op_alu_xo rt ra rb False Xop_subfme True
subfmeo  rt ra rb = XO Op_alu_xo rt ra rb True Xop_subfme False
subfmeo' rt ra rb = XO Op_alu_xo rt ra rb True Xop_subfme True 
addme    rt ra rb = XO Op_alu_xo rt ra rb False Xop_addme False
addme'   rt ra rb = XO Op_alu_xo rt ra rb False Xop_addme True
addmeo   rt ra rb = XO Op_alu_xo rt ra rb True Xop_addme False
addmeo'  rt ra rb = XO Op_alu_xo rt ra rb True Xop_addme True  
mullw    rt ra rb = XO Op_alu_xo rt ra rb False Xop_mullw False
mullw'   rt ra rb = XO Op_alu_xo rt ra rb False Xop_mullw True
mullwo   rt ra rb = XO Op_alu_xo rt ra rb True Xop_mullw False
mullwo'  rt ra rb = XO Op_alu_xo rt ra rb True Xop_mullw True  
add      rt ra rb = XO Op_alu_xo rt ra rb False Xop_add False
add'     rt ra rb = XO Op_alu_xo rt ra rb False Xop_add True
addo     rt ra rb = XO Op_alu_xo rt ra rb True Xop_add False
addo'    rt ra rb = XO Op_alu_xo rt ra rb True Xop_add True    
divwu    rt ra rb = XO Op_alu_xo rt ra rb False Xop_divwu False
divwu'   rt ra rb = XO Op_alu_xo rt ra rb False Xop_divwu True
divwuo   rt ra rb = XO Op_alu_xo rt ra rb True Xop_divwu False
divwuo'  rt ra rb = XO Op_alu_xo rt ra rb True Xop_divwu True  
divw     rt ra rb = XO Op_alu_xo rt ra rb False Xop_divw False
divw'    rt ra rb = XO Op_alu_xo rt ra rb False Xop_divw True
divwo    rt ra rb = XO Op_alu_xo rt ra rb True Xop_divw False
divwo'   rt ra rb = XO Op_alu_xo rt ra rb True Xop_divw True   

data Xl_opcd = 
    Xxop_mcrf     
    | Xxop_bclr   
    | Xxop_crnor  
    | Xxop_rfmci  
    | Xxop_rfi    
    | Xxop_rfci   
    | Xxop_crandc 
    | Xxop_crxor  
    | Xxop_crnand 
    | Xxop_creqv  
    | Xxop_crand  
    | Xxop_crorc  
    | Xxop_cror   
    | Xxop_bcctr  
    deriving (Eq, Show)

xl_opcd Xxop_mcrf   =   0
xl_opcd Xxop_bclr   =  16
xl_opcd Xxop_crnor  =  33
xl_opcd Xxop_rfmci  =  38
xl_opcd Xxop_rfi    =  50
xl_opcd Xxop_rfci   =  51
xl_opcd Xxop_crandc = 129
xl_opcd Xxop_crxor  = 193
xl_opcd Xxop_crnand = 225
xl_opcd Xxop_creqv  = 289
xl_opcd Xxop_crand  = 257
xl_opcd Xxop_crorc  = 417
xl_opcd Xxop_cror   = 449
xl_opcd Xxop_bcctr  = 528

xl_asm Xxop_mcrf   = "mcrf"
xl_asm Xxop_bclr   = "bclr"
xl_asm Xxop_crnor  = "crnor"
xl_asm Xxop_rfmci  = "rfmci"
xl_asm Xxop_rfi    = "rfi"
xl_asm Xxop_rfci   = "rfci"
xl_asm Xxop_crandc = "crandc"
xl_asm Xxop_crxor  = "crxor"
xl_asm Xxop_crnand = "crnand"
xl_asm Xxop_creqv  = "creqv"
xl_asm Xxop_crand  = "crand"
xl_asm Xxop_crorc  = "crorc"
xl_asm Xxop_cror   = "cror"
xl_asm Xxop_bcctr  = "bcctr"

mcrf   bt ba bb = XL Op_bclr bt ba bb Xxop_mcrf
bclr   bt ba bb = XL Op_bclr bt ba bb Xxop_bclr
crnor  bt ba bb = XL Op_bclr bt ba bb Xxop_crnor
rfmci  bt ba bb = XL Op_bclr bt ba bb Xxop_rfmci
rfi    bt ba bb = XL Op_bclr bt ba bb Xxop_rfi
rfci   bt ba bb = XL Op_bclr bt ba bb Xxop_rfci
crandc bt ba bb = XL Op_bclr bt ba bb Xxop_crandc
crxor  bt ba bb = XL Op_bclr bt ba bb Xxop_crxor
crnand bt ba bb = XL Op_bclr bt ba bb Xxop_crnand
creqv  bt ba bb = XL Op_bclr bt ba bb Xxop_creqv
crand  bt ba bb = XL Op_bclr bt ba bb Xxop_crand
crorc  bt ba bb = XL Op_bclr bt ba bb Xxop_crorc
cror   bt ba bb = XL Op_bclr bt ba bb Xxop_cror
bcctr  bt ba bb = XL Op_bclr bt ba bb Xxop_bcctr

data Xfx_opcd = 
    Xop_mfocrf  
    | Xop_mtocrf
    | Xop_mfspr 
    | Xop_mtspr 
    deriving (Eq, Show)

xfx_opcd Xop_mfocrf = 19
xfx_opcd Xop_mtocrf = 144
xfx_opcd Xop_mfspr  = 339
xfx_opcd Xop_mtspr  = 467

xfx_asm Xop_mfocrf = "mfocrf"
xfx_asm Xop_mtocrf = "mtocrf"
xfx_asm Xop_mfspr  = "mfspr"
xfx_asm Xop_mtspr  = "mtspr"

mfocrf rt spr = XFX Op_alu_xo rt spr Xop_mfocrf
mtocrf rt spr = XFX Op_alu_xo rt spr Xop_mtocrf
mfspr  rt spr = XFX Op_alu_xo rt spr Xop_mfspr
mtspr  rt spr = XFX Op_alu_xo rt spr Xop_mtspr

data Fxv_opcd = 
    Xop_fxvmahm         -- ^ fixed-vector-multiply-accumulate-halfword-modulo
    | Xop_fxvmabm       -- ^ fixed-vector-multiply-accumulate-byte-modulo
    | Xop_fxvmtacb      -- ^ fixed-vector-move-to-accumulator-byte
    | Xop_fxvmtach      -- ^ fixed-vector-move-to-accumulator-halfword
    | Xop_fxvmahfs      -- ^ fixed-vector-multiply-accumulate-halfword-fractional-saturating
    | Xop_fxvmabfs      -- ^ fixed-vector-multiply-accumulate-byte-fractional-saturating
    | Xop_fxvmtacbf     -- ^ fixed-vector-move-to-accumulator-byte-fractional
    | Xop_fxvmtachf     -- ^ fixed-vector-move-to-accumulator-halfword-fractional
    | Xop_fxvmatachm    -- ^ fixed-vector-multiply-accumulate-to-accumulator-halfword-modulo
    | Xop_fxvmatacbm    -- ^ fixed-vector-multiply-accumulate-save-to-accumulator-byte-modulo
    | Xop_fxvmatachfs   -- ^ fixed-vector-multiply-accumulate-and-save-to-accumulator-halfword-fractional-saturating
    | Xop_fxvmatacbfs   -- ^ fixed-vector-multiply-accumulate-and-save-to-accumulator-byte-fractional-saturating
    | Xop_fxvmulhm      -- ^ fixed-vector-multiply-halfword-modulo
    | Xop_fxvmulbm      -- ^ fixed-vector-multiply-byte-modulo
    | Xop_fxvmulhfs     -- ^ fixed-vector-multiply-halfword-fractional-saturating
    | Xop_fxvmulbfs     -- ^ fixed-vector-multiply-byte-fractional-saturating
    | Xop_fxvmultachm   -- ^ fixed-vector-multiply-save-to-accumulator-halfword-modulo
    | Xop_fxvmultacbm   -- ^ fixed-vector-multiply-save-to-accumulator-byte-modulo
    | Xop_fxvmultachfs  -- ^ fixed-vector-multiply-save-to-accumulator-halfword-fractional-saturating
    | Xop_fxvmultacbfs  -- ^ fixed-vector-multiply-save-to-accumulator-byte-fractional-saturating
    | Xop_fxvinx        -- ^ fixed-vector-in-indexed
    | Xop_fxvpckbu      -- ^ fixed-vector-pack-byte-upper
    | Xop_fxvoutx       -- ^ fixed-vector-out-indexed
    | Xop_fxvpckbl      -- ^ fixed-vector-pack-byte-lower
    | Xop_fxvsplath     -- ^ fixed-vector-splat-halfword
    | Xop_fxvsplatb     -- ^ fixed-vector-splat-byte
    | Xop_fxvupckbr     -- ^ fixed-vector-unpack-byte-right
    | Xop_fxvupckbl     -- ^ fixed-vector-unpack-byte-left
    | Xop_fxvcmph       -- ^ fixed-vector-compare-halfword
    | Xop_fxvcmpb       -- ^ fixed-vector-compare-byte
    | Xop_fxvshh        -- ^ fixed-vector-shift-halfword
    | Xop_fxvshb        -- ^ fixed-vector-shift-byte
    | Xop_fxvsel        -- ^ fixed-vector-select
    | Xop_fxvsubhm      -- ^ fixed-vector-subtract-halfword-modulo
    | Xop_fxvsubbm      -- ^ fixed-vector-subtract-byte-modulo
    | Xop_fxvsubhfs     -- ^ fixed-vector-subtract-halfword-fractional-saturating
    | Xop_fxvsubbfs     -- ^ fixed-vector-subtract-byte-fractional-saturating
    | Xop_fxvaddactachm -- ^ fixed-vector-add-accumulator-save-to-accumulator-halfword-modulo
    | Xop_fxvaddactacb  -- ^ fixed-vector-add-accumulator-save-to-accumulator-byte
    | Xop_fxvaddactachf -- ^ fixed-vector-add-accumulator-save-to-accumulator-halfword-fractional
    | Xop_fxvaddactacbf -- ^ fixed-vector-add-accumulator-save-to-accumulator-byte-fractional
    | Xop_fxvaddachm    -- ^ fixed-vector-add-accumulator-halfword-modulo
    | Xop_fxvaddacbm    -- ^ fixed-vector-add-accumulator-byte-modulo
    | Xop_fxvaddachfs   -- ^ fixed-vector-add-accumulator-halfword-fractional-saturating
    | Xop_fxvaddacbfs   -- ^ fixed-vector-add-accumulator-byte-fractional-saturating
    | Xop_fxvaddtachm   -- ^ fixed-vector-add-and-save-to-accumulator-halfword-modulo
    | Xop_fxvaddtacb    -- ^ fixed-vector-add-and-save-to-accumulator-byte-modulo
    | Xop_fxvaddhm      -- ^ fixed-vector-add-halfword-modulo
    | Xop_fxvaddbm      -- ^ fixed-vector-add-byte-modulo
    | Xop_fxvaddhfs     -- ^ fixed-vector-add-halfword-fractional-saturating
    | Xop_fxvaddbfs     -- ^ fixed-vector-add-byte-fractional-saturating
    | Xop_fxvlax        -- ^ fixed-vector-load-array-indexed
    | Xop_fxvstax       -- ^ fixed-vector-store-array-indexed
    deriving (Eq, Show)

fxv_opcd Xop_fxvmahm       =  12
fxv_opcd Xop_fxvmabm       =  13
fxv_opcd Xop_fxvmtacb      =  14
fxv_opcd Xop_fxvmtach      =  15
fxv_opcd Xop_fxvmahfs      =  28
fxv_opcd Xop_fxvmabfs      =  29
fxv_opcd Xop_fxvmtacbf     =  30
fxv_opcd Xop_fxvmtachf     =  31
fxv_opcd Xop_fxvmatachm    =  44
fxv_opcd Xop_fxvmatacbm    =  45
fxv_opcd Xop_fxvmatachfs   =  60
fxv_opcd Xop_fxvmatacbfs   =  61
fxv_opcd Xop_fxvmulhm      =  76
fxv_opcd Xop_fxvmulbm      =  77
fxv_opcd Xop_fxvmulhfs     =  92
fxv_opcd Xop_fxvmulbfs     =  93
fxv_opcd Xop_fxvmultachm   = 108
fxv_opcd Xop_fxvmultacbm   = 109
fxv_opcd Xop_fxvmultachfs  = 124
fxv_opcd Xop_fxvmultacbfs  = 125
fxv_opcd Xop_fxvinx        = 236
fxv_opcd Xop_fxvpckbu      = 239
fxv_opcd Xop_fxvoutx       = 252
fxv_opcd Xop_fxvpckbl      = 255
fxv_opcd Xop_fxvsplath     = 268
fxv_opcd Xop_fxvsplatb     = 269
fxv_opcd Xop_fxvupckbr     = 271
fxv_opcd Xop_fxvupckbl     = 287
fxv_opcd Xop_fxvcmph       = 300
fxv_opcd Xop_fxvcmpb       = 301
fxv_opcd Xop_fxvshh        = 316
fxv_opcd Xop_fxvshb        = 317
fxv_opcd Xop_fxvsel        = 319
fxv_opcd Xop_fxvsubhm      = 332
fxv_opcd Xop_fxvsubbm      = 333
fxv_opcd Xop_fxvsubhfs     = 348
fxv_opcd Xop_fxvsubbfs     = 349
fxv_opcd Xop_fxvaddactachm = 364
fxv_opcd Xop_fxvaddactacb  = 365
fxv_opcd Xop_fxvaddactachf = 380
fxv_opcd Xop_fxvaddactacbf = 381
fxv_opcd Xop_fxvaddachm    = 396
fxv_opcd Xop_fxvaddacbm    = 397
fxv_opcd Xop_fxvaddachfs   = 412
fxv_opcd Xop_fxvaddacbfs   = 413
fxv_opcd Xop_fxvaddtachm   = 428
fxv_opcd Xop_fxvaddtacb    = 429
fxv_opcd Xop_fxvaddhm      = 460
fxv_opcd Xop_fxvaddbm      = 461
fxv_opcd Xop_fxvaddhfs     = 476
fxv_opcd Xop_fxvaddbfs     = 477
fxv_opcd Xop_fxvlax        = 492
fxv_opcd Xop_fxvstax       = 508

fxv_asm Xop_fxvmahm       = "fxvmahm"
fxv_asm Xop_fxvmabm       = "fxvmabm"
fxv_asm Xop_fxvmtacb      = "fxvmtacb"
fxv_asm Xop_fxvmtach      = "fxvmtach"
fxv_asm Xop_fxvmahfs      = "fxvmahfs"
fxv_asm Xop_fxvmabfs      = "fxvmabfs"
fxv_asm Xop_fxvmtacbf     = "fxvmtacbf"
fxv_asm Xop_fxvmtachf     = "fxvmtachf"
fxv_asm Xop_fxvmatachm    = "fxvmatachm"
fxv_asm Xop_fxvmatacbm    = "fxvmatacbm"
fxv_asm Xop_fxvmatachfs   = "fxvmatachfs"
fxv_asm Xop_fxvmatacbfs   = "fxvmatacbfs"
fxv_asm Xop_fxvmulhm      = "fxvmulhm"
fxv_asm Xop_fxvmulbm      = "fxvmulbm"
fxv_asm Xop_fxvmulhfs     = "fxvmulhfs"
fxv_asm Xop_fxvmulbfs     = "fxvmulbfs"
fxv_asm Xop_fxvmultachm   = "fxvmultachm"
fxv_asm Xop_fxvmultacbm   = "fxvmultacbm"
fxv_asm Xop_fxvmultachfs  = "fxvmultachfs"
fxv_asm Xop_fxvmultacbfs  = "fxvmultacbfs"
fxv_asm Xop_fxvinx        = "fxvinx"
fxv_asm Xop_fxvpckbu      = "fxvpckbu"
fxv_asm Xop_fxvoutx       = "fxvoutx"
fxv_asm Xop_fxvpckbl      = "fxvpckbl"
fxv_asm Xop_fxvsplath     = "fxvsplath"
fxv_asm Xop_fxvsplatb     = "fxvsplatb"
fxv_asm Xop_fxvupckbr     = "fxvupckbr"
fxv_asm Xop_fxvupckbl     = "fxvupckbl"
fxv_asm Xop_fxvcmph       = "fxvcmph"
fxv_asm Xop_fxvcmpb       = "fxvcmpb"
fxv_asm Xop_fxvshh        = "fxvshh"
fxv_asm Xop_fxvshb        = "fxvshb"
fxv_asm Xop_fxvsel        = "fxvsel"
fxv_asm Xop_fxvsubhm      = "fxvsubhm"
fxv_asm Xop_fxvsubbm      = "fxvsubbm"
fxv_asm Xop_fxvsubhfs     = "fxvsubhfs"
fxv_asm Xop_fxvsubbfs     = "fxvsubbfs"
fxv_asm Xop_fxvaddactachm = "fxvaddactachm"
fxv_asm Xop_fxvaddactacb  = "fxvaddactacb"
fxv_asm Xop_fxvaddactachf = "fxvaddactachf"
fxv_asm Xop_fxvaddactacbf = "fxvaddactacbf"
fxv_asm Xop_fxvaddachm    = "fxvaddachm"
fxv_asm Xop_fxvaddacbm    = "fxvaddacbm"
fxv_asm Xop_fxvaddachfs   = "fxvaddachfs"
fxv_asm Xop_fxvaddacbfs   = "fxvaddacbfs"
fxv_asm Xop_fxvaddtachm   = "fxvaddtachm"
fxv_asm Xop_fxvaddtacb    = "fxvaddtacb"
fxv_asm Xop_fxvaddhm      = "fxvaddhm"
fxv_asm Xop_fxvaddbm      = "fxvaddbm"
fxv_asm Xop_fxvaddhfs     = "fxvaddhfs"
fxv_asm Xop_fxvaddbfs     = "fxvaddbfs"
fxv_asm Xop_fxvlax        = "fxvlax"
fxv_asm Xop_fxvstax       = "fxvstax"

data Fxv_cond = 
    Fxv_cond_null
    | Fxv_cond_gt
    | Fxv_cond_lt
    | Fxv_cond_eq
    deriving (Eq, Show)

fxv_cond cond = case cond of
    Fxv_cond_null -> 0
    Fxv_cond_gt -> 1
    Fxv_cond_lt -> 2 
    Fxv_cond_eq -> 3

fxvmahm        rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmahm       Fxv_cond_null
fxvmahm'       rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmahm       cond
fxvmabm        rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmabm       Fxv_cond_null
fxvmabm'       rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmabm       cond
fxvmtacb       rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmtacb      Fxv_cond_null
fxvmtacb'      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmtacb      cond
fxvmtach       rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmtach      Fxv_cond_null
fxvmtach'      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmtach      cond
fxvmahfs       rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmahfs      Fxv_cond_null
fxvmahfs'      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmahfs      cond
fxvmabfs       rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmabfs      Fxv_cond_null
fxvmabfs'      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmabfs      cond
fxvmtacbf      rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmtacbf     Fxv_cond_null
fxvmtacbf'     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmtacbf     cond
fxvmtachf      rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmtachf     Fxv_cond_null
fxvmtachf'     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmtachf     cond
fxvmatachm     rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmatachm    Fxv_cond_null
fxvmatachm'    rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmatachm    cond
fxvmatacbm     rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmatacbm    Fxv_cond_null
fxvmatacbm'    rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmatacbm    cond
fxvmatachfs    rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmatachfs   Fxv_cond_null
fxvmatachfs'   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmatachfs   cond
fxvmatacbfs    rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmatacbfs   Fxv_cond_null
fxvmatacbfs'   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmatacbfs   cond
fxvmulhm       rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmulhm      Fxv_cond_null
fxvmulhm'      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmulhm      cond
fxvmulbm       rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmulbm      Fxv_cond_null
fxvmulbm'      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmulbm      cond
fxvmulhfs      rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmulhfs     Fxv_cond_null
fxvmulhfs'     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmulhfs     cond
fxvmulbfs      rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmulbfs     Fxv_cond_null
fxvmulbfs'     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmulbfs     cond
fxvmultachm    rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmultachm   Fxv_cond_null
fxvmultachm'   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmultachm   cond
fxvmultacbm    rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmultacbm   Fxv_cond_null
fxvmultacbm'   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmultacbm   cond
fxvmultachfs   rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmultachfs  Fxv_cond_null
fxvmultachfs'  rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmultachfs  cond
fxvmultacbfs   rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvmultacbfs  Fxv_cond_null
fxvmultacbfs'  rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmultacbfs  cond
fxvinx        rt ra rb      = FXVS Op_nve_xo rt ra rb Xop_fxvinx
fxvpckbu      rt ra rb cond = FXV Op_nve_xo rt ra rb  Xop_fxvpckbu      cond
fxvoutx       rt ra rb      = FXVS Op_nve_xo rt ra rb Xop_fxvoutx
fxvpckbl      rt ra rb cond = FXV Op_nve_xo rt ra rb  Xop_fxvpckbl      cond
fxvsplath     rt ra         = FXVS Op_nve_xo rt ra R0 Xop_fxvsplath
fxvsplatb     rt ra         = FXVS Op_nve_xo rt ra R0 Xop_fxvsplatb
fxvupckbr     rt ra rb cond = FXV Op_nve_xo rt ra rb  Xop_fxvupckbr     cond
fxvupckbl     rt ra rb cond = FXV Op_nve_xo rt ra rb  Xop_fxvupckbl     cond
fxvcmph       rt ra rb cond = FXV Op_nve_xo rt ra rb  Xop_fxvcmph       cond
fxvcmpb       rt ra rb cond = FXV Op_nve_xo rt ra rb  Xop_fxvcmpb       cond
-- 
fxvshh        rt ra imm cond = FXVI Op_nve_xo rt ra imm Xop_fxvshh        cond
fxvshb        rt ra imm cond = FXVI Op_nve_xo rt ra imm Xop_fxvshb        cond
--
fxvsel        rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsel        cond
fxvsubhm       rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvsubhm      Fxv_cond_null
fxvsubhm'      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsubhm      cond
fxvsubbm       rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvsubbm      Fxv_cond_null
fxvsubbm'      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsubbm      cond
fxvsubhfs      rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvsubhfs     Fxv_cond_null
fxvsubhfs'     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsubhfs     cond
fxvsubbfs      rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvsubbfs     Fxv_cond_null
fxvsubbfs'     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsubbfs     cond
fxvaddactachm  rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddactachm Fxv_cond_null
fxvaddactachm' rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddactachm cond
fxvaddactacb   rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddactacb  Fxv_cond_null
fxvaddactacb'  rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddactacb  cond
fxvaddactachf  rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddactachf Fxv_cond_null
fxvaddactachf' rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddactachf cond
fxvaddactacbf  rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddactacbf Fxv_cond_null
fxvaddactacbf' rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddactacbf cond
fxvaddachm     rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddachm    Fxv_cond_null
fxvaddachm'    rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddachm    cond
fxvaddacbm     rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddacbm    Fxv_cond_null
fxvaddacbm'    rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddacbm    cond
fxvaddachfs    rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddachfs   Fxv_cond_null
fxvaddachfs'   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddachfs   cond
fxvaddacbfs    rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddacbfs   Fxv_cond_null
fxvaddacbfs'   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddacbfs   cond
fxvaddtachm    rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddtachm   Fxv_cond_null
fxvaddtachm'   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddtachm   cond
fxvaddtacb     rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddtacb    Fxv_cond_null
fxvaddtacb'    rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddtacb    cond
fxvaddhm       rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddhm      Fxv_cond_null
fxvaddhm'      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddhm      cond
fxvaddbm       rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddbm      Fxv_cond_null
fxvaddbm'      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddbm      cond
fxvaddhfs      rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddhfs     Fxv_cond_null
fxvaddhfs'     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddhfs     cond
fxvaddbfs      rt ra rb      = FXV Op_nve_xo rt ra rb Xop_fxvaddbfs     Fxv_cond_null
fxvaddbfs'     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddbfs     cond
fxvlax        rt ra rb      = FXVS Op_nve_xo rt ra rb Xop_fxvlax
fxvstax       rt ra rb      = FXVS Op_nve_xo rt ra rb Xop_fxvstax

-- Instruction formats supported by the PPU
data Inst =
    -- page 8
    I {
        _opcd :: Opcd, -- opcode
        _li :: Word32, -- load immediate value
        _aa :: Bool,
        _lk :: Bool
    } 
    | B {
        _opcd :: Opcd,
        _bo :: Register,
        _bi :: Register,
        _bd :: Word32,
        _aa :: Bool,
        _lk :: Bool
    }
    | D {
        _opcd :: Opcd,
        _rt :: Register,
        _ra :: Register,
        _d :: Word32
    }
    -- page 9
    | XO {
        _opcd :: Opcd, 
        _rt :: Register,
        _ra :: Register,
        _rb :: Register,
        _oe :: Bool,
        _xo :: Xo_opcd,
        _rc :: Bool
    }
    | X {
        _opcd :: Opcd,
        _rt :: Register,
        _ra :: Register,
        _rb :: Register,
        _x  :: X_opcd,
        _rc :: Bool
    }
    -- page 10
    | M {
        _opcd :: Opcd,
        _rs :: Register,
        _ra :: Register,
        _rb :: Register,
        _mb :: Register,
        _me :: Register,
        _rc :: Bool
    }
    | XFX {
        _opcd :: Opcd,
        _rt :: Register, 
        _spr :: SpecialPurposeRegister,
        _xfx :: Xfx_opcd
    }
    | XL {
        _opcd :: Opcd,
        _bt :: Register,
        _ba :: Register,
        _bb :: Register,
        _xl :: Xl_opcd,
        _lk :: Bool
    }
    -- extension for the ppu
    | FXV {
        _opcd :: Opcd,
        _vrt :: VectorRegister,
        _vra :: VectorRegister,
        _vrb :: VectorRegister,
        _fxv :: Fxv_opcd,
        _cond :: Fxv_cond 
    }
    | FXVI {
        _opcd :: Opcd,
        _vrt :: VectorRegister,
        _vra :: VectorRegister,
        _imm :: Int8,
        _fxv :: Fxv_opcd,
        _cond :: Fxv_cond 
    }
    | FXVS {
        _opcd :: Opcd,
        _vrt :: VectorRegister,
        _ra :: Register,
        _rb :: Register,
        _fxv :: Fxv_opcd
    } deriving (Eq, Show)

encode :: Inst -> Word32
encode inst = case inst of
    I {..} -> (opcode _opcd) 
          .|. (_li `shift` 6) 
          .|. (if _aa then 1 `shift` 30 else 0) 
          .|. (if _lk then 1 `shift` 31 else 0)
    B {..} -> (opcode _opcd) 
          .|. (encodeRegister _bo `shift` 6) 
          .|. (encodeRegister _bi `shift` 11)
          .|. (_bd `shift` 16)
          .|. (if _aa then 1 `shift` 30 else 0)
          .|. (if _lk then 1 `shift` 31 else 0)
    D {..} -> (opcode _opcd) 
          .|. (encodeRegister _rt `shift` 6) 
          .|. (encodeRegister _ra `shift` 11)
          .|. (_d `shift` 16)
    XO {..} -> (opcode _opcd)
          .|. (encodeRegister _rt `shift` 6)
          .|. (encodeRegister _ra `shift` 11)
          .|. (encodeRegister _rb `shift` 16)
          .|. (if _oe then 1 `shift` 21 else 0)
          .|. (xo_opcd _xo `shift` 22)
          .|. (if _rc then 1 `shift` 31 else 0)
    X {..} -> (opcode _opcd)
          .|. (encodeRegister _rt `shift` 6)
          .|. (encodeRegister _ra `shift` 11)
          .|. (encodeRegister _rb `shift` 16)
          .|. (x_opcd _x `shift` 21)
          .|. (if _rc then 1 `shift` 31 else 0)
    M {..} -> (opcode _opcd)
          .|. (encodeRegister _rs `shift` 6)
          .|. (encodeRegister _ra `shift` 11)
          .|. (encodeRegister _rb `shift` 16)
          .|. (encodeRegister _mb `shift` 21)
          .|. (encodeRegister _me `shift` 26)
          .|. (if _rc then 1 `shift` 31 else 0)
    XFX {..} -> (opcode _opcd)
          .|. (encodeRegister _rt `shift` 6)
          .|. (encodeSpecialPurposeRegister _spr `shift` 11)
          .|. (xfx_opcd _xfx `shift` 21)
    XL {..} -> (opcode _opcd)
          .|. (encodeRegister _bt `shift` 6)
          .|. (encodeRegister _ba `shift` 11) 
          .|. (encodeRegister _bb `shift` 16)
          .|. (xl_opcd _xl `shift` 21)
          .|. (if _lk then 1 `shift` 31 else 0)
    FXV {..} -> (opcode _opcd)
          .|. (encodeVectorRegister _vrt `shift` 6)
          .|. (encodeVectorRegister _vra `shift` 11)
          .|. (encodeVectorRegister _vrb `shift` 16)
          .|. (fxv_opcd _fxv `shift` 21)
          .|. (fxv_cond _cond `shift` 30)
    FXVI {..} -> (opcode _opcd)
          .|. (encodeVectorRegister _vrt `shift` 6)
          .|. (encodeVectorRegister _vra `shift` 11)
          .|. ((fromIntegral _imm) `shift` 16)
          .|. (fxv_opcd _fxv `shift` 21)
          .|. (fxv_cond _cond `shift` 30)
    FXVS {..} -> (opcode _opcd)
          .|. (encodeVectorRegister _vrt `shift` 6)
          .|. (encodeRegister _ra `shift` 11)
          .|. (encodeRegister _rb `shift` 16)
          .|. (fxv_opcd _fxv `shift` 21)

assembleInstruction :: Inst -> Builder
assembleInstruction inst = case inst of
    I {..} -> asm_opcode _opcd <> " " <> (string8 . show $ (_li:: Word32))
    B {..} -> asm_opcode _opcd <> " " <> reg _bo <> ", " <> reg _bi <> " "
    D {..} -> asm_opcode _opcd <> " " <> reg _rt <> ", " <> reg _ra <> ", " <> (string8 . show $ (_d :: Word32))
    XO {..} -> xo_asm _xo <> xo_asm_suffix _oe _rc <> " " <> reg _rt <> ", " <> reg _ra <> ", " <> reg _rb
    X {..} -> x_asm _x <> " " <> reg _rt <> ", " <> reg _ra <> ", " <> reg _rb
    M {..} -> ""
    XFX {..} -> xfx_asm _xfx <> " " <> reg _rt <> ", " <> sreg _spr
    XL {..} -> xl_asm _xl <> " " <> reg _bt <> ", " <> reg _ba <> ", " <> reg _bb
    FXV {..} -> fxv_asm _fxv <> " " <> vreg _vrt <> ", " <> vreg _vra <> ", " <> vreg _vrb
    FXVS {..} -> fxv_asm _fxv <> " " <> vreg _vrt <> ", " <> reg _ra <> ", " <> reg _rb
    where vreg = string8 . show . encodeVectorRegister
          reg = string8 . show . encodeRegister
          sreg = string8 . show . encodeSpecialPurposeRegister

assembleInstructions :: [Inst] -> Builder
assembleInstructions = mconcat . map (\inst -> assembleInstruction inst <> "\n")