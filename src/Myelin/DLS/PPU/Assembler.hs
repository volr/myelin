{-# LANGUAGE RecordWildCards #-}
module Myelin.DLS.PPU.Assembler where

import Data.Int
import Data.Word
import Data.Bits

data Register = Register Word32 deriving (Eq, Show)
encodeRegister (Register r) = r

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

-- c.f. p. 193-197
cmp     rt ra rb rc = X Op_alu_xo rt ra rb Xop_cmp     rc 
tw      rt ra rb rc = X Op_alu_xo rt ra rb Xop_tw      rc 
lwzx    rt ra rb rc = X Op_alu_xo rt ra rb Xop_lwzx    rc 
slw     rt ra rb rc = X Op_alu_xo rt ra rb Xop_slw     rc 
cntlzw  rt ra rb rc = X Op_alu_xo rt ra rb Xop_cntlzw  rc 
and     rt ra rb rc = X Op_alu_xo rt ra rb Xop_and     rc 
cmpl    rt ra rb rc = X Op_alu_xo rt ra rb Xop_cmpl    rc 
nvem    rt ra rb rc = X Op_alu_xo rt ra rb Xop_nvem    rc 
nves    rt ra rb rc = X Op_alu_xo rt ra rb Xop_nves    rc 
nvemtl  rt ra rb rc = X Op_alu_xo rt ra rb Xop_nvemtl  rc 
lwzux   rt ra rb rc = X Op_alu_xo rt ra rb Xop_lwzux   rc 
andc    rt ra rb rc = X Op_alu_xo rt ra rb Xop_andc    rc 
wait    rt ra rb rc = X Op_alu_xo rt ra rb Xop_wait    rc 
mfmsr   rt ra rb rc = X Op_alu_xo rt ra rb Xop_mfmsr   rc 
lbzx    rt ra rb rc = X Op_alu_xo rt ra rb Xop_lbzx    rc 
lbzux   rt ra rb rc = X Op_alu_xo rt ra rb Xop_lbzux   rc 
popcb   rt ra rb rc = X Op_alu_xo rt ra rb Xop_popcb   rc 
nor     rt ra rb rc = X Op_alu_xo rt ra rb Xop_nor     rc 
mtmsr   rt ra rb rc = X Op_alu_xo rt ra rb Xop_mtmsr   rc 
stwx    rt ra rb rc = X Op_alu_xo rt ra rb Xop_stwx    rc 
prtyw   rt ra rb rc = X Op_alu_xo rt ra rb Xop_prtyw   rc 
stwux   rt ra rb rc = X Op_alu_xo rt ra rb Xop_stwux   rc 
stbx    rt ra rb rc = X Op_alu_xo rt ra rb Xop_stbx    rc 
stbux   rt ra rb rc = X Op_alu_xo rt ra rb Xop_stbux   rc 
lhzx    rt ra rb rc = X Op_alu_xo rt ra rb Xop_lhzx    rc 
eqv     rt ra rb rc = X Op_alu_xo rt ra rb Xop_eqv     rc 
eciwx   rt ra rb rc = X Op_alu_xo rt ra rb Xop_eciwx   rc 
lhzux   rt ra rb rc = X Op_alu_xo rt ra rb Xop_lhzux   rc 
xor     rt ra rb rc = X Op_alu_xo rt ra rb Xop_xor     rc 
lhax    rt ra rb rc = X Op_alu_xo rt ra rb Xop_lhax    rc 
lhaux   rt ra rb rc = X Op_alu_xo rt ra rb Xop_lhaux   rc 
sthx    rt ra rb rc = X Op_alu_xo rt ra rb Xop_sthx    rc 
orc     rt ra rb rc = X Op_alu_xo rt ra rb Xop_orc     rc 
ecowx   rt ra rb rc = X Op_alu_xo rt ra rb Xop_ecowx   rc 
sthux   rt ra rb rc = X Op_alu_xo rt ra rb Xop_sthux   rc 
or      rt ra rb rc = X Op_alu_xo rt ra rb Xop_or      rc 
nand    rt ra rb rc = X Op_alu_xo rt ra rb Xop_nand    rc 
srw     rt ra rb rc = X Op_alu_xo rt ra rb Xop_srw     rc 
sync    rt ra rb rc = X Op_alu_xo rt ra rb Xop_sync    rc 
synm    rt ra rb rc = X Op_alu_xo rt ra rb Xop_synm    rc 
syns    rt ra rb rc = X Op_alu_xo rt ra rb Xop_syns    rc 
synmtl  rt ra rb rc = X Op_alu_xo rt ra rb Xop_synmtl  rc 
synmtvr rt ra rb rc = X Op_alu_xo rt ra rb Xop_synmtvr rc 
synmfvr rt ra rb rc = X Op_alu_xo rt ra rb Xop_synmfvr rc 
synmtp  rt ra rb rc = X Op_alu_xo rt ra rb Xop_synmtp  rc 
synmfp  rt ra rb rc = X Op_alu_xo rt ra rb Xop_synmfp  rc 
synmvvr rt ra rb rc = X Op_alu_xo rt ra rb Xop_synmvvr rc 
synops  rt ra rb rc = X Op_alu_xo rt ra rb Xop_synops  rc 
synswp  rt ra rb rc = X Op_alu_xo rt ra rb Xop_synswp  rc 
sraw    rt ra rb rc = X Op_alu_xo rt ra rb Xop_sraw    rc 
srawi   rt ra rb rc = X Op_alu_xo rt ra rb Xop_srawi   rc 
extsh   rt ra rb rc = X Op_alu_xo rt ra rb Xop_extsh   rc 
extsb  rt ra rb rc  = X Op_alu_xo rt ra rb Xop_extsb   rc 

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

subfc  rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_subfc  rc 
addc   rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_addc   rc 
mulhwu rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_mulhwu rc 
subf   rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_subf   rc 
mulhw  rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_mulhw  rc 
neg    rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_neg    rc 
subfe  rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_subfe  rc 
adde   rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_adde   rc 
subfze rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_subfze rc 
addze  rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_addze  rc 
subfme rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_subfme rc 
addme  rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_addme  rc 
mullw  rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_mullw  rc 
add    rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_add    rc 
divwu  rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_divwu  rc 
divw   rt ra rb oe rc = XO Op_alu_xo rt ra rb oe Xop_divw   rc 

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

mcrf   bt ba bb lk = XL Op_bclr bt ba bb Xxop_mcrf lk
bclr   bt ba bb lk = XL Op_bclr bt ba bb Xxop_bclr lk
crnor  bt ba bb lk = XL Op_bclr bt ba bb Xxop_crnor lk
rfmci  bt ba bb lk = XL Op_bclr bt ba bb Xxop_rfmci lk
rfi    bt ba bb lk = XL Op_bclr bt ba bb Xxop_rfi lk
rfci   bt ba bb lk = XL Op_bclr bt ba bb Xxop_rfci lk
crandc bt ba bb lk = XL Op_bclr bt ba bb Xxop_crandc lk
crxor  bt ba bb lk = XL Op_bclr bt ba bb Xxop_crxor lk
crnand bt ba bb lk = XL Op_bclr bt ba bb Xxop_crnand lk
creqv  bt ba bb lk = XL Op_bclr bt ba bb Xxop_creqv lk
crand  bt ba bb lk = XL Op_bclr bt ba bb Xxop_crand lk
crorc  bt ba bb lk = XL Op_bclr bt ba bb Xxop_crorc lk
cror   bt ba bb lk = XL Op_bclr bt ba bb Xxop_cror lk
bcctr  bt ba bb lk = XL Op_bclr bt ba bb Xxop_bcctr lk

data Xfx_opcd = 
    Xop_mfocrf  
    | Xop_mtocrf
    | Xop_mfspr 
    | Xop_mtspr 

xfx_opcd Xop_mfocrf = 19
xfx_opcd Xop_mtocrf = 144
xfx_opcd Xop_mfspr  = 339
xfx_opcd Xop_mtspr  = 467

mfocrf rt spr = XFX Op_alu_xo rt spr Xop_mfocrf
mtocrf rt spr = XFX Op_alu_xo rt spr Xop_mtocrf
mfspr  rt spr = XFX Op_alu_xo rt spr Xop_mfspr
mtspr  rt spr = XFX Op_alu_xo rt spr Xop_mtspr

data Fxv_opcd = 
    Xop_fxvmahm         -- fixed-vector-multiply-accumulate-halfword-modulo
    | Xop_fxvmabm       -- fixed-vector-multiply-accumulate-byte-modulo
    | Xop_fxvmtacb      -- fixed-vector-move-to-accumulator-byte
    | Xop_fxvmtach      -- fixed-vector-move-to-accumulator-halfword
    | Xop_fxvmahfs      -- fixed-vector-multiply-accumulate-halfword-fractional-saturating
    | Xop_fxvmabfs      -- fixed-vector-multiply-accumulate-byte-fractional-saturating
    | Xop_fxvmtacbf     -- fixed-vector-move-to-accumulator-byte-fractional
    | Xop_fxvmtachf     -- fixed-vector-move-to-accumulator-halfword-fractional
    | Xop_fxvmatachm    -- fixed-vector-multiply-accumulate-to-accumulator-halfword-modulo
    | Xop_fxvmatacbm    -- fixed-vector-multiply-accumulate-save-to-accumulator-byte-modulo
    | Xop_fxvmatachfs   -- fixed-vector-multiply-accumulate-and-save-to-accumulator-halfword-fractional-saturating
    | Xop_fxvmatacbfs   -- fixed-vector-multiply-accumulate-and-save-to-accumulator-byte-fractional-saturating
    | Xop_fxvmulhm      -- fixed-vector-multiply-halfword-modulo
    | Xop_fxvmulbm      -- fixed-vector-multiply-byte-modulo
    | Xop_fxvmulhfs     -- fixed-vector-multiply-halfword-fractional-saturating
    | Xop_fxvmulbfs     -- fixed-vector-multiply-byte-fractional-saturating
    | Xop_fxvmultachm   -- fixed-vector-multiply-save-to-accumulator-halfword-modulo
    | Xop_fxvmultacbm   -- fixed-vector-multiply-save-to-accumulator-byte-modulo
    | Xop_fxvmultachfs  -- fixed-vector-multiply-save-to-accumulator-halfword-fractional-saturating
    | Xop_fxvmultacbfs  -- fixed-vector-multiply-save-to-accumulator-byte-fractional-saturating
    | Xop_fxvinx        -- fixed-vector-in-indexed
    | Xop_fxvpckbu      -- fixed-vector-pack-byte-upper
    | Xop_fxvoutx       -- fixed-vector-out-indexed
    | Xop_fxvpckbl      -- fixed-vector-pack-byte-lower
    | Xop_fxvsplath     -- fixed-vector-splat-halfword
    | Xop_fxvsplatb     -- fixed-vector-splat-byte
    | Xop_fxvupckbr     -- fixed-vector-unpack-byte-right
    | Xop_fxvupckbl     -- fixed-vector-unpack-byte-left
    | Xop_fxvcmph       -- fixed-vector-compare-halfword
    | Xop_fxvcmpb       -- fixed-vector-compare-byte
    | Xop_fxvshh        -- fixed-vector-shift-halfword
    | Xop_fxvshb        -- fixed-vector-shift-byte
    | Xop_fxvsel        -- fixed-vector-select
    | Xop_fxvsubhm      -- fixed-vector-subtract-halfword-modulo
    | Xop_fxvsubbm      -- fixed-vector-subtract-byte-modulo
    | Xop_fxvsubhfs     -- fixed-vector-subtract-halfword-fractional-saturating
    | Xop_fxvsubbfs     -- fixed-vector-subtract-byte-fractional-saturating
    | Xop_fxvaddactachm -- fixed-vector-add-accumulator-save-to-accumulator-halfword-modulo
    | Xop_fxvaddactacb  -- fixed-vector-add-accumulator-save-to-accumulator-byte
    | Xop_fxvaddactachf -- fixed-vector-add-accumulator-save-to-accumulator-halfword-fractional
    | Xop_fxvaddactacbf -- fixed-vector-add-accumulator-save-to-accumulator-byte-fractional
    | Xop_fxvaddachm    -- fixed-vector-add-accumulator-halfword-modulo
    | Xop_fxvaddacbm    -- fixed-vector-add-accumulator-byte-modulo
    | Xop_fxvaddachfs   -- fixed-vector-add-accumulator-halfword-fractional-saturating
    | Xop_fxvaddacbfs   -- fixed-vector-add-accumulator-byte-fractional-saturating
    | Xop_fxvaddtachm   -- fixed-vector-add-and-save-to-accumulator-halfword-modulo
    | Xop_fxvaddtacb    -- fixed-vector-add-and-save-to-accumulator-byte-modulo
    | Xop_fxvaddhm      -- fixed-vector-add-halfword-modulo
    | Xop_fxvaddbm      -- fixed-vector-add-byte-modulo
    | Xop_fxvaddhfs     -- fixed-vector-add-halfword-fractional-saturating
    | Xop_fxvaddbfs     -- fixed-vector-add-byte-fractional-saturating
    | Xop_fxvlax        -- fixed-vector-load-array-indexed
    | Xop_fxvstax       -- fixed-vector-store-array-indexed


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

data Fxv_cond = 
    Fxv_cond_null
    | Fxv_cond_gt
    | Fxv_cond_lt
    | Fxv_cond_eq

fxv_cond cond = case cond of
    Fxv_cond_null -> 0
    Fxv_cond_gt -> 1
    Fxv_cond_lt -> 2 
    Fxv_cond_eq -> 3

fxvmahm       rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmahm       cond
fxvmabm       rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmabm       cond
fxvmtacb      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmtacb      cond
fxvmtach      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmtach      cond
fxvmahfs      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmahfs      cond
fxvmabfs      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmabfs      cond
fxvmtacbf     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmtacbf     cond
fxvmtachf     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmtachf     cond
fxvmatachm    rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmatachm    cond
fxvmatacbm    rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmatacbm    cond
fxvmatachfs   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmatachfs   cond
fxvmatacbfs   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmatacbfs   cond
fxvmulhm      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmulhm      cond
fxvmulbm      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmulbm      cond
fxvmulhfs     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmulhfs     cond
fxvmulbfs     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmulbfs     cond
fxvmultachm   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmultachm   cond
fxvmultacbm   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmultacbm   cond
fxvmultachfs  rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmultachfs  cond
fxvmultacbfs  rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvmultacbfs  cond
fxvinx        rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvinx        cond
fxvpckbu      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvpckbu      cond
fxvoutx       rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvoutx       cond
fxvpckbl      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvpckbl      cond
fxvsplath     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsplath     cond
fxvsplatb     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsplatb     cond
fxvupckbr     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvupckbr     cond
fxvupckbl     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvupckbl     cond
fxvcmph       rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvcmph       cond
fxvcmpb       rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvcmpb       cond
fxvshh        rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvshh        cond
fxvshb        rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvshb        cond
fxvsel        rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsel        cond
fxvsubhm      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsubhm      cond
fxvsubbm      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsubbm      cond
fxvsubhfs     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsubhfs     cond
fxvsubbfs     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvsubbfs     cond
fxvaddactachm rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddactachm cond
fxvaddactacb  rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddactacb  cond
fxvaddactachf rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddactachf cond
fxvaddactacbf rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddactacbf cond
fxvaddachm    rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddachm    cond
fxvaddacbm    rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddacbm    cond
fxvaddachfs   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddachfs   cond
fxvaddacbfs   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddacbfs   cond
fxvaddtachm   rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddtachm   cond
fxvaddtacb    rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddtacb    cond
fxvaddhm      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddhm      cond
fxvaddbm      rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddbm      cond
fxvaddhfs     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddhfs     cond
fxvaddbfs     rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvaddbfs     cond
fxvlax        rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvlax        cond
fxvstax       rt ra rb cond = FXV Op_nve_xo rt ra rb Xop_fxvstax       cond

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
        _rt :: Register,
        _ra :: Register,
        _rb :: Register,
        _fxv :: Fxv_opcd,
        _cond :: Fxv_cond 
    }

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
          .|. (encodeRegister _rt `shift` 6)
          .|. (encodeRegister _ra `shift` 11)
          .|. (encodeRegister _rb `shift` 16)
          .|. (fxv_opcd _fxv `shift` 21)
          .|. (fxv_cond _cond `shift` 30)
