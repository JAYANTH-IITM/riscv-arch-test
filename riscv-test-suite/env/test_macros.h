/* These are macros to manage storing signatures, selecting what to save       */
/* keeping track of the hidden offset, and ensuring it doesn't overflow	       */
/* They're useful across many tests, but generally for specific classes of ops */


/* This function set up the Page table entry for Sv32 Translation scheme
    Arguments:
    _PAR: Register containing Physical Address
    _PR: Register containing Permissions for Leaf PTE. 
        (Note: No-leaf PTE (if-any) has only valid permssion (pte.v) set)
    _TR0, _TR1, _TR2: Temporary registers used and modified by function
    VA: Virtual address 
    level: Level at which PTE would be setup
        0: Two level translation
        1: Superpage
*/

#define LEVEL0 0x00
#define LEVEL1 0x01
#define LEVEL2 0x02
#define LEVEL3 0x03
#define LEVEL4 0x04

#define sv39 0x00
#define sv48 0x01
#define sv57 0x02

#define CODE code_bgn_off
#define DATA data_bgn_off
#define SIG  sig_bgn_off
#define VMEM vmem_bgn_off


#define SATP_SETUP(_TR0, _TR1, MODE);\
    LA(_TR0, rvtest_Sroot_pg_tbl) ;\
    LI(_TR1, MODE) ;\
    srli _TR0, _TR0, 12 ;\
    or _TR0, _TR0, _TR1  ;\
    csrw satp, _TR0   ;\

#define SETUP_PMP_SVADU_TEST(swreg, offset, TR0, TR1, TR2) \
  li TR0, -1                                 ;\
  csrw pmpaddr0, TR0                         ;\
  j PMP_exist                                ;\
  li TR0, 0                                  ;\
  li TR1, 0                                  ;\
  j Mend_PMP                                 ;\
PMP_exist:                                   ;\
  li TR1, PMP_TOR | PMP_X | PMP_W | PMP_R    ;\
  csrw pmpcfg0, TR1                          ;\
  csrr TR2, pmpcfg0                          ;\
  beq TR1, TR2, Mend_PMP                     ;\
no_TOR_try_NAPOT:                            ;\
  li TR1, PMP_NAPOT | PMP_X | PMP_W | PMP_R  ;\
  csrw pmpcfg0, TR1                          ;\
  csrr TR2, pmpcfg0                          ;\
Mend_PMP:                                    ;\
  RVTEST_SIGUPD(x1,TR0,offset)               ;\
  RVTEST_SIGUPD(x1,TR1,offset)               ;\

#define TEST_SVADU(swreg, PTE_ADDR, VA, offset, menvcfgaddr, adue_bit) \
    sfence.vma                                                                          ;\
    la t0, VA                                                                           ;\
    li t2, PTE_X | PTE_W | PTE_R                                                        ;\
1:                                                                                      ;\
    LREG t1, (PTE_ADDR)                                                                 ;\
    andi t1, t1, ~(PTE_X | PTE_W | PTE_R | PTE_V)                                       ;\
    or t1, t1, t2                                                                       ;\
    SREG t1, (PTE_ADDR)                                                                 ;\
    sfence.vma                                                                          ;\
											;\
    li t1, ((MSTATUS_MPP & ~(MSTATUS_MPP<<1)) * PRV_S) | MSTATUS_SUM | MSTATUS_MPRV     ;\
    csrs mstatus, t1                                                                    ;\
											;\
    .align 2                                                                            ;\
    SREG x0, (t0)                                                                       ;\
    unimp                                                                               ;\
											;\
    li t1, MSTATUS_MPRV                                                                 ;\
    csrc mstatus, t1                                                                    ;\
											;\
    beqz t2, 2f                                                                         ;\
    addi t2, t2, -1                                                                     ;\
    li t1, PTE_W | PTE_R | PTE_V                                                        ;\
    bne t2, t1, 1b                                                                      ;\
    addi t2, t2, -1                                                                     ;\
    j 1b                                                                                ;\
2:                                                                                      ;\
    li t0, MSTATUS_MPRV                                                                 ;\
    csrc mstatus, t0                                                                    ;\
    LREG t0, (PTE_ADDR)                                                                 ;\
    and t0, t0, PTE_V | PTE_U | PTE_R | PTE_W | PTE_X | PTE_A | PTE_D                   ;\
    RVTEST_SIGUPD(x1,t0,offset)                                                         ;\
                                                                                        ;\
    LREG t0, (PTE_ADDR)                                                                 ;\
    andi t0, t0, ~(PTE_X | PTE_W | PTE_R | PTE_V | PTE_A | PTE_D | PTE_V)               ;\
    ori t0, t0,  PTE_V | PTE_U | PTE_R | PTE_W | PTE_X | PTE_A                          ;\
    SREG t0, (PTE_ADDR)                                                                 ;\
    sfence.vma                                                                          ;\
											;\
    la t0, VA                                                                           ;\
    li a1, ((MSTATUS_MPP & ~(MSTATUS_MPP<<1)) * PRV_S) | MSTATUS_SUM | MSTATUS_MPRV     ;\
    csrs mstatus, a1                                                                    ;\
                                                                                        ;\
    .align 2                                                                            ;\
    SREG x0, (t0)                                                                       ;\
    unimp                                                                               ;\
											;\
    li t0, MSTATUS_MPRV                                                                 ;\
    csrc mstatus, t0                                                                    ;\
											;\
    LREG t0, (PTE_ADDR)                                                                 ;\
    and t0, t0, PTE_V | PTE_U | PTE_R | PTE_W | PTE_X | PTE_A | PTE_D                   ;\
    RVTEST_SIGUPD(x1,t0,offset)                                                         ;\
											;\
    LREG t0, (PTE_ADDR)                                                                 ;\
    andi t0, t0, ~(PTE_X | PTE_W | PTE_R | PTE_V | PTE_A | PTE_D | PTE_V)               ;\
    ori t0, t0,  PTE_V | PTE_U | PTE_R | PTE_W | PTE_X | PTE_A | PTE_D                  ;\
    SREG t0, (PTE_ADDR)                                                                 ;\
    sfence.vma                                                                          ;\
    la t0, VA                                                                           ;\
    li a1, ((MSTATUS_MPP & ~(MSTATUS_MPP<<1)) * PRV_S) | MSTATUS_SUM | MSTATUS_MPRV     ;\
    csrs mstatus, a1                                                                    ;\
											;\
    SREG x0, (t0)                                                                       ;\
    j 3f                                                                                ;\
    unimp                                                                               ;\
3:                                                                                      ;\
    LREG t0, (PTE_ADDR)                                                                 ;\
    andi t0, t0, ~(PTE_D)                                                               ;\
    SREG t0, (PTE_ADDR)                                                                 ;\
    sfence.vma                                                                          ;\
											;\
    li t0, adue_bit                                                                     ;\
    csrs menvcfgaddr, t0                                                                ;\
											;\
    la t0, VA                                                                           ;\
    li a1, ((MSTATUS_MPP & ~(MSTATUS_MPP<<1)) * PRV_S) | MSTATUS_SUM | MSTATUS_MPRV     ;\
    csrs mstatus, a1                                                                    ;\
											;\
    .align 2                                                                            ;\
    SREG x0, (t0)                                                                       ;\
    j 4f                                                                                ;\
    unimp                                                                               ;\
4:                                                                                      ;\
    li t0, MSTATUS_MPRV                                                                 ;\
    csrc mstatus, t0                                                                    ;\
											;\
    LREG t0, (PTE_ADDR)                                                                 ;\
    and t0, t0, PTE_V | PTE_U | PTE_R | PTE_W | PTE_X | PTE_A | PTE_D                   ;\
    RVTEST_SIGUPD(x1,t0,offset)


#define ALL_MEM_PMP                                               ;\
    	li t2, -1                                                 ;\
    	csrw pmpaddr0, t2                                         ;\
    	li t2, 0x0F	                                          ;\
    	csrw pmpcfg0, t2                                          ;\
    	sfence.vma                                                ;

#define SIGNATURE_AREA(TYPE,ARG1,ARG2, ...)                       ;\
	LI (t0, ARG1)                                             ;\
	.if(TYPE == CODE)                                         ;\
        LI (t1, ARG2)                                             ;\
	    sub t0, t0, t1                                        ;\
            csrr sp, mscratch                                     ;\
	    add t1,sp,t0                                          ;\
	    csrw sscratch, t1                                     ;\
    .else                                                         ;\
        LA (t1, ARG2)                                             ;\
	    sub t0, t0, t1                                        ;\
    .endif                                                        ;\
	LREG t1, TYPE+0*sv_area_sz(sp)                            ;\
	add t2, t1, t0                                            ;\
	SREG t2, TYPE+1*sv_area_sz(sp)                            ;

//****NOTE: label `rvtest_Sroot_pg_tbl` must be declared after RVTEST_DATA_END
//          in the test aligned at 4kiB (use .align 12)
#define PTE_SETUP_COMMON(_PAR, _PR, _TR0, _TR1, _VAR, level)  	  ;\
    srli _VAR, _VAR, (RISCV_PGLEVEL_BITS * level + RISCV_PGSHIFT) ;\
    srli _PAR, _PAR, (RISCV_PGLEVEL_BITS * level + RISCV_PGSHIFT) ;\
    slli _PAR, _PAR, (RISCV_PGLEVEL_BITS * level + RISCV_PGSHIFT) ;\
    LI(_TR0, ((1 << RISCV_PGLEVEL_BITS) - 1))                     ;\
    and _VAR, _VAR, _TR0                                          ;\
    slli _VAR, _VAR, ((XLEN >> 5)+1)                              ;\
    add _TR1, _TR1, _VAR                                          ;\
    srli _PAR, _PAR, 12                                           ;\
    slli _PAR, _PAR, 10                                           ;\
    or _PAR, _PAR, _PR                                            ;\
    SREG _PAR, 0(_TR1);                                          

#define PTE_SETUP_SV32(_PAR, _PR, _TR0, _TR1, _VAR, level)  	  ;\
    .if (level==1)                                                ;\
        LA(_TR1, rvtest_Sroot_pg_tbl)                             ;\
    .endif                                                        ;\
    .if (level==0)                                                ;\
        LA(_TR1, rvtest_slvl1_pg_tbl)                             ;\
    .endif                                                        ;\
    PTE_SETUP_COMMON(_PAR, _PR, _TR0, _TR1, _VAR, level)

#define PTE_SETUP_SV39(_PAR, _PR, _TR0, _TR1, _VAR, level)  	  ;\
    .if (level==2)                                                ;\
        LA(_TR1, rvtest_Sroot_pg_tbl)                             ;\
    .endif                                                        ;\
    .if (level==1)                                                ;\
        LA(_TR1, rvtest_slvl2_pg_tbl)                             ;\
    .endif                                                        ;\
    .if (level==0)                                                ;\
        LA(_TR1, rvtest_slvl1_pg_tbl)                             ;\
    .endif                                                        ;\
    PTE_SETUP_COMMON(_PAR, _PR, _TR0, _TR1, _VAR, level)

#define PTE_SETUP_SV48(_PAR, _PR, _TR0, _TR1, _VAR, level)  	  ;\
    .if (level==3)                                                ;\
        LA(_TR1, rvtest_Sroot_pg_tbl)                             ;\
    .endif                                                        ;\
    .if (level==2)                                                ;\
        LA(_TR1, rvtest_slvl3_pg_tbl)                             ;\
    .endif                                                        ;\
    .if (level==1)                                                ;\
        LA(_TR1, rvtest_slvl2_pg_tbl)                             ;\
    .endif                                                        ;\
    .if (level==0)                                                ;\
        LA(_TR1, rvtest_slvl1_pg_tbl)                             ;\
    .endif                                                        ;\
    PTE_SETUP_COMMON(_PAR, _PR, _TR0, _TR1, _VAR, level)

#define PTE_SETUP_SV57(_PAR, _PR, _TR0, _TR1, _VAR, level)  	  ;\
    .if (level==4)                                                ;\
        LA(_TR1, rvtest_Sroot_pg_tbl)                             ;\
    .endif                                                        ;\
    .if (level==3)                                                ;\
        LA(_TR1, rvtest_slvl4_pg_tbl)                             ;\
    .endif                                                        ;\
    .if (level==2)                                                ;\
        LA(_TR1, rvtest_slvl3_pg_tbl)                             ;\
    .endif                                                        ;\
    .if (level==1)                                                ;\
        LA(_TR1, rvtest_slvl2_pg_tbl)                             ;\
    .endif                                                        ;\
    .if (level==0)                                                ;\
        LA(_TR1, rvtest_slvl1_pg_tbl)                             ;\
    .endif                                                        ;\
    PTE_SETUP_COMMON(_PAR, _PR, _TR0, _TR1, _VAR, level)


#define PTE_SETUP_RV64(_PAR, _PR, _TR0, _TR1, VA, level, mode)  ;\
    srli _PAR, _PAR, 12                                         ;\
    slli _PAR, _PAR, 10                                         ;\
    or _PAR, _PAR, _PR                                          ;\
    .if (mode == sv39)                                          ;\
        .if (level == 2)                                        ;\
            LA(_TR1, rvtest_Sroot_pg_tbl)                       ;\
            .set vpn, ((VA >> 30) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
        .if (level == 1)                                        ;\
            LA(_TR1, rvtest_slvl1_pg_tbl)                       ;\
            .set vpn, ((VA >> 21) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
        .if (level == 0)                                        ;\
            LA(_TR1, rvtest_slvl2_pg_tbl)                       ;\
            .set vpn, ((VA >> 12) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
    .endif                                                      ;\
    .if (mode == sv48)                                          ;\
        .if (level == 3)                                        ;\
            LA(_TR1, rvtest_Sroot_pg_tbl)                       ;\
            .set vpn, ((VA >> 39) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
        .if (level == 2)                                        ;\
            LA(_TR1, rvtest_slvl1_pg_tbl)                       ;\
            .set vpn, ((VA >> 30) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
        .if (level == 1)                                        ;\
            LA(_TR1, rvtest_slvl2_pg_tbl)                       ;\
            .set vpn, ((VA >> 21) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
        .if (level == 0)                                        ;\
            LA(_TR1, rvtest_slvl3_pg_tbl)                       ;\
            .set vpn, ((VA >> 12) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
    .endif                                                      ;\
    .if (mode == sv57)                                          ;\
        .if (level == 4)                                        ;\
            LA(_TR1, rvtest_Sroot_pg_tbl)                       ;\
            .set vpn, ((VA >> 48) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
        .if (level == 3)                                        ;\
            LA(_TR1, rvtest_slvl1_pg_tbl)                       ;\
            .set vpn, ((VA >> 39) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
        .if (level == 2)                                        ;\
            LA(_TR1, rvtest_slvl2_pg_tbl)                       ;\
            .set vpn, ((VA >> 30) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
        .if (level == 1)                                        ;\
            LA(_TR1, rvtest_slvl3_pg_tbl)                       ;\
            .set vpn, ((VA >> 21) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
        .if (level == 0)                                        ;\
            LA(_TR1, rvtest_slvl3_pg_tbl)                       ;\
            .set vpn, ((VA >> 12) & 0x1FF) << 3                 ;\
        .endif                                                  ;\
    .endif                                                      ;\
    LI(_TR0, vpn)                                               ;\
    add _TR1, _TR1, _TR0                                        ;\
    SREG _PAR, 0(_TR1)                                          ;

#define PTE_PERMUPD_RV32(_PR, _TR0, _TR1, VA, level)          	;\
    .if (level==1)                                              ;\
        LA(_TR1, rvtest_Sroot_pg_tbl)                           ;\
        .set vpn, ((VA>>22)&0x3FF)<<2                           ;\
    .endif                                                      ;\
    .if (level==0)                                              ;\
        LA(_TR1, rvtest_slvl1_pg_tbl)                           ;\
        .set vpn, ((VA>>12)&0x3FF)<<2                           ;\
    .endif                                                      ;\
    LI(_TR0, vpn)                                               ;\
    add _TR1, _TR1, _TR0                                        ;\
    LREG _TR0, 0(_TR1)                                          ;\
    srli _TR0, _TR0, 10                                         ;\
    slli _TR0, _TR0, 10                                         ;\
    or _TR0, _TR0, _PR                                          ;\
    SREG _TR0, 0(_TR1)                                          ;   


#define SATP_SETUP_SV32 ;\
    LA(t6, rvtest_Sroot_pg_tbl) ;\
    LI(t5, SATP32_MODE) ;\
    srli t6, t6, 12 ;\
    or t6, t6, t5  ;\
    csrw satp, t6   ;

#define SATP_SETUP_RV64(MODE)                                   ;\
    LA(t6, rvtest_Sroot_pg_tbl)                                 ;\
    .if (MODE == sv39)                                          ;\
    LI(t5, (SATP64_MODE) & (SATP_MODE_SV39 << 60))              ;\
    .endif                                                      ;\
    .if (MODE == sv48)                                          ;\
    LI(t5, (SATP64_MODE) & (SATP_MODE_SV48 << 60))              ;\
    .endif                                                      ;\
    .if (MODE == sv57)                                          ;\
    LI(t5, (SATP64_MODE) & (SATP_MODE_SV57 << 60))              ;\
    .endif                                                      ;\
    .if (MODE == sv64)                                          ;\
    LI(t5, (SATP64_MODE) & (SATP_MODE_SV64 << 60))              ;\
    .endif                                                      ;\
    srli t6, t6, 12                                             ;\
    or t6, t6, t5                                               ;\
    csrw satp, t6                                               ;

//Tests for atomic memory operation(AMO) instructions
#define TEST_AMO_OP(inst, destreg, origptr, reg2, origval, updval, sigptr, ...) ;\
      .if NARG(__VA_ARGS__) == 1			;\
	.set offset,_ARG1(__VA_OPT__(__VA_ARGS__,0))	;\
      .endif						;\
      LI(reg2, MASK_XLEN(origval))			;\
      RVTEST_SIGUPD(sigptr, reg2) /*Write original AMO src */ ;\
      LI(reg2, MASK_XLEN(updval)) ;\
      addi origptr, sigptr, offset-REGWIDTH /* Calculate where orig AMO src is stored */ ;\
      inst destreg, reg2, (origptr) /*origval -> destreg; updated val -> (origptr) */ ;\
      RVTEST_SIGUPD(sigptr, destreg) /* write original AMO val */


#define NAN_BOXED(__val__,__width__,__max__)	;\
     .if __width__ == 16                        ;\
        .hword __val__                         ;\
    .endif                                     ;\
    .if __width__ == 32				;\
	.word __val__				;\
    .else					;\
	.dword __val__				;\
    .endif					;\
    .if __max__ > __width__			;\
        .if __width__ == 16                      ;\
         .set pref_bytes,(__max__-__width__)/16;\
        .else				                             ;\
         .set pref_bytes,(__max__-__width__)/32;\
    	.endif                                   ;\
    .else					;\
	.set pref_bytes, 0			;\
    .endif					;\
    .rept pref_bytes				;\
        .if __width__ == 16         ;\
		      .hword 0xffff         ;\
        .else				        ;\
	        .word 0xffffffff		;\
        .endif                      ;\
    .endr					;

#define ZERO_EXTEND(__val__,__width__,__max__)	;\
    .if __max__ > __width__			;\
	.set pref_bytes,(__max__-__width__)/32	;\
    .else					;\
	.set pref_bytes, 0			;\
    .endif					;\
    .rept pref_bytes				;\
	.word 0					;\
    .endr					;\
    .if __width__ == 32				;\
	.word __val__				;\
    .else					;\
	.dword __val__				;\
    .endif;

#define RVTEST_FP_ENABLE()			;\
 LI(a0, (MSTATUS_FS & (MSTATUS_FS >> 1)))	;\
 csrs mstatus, a0				;\
 csrwi fcsr, 0

// This macro is for vector 
#define RVTEST_VXSAT_ENABLE()			;\
 LI(a0, (MSTATUS_VS & (MSTATUS_VS >> 1)))	;\
 csrs mstatus, a0				;\
 clrov

#define RVTEST_SIGBASE(_R,_TAG) \
  LA(_R,_TAG);\
  .set offset,0;

// This macro is loading data from memory with any offset value
// This macro is loading data from memory with any offset value
#define LOAD_MEM_VAL(_LINST, _AREG, _RD, _OFF, _TREG) ;\
   .if (((_OFF & ~0x07FF)==  0) |((_OFF |  0x07FF)== -1))                       ;\
   _LINST _RD, _OFF(_AREG) /* yes, it fits */         ;\
  .else                    /* no, needs base adj   */ ;\
  .set  _off,  SEXT_IMM(_OFF) /* strip off hi bits */ ;\
  .set cry, BIT(_off,11)<<12    ;\
   LI(  _TREG, (_OFF & ~0x0FFF)+cry) /* strip off hi bits*/ ;\
    add  _AREG, _AREG, _TREG /* modified temp base*/ ;\
    _LINST _RD, _off(_AREG)                          ;\
    sub  _AREG, _AREG, _TREG /* undo modification */ ;\
 .endif
  /* this function ensures individual sig stores don't exceed offset limits  */
  /* if they would, update the base and reduce offset by 2048 - _SZ	     */
  /* an option is to pre-incr offset if there was a previous signature store */
#define CHK_OFFSET(_BREG, _SZ, _PRE_INC)		;\
  .if (_PRE_INC!=0)					;\
    .set offset, offset+_SZ				;\
  .endif						;\
  .if offset >= 2048					;\
     addi   _BREG,  _BREG,   (2048 - _SZ)		;\
     .set   offset, offset - (2048 - _SZ)		;\
  .endif

 /* automatically adjust base and offset if offset gets too big, resetting offset				 */
 /* RVTEST_SIGUPD(basereg, sigreg)	  stores sigreg at offset(basereg) and updates offset by regwidth	 */
 /* RVTEST_SIGUPD(basereg, sigreg,newoff) stores sigreg at newoff(basereg) and updates offset to regwidth+newoff */
#define RVTEST_SIGUPD(_BR,_R,...)			;\
  .if NARG(__VA_ARGS__) == 1				;\
	.set offset,_ARG1(__VA_OPT__(__VA_ARGS__,0))	;\
  .endif						;\
  CHK_OFFSET(_BR, REGWIDTH,0)				;\
  SREG _R,offset(_BR)					;\
  .set offset,offset+REGWIDTH

/* RVTEST_SIGUPD_F(basereg, sigreg,flagreg,newoff)			 */
/* This macro is used to store the signature values of (32 & 64) F and D */
/* teats which use TEST_(FPSR_OP, FPIO_OP, FPRR_OP, FPR4_OP) opcodes	 */
/* It stores both an Xreg and an Freg, first adjusting base & offset to	 */
/* to keep offset < 2048. SIGALIGN is set to the max(FREGWIDTH, REGWIDTH)*/
/* _BR - Base Reg, _R - FReg, _F - Fstatus Xreg				 */
#define RVTEST_SIGUPD_F(_BR,_R,_F,...)			;\
  .if NARG(__VA_ARGS__) == 1				;\
     .set offset,_ARG1(__VA_OPT__(__VA_ARGS__,0))	;\
  .endif						;\
  .if (offset&(SIGALIGN-1))!=0				;\
/* Throw warnings then modify offset to target */	;\
     .warning "Incorrect signature Offset Alignment."	;\
     .set offset, offset&(SIGALIGN-1)+SIGALIGN		;\
  .endif						;\
  CHK_OFFSET(_BR, SIGALIGN, 0)				;\
  FSREG _R,offset(_BR)					;\
  CHK_OFFSET(_BR, SIGALIGN, 1)				;\
  SREG _F,offset(_BR)					;\
  .set offset,offset+SIGALIGN
 
/* RVTEST_SIGUPD_FID(basereg, sigreg,flagreg,newoff)			*/
/* This macro stores the signature values of (32 & 64) F & D insts	*/
/* which uses TEST_(FPID_OP, FCMP_OP) ops				*/
/* It stores two integer registers. SigReg is stored @offset[BaseReg],	*/
/* FlagReg at offset+Regwidth[BaseReg]. It updates offset by 2*regwidth	*/
/* and post increments so repeated uses store sig values sequentially	*/
/*  _BR - Base Reg, _R - Signature reg, _F - Flag reg			*/
#define RVTEST_SIGUPD_FID(_BR,_R,_F,...)		;\
  .if NARG(__VA_ARGS__) == 1				;\
     .set offset,_ARG1(__VA_OPT__(__VA_ARGS__,0))	;\
  .endif						;\
  .if (offset&(SIGALIGN-1))!=0				;\
/* Throw warnings then modify offset to target */	;\
     .warning "Signature Incorrect Offset Alignment."	;\
     .set offset, offset&(SIGALIGN-1)+SIGALIGN		;\
  .endif						;\
  CHK_OFFSET(_BR, REGWIDTH, 0)				;\
  SREG _R,offset(_BR)					;\
  CHK_OFFSET(_BR, REGWIDTH, 1)				;\
  SREG _F,offset(_BR)					;\
  .set offset,offset+REGWIDTH


// for updating signatures that include flagreg for P-ext saturation instructions (RV32/RV64).
#define RVTEST_SIGUPD_PK(_BR,_R,_F,...)			;\
  .if NARG(__VA_ARGS__) == 1				;\
      .set offset,_ARG1(__VA_OPT__(__VA_ARGS__,0))	;\
  .endif						;\
  .if (offset & (REGWIDTH-1)) != 0			;\
      .warning "Signature Incorrect Offset Alignment."	;\
     .set offset, offset&(SIGALIGN-1)+SIGALIGN		;\
  .endif						;\
      CHK_OFFSET(_BR,REGWIDTH,0)			;\
      SREG _R,offset(_BR)				;\
      CHK_OFFSET(_BR,REGWIDTH,1)			;\
      SREG _F,offset(_BR)				;\
      .set offset,offset+(REGWIDTH)

// for updating signatures when 'rd' is a paired register (64-bit)
//  in Zpsfoperand extension in RV32; this reuses RVTEST_SIGUPD_PK()
#define RVTEST_SIGUPD_P64(_BR,_R,_R_HI,...)				;\
  .if NARG(__VA_ARGS__) == 0						;\
      RVTEST_SIGUPD_PK(_BR,_R,_R_HI)					;\
  .else									;\
      RVTEST_SIGUPD_PK(_BR,_R,_R_HI,_ARG1(__VA_OPT__(__VA_ARGS__,0)))	;\
  .endif

// for updating signatures that include flagreg when 'rd' is a 
// paired register (64-bit) in Zpsfoperand extension in RV32.
#define RVTEST_SIGUPD_PK64(_BR,_R,_R_HI,_F,...)			;\
      rdov _F							;\
  .if NARG(__VA_ARGS__) == 1					;\
      .set offset,_ARG1(__VA_OPT__(__VA_ARGS__,0))		;\
  .endif							;\
  .if (offset & (REGWIDTH-1)) != 0				;\
      .warning "Incorrect Offset Alignment for Signature."	;\
     .set offset, offset&(SIGALIGN-1)+SIGALIGN			;\
  .endif							;\
      CHK_OFFSET(_BR,REGWIDTH,0)				;\
      SREG _R,offset(_BR)					;\
      CHK_OFFSET(_BR,REGWIDTH,1)				;\
      SREG _R_HI,offset(_BR)					;\
      CHK_OFFSET(_BR,REGWIDTH,1)				;\
      SREG _F,offset(_BR)					;\
      .set offset,offset+(REGWIDTH)



  /* DEPRECATE this is redundant with RVTEST_BASEUPD(BR,_NR),	*/
  /* except it doesn't correct for offset overflow while moving */
#define RVTEST_VALBASEMOV(_NR,_BR)			;\
  add _NR, _BR, x0;

#define RVTEST_VALBASEUPD(_BR,...)			;\
  .if NARG(__VA_ARGS__) == 0				;\
      addi _BR,_BR,2040					;\
  .endif						;\
  .if NARG(__VA_ARGS__) == 1				;\
      LA(_BR,_ARG1(__VA_ARGS__,x0))			;\
  .endif

/*
 * RVTEST_BASEUPD(base reg) - updates the base register the last signature address + REGWIDTH
 * RVTEST_BASEUPD(base reg, new reg) - moves value of the next signature region to update into new reg
 * The hidden variable offset is reset always
*/

#define RVTEST_BASEUPD(_BR,...)				;\
 /* deal with case where offset>=2047 */		;\
       .set corr 2048-REGWIDTH				;\
    .if offset <2048 					;\
       .set corr offset					;\
    .endif						;\
    .set offset, offset-corr				;\
							;\
    .if NARG(__VA_ARGS__) == 0				;\
	addi _BR,		    _BR, corr		;\
    .else						;\
	addi _ARG1(__VA_ARGS__,x0) ,_BR, corr		;\
    .endif				

//==============================================================================
// This section borrows from Andrew's from Andrew Waterman's risc-v test macros
// They are used to generate tests; some are op specific, some format specific
//==============================================================================

#define TEST_JALR_OP(tempreg, rd, rs1, imm, swreg, offset,adj)	;\
5:					;\
    auipc rd, 0             ;\
    .if adj & 1 == 1			;\
    LA(rs1, 3f-imm+adj-1)		;\
    jalr rd, imm+1(rs1)			;\
    .else				;\
    LA(rs1, 3f-imm+adj)			;\
    jalr rd, imm(rs1)			;\
    .endif				;\
    nop					;\
    nop					;\
    xori rd,rd, 0x2			;\
    j 4f				;\
					;\
3:  .if adj & 2 == 2			;\
    .fill 2,1,0x00			;\
    .endif				;\
    xori rd,rd, 0x3			;\
    j 4f				;\
    .if adj&2 == 2			;\
    .fill 2,1,0x00			;\
    .endif				;\
					;\
4: LA(tempreg, 5b)			;\
   andi tempreg,tempreg,~(3)		;\
    sub rd,rd,tempreg			;\
    RVTEST_SIGUPD(swreg,rd,offset) 


#define TEST_JAL_OP(tempreg, rd, imm, label, swreg, offset, adj)	;\
5:					;\
    LA(tempreg, 2f)			;\
    jalr x0,0(tempreg)			;\
6:  LA(tempreg, 4f)			;\
    jalr x0,0(tempreg)			;\
1:  .if adj & 2 == 2			;\
    .ifc label, 1b			;\
    .fill 2,1,0x00			;\
    .endif				;\
    .endif				;\
    xori rd,rd, 0x1			;\
    beq x0,x0,6b			;\
    .if adj & 2 == 2			;\
    .ifc label, 1b			;\
    .fill 2,1,0x00			;\
    .endif				;\
    .endif				;\
    .if (imm/2) - 2 >= 0		;\
	.set num,(imm/2)-2		;\
    .else				;\
	.set num,0			;\
    .endif				;\
     .ifc label, 3f			;\
	.set num,0			;\
    .endif				;\
    .rept num				;\
    nop					;\
    .endr				;\
					;\
2:  jal rd, label+(adj)			;\
    .if adj & 2 == 2			;\
    nop					;\
    nop					;\
    .endif				;\
    xori rd,rd, 0x2			;\
    j 4f				;\
    .if (imm/2) - 3 >= 0		;\
	.set num,(imm/2)-3		;\
    .else				;\
	.set num,0			;\
    .endif				;\
    .ifc label, 1b			;\
	.set num,0			;\
    .endif				;\
    .rept num				;\
    nop					;\
    .endr				;\
3:  .if adj & 2 == 2			;\
    .ifc label, 3f			;\
    .fill 2,1,0x00			;\
    .endif				;\
    .endif				;\
    xori rd,rd, 0x3			;\
    LA(tempreg, 4f)			;\
    jalr x0,0(tempreg)			;\
    .if adj & 2 == 2			;\
    .ifc label, 3f			;\
    .fill 2,1,0x00			;\
    .endif				;\
    .endif				;\
4: LA(tempreg, 5b)			;\
   andi tempreg,tempreg,~(3)		;\
    sub rd,rd,tempreg			;\
    RVTEST_SIGUPD(swreg,rd,offset) 
//SREG rd, offset(swreg);

#define TEST_BRANCH_OP(inst, tempreg, reg1, reg2, val1, val2, imm, label, swreg, offset,adj) \
    LI(reg1, MASK_XLEN(val1))		;\
    LI(reg2, MASK_XLEN(val2))		;\
    addi tempreg,x0,0			;\
    j 2f				;\
					;\
1:  .if adj & 2 == 2			;\
    .fill 2,1,0x00			;\
    .endif				;\
    addi tempreg,tempreg, 0x1		;\
    j 4f				;\
    .if adj & 2 == 2			;\
    .fill 2,1,0x00			;\
    .endif				;\
    .if (imm/2) - 2 >= 0		;\
	.set num,(imm/2)-2		;\
    .else				;\
	.set num,0			;\
    .endif				;\
     .ifc label, 3f			;\
	.set num,0			;\
    .endif				;\
    .rept num				;\
    nop					;\
    .endr				;\
					;\
2:  inst reg1, reg2, label+adj		;\
    addi tempreg, tempreg,0x2		;\
    j 4f				;\
    .if (imm/4) - 3 >= 0		;\
	.set num,(imm/4)-3		;\
    .else				;\
	.set num,0			;\
    .endif				;\
     .ifc label, 1b			;\
	.set num,0			;\
    .endif				;\
    .rept num				;\
    nop					;\
    .endr				;\
					;\
3:  .if adj & 2 == 2			;\
    .fill 2,1,0x00			;\
    .endif				;\
    addi tempreg, tempreg,0x3		;\
    j 4f				;\
    .if adj&2 == 2			;\
    .fill 2,1,0x00			;\
    .endif				;\
					;\
4:   RVTEST_SIGUPD(swreg,tempreg,offset) 


#define TEST_STORE(swreg,testreg,index,rs1,rs2,rs2_val,imm_val,offset,inst,adj)	;\
LI(rs2,rs2_val)				;\
addi rs1,swreg,offset+adj		;\
LI(testreg,imm_val)			;\
sub rs1,rs1,testreg			;\
inst rs2, imm_val(rs1)			;\
nop					;\
nop									    

#define TEST_LOAD(swreg,testreg,index,rs1,destreg,imm_val,offset,inst,adj);\
LA(rs1,rvtest_data+(index*4)+adj-imm_val);\
inst destreg, imm_val(rs1)		;\
nop					;\
nop					;\
RVTEST_SIGUPD(swreg,destreg,offset) 

#define TEST_STORE_F(swreg,testreg,fcsr_val,rs1,rs2,imm_val,offset,inst,adj,flagreg,valaddr_reg, val_offset);\
LOAD_MEM_VAL(FLREG, valaddr_reg, rs2, val_offset, testreg);\
addi rs1,swreg,offset+adj		;\
LI(testreg,imm_val)			;\
sub rs1,rs1,testreg			;\
inst rs2, imm_val(rs1)			;\
nop					;\
nop					;\
csrr flagreg, fcsr			;\
RVTEST_SIGUPD(swreg,flagreg,offset+SIGALIGN)

#define TEST_LOAD_F(swreg,testreg,fcsr_val,rs1,destreg,imm_val,inst,adj,flagreg)	;\
LA(rs1,rvtest_data+adj-imm_val)		;\
LI(testreg, fcsr_val)			;\
csrw fcsr, testreg			;\
inst destreg, imm_val(rs1)		;\
nop					;\
nop					;\
csrr flagreg, fcsr			;\
RVTEST_SIGUPD_F(swreg,destreg,flagreg) 

#define TEST_CBO_ZERO(swreg,rs1,inst,imm_val)                               ;\
LI(rs1,imm_val&(RVMODEL_CBZ_BLOCKSIZE-1))                                   ;\
add rs1,rs1,swreg                                                           ;\
inst (rs1)                                                                  ;\
nop                                                                         ;\
nop                                                                         ;\
ADDI(swreg, swreg, RVMODEL_CBZ_BLOCKSIZE)

#define TEST_PUSH(inst,imm_val,rs1,rlist) ;\
.if rlist == 4                            ;\
  .if imm_val == 0                        ;\
    inst rlist4 , -VAL0                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist4 , -VAL1                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist4 , -VAL2                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist4 , -VAL3                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 5                            ;\
  .if imm_val == 0                        ;\
    inst rlist5 , -VAL0                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist5 , -VAL1                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist5 , -VAL2                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist5 , -VAL3                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 6                            ;\
  .if imm_val == 0                        ;\
    inst rlist6 , -VAL4                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist6 , -VAL5                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist6 , -VAL6                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist6 , -VAL7                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 7                            ;\
  .if imm_val == 0                        ;\
    inst rlist7 , -VAL4                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist7 , -VAL5                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist7 , -VAL6                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist7 , -VAL7                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 8                            ;\
  .if imm_val == 0                        ;\
    inst rlist8 , -VAL8                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist8 , -VAL9                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist8 , -VAL10                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist8 , -VAL11                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 9                            ;\
  .if imm_val == 0                        ;\
    inst rlist9 , -VAL8                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist9 , -VAL9                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist9 , -VAL10                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist9 , -VAL11                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 10                            ;\
  .if imm_val == 0                        ;\
    inst rlist10 , -VAL12                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist10 , -VAL13                        ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist10 , -VAL14                        ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist10 , -VAL15                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 11                            ;\
  .if imm_val == 0                        ;\
    inst rlist11 , -VAL12                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist11 , -VAL13                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist11 , -VAL14                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist11 , -VAL15                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 12                            ;\
  .if imm_val == 0                        ;\
    inst rlist12 , -VAL16                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist12 , -VAL17                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist12 , -VAL18                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist12, -VAL19                      ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 13                            ;\
  .if imm_val == 0                        ;\
    inst rlist13 , -VAL16                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist13 , -VAL17                    ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist13 , -VAL18                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist13 , -VAL19                      ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 14                            ;\
  .if imm_val == 0                        ;\
    inst rlist14 , -VAL20                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist14 , -VAL21                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist14 , -VAL22                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist14 , -VAL23                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 15                            ;\
  .if imm_val == 0                        ;\
    inst rlist15 , -VAL24                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist15 , -VAL25                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist15 , -VAL26                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist15 , -VAL27                       ;\
  .endif                                  ;\
.endif                                    ;\

#define TEST_POP_POPRET_POPRETZ(inst,imm_val,rs1,rlist) ;\
.if rlist == 4                            ;\
  .if imm_val == 0                        ;\
    inst rlist4 , VAL0                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist4 , VAL1                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist4 , VAL2                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist4 , VAL3                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 5                            ;\
  .if imm_val == 0                        ;\
    inst rlist5 , VAL0                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist5 , VAL1                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist5 , VAL2                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist5 , VAL3                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 6                            ;\
  .if imm_val == 0                        ;\
    inst rlist6 , VAL4                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist6 , VAL5                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist6 , VAL6                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist6 , VAL7                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 7                            ;\
  .if imm_val == 0                        ;\
    inst rlist7 , VAL4                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist7 , VAL5                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist7 , VAL6                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist7 , VAL7                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 8                            ;\
  .if imm_val == 0                        ;\
    inst rlist8 , VAL8                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist8 , VAL9                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist8 , VAL10                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist8 , VAL11                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 9                            ;\
  .if imm_val == 0                        ;\
    inst rlist9 , VAL8                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist9 , VAL9                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist9 , VAL10                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist9 , VAL11                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 10                            ;\
  .if imm_val == 0                        ;\
    inst rlist10 , VAL12                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist10 , VAL13                        ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist10 , VAL14                        ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist10 , VAL15                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 11                            ;\
  .if imm_val == 0                        ;\
    inst rlist11 , VAL12                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist11 , VAL13                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist11 , VAL14                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist11 , VAL15                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 12                            ;\
  .if imm_val == 0                        ;\
    inst rlist12 , VAL16                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist12 , VAL17                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist12 , VAL18                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist12, VAL19                      ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 13                            ;\
  .if imm_val == 0                        ;\
    inst rlist13 , VAL16                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist13 , VAL17                    ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist13 , VAL18                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist13 , VAL19                      ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 14                            ;\
  .if imm_val == 0                        ;\
    inst rlist14 , VAL20                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist14 , VAL21                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist14 , VAL22                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist14 , VAL23                       ;\
  .endif                                  ;\
.endif                                    ;\
.if rlist == 15                            ;\
  .if imm_val == 0                        ;\
    inst rlist15 , VAL24                       ;\
  .endif                                  ;\
  .if imm_val == 1                        ;\
    inst rlist15 , VAL25                       ;\
  .endif                                  ;\
  .if imm_val == 2                        ;\
    inst rlist15 , VAL26                       ;\
  .endif                                  ;\
  .if imm_val == 3                        ;\
    inst rlist15 , VAL27                       ;\
  .endif                                  ;\
.endif                                    ;\

#define TEST_MVAS(inst,rs1,rs2) ;\
  inst rs1 , rs2 ;\

#define TEST_MVSA(inst,rs1,rs2) ;\
  inst rs1 , rs2 ;\

#define TEST_CMJT(testreg,inst,imm_val,swreg);\
  la t2, jump_table             ;\
  csrw CSR_JVT, t2              ;\
  csrr t3, CSR_JVT              ;\
  li t1, imm_val                ;\
  slli t1, t1, 2                ;\
  add t3, t3, t1                ;\
  inst imm_val                  ;\
  .align 6                      ;\
jump_table:                     ;\
    WORD function_0            ;\
    WORD function_1            ;\
    WORD function_2            ;\
    WORD function_3            ;\
    WORD function_4            ;\
    WORD function_5            ;\
    WORD function_6            ;\
    WORD function_7            ;\
    WORD function_8            ;\
    WORD function_9            ;\
    WORD function_10           ;\
    WORD function_11           ;\
    WORD function_12           ;\
    WORD function_13           ;\
    WORD function_14           ;\
    WORD function_15           ;\
    WORD function_16           ;\
    WORD function_17           ;\
    WORD function_18           ;\
    WORD function_19           ;\
    WORD function_20           ;\
    WORD function_21           ;\
    WORD function_22           ;\
    WORD function_23           ;\
    WORD function_24           ;\
    WORD function_25           ;\
    WORD function_26           ;\
    WORD function_27           ;\
    WORD function_28           ;\
    WORD function_29           ;\
    WORD function_30           ;\
function_0:                     ;\
  LI(testreg,0x42)              ;\
function_1:                     ;\
  LI(testreg,0x42)              ;\
function_2:                     ;\
  LI(testreg,0x42)              ;\
function_3:                     ;\
  LI(testreg,0x42)              ;\
function_4:                     ;\
  LI(testreg,0x42)              ;\
function_5:                     ;\
  LI(testreg,0x42)              ;\
function_6:                     ;\
  LI(testreg,0x42)              ;\
function_7:                     ;\
  LI(testreg,0x42)              ;\
function_8:                     ;\
  LI(testreg,0x42)              ;\
function_9:                     ;\
  LI(testreg,0x42)              ;\
function_10:                    ;\
  LI(testreg,0x42)              ;\
function_11:                    ;\
  LI(testreg,0x42)              ;\
function_12:                    ;\
  LI(testreg,0x42)              ;\
function_13:                    ;\
  LI(testreg,0x42)              ;\
function_14:                    ;\
  LI(testreg,0x42)              ;\
function_15:                    ;\
  LI(testreg,0x42)              ;\
function_16:                    ;\
  LI(testreg,0x42)              ;\
function_17:                    ;\
  LI(testreg,0x42)              ;\
function_18:                    ;\
  LI(testreg,0x42)              ;\
function_19:                    ;\
  LI(testreg,0x42)              ;\
function_20:                    ;\
  LI(testreg,0x42)              ;\
function_21:                    ;\
  LI(testreg,0x42)              ;\
function_22:                    ;\
  LI(testreg,0x42)              ;\
function_23:                    ;\
  LI(testreg,0x42)              ;\
function_24:                    ;\
  LI(testreg,0x42)              ;\
function_25:                    ;\
  LI(testreg,0x42)              ;\
function_26:                    ;\
  LI(testreg,0x42)              ;\
function_27:                    ;\
  LI(testreg,0x42)              ;\
function_28:                    ;\
  LI(testreg,0x42)              ;\
function_29:                    ;\
  LI(testreg,0x42)              ;\
function_30:                    ;\
  LI(testreg,0x42)              ;\
function_31:                    ;\
  LI(testreg,0x42)              ;\

#define TEST_CMJALT(testreg,inst,imm_val,swreg);\
  la t2, jump_table             ;\
  csrw CSR_JVT, t2              ;\
  csrr t3, CSR_JVT              ;\
  li t1, imm_val                ;\
  slli t1, t1, 2                ;\
  add t3, t3, t1                ;\
  inst imm_val                  ;\
  .align 6                      ;\
jump_table:                     ;\
    WORD  function_0            ;\
    WORD function_1            ;\
    WORD function_2            ;\
    WORD function_3            ;\
    WORD function_4            ;\
    WORD function_5            ;\
    WORD function_6            ;\
    WORD function_7            ;\
    WORD function_8            ;\
    WORD function_9            ;\
    WORD function_10           ;\
    WORD function_11           ;\
    WORD function_12           ;\
    WORD function_13           ;\
    WORD function_14           ;\
    WORD function_15           ;\
    WORD function_16           ;\
    WORD function_17           ;\
    WORD function_18           ;\
    WORD function_19           ;\
    WORD function_20           ;\
    WORD function_21           ;\
    WORD function_22           ;\
    WORD function_23           ;\
    WORD function_24           ;\
    WORD function_25           ;\
    WORD function_26           ;\
    WORD function_27           ;\
    WORD function_28           ;\
    WORD function_29           ;\
    WORD function_30           ;\
    WORD function_31            ;\
    WORD function_32            ;\
    WORD function_33            ;\
    WORD function_34            ;\
    WORD function_35            ;\
    WORD function_36            ;\
    WORD function_37            ;\
    WORD function_38            ;\
    WORD function_39            ;\
    WORD function_40            ;\
    WORD function_41            ;\
    WORD function_42            ;\
    WORD function_43            ;\
    WORD function_44            ;\
    WORD function_45            ;\
    WORD function_46            ;\
    WORD function_47            ;\
    WORD function_48            ;\
    WORD function_49            ;\
    WORD function_50            ;\
    WORD function_51            ;\
    WORD function_52            ;\
    WORD function_53            ;\
    WORD function_54            ;\
    WORD function_55            ;\
    WORD function_56            ;\
    WORD function_57            ;\
    WORD function_58            ;\
    WORD function_59            ;\
    WORD function_60            ;\
    WORD function_61            ;\
    WORD function_62            ;\
    WORD function_63            ;\
    WORD function_64            ;\
    WORD function_65            ;\
    WORD function_66            ;\
    WORD function_67            ;\
    WORD function_68            ;\
    WORD function_69            ;\
    WORD function_70            ;\
    WORD function_71            ;\
    WORD function_72            ;\
    WORD function_73            ;\
    WORD function_74            ;\
    WORD function_75            ;\
    WORD function_76            ;\
    WORD function_77            ;\
    WORD function_78            ;\
    WORD function_79            ;\
    WORD function_80            ;\
    WORD function_81            ;\
    WORD function_82            ;\
    WORD function_83            ;\
    WORD function_84            ;\
    WORD function_85            ;\
    WORD function_86            ;\
    WORD function_87            ;\
    WORD function_88            ;\
    WORD function_89            ;\
    WORD function_90            ;\
    WORD function_91            ;\
    WORD function_92            ;\
    WORD function_93            ;\
    WORD function_94            ;\
    WORD function_95            ;\
    WORD function_96            ;\
    WORD function_97            ;\
    WORD function_98            ;\
    WORD function_99            ;\
    WORD function_100           ;\
    WORD function_101           ;\
    WORD function_102           ;\
    WORD function_103           ;\
    WORD function_104           ;\
    WORD function_105           ;\
    WORD function_106           ;\
    WORD function_107           ;\
    WORD function_108           ;\
    WORD function_109           ;\
    WORD function_110           ;\
    WORD function_111           ;\
    WORD function_112           ;\
    WORD function_113           ;\
    WORD function_114           ;\
    WORD function_115           ;\
    WORD function_116           ;\
    WORD function_117           ;\
    WORD function_118           ;\
    WORD function_119           ;\
    WORD function_120           ;\
    WORD function_121           ;\
    WORD function_122           ;\
    WORD function_123           ;\
    WORD function_124           ;\
    WORD function_125           ;\
    WORD function_126           ;\
    WORD function_127           ;\
    WORD function_128           ;\
    WORD function_129           ;\
    WORD function_130           ;\
    WORD function_131           ;\
    WORD function_132           ;\
    WORD function_133           ;\
    WORD function_134           ;\
    WORD function_135           ;\
    WORD function_136           ;\
    WORD function_137           ;\
    WORD function_138           ;\
    WORD function_139           ;\
    WORD function_140           ;\
    WORD function_141           ;\
    WORD function_142           ;\
    WORD function_143           ;\
    WORD function_144           ;\
    WORD function_145           ;\
    WORD function_146           ;\
    WORD function_147           ;\
    WORD function_148           ;\
    WORD function_149           ;\
    WORD function_150           ;\
    WORD function_151           ;\
    WORD function_152           ;\
    WORD function_153           ;\
    WORD function_154           ;\
    WORD function_155           ;\
    WORD function_156           ;\
    WORD function_157           ;\
    WORD function_158           ;\
    WORD function_159           ;\
    WORD function_160           ;\
    WORD function_161           ;\
    WORD function_162           ;\
    WORD function_163           ;\
    WORD function_164           ;\
    WORD function_165           ;\
    WORD function_166           ;\
    WORD function_167           ;\
    WORD function_168           ;\
    WORD function_169           ;\
    WORD function_170           ;\
    WORD function_171           ;\
    WORD function_172           ;\
    WORD function_173           ;\
    WORD function_174           ;\
    WORD function_175           ;\
    WORD function_176           ;\
    WORD function_177           ;\
    WORD function_178           ;\
    WORD function_179           ;\
    WORD function_180           ;\
    WORD function_181           ;\
    WORD function_182           ;\
    WORD function_183           ;\
    WORD function_184           ;\
    WORD function_185           ;\
    WORD function_186           ;\
    WORD function_187           ;\
    WORD function_188           ;\
    WORD function_189           ;\
    WORD function_190           ;\
    WORD function_191           ;\
    WORD function_192           ;\
    WORD function_193           ;\
    WORD function_194           ;\
    WORD function_195           ;\
    WORD function_196           ;\
    WORD function_197           ;\
    WORD function_198           ;\
    WORD function_199           ;\
    WORD function_200           ;\
    WORD function_201           ;\
    WORD function_202           ;\
    WORD function_203           ;\
    WORD function_204           ;\
    WORD function_205           ;\
    WORD function_206           ;\
    WORD function_207           ;\
    WORD function_208           ;\
    WORD function_209           ;\
    WORD function_210           ;\
    WORD function_211           ;\
    WORD function_212           ;\
    WORD function_213           ;\
    WORD function_214           ;\
    WORD function_215           ;\
    WORD function_216           ;\
    WORD function_217           ;\
    WORD function_218           ;\
    WORD function_219           ;\
    WORD function_220           ;\
    WORD function_221           ;\
    WORD function_222           ;\
    WORD function_223           ;\
    WORD function_224           ;\
    WORD function_225           ;\
    WORD function_226           ;\
    WORD function_227           ;\
    WORD function_228           ;\
    WORD function_229           ;\
    WORD function_230           ;\
    WORD function_231           ;\
    WORD function_232           ;\
    WORD function_233           ;\
    WORD function_234           ;\
    WORD function_235           ;\
    WORD function_236           ;\
    WORD function_237           ;\
    WORD function_238           ;\
    WORD function_239           ;\
    WORD function_240           ;\
    WORD function_241           ;\
    WORD function_242           ;\
    WORD function_243           ;\
    WORD function_244           ;\
    WORD function_245           ;\
    WORD function_246           ;\
    WORD function_247           ;\
    WORD function_248           ;\
    WORD function_249           ;\
    WORD function_250           ;\
    WORD function_251           ;\
    WORD function_252           ;\
    WORD function_253           ;\
    WORD function_254           ;\
    WORD function_255           ;\
function_0:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_1:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_2:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_3:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_4:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_5:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_6:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_7:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_8:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_9:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_10:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_11:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_12:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_13:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_14:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_15:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_16:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_17:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_18:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_19:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_20:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_21:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_22:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_23:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_24:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_25:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_26:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_27:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_28:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_29:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_30:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_31:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_32:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_33:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_34:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_35:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_36:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_37:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_38:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_39:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_40:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_41:                     ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_42:                    ;\
  LI(testreg,0x42)              ;\
  ret                           ;\
function_43:                    ;\
  LI(testreg,0x42)              ;\
function_44:                    ;\
  LI(testreg,0x42)              ;\
function_45:                    ;\
  LI(testreg,0x42)              ;\
function_46:                    ;\
  LI(testreg,0x42)              ;\
function_47:                    ;\
  LI(testreg,0x42)              ;\
function_48:                    ;\
  LI(testreg,0x42)              ;\
function_49:                    ;\
  LI(testreg,0x42)              ;\
function_50:                    ;\
  LI(testreg,0x42)              ;\
function_51:                    ;\
  LI(testreg,0x42)              ;\
function_52:                    ;\
  LI(testreg,0x42)              ;\
function_53:                    ;\
  LI(testreg,0x42)              ;\
function_54:                    ;\
  LI(testreg,0x42)              ;\
function_55:                    ;\
  LI(testreg,0x42)              ;\
function_56:                    ;\
  LI(testreg,0x42)              ;\
function_57:                    ;\
  LI(testreg,0x42)              ;\
function_58:                    ;\
  LI(testreg,0x42)              ;\
function_59:                    ;\
  LI(testreg,0x42)              ;\
function_60:                    ;\
  LI(testreg,0x42)              ;\
function_61:                    ;\
  LI(testreg,0x42)              ;\
function_62:                    ;\
  LI(testreg,0x42)              ;\
function_63:                    ;\
  LI(testreg,0x42)              ;\
function_64:                    ;\
  LI(testreg,0x42)              ;\
function_65:                    ;\
  LI(testreg,0x42)              ;\
function_66:                    ;\
  LI(testreg,0x42)              ;\
function_67:                    ;\
  LI(testreg,0x42)              ;\
function_68:                    ;\
  LI(testreg,0x42)              ;\
function_69:                    ;\
  LI(testreg,0x42)              ;\
function_70:                    ;\
  LI(testreg,0x42)              ;\
function_71:                    ;\
  LI(testreg,0x42)              ;\
function_72:                    ;\
  LI(testreg,0x42)              ;\
function_73:                    ;\
  LI(testreg,0x42)              ;\
function_74:                    ;\
  LI(testreg,0x42)              ;\
function_75:                    ;\
  LI(testreg,0x42)              ;\
function_76:                    ;\
  LI(testreg,0x42)              ;\
function_77:                    ;\
  LI(testreg,0x42)              ;\
function_78:                    ;\
  LI(testreg,0x42)              ;\
function_79:                    ;\
  LI(testreg,0x42)              ;\
function_80:                    ;\
  LI(testreg,0x42)              ;\
function_81:                    ;\
  LI(testreg,0x42)              ;\
function_82:                    ;\
  LI(testreg,0x42)              ;\
function_83:                    ;\
  LI(testreg,0x42)              ;\
function_84:                    ;\
  LI(testreg,0x42)              ;\
function_85:                    ;\
  LI(testreg,0x42)              ;\
function_86:                    ;\
  LI(testreg,0x42)              ;\
function_87:                    ;\
  LI(testreg,0x42)              ;\
function_88:                    ;\
  LI(testreg,0x42)              ;\
function_89:                    ;\
  LI(testreg,0x42)              ;\
function_90:                    ;\
  LI(testreg,0x42)              ;\
function_91:                    ;\
  LI(testreg,0x42)              ;\
function_92:                    ;\
  LI(testreg,0x42)              ;\
function_93:                    ;\
  LI(testreg,0x42)              ;\
function_94:                    ;\
  LI(testreg,0x42)              ;\
function_95:                    ;\
  LI(testreg,0x42)              ;\
function_96:                    ;\
  LI(testreg,0x42)              ;\
function_97:                    ;\
  LI(testreg,0x42)              ;\
function_98:                    ;\
  LI(testreg,0x42)              ;\
function_99:                    ;\
  LI(testreg,0x42)              ;\
function_100:                    ;\
  LI(testreg,0x42)              ;\
function_101:                    ;\
  LI(testreg,0x42)              ;\
function_102:                    ;\
  LI(testreg,0x42)              ;\
function_103:                    ;\
  LI(testreg,0x42)              ;\
function_104:                    ;\
  LI(testreg,0x42)              ;\
function_105:                    ;\
  LI(testreg,0x42)              ;\
function_106:                    ;\
  LI(testreg,0x42)              ;\
function_107:                    ;\
  LI(testreg,0x42)              ;\
function_108:                    ;\
  LI(testreg,0x42)              ;\
function_109:                    ;\
  LI(testreg,0x42)              ;\
function_110:                    ;\
  LI(testreg,0x42)              ;\
function_111:                    ;\
  LI(testreg,0x42)              ;\
function_112:                    ;\
  LI(testreg,0x42)              ;\
function_113:                    ;\
  LI(testreg,0x42)              ;\
function_114:                    ;\
  LI(testreg,0x42)              ;\
function_115:                    ;\
  LI(testreg,0x42)              ;\
function_116:                    ;\
  LI(testreg,0x42)              ;\
function_117:                    ;\
  LI(testreg,0x42)              ;\
function_118:                    ;\
  LI(testreg,0x42)              ;\
function_119:                    ;\
  LI(testreg,0x42)              ;\
function_120:                    ;\
  LI(testreg,0x42)              ;\
function_121:                    ;\
  LI(testreg,0x42)              ;\
function_122:                    ;\
  LI(testreg,0x42)              ;\
function_123:                    ;\
  LI(testreg,0x42)              ;\
function_124:                    ;\
  LI(testreg,0x42)              ;\
function_125:                    ;\
  LI(testreg,0x42)              ;\
function_126:                    ;\
  LI(testreg,0x42)              ;\
function_127:                    ;\
  LI(testreg,0x42)              ;\
function_128:                    ;\
  LI(testreg,0x42)              ;\
function_129:                    ;\
  LI(testreg,0x42)              ;\
function_130:                    ;\
  LI(testreg,0x42)              ;\
function_131:                    ;\
  LI(testreg,0x42)              ;\
function_132:                    ;\
  LI(testreg,0x42)              ;\
function_133:                    ;\
  LI(testreg,0x42)              ;\
function_134:                    ;\
  LI(testreg,0x42)              ;\
function_135:                    ;\
  LI(testreg,0x42)              ;\
function_136:                    ;\
  LI(testreg,0x42)              ;\
function_137:                    ;\
  LI(testreg,0x42)              ;\
function_138:                    ;\
  LI(testreg,0x42)              ;\
function_139:                    ;\
  LI(testreg,0x42)              ;\
function_140:                    ;\
  LI(testreg,0x42)              ;\
function_141:                    ;\
  LI(testreg,0x42)              ;\
function_142:                    ;\
  LI(testreg,0x42)              ;\
function_143:                    ;\
  LI(testreg,0x42)              ;\
function_144:                    ;\
  LI(testreg,0x42)              ;\
function_145:                    ;\
  LI(testreg,0x42)              ;\
function_146:                    ;\
  LI(testreg,0x42)              ;\
function_147:                    ;\
  LI(testreg,0x42)              ;\
function_148:                    ;\
  LI(testreg,0x42)              ;\
function_149:                    ;\
  LI(testreg,0x42)              ;\
function_150:                    ;\
  LI(testreg,0x42)              ;\
function_151:                    ;\
  LI(testreg,0x42)              ;\
function_152:                    ;\
  LI(testreg,0x42)              ;\
function_153:                    ;\
  LI(testreg,0x42)              ;\
function_154:                    ;\
  LI(testreg,0x42)              ;\
function_155:                    ;\
  LI(testreg,0x42)              ;\
function_156:                    ;\
  LI(testreg,0x42)              ;\
function_157:                    ;\
  LI(testreg,0x42)              ;\
function_158:                    ;\
  LI(testreg,0x42)              ;\
function_159:                    ;\
  LI(testreg,0x42)              ;\
function_160:                    ;\
  LI(testreg,0x42)              ;\
function_161:                    ;\
  LI(testreg,0x42)              ;\
function_162:                    ;\
  LI(testreg,0x42)              ;\
function_163:                    ;\
  LI(testreg,0x42)              ;\
function_164:                    ;\
  LI(testreg,0x42)              ;\
function_165:                    ;\
  LI(testreg,0x42)              ;\
function_166:                    ;\
  LI(testreg,0x42)              ;\
function_167:                    ;\
  LI(testreg,0x42)              ;\
function_168:                    ;\
  LI(testreg,0x42)              ;\
function_169:                    ;\
  LI(testreg,0x42)              ;\
function_170:                    ;\
  LI(testreg,0x42)              ;\
function_171:                    ;\
  LI(testreg,0x42)              ;\
function_172:                    ;\
  LI(testreg,0x42)              ;\
function_173:                    ;\
  LI(testreg,0x42)              ;\
function_174:                    ;\
  LI(testreg,0x42)              ;\
function_175:                    ;\
  LI(testreg,0x42)              ;\
function_176:                    ;\
  LI(testreg,0x42)              ;\
function_177:                    ;\
  LI(testreg,0x42)              ;\
function_178:                    ;\
  LI(testreg,0x42)              ;\
function_179:                    ;\
  LI(testreg,0x42)              ;\
function_180:                    ;\
  LI(testreg,0x42)              ;\
function_181:                    ;\
  LI(testreg,0x42)              ;\
function_182:                    ;\
  LI(testreg,0x42)              ;\
function_183:                    ;\
  LI(testreg,0x42)              ;\
function_184:                    ;\
  LI(testreg,0x42)              ;\
function_185:                    ;\
  LI(testreg,0x42)              ;\
function_186:                    ;\
  LI(testreg,0x42)              ;\
function_187:                    ;\
  LI(testreg,0x42)              ;\
function_188:                    ;\
  LI(testreg,0x42)              ;\
function_189:                    ;\
  LI(testreg,0x42)              ;\
function_190:                    ;\
  LI(testreg,0x42)              ;\
function_191:                    ;\
  LI(testreg,0x42)              ;\
function_192:                    ;\
  LI(testreg,0x42)              ;\
function_193:                    ;\
  LI(testreg,0x42)              ;\
function_194:                    ;\
  LI(testreg,0x42)              ;\
function_195:                    ;\
  LI(testreg,0x42)              ;\
function_196:                    ;\
  LI(testreg,0x42)              ;\
function_197:                    ;\
  LI(testreg,0x42)              ;\
function_198:                    ;\
  LI(testreg,0x42)              ;\
function_199:                    ;\
  LI(testreg,0x42)              ;\
function_200:                    ;\
  LI(testreg,0x42)              ;\
function_201:                    ;\
  LI(testreg,0x42)              ;\
function_202:                    ;\
  LI(testreg,0x42)              ;\
function_203:                    ;\
  LI(testreg,0x42)              ;\
function_204:                    ;\
  LI(testreg,0x42)              ;\
function_205:                    ;\
  LI(testreg,0x42)              ;\
function_206:                    ;\
  LI(testreg,0x42)              ;\
function_207:                    ;\
  LI(testreg,0x42)              ;\
function_208:                    ;\
  LI(testreg,0x42)              ;\
function_209:                    ;\
  LI(testreg,0x42)              ;\
function_210:                    ;\
  LI(testreg,0x42)              ;\
function_211:                    ;\
  LI(testreg,0x42)              ;\
function_212:                    ;\
  LI(testreg,0x42)              ;\
function_213:                    ;\
  LI(testreg,0x42)              ;\
function_214:                    ;\
  LI(testreg,0x42)              ;\
function_215:                    ;\
  LI(testreg,0x42)              ;\
function_216:                    ;\
  LI(testreg,0x42)              ;\
function_217:                    ;\
  LI(testreg,0x42)              ;\
function_218:                    ;\
  LI(testreg,0x42)              ;\
function_219:                    ;\
  LI(testreg,0x42)              ;\
function_220:                    ;\
  LI(testreg,0x42)              ;\
function_221:                    ;\
  LI(testreg,0x42)              ;\
function_222:                    ;\
  LI(testreg,0x42)              ;\
function_223:                    ;\
  LI(testreg,0x42)              ;\
function_224:                    ;\
  LI(testreg,0x42)              ;\
function_225:                    ;\
  LI(testreg,0x42)              ;\
function_226:                    ;\
  LI(testreg,0x42)              ;\
function_227:                    ;\
  LI(testreg,0x42)              ;\
function_228:                    ;\
  LI(testreg,0x42)              ;\
function_229:                    ;\
  LI(testreg,0x42)              ;\
function_230:                    ;\
  LI(testreg,0x42)              ;\
function_231:                    ;\
  LI(testreg,0x42)              ;\
function_232:                    ;\
  LI(testreg,0x42)              ;\
function_233:                    ;\
  LI(testreg,0x42)              ;\
function_234:                    ;\
  LI(testreg,0x42)              ;\
function_235:                    ;\
  LI(testreg,0x42)              ;\
function_236:                    ;\
  LI(testreg,0x42)              ;\
function_237:                    ;\
  LI(testreg,0x42)              ;\
function_238:                    ;\
  LI(testreg,0x42)              ;\
function_239:                    ;\
  LI(testreg,0x42)              ;\
function_240:                    ;\
  LI(testreg,0x42)              ;\
function_241:                    ;\
  LI(testreg,0x42)              ;\
function_242:                    ;\
  LI(testreg,0x42)              ;\
function_243:                    ;\
  LI(testreg,0x42)              ;\
function_244:                    ;\
  LI(testreg,0x42)              ;\
function_245:                    ;\
  LI(testreg,0x42)              ;\
function_246:                    ;\
  LI(testreg,0x42)              ;\
function_247:                    ;\
  LI(testreg,0x42)              ;\
function_248:                    ;\
  LI(testreg,0x42)              ;\
function_249:                    ;\
  LI(testreg,0x42)              ;\
function_250:                    ;\
  LI(testreg,0x42)              ;\
function_251:                    ;\
  LI(testreg,0x42)              ;\
function_252:                    ;\
  LI(testreg,0x42)              ;\
function_253:                    ;\
  LI(testreg,0x42)              ;\
function_254:                    ;\
  LI(testreg,0x42)              ;\
function_255:                    ;\
  LI(testreg,0x42)              ;\


#define TEST_CSR_FIELD(ADDRESS,TEMP_REG,MASK_REG,NEG_MASK_REG,VAL,DEST_REG,OFFSET,BASE_REG) ;\
    LI(TEMP_REG,VAL)			;\
    and TEMP_REG,TEMP_REG,MASK_REG	;\
    csrr DEST_REG,ADDRESS		;\
    and DEST_REG,DEST_REG,NEG_MASK_REG	;\
    or TEMP_REG,TEMP_REG,DEST_REG	;\
    csrw ADDRESS,TEMP_REG		;\
    csrr DEST_REG,ADDRESS		;\
    RVTEST_SIGUPD(BASE_REG,DEST_REG,OFFSET)


#define TEST_CASE(testreg, destreg, correctval, swreg, offset, code... )	;\
    code				;\
    RVTEST_SIGUPD(swreg,destreg,offset)	;\
    RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval)

#define TEST_CASE_F(testreg, destreg, correctval, swreg, flagreg, code... )	;\
    code					;\
    RVTEST_SIGUPD_F(swreg,destreg,flagreg)	;\
#if FLEN==32 \
    RVMODEL_IO_ASSERT_SFPR_EQ(testreg, destreg, correctval);\
#elif FLEN==64 \
    RVMODEL_IO_ASSERT_DFPR_EQ(testreg, destreg, correctval);\
#endif
    
#define TEST_CASE_FID(testreg, destreg, correctval, swreg, flagreg, code... )	;\
    code; \
    RVTEST_SIGUPD_FID(swreg,destreg,flagreg)	;\
    RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval)

#define TEST_AUIPC(inst, destreg, correctval, imm, swreg, offset, testreg)	;\
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LA testreg, 1f			;\
      1:				;\
      inst destreg, imm			;\
      sub destreg, destreg, testreg	;\
      )

//Tests for instructions with register-immediate operand
#define TEST_IMM_OP( inst, destreg, reg, correctval, val, imm, swreg, offset, testreg)	;\
    TEST_CASE(testreg, destreg, correctval, swreg, offset,	 ;\
      LI(reg, MASK_XLEN(val))		;\
      inst destreg, reg, SEXT_IMM(imm)	;\
    )

//Tests for floating-point instructions with a single register operand
#define TEST_FPSR_OP( inst, destreg, freg, rm, fcsr_val, correctval, valaddr_reg, val_offset, flagreg, swreg, testreg) \
    TEST_CASE_F(testreg, destreg, correctval, swreg, flagreg, \
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg, val_offset, testreg); \
      LI(testreg, fcsr_val)	;\
      csrw fcsr, testreg	;\
      inst destreg, freg, rm	;\
      csrr flagreg, fcsr	;\
    )

//Tests for floating-point instructions with a single register operand
//This variant does not take the rm field and set it while writing the instruction
#define TEST_FPSR_OP_NRM( inst, destreg, freg, fcsr_val, correctval, valaddr_reg, val_offset, flagreg, swreg, testreg) \
    TEST_CASE_F(testreg, destreg, correctval, swreg, flagreg,		 \
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg, val_offset, testreg)	;\
      li testreg, fcsr_val; csrw fcsr, testreg	;\
      inst destreg, freg			;\
      csrr flagreg, fcsr			;\
    )
    
//Tests for floating-point instructions with a single register operand and integer destination register
#define TEST_FPID_OP( inst, destreg, freg, rm, fcsr_val, correctval, valaddr_reg, val_offset, flagreg, swreg, testreg,load_instr) \
    TEST_CASE_FID(testreg, destreg, correctval, swreg, flagreg,		 \
      LOAD_MEM_VAL(load_instr, valaddr_reg, freg, val_offset, testreg)	;\
      li testreg, fcsr_val; csrw fcsr, testreg	;\
      inst destreg, freg, rm			;\
      csrr flagreg, fcsr			;\
      )
    
//Tests for floating-point instructions with a single register operand and integer operand register
#define TEST_FPIO_OP( inst, destreg, freg, rm, fcsr_val, correctval, valaddr_reg, val_offset, flagreg, swreg, testreg, load_instr) \
    TEST_CASE_F(testreg, destreg, correctval, swreg, flagreg,		 \
      LOAD_MEM_VAL(load_instr, valaddr_reg, freg, val_offset, testreg)	;\
      li testreg, fcsr_val; csrw fcsr, testreg	;\
      inst destreg, freg, rm			;\
      csrr flagreg, fcsr			;\
    )

//Tests for floating-point instructions with a single register operand and integer destination register
//This variant does not take the rm field and set it while writing the instruction
#define TEST_FPID_OP_NRM( inst, destreg, freg, fcsr_val, correctval, valaddr_reg, val_offset, flagreg, swreg, testreg) \
    TEST_CASE_FID(testreg, destreg, correctval, swreg, flagreg,		 \
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg, val_offset, testreg)	;\
      li testreg, fcsr_val; csrw fcsr, testreg	;\
      inst destreg, freg			;\
      csrr flagreg, fcsr			;\
      )
    
//Tests for floating-point instructions with a single register operand and integer operand register
//This variant does not take the rm field and set it while writing the instruction
#define TEST_FPIO_OP_NRM( inst, destreg, freg, fcsr_val, correctval, valaddr_reg, val_offset, flagreg, swreg, testreg, load_instr) \
    TEST_CASE_F(testreg, destreg, correctval, swreg, flagreg,		 \
      LOAD_MEM_VAL(load_instr, valaddr_reg, freg, val_offset, testreg)	;\
      li testreg, fcsr_val; csrw fcsr, testreg	;\
      inst destreg, freg			;\
      csrr flagreg, fcsr			;\
    )

//Tests for instructions with register-register-immediate operands
#define TEST_RRI_OP(inst, destreg, reg1, reg2, imm, correctval, val1, val2, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LI(reg1, MASK_XLEN(val1))			;\
      LI(reg2, MASK_XLEN(val2))			;\
      inst destreg, reg1, reg2, imm		;\
    )

//Tests for a instructions with register-register operand
#define TEST_RI_OP(inst, destreg, reg1, reg2, imm, correctval, val1, val2, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LI(reg1, MASK_XLEN(val1))			;\
      LI(reg2, MASK_XLEN(val2))			;\
      inst destreg, reg1, reg2, imm		;\
    )

//Tests for a instructions with register-register operand
#define TEST_RR_OP(inst, destreg, reg1, reg2, correctval, val1, val2, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LI(reg1, MASK_XLEN(val1))			;\
      LI(reg2, MASK_XLEN(val2))			;\
      inst destreg, reg1, reg2			;\
    )
//Tests for a instructions with single-register operand
#define TEST_R_OP(inst, destreg, reg1, correctval, val1, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LI(reg1, MASK_XLEN(val1))			;\
      inst destreg, reg1    			;\
    )
//Tests for floating-point instructions with register-register operand
//This variant does not take the rm field and set it while writing the instruction
#define TEST_FPRR_OP_NRM(inst, destreg, freg1, freg2, fcsr_val, correctval, valaddr_reg, val_offset, flagreg, swreg, testreg) \
    TEST_CASE_F(testreg, destreg, correctval, swreg, flagreg,			 \
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg1, val_offset, testreg)		;\
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg2, (val_offset+FREGWIDTH), testreg)	;\
      li testreg, fcsr_val; csrw fcsr, testreg	;\
      inst destreg, freg1, freg2		;\
      csrr flagreg, fcsr			;\
    )

//Tests for floating-point instructions with register-register operand
#define TEST_FPRR_OP(inst, destreg, freg1, freg2, rm, fcsr_val, correctval, valaddr_reg, val_offset, flagreg, swreg, testreg) \
    TEST_CASE_F(testreg, destreg, correctval, swreg, flagreg,			\
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg1, val_offset, testreg)		;\
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg2, (val_offset+FREGWIDTH), testreg)	;\
      li testreg, fcsr_val; csrw fcsr, testreg	;\
      inst destreg, freg1, freg2, rm		;\
      csrr flagreg, fcsr			;\
    )
    
//Tests for floating-point CMP instructions with register-register operand
#define TEST_FCMP_OP(inst, destreg, freg1, freg2, fcsr_val, correctval, valaddr_reg, val_offset, flagreg, swreg, testreg) \
    TEST_CASE_FID(testreg, destreg, correctval, swreg, flagreg,			 \
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg1, val_offset, testreg)		;\
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg2, (val_offset+FREGWIDTH), testreg)	;\
      li testreg, fcsr_val; csrw fcsr, testreg	;\
      inst destreg, freg1, freg2		;\
      csrr flagreg, fcsr			;\
    )

//Tests for floating-point R4 type instructions
#define TEST_FPR4_OP(inst, destreg, freg1, freg2, freg3, rm , fcsr_val, correctval, valaddr_reg, val_offset, flagreg, swreg, testreg) \
    TEST_CASE_F(testreg, destreg, correctval, swreg, flagreg,			\
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg1, val_offset, testreg)		;\
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg2, (val_offset+FREGWIDTH), testreg)	;\
      LOAD_MEM_VAL(FLREG, valaddr_reg, freg3, (val_offset+2*FREGWIDTH), testreg);\
      li testreg, fcsr_val; csrw fcsr, testreg	;\
      inst destreg, freg1, freg2, freg3, rm	;\
      csrr flagreg, fcsr			;\
    )

#define TEST_CNOP_OP( inst, testreg, imm_val, swreg, offset) \
    TEST_CASE(testreg, x0, 0, swreg, offset,	 \
      inst imm_val				;\
      )

//Tests c.mop.* instructions
#define TEST_CMOP_OP(inst, destreg, imm_val, swreg, testreg, offset) \
    TEST_CASE(testreg, destreg, imm_val, swreg, offset,	 \
      mv destreg, swreg; \
      inst;                \
      )

//Tests for instructions with register-immediate operand and update the saturation flag
#define TEST_PKIMM_OP( inst, destreg, reg, correctval, val, imm, flagreg, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LI(reg, MASK_XLEN(val))		;\
      inst destreg, reg, SEXT_IMM(imm)	;\
      rdov flagreg			;\
    )

//Tests for instructions with register-register operand and update the saturation flag
#define TEST_PKRR_OP(inst, destreg, reg1, reg2, correctval, val1, val2, flagreg, swreg, offset, testreg) \
    LI(reg1, MASK_XLEN(val1))		;\
    LI(reg2, MASK_XLEN(val2))		;\
    inst destreg, reg1, reg2		;\
    rdov flagreg			;\
    RVTEST_SIGUPD_PK(swreg, destreg, flagreg, offset)	;\
    RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval)

//Tests for instructions with a single register operand and update the saturation flag
#define TEST_PKR_OP( inst, destreg, reg, correctval, val, flagreg, swreg, offset, testreg) \
    LI(reg, MASK_XLEN(val))	;\
    inst destreg, reg		;\
    rdov flagreg		;\
    RVTEST_SIGUPD_PK(swreg,destreg,flagreg,offset)	;\
    RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval)

#if __riscv_xlen == 32
//Tests for a instruction with register pair operands for all its three operands
#define TEST_P64_PPP_OP_32(inst, destreg, destreg_hi, reg1, reg1_hi, reg2, reg2_hi, correctval, correctval_hi, val1, val1_hi, val2, val2_hi, swreg, offset, testreg) \
      LI(reg1, MASK_XLEN(val1))		;\
      LI(reg1_hi, MASK_XLEN(val1_hi))	;\
      LI(reg2, MASK_XLEN(val2))		;\
      LI(reg2_hi, MASK_XLEN(val2_hi))	;\
      inst destreg, reg1, reg2		;\
      RVTEST_SIGUPD_P64(swreg,destreg, destreg_hi, offset)	;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval)	;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg_hi, correctval_hi)

#define TEST_PK64_PPP_OP_32(inst, destreg, destreg_hi, reg1, reg1_hi, reg2, reg2_hi, correctval, correctval_hi, val1, val1_hi, val2, val2_hi, flagreg, swreg, offset, testreg) \
      LI(reg1, MASK_XLEN(val1))		;\
      LI(reg1_hi, MASK_XLEN(val1_hi))	;\
      LI(reg2, MASK_XLEN(val2))		;\
      LI(reg2_hi, MASK_XLEN(val2_hi))	;\
      inst destreg, reg1, reg2		;\
      RVTEST_SIGUPD_PK64(swreg,destreg, destreg_hi, flagreg, offset)	;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval)		;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg_hi, correctval_hi)

#define TEST_P64_PPN_OP_32(inst, destreg, destreg_hi, reg1, reg1_hi, reg2, correctval, correctval_hi, val1, val1_hi, val2, swreg, offset, testreg) \
      LI(reg1, MASK_XLEN(val1))		;\
      LI(reg1_hi, MASK_XLEN(val1_hi))	;\
      LI(reg2, MASK_XLEN(val2))		;\
      inst destreg, reg1, reg2		;\
      RVTEST_SIGUPD_P64(swreg, destreg, destreg_hi, offset)	;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval)	;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg_hi, correctval_hi)

#define TEST_P64_PNN_OP_32(inst, destreg, destreg_hi, reg1, reg2, correctval, correctval_hi, val1, val2, swreg, offset, testreg) \
      LI(reg1, MASK_XLEN(val1))		;\
      LI(reg2, MASK_XLEN(val2))		;\
      inst destreg, reg1, reg2		;\
      RVTEST_SIGUPD_P64(swreg, destreg, destreg_hi, offset)	;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval)	;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg_hi, correctval_hi)

#define TEST_PK64_PNN_OP_32(inst, destreg, destreg_hi, reg1, reg2, correctval, correctval_hi, val1, val2, flagreg, swreg, offset, testreg) \
      LI(reg1, MASK_XLEN(val1))		;\
      LI(reg2, MASK_XLEN(val2))		;\
      inst destreg, reg1, reg2		;\
      RVTEST_SIGUPD_PK64(swreg, destreg, destreg_hi, flagreg, offset)	;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval)		;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg_hi, correctval_hi)

#define TEST_P64_NPN_OP_32(inst, destreg, reg1, reg1_hi, reg2, correctval, val1, val1_hi, val2, swreg, offset, testreg) \
      LI(reg1, MASK_XLEN(val1))			;\
      LI(reg1_hi, MASK_XLEN(val1_hi))		;\
      LI(reg2, MASK_XLEN(val2))			;\
      inst destreg, reg1, reg2			;\
      RVTEST_SIGUPD(swreg,destreg,offset)	;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval);

#define TEST_P64_NP_OP_32(inst, destreg, reg1, reg1_hi, correctval, val1, val1_hi, imm_val, swreg, offset, testreg) \
      LI(reg1, MASK_XLEN(val1))			;\
      LI(reg1_hi, MASK_XLEN(val1_hi))		;\
      inst destreg, reg1, imm_val		;\
      RVTEST_SIGUPD(swreg,destreg,offset)	;\
      RVMODEL_IO_ASSERT_GPR_EQ(testreg, destreg, correctval);

//Tests for a instruction with pair register rd, pair register rs1 and pair register rs2
#define TEST_P64_PPP_OP(inst, rd, rd_hi, rs1, rs1_hi, rs2, rs2_hi, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, rs2_val_hi, swreg, offset, testreg) \
    TEST_P64_PPP_OP_32(inst, rd, rd_hi, rs1, rs1_hi, rs2, rs2_hi, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, rs2_val_hi, swreg, offset, testreg)
#define TEST_PK64_PPP_OP(inst, rd, rd_hi, rs1, rs1_hi, rs2, rs2_hi, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, rs2_val_hi, flagreg, swreg, offset, testreg) \
    TEST_PK64_PPP_OP_32(inst, rd, rd_hi, rs1, rs1_hi, rs2, rs2_hi, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, rs2_val_hi, flagreg, swreg, offset, testreg)
//Tests for a instruction with pair register rd, pair register rs1 and normal register rs2
#define TEST_P64_PPN_OP(inst, rd, rd_hi, rs1, rs1_hi, rs2, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, swreg, offset, testreg) \
    TEST_P64_PPN_OP_32(inst, rd, rd_hi, rs1, rs1_hi, rs2, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, swreg, offset, testreg)
//Tests for a instruction with pair register rd, normal register rs1 and normal register rs2
#define TEST_P64_PNN_OP(inst, rd, rd_hi, rs1, rs2, correctval, correctval_hi, rs1_val, rs2_val, swreg, offset, testreg) \
    TEST_P64_PNN_OP_32(inst, rd, rd_hi, rs1, rs2, correctval, correctval_hi, rs1_val, rs2_val, swreg, offset, testreg)
//Tests for a instruction with pair register rd, normal register rs1 and normal register rs2
#define TEST_PK64_PNN_OP(inst, rd, rd_hi, rs1, rs2, correctval, correctval_hi, rs1_val, rs2_val, flagreg, swreg, offset, testreg) \
    TEST_PK64_PNN_OP_32(inst, rd, rd_hi, rs1, rs2, correctval, correctval_hi, rs1_val, rs2_val, flagreg, swreg, offset, testreg)
//Tests for a instruction with normal register rd, pair register rs1 and normal register rs2
#define TEST_P64_NPN_OP(inst, rd, rs1, rs1_hi, rs2, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, swreg, offset, testreg) \
    TEST_P64_NPN_OP_32(inst, rd, rs1, rs1_hi, rs2, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, swreg, offset, testreg)
//Tests for a instruction with normal register rd, pair register rs1
#define TEST_P64_NP_OP(inst, rd, rs1, rs1_hi, correctval, correctval_hi, rs1_val, rs1_val_hi, imm_val, swreg, offset, testreg) \
    TEST_P64_NP_OP_32(inst, rd, rs1, rs1_hi, correctval, correctval_hi, rs1_val, rs1_val_hi, imm_val, swreg, offset, testreg)

#else

// When in rv64, there are no instructions with pair operand, so Macro is redefined to normal TEST_RR_OP
#define TEST_P64_PPP_OP(inst, rd, rd_hi, rs1, rs1_hi, rs2, rs2_hi, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, rs2_val_hi, swreg, offset, testreg) \
    TEST_RR_OP(inst, rd, rs1, rs2, correctval, rs1_val, rs2_val, swreg, offset, testreg)
#define TEST_PK64_PPP_OP(inst, rd, rd_hi, rs1, rs1_hi, rs2, rs2_hi, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, rs2_val_hi, flagreg, swreg, offset, testreg) \
    TEST_PKRR_OP(inst, rd, rs1, rs2, correctval, rs1_val, rs2_val, flagreg, swreg, offset, testreg)
#define TEST_P64_PPN_OP(inst, rd, rd_hi, rs1, rs1_hi, rs2, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, swreg, offset, testreg) \
    TEST_RR_OP(inst, rd, rs1, rs2, correctval, rs1_val, rs2_val, swreg, offset, testreg)
#define TEST_P64_PNN_OP(inst, rd, rd_hi, rs1, rs2, correctval, correctval_hi, rs1_val, rs2_val, swreg, offset, testreg) \
    TEST_RR_OP(inst, rd, rs1, rs2, correctval, rs1_val, rs2_val, swreg, offset, testreg)
#define TEST_PK64_PNN_OP(inst, rd, rd_hi, rs1, rs2, correctval, correctval_hi, rs1_val, rs2_val, flagreg, swreg, offset, testreg) \
    TEST_PKRR_OP(inst, rd, rs1, rs2, correctval, rs1_val, rs2_val, flagreg, swreg, offset, testreg)
#define TEST_P64_NPN_OP(inst, rd, rs1, rs1_hi, rs2, correctval, correctval_hi, rs1_val, rs1_val_hi, rs2_val, swreg, offset, testreg) \
    TEST_RR_OP(inst, rd, rs1, rs2, correctval, rs1_val, rs2_val, swreg, offset, testreg)
#define TEST_P64_NP_OP(inst, rd, rs1, rs1_hi, correctval, correctval_hi, rs1_val, rs1_val_hi, imm_val, swreg, offset, testreg) \
    TEST_IMM_OP(inst, rd, rs1, correctval, rs1_val, imm_val, swreg, offset, testreg)

#endif




#define TEST_CMV_OP( inst, destreg, reg, correctval, val2, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LI(reg, MASK_XLEN(val2))		;\
      inst destreg, reg			;\
      )

#define TEST_CR_OP( inst, destreg, reg, correctval, val1, val2, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LI(reg, MASK_XLEN(val2))		;\
      LI(destreg, MASK_XLEN(val1))	;\
      inst destreg, reg			;\
      )

#define TEST_CI_OP( inst, destreg, correctval, val, imm, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LI(destreg, MASK_XLEN(val))	;\
      inst destreg, imm			;\
      )

#define TEST_CADDI4SPN_OP( inst, destreg, correctval, imm, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LI(x2, 0)				;\
      inst destreg, x2,imm		;\
      )

//Tests for instructions with single (rd/rs1) register operand.
#define TEST_CRD_OP(inst, destreg, correctval, val1, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      LI(destreg, MASK_XLEN(val1))		;\
      inst destreg		;\
      )

//Tests for instructions with a destination and single source register operand
#define TEST_RD_OP(inst, destreg, reg1, correctval, val1, swreg, offset, testreg) \
  TEST_CMV_OP(inst, destreg, reg1, correctval, val1, swreg, offset, testreg)

#define TEST_CBRANCH_OP(inst, tempreg, reg2, val2, imm, label, swreg, offset) \
    LI(reg2, MASK_XLEN(val2))		;\
    j 2f				;\
    addi tempreg, x0,0			;\
    .option push			;\
    .option norvc			;\
1:  addi tempreg, tempreg,0x1		;\
    j 4f				;\
    .option pop				;\
    .if (imm/2) - 4 >= 0		;\
	.set num,(imm/2)-4		;\
    .else				;\
	.set num,0			;\
    .endif				;\
    .ifc label, 3f			;\
	.set num,0			;\
    .endif				;\
    .rept num				;\
    c.nop				;\
    .endr				;\
2:  inst reg2, label			;\
    .option push			;\
    .option norvc			;\
    addi tempreg, tempreg, 0x2		;\
    j 4f				;\
    .option pop				;\
    .if (imm/2) - 5 >= 0		;\
	.set num,(imm/2)-5		;\
    .else				;\
	.set num,0			;\
    .endif				;\
     .ifc label, 1b			;\
	.set num,0			;\
    .endif				;\
    .rept num				;\
    c.nop				;\
    .endr				;\
					;\
3:  addi tempreg, tempreg ,0x3		;\
					;\
4:  RVTEST_SIGUPD(swreg,tempreg,offset) 

#define TEST_CJ_OP(inst, tempreg, imm, label, swreg, offset) \
    .option push			;\
    .option norvc			;\
    j 2f				;\
    addi tempreg,x0,0			;\
1:  addi tempreg, tempreg,0x1		;\
    j 4f				;\
    .option pop				;\
    .if (imm/2) - 4 >= 0		;\
	.set num,(imm/2)-4		;\
    .else				;\
	.set num,0			;\
    .endif				;\
    .ifc label, 3f			;\
	.set num,0			;\
    .endif				;\
    .rept num				;\
    c.nop				;\
    .endr				;\
2:  inst label				;\
    .option push			;\
    .option norvc			;\
    addi tempreg, tempreg, 0x2		;\
    j 4f				;\
    .option pop				;\
    .if (imm/2) - 5 >= 0		;\
	.set num,(imm/2)-5		;\
    .else				;\
	.set num,0			;\
    .endif				;\
     .ifc label, 1b			;\
	.set num,0			;\
    .endif				;\
    .rept num				;\
    c.nop				;\
    .endr				;\
					;\
3:  addi tempreg, tempreg, 0x3		;\
					;\
4:  RVTEST_SIGUPD(swreg,tempreg,offset) 

#define TEST_CJAL_OP(inst, tempreg, imm, label, swreg, offset) \
5:					;\
    j 2f				;\
					;\
    .option push			;\
    .option norvc			;\
1:  xori x1,x1, 0x1			;\
    j 4f				;\
    .option pop				;\
    .if (imm/2) - 4 >= 0		;\
	.set num,(imm/2)-4		;\
    .else				;\
	.set num,0			;\
    .endif				;\
    .ifc label, 3f			;\
	.set num,0			;\
    .endif				;\
    .rept num				;\
    c.nop				;\
    .endr				;\
2:  inst label				;\
    .option push			;\
    .option norvc			;\
    xori x1,x1, 0x2			;\
    j 4f				;\
    .option pop				;\
    .if (imm/2) - 5 >= 0		;\
	.set num,(imm/2)-5		;\
    .else				;\
	.set num,0			;\
    .endif				;\
     .ifc label, 1b			;\
	.set num,0			;\
    .endif				;\
    .rept num				;\
    c.nop				;\
    .endr				;\
					;\
3:  xori x1,x1, 0x3			;\
					;\
4: LA(tempreg, 5b)			;\
   andi tempreg,tempreg,~(3)		;\
    sub x1,x1,tempreg			;\
  RVTEST_SIGUPD(swreg,x1,offset) 

#define TEST_CJR_OP(tempreg, rs1, swreg, offset) \
5:					;\
    LA(rs1, 3f)				;\
					;\
2:  c.jr rs1				;\
    xori rs1,rs1, 0x2			;\
    j 4f				;\
					;\
3:  xori rs1,rs1, 0x3			;\
					;\
4: LA(tempreg, 5b)			;\
   andi tempreg,tempreg,~(3)		;\
    sub rs1,rs1,tempreg			;\
    RVTEST_SIGUPD(swreg,rs1,offset) 

#define TEST_CJALR_OP(tempreg, rs1, swreg, offset) \
5:					;\
    LA(rs1, 3f)				;\
					;\
2:  c.jalr rs1				;\
    xori x1,x1, 0x2			;\
    j 4f				;\
					;\
3:  xori x1,x1, 0x3			;\
					;\
4: LA(tempreg, 5b)			;\
   andi tempreg,tempreg,~(3)		;\
    sub x1,x1,tempreg			;\
    RVTEST_SIGUPD(swreg,x1,offset) 


// for updating signatures of Zacas paired destination register (RV32/RV64).
#define RVTEST_SIGUPD_PZACAS(_BR,_R1,_R2,...)		;\
  .if NARG(__VA_ARGS__) == 1				;\
      .set offset,_ARG1(__VA_OPT__(__VA_ARGS__,0))	;\
  .endif						;\
  .if (offset & (REGWIDTH-1)) != 0			;\
      .warning "Signature Incorrect Offset Alignment."	;\
     .set offset, offset&(SIGALIGN-1)+SIGALIGN		;\
  .endif						;\
      CHK_OFFSET(_BR,REGWIDTH,0)			;\
      SREG _R1,offset(_BR)				;\
      CHK_OFFSET(_BR,REGWIDTH,1)			;\
      SREG _R2,offset(_BR)				;\
      .set offset,offset+(REGWIDTH)

// Tests for a AMOCAS where operation width is <= xlen
// First store a value that will cause a mismatch on cas
// Test a failing amocas followed by a successful amocas
#define TEST_CAS_OP(inst, rd, rs1, rs2, swap_val, sigptr, offset) \
    LI(rd, swap_val);\
    neg rd, rd;\
    LA(rs1, rvtest_data);\
    SREG rd, (rs1);\
    LI(rd, swap_val);\
    LI(rs2, swap_val);\
    LA(rs1, rvtest_data);\
    inst rd, rs2, (rs1);\
    inst rd, rs2, (rs1);\
    RVTEST_SIGUPD(sigptr,rd,offset);

// Tests for a AMOCAS where operation width is <= xlen
// First store a value that will cause a mismatch on cas
// Test a failing amocas followed by a successful amocas
#define TEST_DCAS_OP(inst, rd, rd_hi, rs1, rs2, rs2_hi, swap_val, swap_val_hi, sigptr, offset) \
    LA(rs1, rvtest_data);\
    LI(rd, swap_val);\
    neg rd, rd;\
    SREG rd, (rs1);\
    LI(rd, swap_val_hi);\
    neg rd, rd;\
    SREG rd, (__riscv_xlen/8)(rs1);\
    LI(rd, swap_val);\
    LI(rd_hi, swap_val_hi);\
    LI(rs2, swap_val);\
    LI(rs2_hi, swap_val_hi);\
    LA(rs1, rvtest_data);\
    inst rd, rs2, (rs1);\
    LA(rs1, rvtest_data);\
    inst rd, rs2, (rs1);\
    RVTEST_SIGUPD_PZACAS(sigptr,rd,rd_hi,offset);

//--------------------------------- Migration aliases ------------------------------------------
#ifdef RV_COMPLIANCE_RV32M
  #warning "RV_COMPLIANCE_RV32M macro will be deprecated."
  #define RVMODEL_BOOT	 \
    RVTEST_IO_INIT	;\
    RV_COMPLIANCE_RV32M ;\
    RV_COMPLIANCE_CODE_BEGIN
#endif

#define SWSIG(a, b)
  
#ifdef RV_COMPLIANCE_DATA_BEGIN
  #warning "RV_COMPLIANCE_DATA_BEGIN macro deprecated in v0.2. Please use RVMODEL_DATA_BEGIN instead"
  #define RVMODEL_DATA_BEGIN \
    RV_COMPLIANCE_DATA_BEGIN
#endif

#ifdef RV_COMPLIANCE_DATA_END
  #warning "RV_COMPLIANCE_DATA_END macro deprecated in v0.2. Please use RVMODEL_DATA_END instead"
  #define RVMODEL_DATA_END \
    RV_COMPLIANCE_DATA_END
#endif

#ifdef RV_COMPLIANCE_HALT
  #warning "RV_COMPLIANCE_HALT macro deprecated in v0.2. Please use RVMODEL_HALT instead"
  #define RVMODEL_HALT \
    RV_COMPLIANCE_HALT
#endif

#ifdef RVTEST_IO_ASSERT_GPR_EQ
  #warning "RVTEST_IO_ASSERT_GPR_EQ macro deprecated in v0.2. Please use RVMODEL_IO_ASSERT_GPR_EQ instead"
  #define RVMODEL_IO_ASSERT_GPR_EQ(_SP, _R, _I) \
    RVTEST_IO_ASSERT_GPR_EQ(_SP,_R, _I)
#endif

#ifdef RVTEST_IO_WRITE_STR 
  #warning "RVTEST_IO_WRITE_STR macro deprecated in v0.2. Please use RVMODEL_IO_WRITE_STR instead"
  #define RVMODEL_IO_WRITE_STR(_SP, _STR) \
    RVTEST_IO_WRITE_STR(_SP, _STR)
#endif

#ifdef RVTEST_IO_INIT
  #warning "RVTEST_IO_INIT is deprecated in v0.2. Please use RVMODEL_BOOT for initialization"
#endif
  
#ifdef RVTEST_IO_CHECK
  #warning "RVTEST_IO_CHECK is deprecated in v0.2.
#endif
