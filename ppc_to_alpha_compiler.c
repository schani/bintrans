/*
 * ppc_to_alpha_compiler.c
 *
 * bintrans
 *
 * Copyright (C) 2001 Mark Probst
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define FIELD_MBE    ((insn >> 5) & 0x1)
#define FIELD_CRZ2    ((insn >> 20) & 0x1)
#define FIELD_CRZ1    ((insn >> 11) & 0x1)
#define FIELD_CRM    ((insn >> 12) & 0xFF)
#define FIELD_FMZ2    ((insn >> 25) & 0x1)
#define FIELD_FMZ1    ((insn >> 16) & 0x1)
#define FIELD_FM    ((insn >> 17) & 0xFF)
#define FIELD_TBRL    ((insn >> 16) & 0x1F)
#define FIELD_TBRH    ((insn >> 11) & 0x1F)
#define FIELD_SPR    ((insn >> 11) & 0x3FF)
#define FIELD_IMMZ    ((insn >> 11) & 0x1)
#define FIELD_IMM    ((insn >> 12) & 0xF)
#define FIELD_RC    ((insn >> 0) & 0x1)
#define FIELD_SRZ    ((insn >> 20) & 0x1)
#define FIELD_SR    ((insn >> 16) & 0xF)
#define FIELD_OE    ((insn >> 10) & 0x1)
#define FIELD_XO9    ((insn >> 1) & 0x1FF)
#define FIELD_XO5    ((insn >> 1) & 0x1F)
#define FIELD_XO4    ((insn >> 2) & 0x7)
#define FIELD_XO2    ((insn >> 2) & 0x1FF)
#define FIELD_XO1    ((insn >> 1) & 0x3FF)
#define FIELD_XO0    ((insn >> 0) & 0x3)
#define FIELD_DS    ((insn >> 2) & 0x3FFF)
#define FIELD_UIMM    ((insn >> 0) & 0xFFFF)
#define FIELD_SIMM    ((insn >> 0) & 0xFFFF)
#define FIELD_D    ((insn >> 0) & 0xFFFF)
#define FIELD_ME    ((insn >> 1) & 0x1F)
#define FIELD_MB    ((insn >> 6) & 0x1F)
#define FIELD_FRC    ((insn >> 6) & 0x1F)
#define FIELD_C    ((insn >> 6) & 0x1F)
#define FIELD_SH    ((insn >> 11) & 0x1F)
#define FIELD_NB    ((insn >> 11) & 0x1F)
#define FIELD_CRBB    ((insn >> 11) & 0x1F)
#define FIELD_FRB    ((insn >> 11) & 0x1F)
#define FIELD_RB    ((insn >> 11) & 0x1F)
#define FIELD_CRSZ    ((insn >> 16) & 0x3)
#define FIELD_CRFS    ((insn >> 18) & 0x7)
#define FIELD_CRBA    ((insn >> 16) & 0x1F)
#define FIELD_FRA    ((insn >> 16) & 0x1F)
#define FIELD_RA    ((insn >> 16) & 0x1F)
#define FIELD_BICC    ((insn >> 16) & 0x3)
#define FIELD_BICR    ((insn >> 18) & 0x7)
#define FIELD_BI    ((insn >> 16) & 0x1F)
#define FIELD_TO    ((insn >> 21) & 0x1F)
#define FIELD_LZ    ((insn >> 22) & 0x1)
#define FIELD_L    ((insn >> 21) & 0x1)
#define FIELD_CRDZ    ((insn >> 21) & 0x3)
#define FIELD_CRFD    ((insn >> 23) & 0x7)
#define FIELD_CRBD    ((insn >> 21) & 0x1F)
#define FIELD_FRD    ((insn >> 21) & 0x1F)
#define FIELD_RD    ((insn >> 21) & 0x1F)
#define FIELD_FRS    ((insn >> 21) & 0x1F)
#define FIELD_RS    ((insn >> 21) & 0x1F)
#define FIELD_BO    ((insn >> 21) & 0x1F)
#define FIELD_BD    ((insn >> 2) & 0x3FFF)
#define FIELD_LK    ((insn >> 0) & 0x1)
#define FIELD_AA    ((insn >> 1) & 0x1)
#define FIELD_LI    ((insn >> 2) & 0xFFFFFF)
#define FIELD_OPCD    ((insn >> 26) & 0x3F)

#define SPR_LR      0
#define SPR_CR      1
#define SPR_XER     2
#define SPR_CTR     3
#define SPR_FPSCR   4

#define ref_ppc_spr_r(x)        ref_integer_reg_for_reading(x)
#define ref_ppc_spr_w(x)        ref_integer_reg_for_writing(x)
#define ref_ppc_spr_rw(x)       ref_integer_reg_for_reading_and_writing(x)

#define ref_ppc_gpr_r(x)        ref_integer_reg_for_reading(x + 5)
#define ref_ppc_gpr_w(x)        ref_integer_reg_for_writing(x + 5)
#define ref_ppc_gpr_rw(x)       ref_integer_reg_for_reading_and_writing(x + 5)

#define ref_ppc_fpr_r(x)        ref_float_reg_for_reading(x + 5 + 32)
#define ref_ppc_fpr_w(x)        ref_float_reg_for_writing(x + 5 + 32)
#define ref_ppc_fpr_rw(x)       ref_float_reg_for_reading_and_writing(x + 5 + 32)

#define ref_ppc_crf0_r(x)       ref_integer_reg_for_reading(x + 5 + 32 + 32)
#define ref_ppc_crf0_w(x)       ref_integer_reg_for_writing(x + 5 + 32 + 32)
#define ref_ppc_crf0_rw(x)      ref_integer_reg_for_reading_and_writing(x + 5 + 32 + 32)

#define ref_ppc_xer_so_r()      ref_integer_reg_for_reading(0 + 5 + 32 + 32 + 4)
#define ref_ppc_xer_so_w()      ref_integer_reg_for_writing(0 + 5 + 32 + 32 + 4)
#define ref_ppc_xer_so_rw()     ref_integer_reg_for_reading_and_writing(0 + 5 + 32 + 32 + 4)

#define ref_ppc_xer_ca_r()      ref_integer_reg_for_reading(1 + 5 + 32 + 32 + 4)
#define ref_ppc_xer_ca_w()      ref_integer_reg_for_writing(1 + 5 + 32 + 32 + 4)
#define ref_ppc_xer_ca_rw()     ref_integer_reg_for_reading_and_writing(1 + 5 + 32 + 32 + 4)

#define KILL_GPR(x)             (kill_gpr & (1 << (x)))
#define KILL_FPR(x)             1
#define KILL_CRFB(x)            (kill_cr & (1 << (31 - (x))))
#define KILL_XER_CA             (kill_xer & (1 << 29))

#define SCRATCH_OFFSET   ((5+32+32+4+2)*8)

#if 0
#define announce(x)             fprintf(stderr, "translating %s\n", x); sleep(1)
#else
#define announce(x)
#endif

#ifdef DYNAMO_TRACES
#define NO_DYNAMO_TRACES         0
#else
#define NO_DYNAMO_TRACES         1
#endif

void
move_ppc_regs_interpreter_to_compiler (interpreter_t *intp)
{
    *(word_32*)&constant_area[0 * 2] = (intp->regs_SPR[0]); /* lr */
    *(word_32*)&constant_area[1 * 2] = (intp->regs_SPR[1] & 0x0fffffff); /* cr */
    *(word_32*)&constant_area[2 * 2] = (intp->regs_SPR[2] & 0x5fffffff); /* xer */
    *(word_32*)&constant_area[3 * 2] = (intp->regs_SPR[3]); /* ctr */
    *(word_32*)&constant_area[4 * 2] = (intp->regs_SPR[4]); /* fpscr */

    *(word_32*)&constant_area[5 * 2] = (intp->regs_GPR[0]);
    *(word_32*)&constant_area[6 * 2] = (intp->regs_GPR[1]);
    *(word_32*)&constant_area[7 * 2] = (intp->regs_GPR[2]);
    *(word_32*)&constant_area[8 * 2] = (intp->regs_GPR[3]);
    *(word_32*)&constant_area[9 * 2] = (intp->regs_GPR[4]);
    *(word_32*)&constant_area[10 * 2] = (intp->regs_GPR[5]);
    *(word_32*)&constant_area[11 * 2] = (intp->regs_GPR[6]);
    *(word_32*)&constant_area[12 * 2] = (intp->regs_GPR[7]);
    *(word_32*)&constant_area[13 * 2] = (intp->regs_GPR[8]);
    *(word_32*)&constant_area[14 * 2] = (intp->regs_GPR[9]);
    *(word_32*)&constant_area[15 * 2] = (intp->regs_GPR[10]);
    *(word_32*)&constant_area[16 * 2] = (intp->regs_GPR[11]);
    *(word_32*)&constant_area[17 * 2] = (intp->regs_GPR[12]);
    *(word_32*)&constant_area[18 * 2] = (intp->regs_GPR[13]);
    *(word_32*)&constant_area[19 * 2] = (intp->regs_GPR[14]);
    *(word_32*)&constant_area[20 * 2] = (intp->regs_GPR[15]);
    *(word_32*)&constant_area[21 * 2] = (intp->regs_GPR[16]);
    *(word_32*)&constant_area[22 * 2] = (intp->regs_GPR[17]);
    *(word_32*)&constant_area[23 * 2] = (intp->regs_GPR[18]);
    *(word_32*)&constant_area[24 * 2] = (intp->regs_GPR[19]);
    *(word_32*)&constant_area[25 * 2] = (intp->regs_GPR[20]);
    *(word_32*)&constant_area[26 * 2] = (intp->regs_GPR[21]);
    *(word_32*)&constant_area[27 * 2] = (intp->regs_GPR[22]);
    *(word_32*)&constant_area[28 * 2] = (intp->regs_GPR[23]);
    *(word_32*)&constant_area[29 * 2] = (intp->regs_GPR[24]);
    *(word_32*)&constant_area[30 * 2] = (intp->regs_GPR[25]);
    *(word_32*)&constant_area[31 * 2] = (intp->regs_GPR[26]);
    *(word_32*)&constant_area[32 * 2] = (intp->regs_GPR[27]);
    *(word_32*)&constant_area[33 * 2] = (intp->regs_GPR[28]);
    *(word_32*)&constant_area[34 * 2] = (intp->regs_GPR[29]);
    *(word_32*)&constant_area[35 * 2] = (intp->regs_GPR[30]);
    *(word_32*)&constant_area[36 * 2] = (intp->regs_GPR[31]);

    *(double*)&constant_area[37 * 2] = (intp->regs_FPR[0]);
    *(double*)&constant_area[38 * 2] = (intp->regs_FPR[1]);
    *(double*)&constant_area[39 * 2] = (intp->regs_FPR[2]);
    *(double*)&constant_area[40 * 2] = (intp->regs_FPR[3]);
    *(double*)&constant_area[41 * 2] = (intp->regs_FPR[4]);
    *(double*)&constant_area[42 * 2] = (intp->regs_FPR[5]);
    *(double*)&constant_area[43 * 2] = (intp->regs_FPR[6]);
    *(double*)&constant_area[44 * 2] = (intp->regs_FPR[7]);
    *(double*)&constant_area[45 * 2] = (intp->regs_FPR[8]);
    *(double*)&constant_area[46 * 2] = (intp->regs_FPR[9]);
    *(double*)&constant_area[47 * 2] = (intp->regs_FPR[10]);
    *(double*)&constant_area[48 * 2] = (intp->regs_FPR[11]);
    *(double*)&constant_area[49 * 2] = (intp->regs_FPR[12]);
    *(double*)&constant_area[50 * 2] = (intp->regs_FPR[13]);
    *(double*)&constant_area[51 * 2] = (intp->regs_FPR[14]);
    *(double*)&constant_area[52 * 2] = (intp->regs_FPR[15]);
    *(double*)&constant_area[53 * 2] = (intp->regs_FPR[16]);
    *(double*)&constant_area[54 * 2] = (intp->regs_FPR[17]);
    *(double*)&constant_area[55 * 2] = (intp->regs_FPR[18]);
    *(double*)&constant_area[56 * 2] = (intp->regs_FPR[19]);
    *(double*)&constant_area[57 * 2] = (intp->regs_FPR[20]);
    *(double*)&constant_area[58 * 2] = (intp->regs_FPR[21]);
    *(double*)&constant_area[59 * 2] = (intp->regs_FPR[22]);
    *(double*)&constant_area[60 * 2] = (intp->regs_FPR[23]);
    *(double*)&constant_area[61 * 2] = (intp->regs_FPR[24]);
    *(double*)&constant_area[62 * 2] = (intp->regs_FPR[25]);
    *(double*)&constant_area[63 * 2] = (intp->regs_FPR[26]);
    *(double*)&constant_area[64 * 2] = (intp->regs_FPR[27]);
    *(double*)&constant_area[65 * 2] = (intp->regs_FPR[28]);
    *(double*)&constant_area[66 * 2] = (intp->regs_FPR[29]);
    *(double*)&constant_area[67 * 2] = (intp->regs_FPR[30]);
    *(double*)&constant_area[68 * 2] = (intp->regs_FPR[31]);

    *(word_32*)&constant_area[69 * 2] = (intp->regs_SPR[1] >> 31) & 1;
    *(word_32*)&constant_area[70 * 2] = (intp->regs_SPR[1] >> 30) & 1;
    *(word_32*)&constant_area[71 * 2] = (intp->regs_SPR[1] >> 29) & 1;
    *(word_32*)&constant_area[72 * 2] = (intp->regs_SPR[1] >> 28) & 1;

    *(word_32*)&constant_area[73 * 2] = (intp->regs_SPR[2] >> 31) & 1; /* so */
    *(word_32*)&constant_area[74 * 2] = (intp->regs_SPR[2] >> 29) & 1; /* ca */
}

void
move_ppc_regs_compiler_to_interpreter (interpreter_t *intp)
{
    (intp->regs_SPR[0]) = *(word_32*)&constant_area[0 * 2];
    (intp->regs_SPR[1]) = *(word_32*)&constant_area[1 * 2] /* cr */
	| (*(word_32*)&constant_area[69 * 2] << 31)
	| (*(word_32*)&constant_area[70 * 2] << 30)
	| (*(word_32*)&constant_area[71 * 2] << 29)
	| (*(word_32*)&constant_area[72 * 2] << 28);
    (intp->regs_SPR[2]) = *(word_32*)&constant_area[2 * 2] /* xer */
	| (*(word_32*)&constant_area[73 * 2] << 31)
	| (*(word_32*)&constant_area[74 * 2] << 29);
    (intp->regs_SPR[3]) = *(word_32*)&constant_area[3 * 2];
    (intp->regs_SPR[4]) = *(word_32*)&constant_area[4 * 2];

    (intp->regs_GPR[0]) = *(word_32*)&constant_area[5 * 2];
    (intp->regs_GPR[1]) = *(word_32*)&constant_area[6 * 2];
    (intp->regs_GPR[2]) = *(word_32*)&constant_area[7 * 2];
    (intp->regs_GPR[3]) = *(word_32*)&constant_area[8 * 2];
    (intp->regs_GPR[4]) = *(word_32*)&constant_area[9 * 2];
    (intp->regs_GPR[5]) = *(word_32*)&constant_area[10 * 2];
    (intp->regs_GPR[6]) = *(word_32*)&constant_area[11 * 2];
    (intp->regs_GPR[7]) = *(word_32*)&constant_area[12 * 2];
    (intp->regs_GPR[8]) = *(word_32*)&constant_area[13 * 2];
    (intp->regs_GPR[9]) = *(word_32*)&constant_area[14 * 2];
    (intp->regs_GPR[10]) = *(word_32*)&constant_area[15 * 2];
    (intp->regs_GPR[11]) = *(word_32*)&constant_area[16 * 2];
    (intp->regs_GPR[12]) = *(word_32*)&constant_area[17 * 2];
    (intp->regs_GPR[13]) = *(word_32*)&constant_area[18 * 2];
    (intp->regs_GPR[14]) = *(word_32*)&constant_area[19 * 2];
    (intp->regs_GPR[15]) = *(word_32*)&constant_area[20 * 2];
    (intp->regs_GPR[16]) = *(word_32*)&constant_area[21 * 2];
    (intp->regs_GPR[17]) = *(word_32*)&constant_area[22 * 2];
    (intp->regs_GPR[18]) = *(word_32*)&constant_area[23 * 2];
    (intp->regs_GPR[19]) = *(word_32*)&constant_area[24 * 2];
    (intp->regs_GPR[20]) = *(word_32*)&constant_area[25 * 2];
    (intp->regs_GPR[21]) = *(word_32*)&constant_area[26 * 2];
    (intp->regs_GPR[22]) = *(word_32*)&constant_area[27 * 2];
    (intp->regs_GPR[23]) = *(word_32*)&constant_area[28 * 2];
    (intp->regs_GPR[24]) = *(word_32*)&constant_area[29 * 2];
    (intp->regs_GPR[25]) = *(word_32*)&constant_area[30 * 2];
    (intp->regs_GPR[26]) = *(word_32*)&constant_area[31 * 2];
    (intp->regs_GPR[27]) = *(word_32*)&constant_area[32 * 2];
    (intp->regs_GPR[28]) = *(word_32*)&constant_area[33 * 2];
    (intp->regs_GPR[29]) = *(word_32*)&constant_area[34 * 2];
    (intp->regs_GPR[30]) = *(word_32*)&constant_area[35 * 2];
    (intp->regs_GPR[31]) = *(word_32*)&constant_area[36 * 2];

    (intp->regs_FPR[0]) = *(double*)&constant_area[37 * 2];
    (intp->regs_FPR[1]) = *(double*)&constant_area[38 * 2];
    (intp->regs_FPR[2]) = *(double*)&constant_area[39 * 2];
    (intp->regs_FPR[3]) = *(double*)&constant_area[40 * 2];
    (intp->regs_FPR[4]) = *(double*)&constant_area[41 * 2];
    (intp->regs_FPR[5]) = *(double*)&constant_area[42 * 2];
    (intp->regs_FPR[6]) = *(double*)&constant_area[43 * 2];
    (intp->regs_FPR[7]) = *(double*)&constant_area[44 * 2];
    (intp->regs_FPR[8]) = *(double*)&constant_area[45 * 2];
    (intp->regs_FPR[9]) = *(double*)&constant_area[46 * 2];
    (intp->regs_FPR[10]) = *(double*)&constant_area[47 * 2];
    (intp->regs_FPR[11]) = *(double*)&constant_area[48 * 2];
    (intp->regs_FPR[12]) = *(double*)&constant_area[49 * 2];
    (intp->regs_FPR[13]) = *(double*)&constant_area[50 * 2];
    (intp->regs_FPR[14]) = *(double*)&constant_area[51 * 2];
    (intp->regs_FPR[15]) = *(double*)&constant_area[52 * 2];
    (intp->regs_FPR[16]) = *(double*)&constant_area[53 * 2];
    (intp->regs_FPR[17]) = *(double*)&constant_area[54 * 2];
    (intp->regs_FPR[18]) = *(double*)&constant_area[55 * 2];
    (intp->regs_FPR[19]) = *(double*)&constant_area[56 * 2];
    (intp->regs_FPR[20]) = *(double*)&constant_area[57 * 2];
    (intp->regs_FPR[21]) = *(double*)&constant_area[58 * 2];
    (intp->regs_FPR[22]) = *(double*)&constant_area[59 * 2];
    (intp->regs_FPR[23]) = *(double*)&constant_area[60 * 2];
    (intp->regs_FPR[24]) = *(double*)&constant_area[61 * 2];
    (intp->regs_FPR[25]) = *(double*)&constant_area[62 * 2];
    (intp->regs_FPR[26]) = *(double*)&constant_area[63 * 2];
    (intp->regs_FPR[27]) = *(double*)&constant_area[64 * 2];
    (intp->regs_FPR[28]) = *(double*)&constant_area[65 * 2];
    (intp->regs_FPR[29]) = *(double*)&constant_area[66 * 2];
    (intp->regs_FPR[30]) = *(double*)&constant_area[67 * 2];
    (intp->regs_FPR[31]) = *(double*)&constant_area[68 * 2];
}

static int optimize_taken_jump;
static label_t taken_jump_label;
static word_32 next_pc;

static word_32 kill_cr, kill_xer, kill_gpr;

static void
gen_rc_code (reg_t reg)
{
    if (KILL_CRFB(0))
    {
	reg_t bit_reg;

	bit_reg = ref_ppc_crf0_w(0);

	emit(COMPOSE_CMPLT(reg, 31, bit_reg));

	unref_integer_reg(bit_reg);
    }
    if (KILL_CRFB(1))
    {
	reg_t bit_reg;

	bit_reg = ref_ppc_crf0_w(1);

	emit(COMPOSE_CMPLT(31, reg, bit_reg));

	unref_integer_reg(bit_reg);
    }
    if (KILL_CRFB(2))
    {
	reg_t bit_reg;

	bit_reg = ref_ppc_crf0_w(2);

	emit(COMPOSE_CMPEQ(reg, 31, bit_reg));

	unref_integer_reg(bit_reg);
    }
    if (KILL_CRFB(3))
    {
	reg_t so_reg, bit_reg;

	so_reg = ref_ppc_xer_so_r();
	bit_reg = ref_ppc_crf0_w(3);

	emit(COMPOSE_MOV(so_reg, bit_reg));

	unref_integer_reg(bit_reg);
	unref_integer_reg(so_reg);
    }
}

static void
gen_simple_xo9_rc_insn (word_32 insn, void (*emitter) (reg_t, reg_t, reg_t))
{
    reg_t rd_reg, ra_reg, rb_reg;

    ra_reg = ref_ppc_gpr_r(FIELD_RA);
    rb_reg = ref_ppc_gpr_r(FIELD_RB);
    rd_reg = ref_ppc_gpr_w(FIELD_RD);

    emitter(ra_reg, rb_reg, rd_reg);

    unref_integer_reg(ra_reg);
    unref_integer_reg(rb_reg);

    if (FIELD_RC)
	gen_rc_code(rd_reg);

    unref_integer_reg(rd_reg);
}

static void
emit_addl (reg_t a, reg_t b, reg_t d)
{
    emit(COMPOSE_ADDL(a, b, d));
}

static void
handle_add_insn (word_32 insn, word_32 pc)
{
    gen_simple_xo9_rc_insn(insn, emit_addl);
}

static void
handle_addd_insn (word_32 insn, word_32 pc)
{
    handle_add_insn(insn, pc);
}

static void
handle_addc_insn (word_32 insn, word_32 pc)
{
    if (KILL_XER_CA)
    {
	reg_t ra_reg, rb_reg, ca_reg, tmp_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	ca_reg = ref_ppc_xer_ca_w();

	emit(COMPOSE_ZAPNOT_IMM(ra_reg, 15, ca_reg));

	unref_integer_reg(ra_reg);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(rb_reg, 15, tmp_reg));

	unref_integer_reg(rb_reg);

	emit(COMPOSE_ADDQ(ca_reg, tmp_reg, ca_reg));

	free_tmp_integer_reg(tmp_reg);

	if (KILL_GPR(FIELD_RD))
	{
	    reg_t rd_reg;

	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_ADDL(ca_reg, 31, rd_reg));

	    if (FIELD_RC)
		gen_rc_code(rd_reg);

	    unref_integer_reg(rd_reg);
	}

	emit(COMPOSE_SRL_IMM(ca_reg, 32, ca_reg));

	unref_integer_reg(ca_reg);
    }
    else if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, rd_reg));

	unref_integer_reg(rd_reg);
	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_addcd_insn (word_32 insn, word_32 pc)
{
    handle_addc_insn(insn, pc);
}

static void
handle_adde_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD) && KILL_XER_CA)
    {
	reg_t ra_reg, rb_reg, rd_reg, ca_reg, tmp_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(ra_reg, 15, tmp_reg));

	unref_integer_reg(ra_reg);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_ZAPNOT_IMM(rb_reg, 15, rd_reg));

	unref_integer_reg(rb_reg);

	emit(COMPOSE_ADDQ(tmp_reg, rd_reg, rd_reg));

	free_tmp_integer_reg(tmp_reg);
	ca_reg = ref_ppc_xer_ca_rw();

	emit(COMPOSE_ADDQ(rd_reg, ca_reg, rd_reg));
	emit(COMPOSE_SRL_IMM(rd_reg, 32, ca_reg));

	unref_integer_reg(ca_reg);

	emit(COMPOSE_ADDL(rd_reg, 31, rd_reg));

	if (FIELD_RC)
	    gen_rc_code(rd_reg);

	unref_integer_reg(rd_reg);
    }
    else if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg, ca_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, rd_reg));

	unref_integer_reg(ra_reg);
	unref_integer_reg(rb_reg);
	ca_reg = ref_ppc_xer_ca_r();

	emit(COMPOSE_ADDL(rd_reg, ca_reg, rd_reg));

	unref_integer_reg(ca_reg);

	if (FIELD_RC)
	    gen_rc_code(rd_reg);

	unref_integer_reg(rd_reg);
    }
    else if (KILL_XER_CA)
    {
	reg_t ra_reg, rb_reg, tmp1_reg, tmp2_reg, ca_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	tmp1_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(ra_reg, 15, tmp1_reg));

	unref_integer_reg(ra_reg);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	tmp2_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(rb_reg, 15, tmp2_reg));

	unref_integer_reg(rb_reg);

	emit(COMPOSE_ADDQ(tmp1_reg, tmp2_reg, tmp1_reg));

	free_tmp_integer_reg(tmp2_reg);
	ca_reg = ref_ppc_xer_ca_rw();

	emit(COMPOSE_ADDQ(tmp1_reg, ca_reg, tmp1_reg));
	emit(COMPOSE_SRL_IMM(tmp1_reg, 32, ca_reg));

	unref_integer_reg(ca_reg);
	free_tmp_integer_reg(tmp1_reg);

	assert(!FIELD_RC);
    }
}

static void
handle_added_insn (word_32 insn, word_32 pc)
{
    handle_adde_insn(insn, pc);
}

static void
handle_addi_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rd_reg;

	if (FIELD_RA == 0)
	{
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDA(rd_reg, FIELD_SIMM, 31));

	    unref_integer_reg(rd_reg);
	}
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    if ((FIELD_SIMM & 0xff00) == 0)
		emit(COMPOSE_ADDL_IMM(ra_reg, FIELD_SIMM, rd_reg));
	    else if ((FIELD_SIMM & 0xff00) == 0xff00 && (FIELD_SIMM & 0x00ff) != 0)
		emit(COMPOSE_SUBL_IMM(ra_reg, (~(FIELD_SIMM & 0xff) & 0xff) + 1, rd_reg));
	    else
	    {
		emit(COMPOSE_LDA(rd_reg, FIELD_SIMM, ra_reg));
		emit(COMPOSE_ADDL(rd_reg, 31, rd_reg));
	    }

	    unref_integer_reg(rd_reg);
	    unref_integer_reg(ra_reg);
	}
    }
}

static void
gen_addic (word_32 insn, reg_t *result_reg)
{
    reg_t ra_reg;

    if (KILL_XER_CA)
    {
	reg_t ca_reg, rd_reg, tmp1_reg, tmp2_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	tmp1_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(ra_reg, 15, tmp1_reg));

	unref_integer_reg(ra_reg);
	tmp2_reg = alloc_tmp_integer_reg();

	emit_load_integer_64(tmp2_reg, SEX32(FIELD_SIMM, 16));

	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_ADDQ(tmp1_reg, tmp2_reg, tmp1_reg));

	free_tmp_integer_reg(tmp2_reg);
	ca_reg = ref_ppc_xer_ca_w();

	emit(COMPOSE_SRL_IMM(tmp1_reg, 32, ca_reg));

	unref_integer_reg(ca_reg);

	if (KILL_GPR(FIELD_RD))
	{
	    emit(COMPOSE_ADDL(tmp1_reg, 31, rd_reg));

	    if (result_reg != 0)
		*result_reg = rd_reg;
	    else
		unref_integer_reg(rd_reg);
	}
	else
	    assert(result_reg == 0);

	free_tmp_integer_reg(tmp1_reg);
    }
    else
    {
	reg_t rd_reg;

	assert(KILL_GPR(FIELD_RD));

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	if ((FIELD_SIMM & 0xff00) == 0)
	    emit(COMPOSE_ADDL_IMM(ra_reg, FIELD_SIMM, rd_reg));
	else
	{
	    emit(COMPOSE_LDA(rd_reg, FIELD_SIMM, ra_reg));
	    emit(COMPOSE_ADDL(rd_reg, 31, rd_reg));
	}

	if (result_reg != 0)
	    *result_reg = rd_reg;
	else
	    unref_integer_reg(rd_reg);

	unref_integer_reg(ra_reg);
    }
}

static void
handle_addic_insn (word_32 insn, word_32 pc)
{
    gen_addic(insn, 0);
}

static void
handle_addicd_insn (word_32 insn, word_32 pc)
{
    reg_t rd_reg;

    gen_addic(insn, &rd_reg);
    gen_rc_code(rd_reg);

    unref_integer_reg(rd_reg);
}

static void
handle_addis_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rd_reg;

	if (FIELD_RA == 0)
	{
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDAH(rd_reg, FIELD_SIMM, 31));

	    unref_integer_reg(rd_reg);
	}
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDAH(rd_reg, FIELD_SIMM, ra_reg));

	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_ADDL(rd_reg, 31, rd_reg));

	    unref_integer_reg(rd_reg);
	}
    }
}

static void
handle_addze_insn (word_32 insn, word_32 pc)
{
    if (KILL_XER_CA)
    {
	reg_t ra_reg, rd_reg, ca_reg, tmp_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	ca_reg = ref_ppc_xer_ca_rw();
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_ADDL(ra_reg, ca_reg, rd_reg));

	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_CMPEQ(rd_reg, 31, tmp_reg));

	unref_integer_reg(rd_reg);

	emit(COMPOSE_AND(tmp_reg, ca_reg, ca_reg));

	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(ca_reg);
	unref_integer_reg(ra_reg);
    }
    else if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rd_reg, ca_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	ca_reg = ref_ppc_xer_ca_r();
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_ADDL(ra_reg, ca_reg, rd_reg));

	unref_integer_reg(rd_reg);
	unref_integer_reg(ca_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
gen_simple_xo1_rc_insn (word_32 insn, void (*emitter) (reg_t, reg_t, reg_t))
{
    reg_t ra_reg, rs_reg, rb_reg;

    rs_reg = ref_ppc_gpr_r(FIELD_RS);
    rb_reg = ref_ppc_gpr_r(FIELD_RB);
    ra_reg = ref_ppc_gpr_w(FIELD_RA);

    emitter(rs_reg, rb_reg, ra_reg);

    unref_integer_reg(rb_reg);
    unref_integer_reg(rs_reg);

    if (FIELD_RC)
	gen_rc_code(ra_reg);

    unref_integer_reg(ra_reg);
}

static void
emit_and (reg_t a, reg_t b, reg_t d)
{
    emit(COMPOSE_AND(a, b, d));
}

static void
handle_and_insn (word_32 insn, word_32 pc)
{
    gen_simple_xo1_rc_insn(insn, emit_and);
}

static void
handle_andd_insn (word_32 insn, word_32 pc)
{
    handle_and_insn(insn, pc);
}

static void
emit_bic (reg_t a, reg_t b, reg_t d)
{
    emit(COMPOSE_BIC(a, b, d));
}

static void
handle_andc_insn (word_32 insn, word_32 pc)
{
    gen_simple_xo1_rc_insn(insn, emit_bic);
}

static void
handle_andcd_insn (word_32 insn, word_32 pc)
{
    handle_andc_insn(insn, pc);
}

static void
handle_andid_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	if ((FIELD_UIMM & 0xff00) == 0)
	    emit(COMPOSE_AND_IMM(rs_reg, FIELD_UIMM, ra_reg));
	else
	{
	    reg_t tmp_reg;

	    tmp_reg = alloc_tmp_integer_reg();

	    emit_load_integer_32(tmp_reg, FIELD_UIMM);
	    emit(COMPOSE_AND(rs_reg, tmp_reg, ra_reg));

	    free_tmp_integer_reg(tmp_reg);
	}

	unref_integer_reg(rs_reg);

	gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
}

static void
handle_andisd_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	reg_t rs_reg, ra_reg, tmp_reg;

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, FIELD_UIMM << 16);
	emit(COMPOSE_AND(rs_reg, tmp_reg, ra_reg));

	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(rs_reg);

	gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
}

static void
handle_b_insn (word_32 insn, word_32 pc)
{
    if (optimize_taken_jump)
    {
	emit_branch(COMPOSE_BR(31, 0), taken_jump_label);
	have_jumped = 1;
    }
    else if (next_pc != NO_FOREIGN_ADDR)
	assert(pc + (SEX32(FIELD_LI, 24) << 2) == next_pc);
    else
    {
	word_32 target = pc + (SEX32(FIELD_LI, 24) << 2);

	emit_freeze_save();

	emit_start_direct_jump(1);

	emit_store_regs(EMU_INSN_EPILOGUE, target);

	emit_direct_jump(target);
    }
}

static void
gen_indirect_branch (void)
{
    assert(!optimize_taken_jump);

    if (NO_DYNAMO_TRACES || next_pc == NO_FOREIGN_ADDR)
    {
	emit_freeze_save();

	emit_store_regs(EMU_INSN_EPILOGUE, NO_FOREIGN_ADDR);

	emit_indirect_jump();
    }
    else
    {
	reg_t tmp_reg;
	label_t alt_label = alloc_label();

	emit_freeze_save();

	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, next_pc);
	emit(COMPOSE_CMPEQ(tmp_reg, JUMP_TARGET_REG, tmp_reg));

	emit_branch(COMPOSE_BEQ(tmp_reg, 0), alt_label);

	free_tmp_integer_reg(tmp_reg);

	emit_begin_alt();

	emit_label(alt_label);
	free_label(alt_label);

	emit_store_regs(EMU_INSN_EPILOGUE, NO_FOREIGN_ADDR);

	emit_indirect_jump();

	emit_end_alt();
    }
}

static void
handle_bctr_insn (word_32 insn, word_32 pc)
{
    reg_t ctr_reg;

    ctr_reg = ref_ppc_spr_r(SPR_CTR);

    emit(COMPOSE_BIC_IMM(ctr_reg, 3, JUMP_TARGET_REG));

    unref_integer_reg(ctr_reg);

    gen_indirect_branch();
}

static void
gen_cond_branch (word_32 insn, word_32 pc, void (*pos_emitter) (reg_t, label_t), void (*neg_emitter) (reg_t, label_t),
		 reg_t cond_reg, int unref_cond_reg)
{
    if (optimize_taken_jump)
    {
	pos_emitter(cond_reg, taken_jump_label);

	if (unref_cond_reg)
	    unref_integer_reg(cond_reg);

	have_jumped = 1;
    }
    else if (NO_DYNAMO_TRACES || next_pc == NO_FOREIGN_ADDR)
    {
	label_t end_label = alloc_label();
	word_32 target = pc + (SEX32(FIELD_BD, 14) << 2);

	emit_freeze_save();

	neg_emitter(cond_reg, end_label);

	if (unref_cond_reg)
	    unref_integer_reg(cond_reg);

	emit_start_direct_jump(1);

	emit_store_regs(EMU_INSN_EPILOGUE, target);

	emit_direct_jump(target);

	emit_label(end_label);
	free_label(end_label);
    }
    else
    {
	word_32 target = pc + (SEX32(FIELD_BD, 14) << 2);
	label_t alt_label = alloc_label();

	if (target == pc + 4)
	{
	    if (unref_cond_reg)
		unref_integer_reg(cond_reg);
	}
	else
	{
	    assert(target != pc + 4);
	    assert(next_pc == target || next_pc == pc + 4);

	    emit_freeze_save();

	    if (next_pc == pc + 4)
		pos_emitter(cond_reg, alt_label);
	    else
		neg_emitter(cond_reg, alt_label);

	    if (unref_cond_reg)
		unref_integer_reg(cond_reg);

	    emit_begin_alt();

	    emit_label(alt_label);
	    free_label(alt_label);

	    emit_start_direct_jump(1);

	    emit_store_regs(EMU_INSN_EPILOGUE, (next_pc == pc + 4) ? target : pc + 4);

	    if (next_pc == pc + 4)
		emit_direct_jump(target);
	    else
		emit_direct_jump(pc + 4);

	    emit_end_alt();
	}
    }
}

static void
emit_beq (reg_t reg, label_t label)
{
    emit_branch(COMPOSE_BEQ(reg, 0), label);
}

static void
emit_bne (reg_t reg, label_t label)
{
    emit_branch(COMPOSE_BNE(reg, 0), label);
}

static void
gen_branch_with_decrement_insn (word_32 insn, word_32 pc, int zero)
{
    reg_t ctr_reg;

    ctr_reg = ref_ppc_spr_rw(SPR_CTR);

    emit(COMPOSE_SUBL_IMM(ctr_reg, 1, ctr_reg));

    gen_cond_branch(insn, pc, zero ? emit_beq : emit_bne, zero ? emit_bne : emit_beq, ctr_reg, 1);
}

static void
handle_bdnz_insn (word_32 insn, word_32 pc)
{
    gen_branch_with_decrement_insn(insn, pc, 0);
}

static void
emit_blbs (reg_t reg, label_t label)
{
    emit_branch(COMPOSE_BLBS(reg, 0), label);
}

static void
emit_blbc (reg_t reg, label_t label)
{
    emit_branch(COMPOSE_BLBC(reg, 0), label);
}

static void
gen_bcrfb_insn (word_32 insn, word_32 pc, int eq)
{
    if (FIELD_BI < 4)
    {
	reg_t crfb_reg;

	crfb_reg = ref_ppc_crf0_r(FIELD_BI);

	gen_cond_branch(insn, pc, eq ? emit_bne : emit_beq, eq ? emit_beq : emit_bne, crfb_reg, 1);
    }
    else
    {
	reg_t cr_reg, tmp_reg;

	cr_reg = ref_ppc_spr_r(SPR_CR);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_SRL_IMM(cr_reg, 31 - FIELD_BI, tmp_reg));

	unref_integer_reg(cr_reg);

	gen_cond_branch(insn, pc, eq ? emit_blbs : emit_blbc, eq ? emit_blbc : emit_blbs, tmp_reg, 0);

	free_tmp_integer_reg(tmp_reg);
    }
}

static void
handle_bdz_insn (word_32 insn, word_32 pc)
{
    gen_branch_with_decrement_insn(insn, pc, 1);
}

static void
handle_beq_insn (word_32 insn, word_32 pc)
{
    gen_bcrfb_insn(insn, pc, 1);
}

static void
handle_beqp_insn (word_32 insn, word_32 pc)
{
    handle_beq_insn(insn, pc);
}

static void
gen_bcrfblr_insn (word_32 insn, word_32 pc, int eq)
{
    label_t end_label = alloc_label();
    reg_t cond_reg, lr_reg;

    if (FIELD_BI < 4)
	cond_reg = ref_ppc_crf0_r(FIELD_BI);
    else
    {
	reg_t cr_reg;

	cr_reg = ref_ppc_spr_r(SPR_CR);
	cond_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_SRL_IMM(cr_reg, 31 - FIELD_BI, cond_reg));

	unref_integer_reg(cr_reg);
    }

    if (NO_DYNAMO_TRACES || next_pc == NO_FOREIGN_ADDR)
    {
	emit_freeze_save();

	if (eq)
	    emit_branch(COMPOSE_BLBC(cond_reg, 0), end_label);
	else
	    emit_branch(COMPOSE_BLBS(cond_reg, 0), end_label);

	if (FIELD_BI < 4)
	    unref_integer_reg(cond_reg);
	else
	    free_tmp_integer_reg(cond_reg);

	lr_reg = ref_ppc_spr_r(SPR_LR);

	emit(COMPOSE_BIC_IMM(lr_reg, 3, JUMP_TARGET_REG));

	unref_integer_reg(lr_reg);

	assert(!optimize_taken_jump);

	emit_store_regs(EMU_INSN_EPILOGUE, NO_FOREIGN_ADDR);

	emit_indirect_jump();

	emit_label(end_label);
	free_label(end_label);
    }
    else
    {
	label_t alt_label = alloc_label();

	emit_freeze_save();

	lr_reg = ref_ppc_spr_r(SPR_LR);

	if ((eq && next_pc == pc + 4)
	    || (!eq && next_pc != pc + 4))
	    emit_branch(COMPOSE_BLBS(cond_reg, 0), alt_label);
	else
	    emit_branch(COMPOSE_BLBC(cond_reg, 0), alt_label);

	if (FIELD_BI < 4)
	    unref_integer_reg(cond_reg);
	else
	    free_tmp_integer_reg(cond_reg);

	if (next_pc != pc + 4)
	{
	    emit(COMPOSE_BIC_IMM(lr_reg, 3, JUMP_TARGET_REG));

	    gen_indirect_branch();
	}

	emit_begin_alt();

	emit_label(alt_label);
	free_label(alt_label);

	if (next_pc == pc + 4)
	{
	    emit(COMPOSE_BIC_IMM(lr_reg, 3, JUMP_TARGET_REG));

	    emit_store_regs(EMU_INSN_EPILOGUE, NO_FOREIGN_ADDR);

	    emit_indirect_jump();
	}
	else
	{
	    emit_start_direct_jump(1);

	    emit_store_regs(EMU_INSN_EPILOGUE, pc + 4);

	    emit_direct_jump(pc + 4);
	}

	emit_end_alt();

	unref_integer_reg(lr_reg);
    }
}

static void
handle_beqlr_insn (word_32 insn, word_32 pc)
{
    gen_bcrfblr_insn(insn, pc, 1);
}

static void
handle_bl_insn (word_32 insn, word_32 pc)
{
    reg_t lr_reg;

    lr_reg = ref_ppc_spr_w(SPR_LR);

    emit_load_integer_32(lr_reg, pc + 4);

    unref_integer_reg(lr_reg);

    handle_b_insn(insn, pc);
}

static void
handle_blr_insn (word_32 insn, word_32 pc)
{
    reg_t lr_reg;

    lr_reg = ref_ppc_spr_r(SPR_LR);

    emit(COMPOSE_BIC_IMM(lr_reg, 3, JUMP_TARGET_REG));

    unref_integer_reg(lr_reg);

    gen_indirect_branch();
}

static void
handle_blrl_insn (word_32 insn, word_32 pc)
{
    reg_t lr_reg;

    lr_reg = ref_ppc_spr_rw(SPR_LR);

    emit(COMPOSE_BIC_IMM(lr_reg, 3, JUMP_TARGET_REG));
    emit_load_integer_32(lr_reg, pc + 4);

    unref_integer_reg(lr_reg);

    gen_indirect_branch();
}

static void
handle_bne_insn (word_32 insn, word_32 pc)
{
    gen_bcrfb_insn(insn, pc, 0);
}

static void
handle_bne__insn (word_32 insn, word_32 pc)
{
    handle_bne_insn(insn, pc);
}

static void
handle_bnelr_insn (word_32 insn, word_32 pc)
{
    gen_bcrfblr_insn(insn, pc, 0);
}

static void
handle_bnelrp_insn (word_32 insn, word_32 pc)
{
    handle_bnelr_insn(insn, pc);
}

static void
gen_cmp_insn (reg_t ra_reg, reg_t rb_reg, int crfd, int is_unsigned, int with_imm, word_8 imm)
{
    if (crfd == 0)
    {
	if (KILL_CRFB(0))
	{
	    reg_t bit_reg;

	    bit_reg = ref_ppc_crf0_w(0);

	    if (is_unsigned)
	    {
		if (with_imm)
		    emit(COMPOSE_CMPULT_IMM(ra_reg, imm, bit_reg));
		else
		    emit(COMPOSE_CMPULT(ra_reg, rb_reg, bit_reg));
	    }
	    else
	    {
		if (with_imm)
		    emit(COMPOSE_CMPLT_IMM(ra_reg, imm, bit_reg));
		else
		    emit(COMPOSE_CMPLT(ra_reg, rb_reg, bit_reg));
	    }

	    unref_integer_reg(bit_reg);
	}
	if (KILL_CRFB(1))
	{
	    reg_t bit_reg;

	    bit_reg = ref_ppc_crf0_w(1);

	    if (is_unsigned)
	    {
		if (with_imm)
		    emit(COMPOSE_CMPULE_IMM(ra_reg, imm, bit_reg));
		else
		    emit(COMPOSE_CMPULT(rb_reg, ra_reg, bit_reg));
	    }
	    else
	    {
		if (with_imm)
		    emit(COMPOSE_CMPLE_IMM(ra_reg, imm, bit_reg));
		else
		    emit(COMPOSE_CMPLT(rb_reg, ra_reg, bit_reg));
	    }
	    if (with_imm)
		emit(COMPOSE_XOR_IMM(bit_reg, 1, bit_reg));

	    unref_integer_reg(bit_reg);
	}
	if (KILL_CRFB(2))
	{
	    reg_t bit_reg;

	    bit_reg = ref_ppc_crf0_w(2);

	    if (with_imm)
		emit(COMPOSE_CMPEQ_IMM(ra_reg, imm, bit_reg));
	    else
		emit(COMPOSE_CMPEQ(ra_reg, rb_reg, bit_reg));

	    unref_integer_reg(bit_reg);
	}
	if (KILL_CRFB(3))
	{
	    reg_t bit_reg, so_reg;

	    so_reg = ref_ppc_xer_so_r();
	    bit_reg = ref_ppc_crf0_w(3);

	    emit(COMPOSE_MOV(so_reg, bit_reg));

	    unref_integer_reg(bit_reg);
	    unref_integer_reg(so_reg);
	}
    }
    else
    {
	reg_t crf_reg, tmp_reg, so_reg, cr_reg;

	crf_reg = alloc_tmp_integer_reg();
	tmp_reg = alloc_tmp_integer_reg();

	if (is_unsigned)
	{
	    if (with_imm)
		emit(COMPOSE_CMPULT_IMM(ra_reg, imm, tmp_reg));
	    else
		emit(COMPOSE_CMPULT(ra_reg, rb_reg, tmp_reg));
	}
	else
	{
	    if (with_imm)
		emit(COMPOSE_CMPLT_IMM(ra_reg, imm, tmp_reg));
	    else
		emit(COMPOSE_CMPLT(ra_reg, rb_reg, tmp_reg));
	}
	emit(COMPOSE_CMOVNE_IMM(tmp_reg, 8, crf_reg));

	if (is_unsigned)
	{
	    if (with_imm)
		emit(COMPOSE_CMPULE_IMM(ra_reg, imm, tmp_reg));
	    else
		emit(COMPOSE_CMPULT(rb_reg, ra_reg, tmp_reg));
	}
	else
	{
	    if (with_imm)
		emit(COMPOSE_CMPLE_IMM(ra_reg, imm, tmp_reg));
	    else
		emit(COMPOSE_CMPLT(rb_reg, ra_reg, tmp_reg));
	}
	if (with_imm)
	    emit(COMPOSE_CMOVEQ_IMM(tmp_reg, 4, crf_reg));
	else
	    emit(COMPOSE_CMOVNE_IMM(tmp_reg, 4, crf_reg));

	if (with_imm)
	    emit(COMPOSE_CMPEQ_IMM(ra_reg, imm, tmp_reg));
	else
	    emit(COMPOSE_CMPEQ(ra_reg, rb_reg, tmp_reg));
	emit(COMPOSE_CMOVNE_IMM(tmp_reg, 2, crf_reg));

	so_reg = ref_ppc_xer_so_r();

	emit(COMPOSE_BIS(crf_reg, so_reg, crf_reg));

	unref_integer_reg(so_reg);

	emit_load_integer_32(tmp_reg, 15 << ((7 - crfd) * 4));

	cr_reg = ref_ppc_spr_rw(SPR_CR);

	emit(COMPOSE_BIC(cr_reg, tmp_reg, cr_reg));

	free_tmp_integer_reg(tmp_reg);

	emit(COMPOSE_SLL_IMM(crf_reg, (7 - crfd) * 4, crf_reg));
	emit(COMPOSE_BIS(cr_reg, crf_reg, cr_reg));

	unref_integer_reg(cr_reg);
	free_tmp_integer_reg(crf_reg);
    }
}

static void
gen_cmp_regs_insn (word_32 insn, int is_unsigned)
{
    reg_t ra_reg, rb_reg;

    ra_reg = ref_ppc_gpr_r(FIELD_RA);
    rb_reg = ref_ppc_gpr_r(FIELD_RB);

    gen_cmp_insn(ra_reg, rb_reg, FIELD_CRFD, is_unsigned, 0, 0);

    unref_integer_reg(rb_reg);
    unref_integer_reg(ra_reg);
}

static void
handle_cmplw_insn (word_32 insn, word_32 pc)
{
    gen_cmp_regs_insn(insn, 1);
}

static void
handle_cmplwi_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg;

    ra_reg = ref_ppc_gpr_r(FIELD_RA);

    if ((FIELD_UIMM & 0xff00) == 0)
	gen_cmp_insn(ra_reg, 0, FIELD_CRFD, 1, 1, FIELD_UIMM);
    else
    {
	reg_t imm_reg;

	imm_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(imm_reg, FIELD_UIMM);

	gen_cmp_insn(ra_reg, imm_reg, FIELD_CRFD, 1, 0, 0);

	free_tmp_integer_reg(imm_reg);
    }

    unref_integer_reg(ra_reg);
}

static void
handle_cmpw_insn (word_32 insn, word_32 pc)
{
    gen_cmp_regs_insn(insn, 0);
}

static void
handle_cmpwi_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg;

    ra_reg = ref_ppc_gpr_r(FIELD_RA);

    if ((FIELD_SIMM & 0xff00) == 0)
	gen_cmp_insn(ra_reg, 0, FIELD_CRFD, 0, 1, FIELD_SIMM);
    else
    {
	reg_t imm_reg;

	imm_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_LDA(imm_reg, FIELD_SIMM, 31));

	gen_cmp_insn(ra_reg, imm_reg, FIELD_CRFD, 0, 0, 0);

	free_tmp_integer_reg(imm_reg);
    }

    unref_integer_reg(ra_reg);
}

static void
handle_cntlzw_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(FIELD_RS);

	emit(COMPOSE_MOV(rs_reg, 16));

	unref_integer_reg(rs_reg);

	emit(COMPOSE_LDQ(0, LEADING_ZEROS_CONST * 4, CONSTANT_AREA_REG));
	emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, C_STUB_CONST * 4, CONSTANT_AREA_REG));
	emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));

	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_MOV(0, ra_reg));

	unref_integer_reg(ra_reg);
    }
}

static void
gen_crop_insn (word_32 insn, void (*emitter) (reg_t, reg_t, reg_t))
{
    reg_t bit_a_reg, bit_b_reg;
    int need_mask = 0;

    if (FIELD_CRBA < 4)
	bit_a_reg = ref_ppc_crf0_r(FIELD_CRBA);
    else
    {
	reg_t cr_reg;

	cr_reg = ref_ppc_spr_r(SPR_CR);
	bit_a_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_SRL_IMM(cr_reg, 31 - FIELD_CRBA, bit_a_reg));

	unref_integer_reg(cr_reg);

	need_mask = 1;
    }

    if (FIELD_CRBB < 4)
	bit_b_reg = ref_ppc_crf0_r(FIELD_CRBB);
    else
    {
	reg_t cr_reg;

	cr_reg = ref_ppc_spr_r(SPR_CR);
	bit_b_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_SRL_IMM(cr_reg, 31 - FIELD_CRBB, bit_b_reg));

	unref_integer_reg(cr_reg);

	need_mask = 1;
    }

    if (FIELD_CRBD < 4)
    {
	reg_t crfb_reg;

	crfb_reg = ref_ppc_crf0_w(FIELD_CRBD);

	emitter(bit_a_reg, bit_b_reg, crfb_reg);
	if (need_mask)
	    emit(COMPOSE_AND_IMM(crfb_reg, 1, crfb_reg));

	unref_integer_reg(crfb_reg);
    }
    else
    {
	reg_t cr_reg, mask_reg;

	emitter(bit_a_reg, bit_b_reg, bit_a_reg);
	if (need_mask)
	    emit(COMPOSE_AND_IMM(bit_a_reg, 1, bit_a_reg));

	cr_reg = ref_ppc_spr_rw(SPR_CR);
	mask_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(mask_reg, 1 << (31 - FIELD_CRBD));
	emit(COMPOSE_SLL_IMM(bit_a_reg, 31 - FIELD_CRBD, bit_a_reg));
	emit(COMPOSE_BIC(cr_reg, mask_reg, cr_reg));

	free_tmp_integer_reg(mask_reg);

	emit(COMPOSE_BIS(cr_reg, bit_a_reg, cr_reg));

	unref_integer_reg(cr_reg);
    }

    if (FIELD_CRBB < 4)
	unref_integer_reg(bit_b_reg);
    else
	free_tmp_integer_reg(bit_b_reg);

    if (FIELD_CRBA < 4)
	unref_integer_reg(bit_a_reg);
    else
	free_tmp_integer_reg(bit_a_reg);
}

static void
emit_eqv (reg_t a, reg_t b, reg_t d)
{
    emit(COMPOSE_EQV(a, b, d));
}

static void
handle_creqv_insn (word_32 insn, word_32 pc)
{
    gen_crop_insn(insn, emit_eqv);
}

static void
emit_nor (reg_t a, reg_t b, reg_t d)
{
    emit(COMPOSE_BIS(a, b, d));
    emit(COMPOSE_NOT(d, d));
}

static void
handle_crnor_insn (word_32 insn, word_32 pc)
{
    gen_crop_insn(insn, emit_nor);
}

static void
emit_bis (reg_t a, reg_t b, reg_t d)
{
    emit(COMPOSE_BIS(a, b, d));
}

static void
handle_cror_insn (word_32 insn, word_32 pc)
{
    gen_crop_insn(insn, emit_bis);
}

static void
emit_xor (reg_t a, reg_t b, reg_t d)
{
    emit(COMPOSE_XOR(a, b, d));
}

static void
handle_crxor_insn (word_32 insn, word_32 pc)
{
    gen_crop_insn(insn, emit_xor);
}

static void
handle_dcbst_insn (word_32 insn, word_32 pc)
{
}

static void
handle_dcbz_insn (word_32 insn, word_32 pc)
{
    reg_t rb_reg, addr_reg;

    rb_reg = ref_ppc_gpr_r(FIELD_RB);

    if (FIELD_RA == 0)
    {
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_BIC_IMM(rb_reg, 31, addr_reg));

	unref_integer_reg(rb_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);

	emit(COMPOSE_BIC_IMM(addr_reg, 31, addr_reg));
    }

    emit(COMPOSE_STQ(31, 0, addr_reg));
    emit(COMPOSE_STQ(31, 8, addr_reg));
    emit(COMPOSE_STQ(31, 16, addr_reg));
    emit(COMPOSE_STQ(31, 24, addr_reg));

    free_tmp_integer_reg(addr_reg);
}

static void
gen_div_insn (word_32 insn, word_32 const_index)
{
    reg_t ra_reg, rb_reg, rd_reg;

    ra_reg = ref_ppc_gpr_r(FIELD_RA);
    rb_reg = ref_ppc_gpr_r(FIELD_RB);

    emit(COMPOSE_MOV(ra_reg, 16));
    emit(COMPOSE_MOV(rb_reg, 17));

    unref_integer_reg(rb_reg);
    unref_integer_reg(ra_reg);

    emit(COMPOSE_LDQ(0, const_index * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, C_STUB_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));

    rd_reg = ref_ppc_gpr_w(FIELD_RD);

    emit(COMPOSE_MOV(0, rd_reg));

    unref_integer_reg(rd_reg);
}

static void
handle_divw_insn (word_32 insn, word_32 pc)
{
    gen_div_insn(insn, DIV_SIGNED_32_CONST);
}

static void
handle_divwu_insn (word_32 insn, word_32 pc)
{
    gen_div_insn(insn, DIV_UNSIGNED_32_CONST);
}

static void
handle_eqv_insn (word_32 insn, word_32 pc)
{
    gen_simple_xo1_rc_insn(insn, emit_eqv);
}

static void
handle_eqvd_insn (word_32 insn, word_32 pc)
{
    handle_eqv_insn(insn, pc);
}

static void
handle_extsb_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_SEXTB(rs_reg, ra_reg));

	unref_integer_reg(ra_reg);
	unref_integer_reg(rs_reg);
    }
}

static void
handle_extsh_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_SEXTW(rs_reg, ra_reg));

	unref_integer_reg(ra_reg);
	unref_integer_reg(rs_reg);
    }
}

static void
handle_extshd_insn (word_32 insn, word_32 pc)
{
    handle_extsh_insn(insn, pc);
}

static void
handle_fabs_insn (word_32 insn, word_32 pc)
{
    announce("fabs");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t frb_reg, frd_reg;

	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_CPYS(31, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
    }
}

static void
handle_fadd_insn (word_32 insn, word_32 pc)
{
    announce("fadd");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frd_reg;

	fra_reg = ref_ppc_fpr_r(FIELD_FRA);
	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_ADDT(fra_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
	unref_float_reg(fra_reg);
    }
}

static void
handle_fadds_insn (word_32 insn, word_32 pc)
{
    handle_fadd_insn(insn, pc);
}

static void
handle_fcmpu_insn (word_32 insn, word_32 pc)
{
    reg_t fra_reg, frb_reg;

    announce("fcmpu");

    fra_reg = ref_ppc_fpr_r(FIELD_FRA);
    frb_reg = ref_ppc_fpr_r(FIELD_FRB);

    if (FIELD_CRFD == 0)
    {
	reg_t bit0_reg, bit1_reg, bit2_reg, bit3_reg, tmp_reg;
	label_t l1 = alloc_label(), l2 = alloc_label(), end = alloc_label();

	bit0_reg = ref_ppc_crf0_w(0);
	bit1_reg = ref_ppc_crf0_w(1);
	bit2_reg = ref_ppc_crf0_w(2);

	tmp_reg = alloc_tmp_float_reg();

	emit(COMPOSE_CMPTLT(fra_reg, frb_reg, tmp_reg));
	emit_branch(COMPOSE_FBEQ(tmp_reg, 0), l1);
	emit(COMPOSE_BIS_IMM(31, 1, bit0_reg));
	emit(COMPOSE_BIS(31, 31, bit1_reg));
	emit(COMPOSE_BIS(31, 31, bit2_reg));
	emit_branch(COMPOSE_BR(31, 0), end);

	emit_label(l1);
	free_label(l1);

	emit(COMPOSE_CMPTLT(frb_reg, fra_reg, tmp_reg));
	emit_branch(COMPOSE_FBEQ(tmp_reg, 0), l2);
	emit(COMPOSE_BIS(31, 31, bit0_reg));
	emit(COMPOSE_BIS_IMM(31, 1, bit1_reg));
	emit(COMPOSE_BIS(31, 31, bit2_reg));
	emit_branch(COMPOSE_BR(31, 0), end);

	emit_label(l2);
	free_label(l2);

	emit(COMPOSE_BIS(31, 31, bit0_reg));
	emit(COMPOSE_BIS(31, 31, bit1_reg));
	emit(COMPOSE_BIS_IMM(31, 1, bit2_reg));

	emit_label(end);
	free_label(end);

	free_tmp_float_reg(tmp_reg);
	unref_integer_reg(bit2_reg);
	unref_integer_reg(bit1_reg);
	unref_integer_reg(bit0_reg);
	bit3_reg = ref_ppc_crf0_w(3);

	emit(COMPOSE_MOV(31, bit3_reg));

	unref_integer_reg(bit3_reg);
    }
    else
    {
	reg_t crf_reg, tmp_reg, cr_reg;
	label_t l1 = alloc_label(), l2 = alloc_label(), end = alloc_label();

	crf_reg = alloc_tmp_integer_reg();
	tmp_reg = alloc_tmp_float_reg();

	emit(COMPOSE_CMPTLT(fra_reg, frb_reg, tmp_reg));
	emit_branch(COMPOSE_FBEQ(tmp_reg, 0), l1);
	emit(COMPOSE_BIS_IMM(31, 8, crf_reg));
	emit_branch(COMPOSE_BR(31, 0), end);

	emit_label(l1);
	free_label(l1);

	emit(COMPOSE_CMPTLT(frb_reg, fra_reg, tmp_reg));
	emit_branch(COMPOSE_FBEQ(tmp_reg, 0), l2);
	emit(COMPOSE_BIS_IMM(31, 4, crf_reg));
	emit_branch(COMPOSE_BR(31, 0), end);

	emit_label(l2);
	free_label(l2);

	emit(COMPOSE_BIS_IMM(31, 2, crf_reg));

	emit_label(end);
	free_label(end);

	free_tmp_float_reg(tmp_reg);
	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, 15 << ((7 - FIELD_CRFD) * 4));

	cr_reg = ref_ppc_spr_rw(SPR_CR);

	emit(COMPOSE_BIC(cr_reg, tmp_reg, cr_reg));

	free_tmp_integer_reg(tmp_reg);

	emit(COMPOSE_SLL_IMM(crf_reg, (7 - FIELD_CRFD) * 4, crf_reg));
	emit(COMPOSE_BIS(cr_reg, crf_reg, cr_reg));

	unref_integer_reg(cr_reg);
	free_tmp_integer_reg(crf_reg);
    }

    unref_float_reg(frb_reg);
    unref_float_reg(fra_reg);
}

static void
gen_bits_to_float (reg_t bits_reg, reg_t float_reg, int single)
{
    if (single)
    {
	emit(COMPOSE_STL(bits_reg, SCRATCH_OFFSET, CONSTANT_AREA_REG));
	emit(COMPOSE_LDS(float_reg, SCRATCH_OFFSET, CONSTANT_AREA_REG));
    }
    else
    {
	emit(COMPOSE_STQ(bits_reg, SCRATCH_OFFSET, CONSTANT_AREA_REG));
	emit(COMPOSE_LDT(float_reg, SCRATCH_OFFSET, CONSTANT_AREA_REG));
    }
}

static void
gen_float_to_bits (reg_t float_reg, reg_t bits_reg, int single)
{
    if (single)
    {
	emit(COMPOSE_STS(float_reg, SCRATCH_OFFSET, CONSTANT_AREA_REG));
	emit(COMPOSE_LDL(bits_reg, SCRATCH_OFFSET, CONSTANT_AREA_REG));
    }
    else
    {
	emit(COMPOSE_STT(float_reg, SCRATCH_OFFSET, CONSTANT_AREA_REG));
	emit(COMPOSE_LDQ(bits_reg, SCRATCH_OFFSET, CONSTANT_AREA_REG));
    }
}

static void
handle_fctiwz_insn (word_32 insn, word_32 pc)
{
    announce("fctiwz");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t frb_reg, frd_reg, tmp_reg;

	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_CVTTQC(frb_reg, frd_reg));

	unref_float_reg(frb_reg);
	tmp_reg = alloc_tmp_integer_reg();

	gen_float_to_bits(frd_reg, tmp_reg, 0);

	emit(COMPOSE_ZAPNOT_IMM(tmp_reg, 15, tmp_reg));

	gen_bits_to_float(tmp_reg, frd_reg, 0);

	free_tmp_integer_reg(tmp_reg);
	unref_float_reg(frd_reg);
    }
}

static void
handle_fdiv_insn (word_32 insn, word_32 pc)
{
    announce("fdiv");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frd_reg;

	fra_reg = ref_ppc_fpr_r(FIELD_FRA);
	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_DIVT(fra_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
	unref_float_reg(fra_reg);
    }
}

static void
handle_fdivs_insn (word_32 insn, word_32 pc)
{
    handle_fdiv_insn(insn, pc);
}

static void
handle_fmadd_insn (word_32 insn, word_32 pc)
{
    announce("fmadd");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frc_reg, frd_reg, tmp_reg;

	fra_reg = ref_ppc_fpr_r(FIELD_FRA);
	frc_reg = ref_ppc_fpr_r(FIELD_FRC);
	tmp_reg = alloc_tmp_float_reg();

	emit(COMPOSE_MULT(fra_reg, frc_reg, tmp_reg));

	unref_float_reg(frc_reg);
	unref_float_reg(fra_reg);

	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_ADDT(tmp_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
	free_tmp_float_reg(tmp_reg);
    }
}

static void
handle_fmadds_insn (word_32 insn, word_32 pc)
{
    handle_fmadd_insn(insn, pc);
}

static void
handle_fmr_insn (word_32 insn, word_32 pc)
{
    announce("fmr");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t frb_reg, frd_reg;

	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_CPYS(frb_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
    }
}

static void
handle_fmsub_insn (word_32 insn, word_32 pc)
{
    announce("fmsub");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frc_reg, frd_reg, tmp_reg;

	fra_reg = ref_ppc_fpr_r(FIELD_FRA);
	frc_reg = ref_ppc_fpr_r(FIELD_FRC);
	tmp_reg = alloc_tmp_float_reg();

	emit(COMPOSE_MULT(fra_reg, frc_reg, tmp_reg));

	unref_float_reg(frc_reg);
	unref_float_reg(fra_reg);

	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_SUBT(tmp_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
	free_tmp_float_reg(tmp_reg);
    }
}

static void
handle_fmsubs_insn (word_32 insn, word_32 pc)
{
    handle_fmsub_insn(insn, pc);
}

static void
handle_fmul_insn (word_32 insn, word_32 pc)
{
    announce("fmul");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t fra_reg, frc_reg, frd_reg;

	fra_reg = ref_ppc_fpr_r(FIELD_FRA);
	frc_reg = ref_ppc_fpr_r(FIELD_FRC);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_MULT(fra_reg, frc_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frc_reg);
	unref_float_reg(fra_reg);
    }
}

static void
handle_fmuls_insn (word_32 insn, word_32 pc)
{
    handle_fmul_insn(insn, pc);
}

static void
handle_fneg_insn (word_32 insn, word_32 pc)
{
    announce("fneg");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t frb_reg, frd_reg;

	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_CPYSN(frb_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
    }
}

static void
handle_fnmsub_insn (word_32 insn, word_32 pc)
{
    if (KILL_FPR(FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frc_reg, frd_reg, tmp_reg;

	fra_reg = ref_ppc_fpr_r(FIELD_FRA);
	frc_reg = ref_ppc_fpr_r(FIELD_FRC);
	tmp_reg = alloc_tmp_float_reg();

	emit(COMPOSE_MULT(fra_reg, frc_reg, tmp_reg));

	unref_float_reg(frc_reg);
	unref_float_reg(fra_reg);

	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_SUBT(tmp_reg, frb_reg, frd_reg));

	unref_float_reg(frb_reg);
	free_tmp_float_reg(tmp_reg);

	emit(COMPOSE_CPYSN(frd_reg, frd_reg, frd_reg));

	unref_float_reg(frd_reg);
    }
}

static void
handle_frsp_insn (word_32 insn, word_32 pc)
{
    announce("frsp");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t frb_reg, frd_reg;

	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_CPYS(frb_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
    }
}

static void
handle_fsub_insn (word_32 insn, word_32 pc)
{
    announce("fsub");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frd_reg;

	fra_reg = ref_ppc_fpr_r(FIELD_FRA);
	frb_reg = ref_ppc_fpr_r(FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	emit(COMPOSE_SUBT(fra_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
	unref_float_reg(fra_reg);
    }
}

static void
handle_fsubs_insn (word_32 insn, word_32 pc)
{
    handle_fsub_insn(insn, pc);
}

static void
handle_icbi_insn (word_32 insn, word_32 pc)
{
}

static void
handle_isync_insn (word_32 insn, word_32 pc)
{
    emit_store_regs(EMU_INSN_CONTINUE, NO_FOREIGN_ADDR);

    emit_load_integer_32(16, pc + 4);
    emit_isync();
}

static void
handle_lbz_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rd_reg;

	if (FIELD_RA == 0)
	{
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDBU(rd_reg, FIELD_D ^ 3, 31));
	}
	else
	{
	    reg_t ra_reg, addr_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_LDA(addr_reg, FIELD_D, ra_reg));

	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));

	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDBU(rd_reg, 0, addr_reg));

	    free_tmp_integer_reg(addr_reg);
	}

	unref_integer_reg(rd_reg);
    }
}

static void
handle_lbzu_insn (word_32 insn, word_32 pc)
{
    assert(FIELD_RD != FIELD_RA);

    if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rd_reg, addr_reg;

	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, FIELD_D, ra_reg));

	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_XOR_IMM(ra_reg, 3, addr_reg));

	unref_integer_reg(ra_reg);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_LDBU(rd_reg, 0, addr_reg));

	unref_integer_reg(rd_reg);
	free_tmp_integer_reg(addr_reg);
    }
    else if (KILL_GPR(FIELD_RA))
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, FIELD_D ^ 3, ra_reg));

	unref_integer_reg(ra_reg);
    }
}

static void
handle_lbzux_insn (word_32 insn, word_32 pc)
{
    assert(FIELD_RD != FIELD_RA && FIELD_RD != FIELD_RB);

    if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg, addr_reg;

	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, ra_reg));

	unref_integer_reg(rb_reg);

	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_XOR_IMM(ra_reg, 3, addr_reg));

	unref_integer_reg(ra_reg);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_LDBU(rd_reg, 0, addr_reg));

	unref_integer_reg(rd_reg);
	free_tmp_integer_reg(addr_reg);
    }
    else if (KILL_GPR(FIELD_RA))
    {
	reg_t ra_reg, rb_reg;

	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, ra_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_lbzx_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rb_reg, rd_reg, addr_reg;

	rb_reg = ref_ppc_gpr_r(FIELD_RB);

	if (FIELD_RA == 0)
	{
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_XOR_IMM(rb_reg, 3, addr_reg));

	    unref_integer_reg(rb_reg);
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDBU(rd_reg, 0, addr_reg));

	    unref_integer_reg(rd_reg);
	    free_tmp_integer_reg(addr_reg);
	}
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_rw(FIELD_RA);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));

	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDBU(rd_reg, 0, addr_reg));

	    unref_integer_reg(rd_reg);
	    free_tmp_integer_reg(addr_reg);
	}
    }
}

static void
handle_lfd_insn (word_32 insn, word_32 pc)
{
    announce("lfd");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t addr_reg, bits_reg, frd_reg;

	addr_reg = alloc_tmp_integer_reg();

	if (FIELD_RA == 0)
	    emit(COMPOSE_LDA(addr_reg, FIELD_D, 31));
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);

	    emit(COMPOSE_LDA(addr_reg, FIELD_D, ra_reg));

	    unref_integer_reg(ra_reg);
	}

	bits_reg = alloc_tmp_integer_reg();

	emit_load_mem_64(bits_reg, addr_reg);

	free_tmp_integer_reg(addr_reg);

	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	gen_bits_to_float(bits_reg, frd_reg, 0);

	unref_float_reg(frd_reg);
	free_tmp_integer_reg(bits_reg);
    }
}

static void
handle_lfdx_insn (word_32 insn, word_32 pc)
{
    announce("lfdx");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t addr_reg, bits_reg, frd_reg;

	if (FIELD_RA == 0)
	    addr_reg = ref_ppc_gpr_r(FIELD_RB);
	else
	{
	    reg_t ra_reg, rb_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);
	    rb_reg = ref_ppc_gpr_r(FIELD_RB);

	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_ADDQ(ra_reg, rb_reg, addr_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);
	}

	bits_reg = alloc_tmp_integer_reg();

	emit_load_mem_64(bits_reg, addr_reg);

	if (FIELD_RA == 0)
	    unref_integer_reg(addr_reg);
	else
	    free_tmp_integer_reg(addr_reg);

	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	gen_bits_to_float(bits_reg, frd_reg, 0);

	unref_float_reg(frd_reg);
	free_tmp_integer_reg(bits_reg);
    }
}

static void
handle_lfs_insn (word_32 insn, word_32 pc)
{
    announce("lfs");

    if (KILL_FPR(FIELD_FRD))
    {
	reg_t frd_reg;

	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	if (FIELD_RA == 0)
	    emit(COMPOSE_LDS(frd_reg, FIELD_D, 31));
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);

	    emit(COMPOSE_LDS(frd_reg, FIELD_D, ra_reg));

	    unref_integer_reg(ra_reg);
	}

	unref_float_reg(frd_reg);
    }
}

static void
handle_lfsx_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	if (FIELD_RA == 0)
	{
	    reg_t rb_reg, frd_reg;

	    rb_reg = ref_ppc_gpr_r(FIELD_RB);
	    frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	    emit(COMPOSE_LDS(frd_reg, 0, rb_reg));

	    unref_integer_reg(frd_reg);
	    unref_float_reg(rb_reg);
	}
	else
	{
	    reg_t ra_reg, rb_reg, addr_reg, frd_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);
	    rb_reg = ref_ppc_gpr_r(FIELD_RB);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);

	    frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	    emit(COMPOSE_LDS(frd_reg, 0, addr_reg));

	    unref_float_reg(frd_reg);
	    free_tmp_integer_reg(addr_reg);
	}
    }
}

static void
gen_load_half_imm_insn (word_32 insn, int sex)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rd_reg;

	if (FIELD_RA == 0)
	{
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDWU(rd_reg, FIELD_D ^ 2, 31));
	}
	else
	{
	    reg_t ra_reg, addr_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_LDA(addr_reg, FIELD_D, ra_reg));

	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));

	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDWU(rd_reg, 0, addr_reg));

	    free_tmp_integer_reg(addr_reg);
	}

	if (sex)
	    emit(COMPOSE_SEXTW(rd_reg, rd_reg));

	unref_integer_reg(rd_reg);
    }
}

static void
handle_lha_insn (word_32 insn, word_32 pc)
{
    gen_load_half_imm_insn(insn, 1);
}

static void
gen_load_half_update_insn (word_32 insn, int sex)
{
    assert(FIELD_RD != FIELD_RA);

    if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rd_reg, addr_reg;

	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, FIELD_D, ra_reg));

	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_XOR_IMM(ra_reg, 2, addr_reg));

	unref_integer_reg(ra_reg);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_LDWU(rd_reg, 0, addr_reg));

	free_tmp_integer_reg(addr_reg);

	if (sex)
	    emit(COMPOSE_SEXTW(rd_reg, rd_reg));

	unref_integer_reg(rd_reg);
    }
    else if (KILL_GPR(FIELD_RA))
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, FIELD_D ^ 2, ra_reg));

	unref_integer_reg(ra_reg);
    }
}

static void
handle_lhau_insn (word_32 insn, word_32 pc)
{
    gen_load_half_update_insn(insn, 1);
}

static void
gen_load_half_indexed_insn (word_32 insn, int sex)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rb_reg, rd_reg, addr_reg;

	rb_reg = ref_ppc_gpr_r(FIELD_RB);

	if (FIELD_RA == 0)
	{
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_XOR_IMM(rb_reg, 2, addr_reg));

	    unref_integer_reg(rb_reg);
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDWU(rd_reg, 0, addr_reg));

	    free_tmp_integer_reg(addr_reg);

	    if (sex)
		emit(COMPOSE_SEXTW(rd_reg, rd_reg));

	    unref_integer_reg(rd_reg);
	}
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_rw(FIELD_RA);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));

	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDWU(rd_reg, 0, addr_reg));

	    free_tmp_integer_reg(addr_reg);

	    if (sex)
		emit(COMPOSE_SEXTW(rd_reg, rd_reg));

	    unref_integer_reg(rd_reg);
	}
    }
}

static void
handle_lhax_insn (word_32 insn, word_32 pc)
{
    gen_load_half_indexed_insn(insn, 1);
}

static void
handle_lhz_insn (word_32 insn, word_32 pc)
{
    gen_load_half_imm_insn(insn, 0);
}

static void
handle_lhzu_insn (word_32 insn, word_32 pc)
{
    gen_load_half_update_insn(insn, 0);
}

static void
handle_lhzx_insn (word_32 insn, word_32 pc)
{
    gen_load_half_indexed_insn(insn, 0);
}

static void
handle_lwz_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rd_reg;

	if (FIELD_RA == 0)
	{
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDL(rd_reg, FIELD_D, 31));

	    unref_integer_reg(rd_reg);
	}
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDL(rd_reg, FIELD_D, ra_reg));

	    unref_integer_reg(rd_reg);
	    unref_integer_reg(ra_reg);
	}
    }
}

static void
handle_lwzu_insn (word_32 insn, word_32 pc)
{
    assert(FIELD_RD != FIELD_RA);

    if (KILL_GPR(FIELD_RD) || KILL_GPR(FIELD_RA))
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, FIELD_D, ra_reg));

	if (KILL_GPR(FIELD_RD))
	{
	    reg_t rd_reg;

	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDL(rd_reg, 0, ra_reg));

	    unref_integer_reg(rd_reg);
	}

	unref_integer_reg(ra_reg);
    }
}

static void
handle_lwzux_insn (word_32 insn, word_32 pc)
{
    assert(FIELD_RD != FIELD_RA && FIELD_RD != FIELD_RB);

    if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg;

	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, ra_reg));

	unref_integer_reg(rb_reg);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_LDL(rd_reg, 0, ra_reg));

	unref_integer_reg(ra_reg);
	unref_integer_reg(rd_reg);
    }
    else if (KILL_GPR(FIELD_RA))
    {
	reg_t ra_reg, rb_reg;

	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, ra_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_lwzx_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	if (FIELD_RA == 0)
	{
	    reg_t rb_reg, rd_reg;

	    rb_reg = ref_ppc_gpr_r(FIELD_RB);
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDL(rd_reg, 0, rb_reg));

	    unref_integer_reg(rd_reg);
	    unref_integer_reg(rb_reg);
	}
	else
	{
	    reg_t ra_reg, rb_reg, addr_reg, rd_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);
	    rb_reg = ref_ppc_gpr_r(FIELD_RB);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);

	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_LDL(rd_reg, 0, addr_reg));

	    unref_integer_reg(rd_reg);
	    free_tmp_integer_reg(addr_reg);
	}
    }
}

static void
handle_mcrf_insn (word_32 insn, word_32 pc)
{
    if (FIELD_CRFD != FIELD_CRFS)
    {
	if (FIELD_CRFD == 0)
	{
	    reg_t cr_reg;

	    cr_reg = ref_ppc_spr_r(SPR_CR);

	    if (KILL_CRFB(0))
	    {
		reg_t bit_reg;

		bit_reg = ref_ppc_crf0_w(0);

		emit(COMPOSE_SRL_IMM(cr_reg, 31 - (FIELD_CRFS * 4 + 0), bit_reg));
		emit(COMPOSE_AND_IMM(bit_reg, 1, bit_reg));

		unref_integer_reg(bit_reg);
	    }
	    if (KILL_CRFB(1))
	    {
		reg_t bit_reg;

		bit_reg = ref_ppc_crf0_w(1);

		emit(COMPOSE_SRL_IMM(cr_reg, 31 - (FIELD_CRFS * 4 + 1), bit_reg));
		emit(COMPOSE_AND_IMM(bit_reg, 1, bit_reg));

		unref_integer_reg(bit_reg);
	    }
	    if (KILL_CRFB(2))
	    {
		reg_t bit_reg;

		bit_reg = ref_ppc_crf0_w(2);

		emit(COMPOSE_SRL_IMM(cr_reg, 31 - (FIELD_CRFS * 4 + 2), bit_reg));
		emit(COMPOSE_AND_IMM(bit_reg, 1, bit_reg));

		unref_integer_reg(bit_reg);
	    }
	    if (KILL_CRFB(3))
	    {
		reg_t bit_reg;

		bit_reg = ref_ppc_crf0_w(3);

		emit(COMPOSE_SRL_IMM(cr_reg, 31 - (FIELD_CRFS * 4 + 3), bit_reg));
		emit(COMPOSE_AND_IMM(bit_reg, 1, bit_reg));

		unref_integer_reg(bit_reg);
	    }

	    unref_integer_reg(cr_reg);
	}
	else
	{
	    if (FIELD_CRFS == 0)
	    {
		reg_t field_reg, mask_reg, cr_reg;
		int have_set = 0;

		field_reg = alloc_tmp_integer_reg();

		if (KILL_CRFB(4 * FIELD_CRFD + 0))
		{
		    reg_t bit_reg;

		    bit_reg = ref_ppc_crf0_r(0);

		    emit(COMPOSE_SLL_IMM(bit_reg, 3, field_reg));

		    unref_integer_reg(bit_reg);

		    have_set = 1;
		}
		if (KILL_CRFB(4 * FIELD_CRFD + 1))
		{
		    reg_t bit_reg;

		    bit_reg = ref_ppc_crf0_r(1);

		    if (have_set)
		    {
			reg_t tmp_reg;

			tmp_reg = alloc_tmp_integer_reg();

			emit(COMPOSE_SLL_IMM(bit_reg, 2, tmp_reg));
			emit(COMPOSE_BIS(field_reg, tmp_reg, field_reg));

			free_tmp_integer_reg(tmp_reg);
		    }
		    else
		    {
			emit(COMPOSE_SLL_IMM(bit_reg, 2, field_reg));

			have_set = 1;
		    }

		    unref_integer_reg(bit_reg);

		    have_set = 1;
		}
		if (KILL_CRFB(4 * FIELD_CRFD + 2))
		{
		    reg_t bit_reg;

		    bit_reg = ref_ppc_crf0_r(2);

		    if (have_set)
		    {
			reg_t tmp_reg;

			tmp_reg = alloc_tmp_integer_reg();

			emit(COMPOSE_SLL_IMM(bit_reg, 1, tmp_reg));
			emit(COMPOSE_BIS(field_reg, tmp_reg, field_reg));

			free_tmp_integer_reg(tmp_reg);
		    }
		    else
		    {
			emit(COMPOSE_SLL_IMM(bit_reg, 1, field_reg));

			have_set = 1;
		    }

		    unref_integer_reg(bit_reg);

		    have_set = 1;
		}
		if (KILL_CRFB(4 * FIELD_CRFD + 3))
		{
		    reg_t bit_reg;

		    bit_reg = ref_ppc_crf0_r(3);

		    if (have_set)
			emit(COMPOSE_BIS(field_reg, bit_reg, field_reg));
		    else
			emit(COMPOSE_MOV(bit_reg, field_reg));

		    unref_integer_reg(bit_reg);
		}

		mask_reg = alloc_tmp_integer_reg();

		emit_load_integer_32(mask_reg, 15 << ((7 - FIELD_CRFD) * 4));

		cr_reg = ref_ppc_spr_rw(SPR_CR);

		emit(COMPOSE_BIC(cr_reg, mask_reg, cr_reg));

		free_tmp_integer_reg(mask_reg);

		emit(COMPOSE_SLL_IMM(field_reg, ((7 - FIELD_CRFD) * 4), field_reg));
		emit(COMPOSE_BIS(cr_reg, field_reg, cr_reg));

		unref_integer_reg(cr_reg);
		free_tmp_integer_reg(field_reg);
	    }
	    else
	    {
		reg_t cr_reg, mask_reg, field_reg;

		cr_reg = ref_ppc_spr_rw(SPR_CR);
		field_reg = alloc_tmp_integer_reg();

		emit(COMPOSE_SRL_IMM(cr_reg, ((7 - FIELD_CRFS) * 4), field_reg));
		emit(COMPOSE_AND_IMM(field_reg, 15, field_reg));
		emit(COMPOSE_SLL_IMM(field_reg, ((7 - FIELD_CRFD) * 4), field_reg));

		mask_reg = alloc_tmp_integer_reg();

		emit_load_integer_32(mask_reg, 15 << ((7 - FIELD_CRFD) * 4));

		emit(COMPOSE_BIC(cr_reg, mask_reg, cr_reg));

		free_tmp_integer_reg(mask_reg);

		emit(COMPOSE_BIS(cr_reg, field_reg, cr_reg));

		free_tmp_integer_reg(field_reg);
		unref_integer_reg(cr_reg);
	    }
	}
    }
}

static void
handle_mfcr_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rd_reg, cr_reg, tmp_reg;

	cr_reg = ref_ppc_spr_r(SPR_CR);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_MOV(cr_reg, rd_reg));

	unref_integer_reg(cr_reg);

	cr_reg = ref_ppc_crf0_r(0);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_SLL_IMM(cr_reg, 31, tmp_reg));
	emit(COMPOSE_BIS(rd_reg, tmp_reg, rd_reg));

	unref_integer_reg(cr_reg);
	cr_reg = ref_ppc_crf0_r(1);

	emit(COMPOSE_SLL_IMM(cr_reg, 30, tmp_reg));
	emit(COMPOSE_BIS(rd_reg, tmp_reg, rd_reg));

	unref_integer_reg(cr_reg);
	cr_reg = ref_ppc_crf0_r(2);

	emit(COMPOSE_SLL_IMM(cr_reg, 29, tmp_reg));
	emit(COMPOSE_BIS(rd_reg, tmp_reg, rd_reg));

	unref_integer_reg(cr_reg);
	cr_reg = ref_ppc_crf0_r(3);

	emit(COMPOSE_SLL_IMM(cr_reg, 28, tmp_reg));
	emit(COMPOSE_BIS(rd_reg, tmp_reg, rd_reg));

	unref_integer_reg(cr_reg);
	free_tmp_integer_reg(tmp_reg);

	emit(COMPOSE_ADDL(rd_reg, 31, rd_reg));

	unref_integer_reg(rd_reg);
    }
}

static void
handle_mfctr_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rd_reg, ctr_reg;

	ctr_reg = ref_ppc_spr_r(SPR_CTR);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_MOV(ctr_reg, rd_reg));

	unref_integer_reg(rd_reg);
	unref_integer_reg(ctr_reg);
    }
}

static void
handle_mffs_insn (word_32 insn, word_32 pc)
{
    if (KILL_FPR(FIELD_RD))
    {
	reg_t fpscr_reg, frd_reg;

	fpscr_reg = ref_ppc_spr_r(SPR_FPSCR);
	frd_reg = ref_ppc_fpr_w(FIELD_FRD);

	gen_bits_to_float(fpscr_reg, frd_reg, 1);

	unref_float_reg(frd_reg);
	unref_integer_reg(fpscr_reg);
    }
}

static void
handle_mflr_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rd_reg, lr_reg;

	lr_reg = ref_ppc_spr_r(SPR_LR);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_MOV(lr_reg, rd_reg));

	unref_integer_reg(rd_reg);
	unref_integer_reg(lr_reg);
    }
}

static void
handle_mfxer_insn (word_32 insn, word_32 pc)
{
    assert(0);
}

static void
handle_mtcrf_insn (word_32 insn, word_32 pc)
{
    reg_t mask_reg, rs_reg, cr_reg;

    rs_reg = ref_ppc_gpr_r(FIELD_RS);

    if (FIELD_CRM & 0x80)
    {
	int i;

	for (i = 0; i < 4; ++i)
	    if (KILL_CRFB(i))
	    {
		reg_t bit_reg;

		bit_reg = ref_ppc_crf0_w(i);

		emit(COMPOSE_SRL_IMM(rs_reg, 31 - i, bit_reg));
		emit(COMPOSE_AND_IMM(bit_reg, 1, bit_reg));

		unref_integer_reg(bit_reg);
	    }
    }

    /* FIXME: we don't have to the all the following if FIELD_CRM ==
       0x80 */

    mask_reg = alloc_tmp_integer_reg();

    emit_load_integer_32(mask_reg, maskmask(4, 8, FIELD_CRM & 0x7f));

    cr_reg = ref_ppc_spr_rw(SPR_CR);

    emit(COMPOSE_BIC(cr_reg, mask_reg, cr_reg));
    emit(COMPOSE_AND(rs_reg, mask_reg, mask_reg));

    unref_integer_reg(rs_reg);

    emit(COMPOSE_BIS(cr_reg, mask_reg, cr_reg));

    free_tmp_integer_reg(mask_reg);
    unref_integer_reg(cr_reg);
}

static void
handle_mtctr_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rs_reg, ctr_reg;

	rs_reg = ref_ppc_gpr_r(FIELD_RD);
	ctr_reg = ref_ppc_spr_w(SPR_CTR);

	emit(COMPOSE_MOV(rs_reg, ctr_reg));

	unref_integer_reg(ctr_reg);
	unref_integer_reg(rs_reg);
    }
}

static void
handle_mtfsb0_insn (word_32 insn, word_32 pc)
{
    if (FIELD_CRBD < 4)
    {
	if (KILL_CRFB(FIELD_CRBD))
	{
	    reg_t bit_reg;

	    bit_reg = ref_ppc_crf0_w(FIELD_CRBD);

	    emit(COMPOSE_BIS(31, 31, bit_reg));

	    unref_integer_reg(bit_reg);
	}
    }
    else
    {
	reg_t cr_reg, tmp_reg;

	cr_reg = ref_ppc_spr_rw(SPR_CR);
	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, 1 << (31 - FIELD_CRBD));
	emit(COMPOSE_BIC(cr_reg, tmp_reg, cr_reg));

	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(cr_reg);
    }
}

static void
handle_mtfsf_insn (word_32 insn, word_32 pc)
{
    reg_t fpscr_reg, frb_reg;

    frb_reg = ref_ppc_fpr_r(FIELD_FRB);
    fpscr_reg = ref_ppc_spr_w(SPR_FPSCR);

    emit(COMPOSE_STT(frb_reg, SCRATCH_OFFSET, CONSTANT_AREA_REG));
    emit(COMPOSE_LDL(fpscr_reg, SCRATCH_OFFSET, CONSTANT_AREA_REG));

    unref_integer_reg(fpscr_reg);
    unref_float_reg(frb_reg);
}

static void
handle_mtfsfi_insn (word_32 insn, word_32 pc)
{
    reg_t fpscr_reg, tmp_reg;

    fpscr_reg = ref_ppc_spr_rw(SPR_FPSCR);
    tmp_reg = alloc_tmp_integer_reg();

    emit_load_integer_32(tmp_reg, 15 << (7 - FIELD_CRFD));
    emit(COMPOSE_BIC(fpscr_reg, tmp_reg, fpscr_reg));
    emit_load_integer_32(tmp_reg, FIELD_IMM << (7 - FIELD_CRFD));
    emit(COMPOSE_BIS(fpscr_reg, tmp_reg, fpscr_reg));

    free_tmp_integer_reg(tmp_reg);
    unref_integer_reg(fpscr_reg);
}

static void
handle_mtlr_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rs_reg, lr_reg;

	rs_reg = ref_ppc_gpr_r(FIELD_RD);
	lr_reg = ref_ppc_spr_w(SPR_LR);

	emit(COMPOSE_MOV(rs_reg, lr_reg));

	unref_integer_reg(lr_reg);
	unref_integer_reg(rs_reg);
    }
}

static void
handle_mulhw_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_MULQ(ra_reg, rb_reg, rd_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);

	emit(COMPOSE_SRA_IMM(rd_reg, 32, rd_reg));

	unref_integer_reg(rd_reg);
    }
}

static void
handle_mulhwu_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg, zapped_ra_reg, zapped_rb_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	zapped_ra_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(ra_reg, 15, zapped_ra_reg));

	unref_integer_reg(ra_reg);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	zapped_rb_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(rb_reg, 15, zapped_rb_reg));

	unref_integer_reg(rb_reg);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_MULQ(zapped_ra_reg, zapped_rb_reg, rd_reg));

	free_tmp_integer_reg(zapped_rb_reg);
	free_tmp_integer_reg(zapped_ra_reg);

	emit(COMPOSE_SRA_IMM(rd_reg, 32, rd_reg));

	unref_integer_reg(rd_reg);
    }
}

static void
handle_mulli_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rd_reg, ra_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	if ((FIELD_SIMM & 0xff00) == 0)
	    emit(COMPOSE_MULL_IMM(ra_reg, FIELD_SIMM, rd_reg));
	else
	{
	    reg_t tmp_reg;

	    tmp_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_LDA(tmp_reg, FIELD_SIMM, 31));

	    free_tmp_integer_reg(tmp_reg);

	    emit(COMPOSE_MULL(ra_reg, tmp_reg, rd_reg));
	}

	unref_integer_reg(rd_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
emit_mull (reg_t a, reg_t b, reg_t d)
{
    emit(COMPOSE_MULL(a, b, d));
}

static void
handle_mullw_insn (word_32 insn, word_32 pc)
{
    gen_simple_xo9_rc_insn(insn, emit_mull);
}

static void
handle_mullwd_insn (word_32 insn, word_32 pc)
{
    handle_mullw_insn(insn, pc);
}

static void
emit_nand (reg_t a, reg_t b, reg_t d)
{
    emit(COMPOSE_AND(a, b, d));
    emit(COMPOSE_NOT(d, d));
}

static void
handle_nand_insn (word_32 insn, word_32 pc)
{
    gen_simple_xo1_rc_insn(insn, emit_nand);
}

static void
handle_nandd_insn (word_32 insn, word_32 pc)
{
    handle_nand_insn(insn, pc);
}

static void
handle_neg_insn (word_32 insn, word_32 pc)
{
    reg_t rd_reg, ra_reg;

    ra_reg = ref_ppc_gpr_r(FIELD_RA);
    rd_reg = ref_ppc_gpr_w(FIELD_RD);

    emit(COMPOSE_NEGL(ra_reg, rd_reg));

    unref_integer_reg(ra_reg);

    if (FIELD_RC)
	gen_rc_code(rd_reg);

    unref_integer_reg(rd_reg);
}

static void
handle_negd_insn (word_32 insn, word_32 pc)
{
    handle_neg_insn(insn, pc);
}

static void
handle_nor_insn (word_32 insn, word_32 pc)
{
    gen_simple_xo1_rc_insn(insn, emit_nor);
}

static void
handle_nord_insn (word_32 insn, word_32 pc)
{
    handle_nor_insn(insn, pc);
}

static void
handle_or_insn (word_32 insn, word_32 pc)
{
    gen_simple_xo1_rc_insn(insn, emit_bis);
}

static void
handle_ord_insn (word_32 insn, word_32 pc)
{
    handle_or_insn(insn, pc);
}

static void
emit_ornot (reg_t a, reg_t b, reg_t d)
{
    emit(COMPOSE_ORNOT(a, b, d));
}

static void
handle_orc_insn (word_32 insn, word_32 pc)
{
    gen_simple_xo1_rc_insn(insn, emit_ornot);
}

static void
handle_orcd_insn (word_32 insn, word_32 pc)
{
    handle_orc_insn(insn, pc);
}

static void
handle_ori_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	if ((FIELD_UIMM & 0xff00) == 0)
	    emit(COMPOSE_BIS_IMM(rs_reg, FIELD_UIMM, ra_reg));
	else
	{
	    reg_t tmp_reg;

	    tmp_reg = alloc_tmp_integer_reg();

	    emit_load_integer_32(tmp_reg, FIELD_UIMM);
	    emit(COMPOSE_BIS(rs_reg, tmp_reg, ra_reg));

	    free_tmp_integer_reg(tmp_reg);
	}

	unref_integer_reg(ra_reg);
	unref_integer_reg(rs_reg);
    }
}

static void
handle_oris_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	reg_t ra_reg, rs_reg, tmp_reg;

	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_LDAH(tmp_reg, FIELD_UIMM, 31));

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_BIS(rs_reg, tmp_reg, ra_reg));

	unref_integer_reg(ra_reg);
	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(tmp_reg);
    }
}

static void
gen_rotl (reg_t s, reg_t d, word_5 amount)
{
    if (amount != 0)
    {
	reg_t zapped_s_reg, tmp_reg;

	zapped_s_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(s, 15, zapped_s_reg));

	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_SLL_IMM(zapped_s_reg, amount, tmp_reg));
	emit(COMPOSE_SLL_IMM(zapped_s_reg, amount + 32, d));
	emit(COMPOSE_BIS(d, tmp_reg, d));

	free_tmp_integer_reg(tmp_reg);
	free_tmp_integer_reg(zapped_s_reg);

	emit(COMPOSE_SRA_IMM(d, 32, d));
    }
    else if (s != d)
	emit(COMPOSE_MOV(s, d));
}

static void
handle_rlwimi_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	word_32 mask = mask_32(31 - FIELD_ME, 31 - FIELD_MB);
	reg_t ra_reg, rs_reg, mask_reg, tmp_reg;

	tmp_reg = alloc_tmp_integer_reg();

	rs_reg = ref_ppc_gpr_r(FIELD_RS);

	if (FIELD_SH == 0)
	{
	    mask_reg = alloc_tmp_integer_reg();
	    emit_load_integer_32(mask_reg, mask);

	    emit(COMPOSE_AND(rs_reg, mask_reg, tmp_reg));
	}
	else
	{
	    gen_rotl(rs_reg, tmp_reg, FIELD_SH);

	    mask_reg = alloc_tmp_integer_reg();
	    emit_load_integer_32(mask_reg, mask);

	    emit(COMPOSE_AND(tmp_reg, mask_reg, tmp_reg));
	}

	unref_integer_reg(rs_reg);

	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_BIC(ra_reg, mask_reg, ra_reg));

	free_tmp_integer_reg(mask_reg);

	emit(COMPOSE_BIS(ra_reg, tmp_reg, ra_reg));

	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
gen_and_with_const (reg_t s, reg_t d, word_32 mask)
{
    if ((mask & 0xffffff00) == 0)
	emit(COMPOSE_AND_IMM(s, mask, d));
    else if ((~mask & 0xffffff00) == 0)
	emit(COMPOSE_BIC_IMM(s, ~mask, d));
    else
    {
	reg_t tmp_reg;

	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, mask);
	emit(COMPOSE_AND(s, tmp_reg, d));

	free_tmp_integer_reg(tmp_reg);
    }
}

static void
handle_rlwinm_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	word_32 mask = mask_32(31 - FIELD_ME, 31 - FIELD_MB);
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	if (FIELD_MB == 24 && FIELD_ME == 31 && (FIELD_SH & 7) == 0)
	{
	    emit(COMPOSE_EXTBL_IMM(rs_reg, FIELD_SH == 0 ? 0 : 4 - (FIELD_SH >> 3), ra_reg));

	    unref_integer_reg(rs_reg);
	}
	else if (FIELD_MB == 16 && FIELD_ME == 31 && (FIELD_SH & 7) == 0 && FIELD_SH != 8)
	{
	    emit(COMPOSE_EXTWL_IMM(rs_reg, FIELD_SH == 0 ? 0 : 4 - (FIELD_SH >> 3), ra_reg));

	    unref_integer_reg(rs_reg);
	}
	else if (FIELD_SH == 0)
	{
	    gen_and_with_const(rs_reg, ra_reg, mask);

	    unref_integer_reg(rs_reg);
	}
	else if (FIELD_MB == 0 && FIELD_ME + FIELD_SH == 31)
	{
	    emit(COMPOSE_SLL_IMM(rs_reg, FIELD_SH, ra_reg));

	    unref_integer_reg(rs_reg);

	    emit(COMPOSE_ADDL(ra_reg, 31, ra_reg));
	}
	else if (FIELD_ME == 31 && (32 - FIELD_MB) <= FIELD_SH)
	{
	    assert(FIELD_MB != 0);

	    emit(COMPOSE_SLL_IMM(rs_reg, 32 + FIELD_SH - (32 - FIELD_MB), ra_reg));

	    unref_integer_reg(rs_reg);

	    emit(COMPOSE_SRL_IMM(ra_reg, 32 + FIELD_MB, ra_reg));
	}
	else
	{
	    gen_rotl(rs_reg, ra_reg, FIELD_SH);

	    unref_integer_reg(rs_reg);

	    gen_and_with_const(ra_reg, ra_reg, mask);
	}

	if (FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
}

static void
handle_rlwinmd_insn (word_32 insn, word_32 pc)
{
    handle_rlwinm_insn(insn, pc);
}

static void
handle_sc_insn (word_32 insn, word_32 pc)
{
    emit_freeze_save();

    emit_store_regs(EMU_INSN_EPILOGUE, NO_FOREIGN_ADDR);

    emit_system_call();

    emit_start_direct_jump(0);
    emit_direct_jump(pc + 4);

    set_all_regs_must_be_saved();
}

static void
handle_slw_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	reg_t rb_reg, rs_reg, ra_reg, tmp_reg;

	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 63, tmp_reg));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_SLL(rs_reg, tmp_reg, ra_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(tmp_reg);

	emit(COMPOSE_ADDL(ra_reg, 31, ra_reg));

	unref_integer_reg(ra_reg);
    }
}

static void
handle_sraw_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA) && KILL_XER_CA)
    {
	reg_t rs_reg, ra_reg, rb_reg, ca_reg, mask_reg, amount_reg, tmp_reg;
	label_t no_ca_set = alloc_label();

	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	amount_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 63, amount_reg));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ca_reg = ref_ppc_xer_ca_w();

	mask_reg = alloc_tmp_integer_reg();
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_MOV(31, ca_reg));
	emit_branch(COMPOSE_BGE(rs_reg, 0), no_ca_set);

	/* push_alloc(); */

	emit(COMPOSE_NOT(31, mask_reg));
	emit(COMPOSE_SLL(mask_reg, amount_reg, mask_reg));
	emit(COMPOSE_BIC(rs_reg, mask_reg, tmp_reg));
	emit(COMPOSE_CMPLT(31, tmp_reg, ca_reg));

	/* pop_alloc(); */

	free_tmp_integer_reg(tmp_reg);
	free_tmp_integer_reg(mask_reg);

	emit_label(no_ca_set);
	free_label(no_ca_set);

	unref_integer_reg(ca_reg);

	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_SRA(rs_reg, amount_reg, ra_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(amount_reg);

	if (FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
    else if (KILL_GPR(FIELD_RA))
    {
	reg_t rb_reg, rs_reg, ra_reg, tmp_reg;

	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 63, tmp_reg));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_SRA(rs_reg, tmp_reg, ra_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(tmp_reg);

	if (FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
    else if (KILL_XER_CA)
    {
	reg_t rs_reg, rb_reg, ca_reg, mask_reg, amount_reg, tmp_reg;
	label_t end = alloc_label();

	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	amount_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 63, amount_reg));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ca_reg = ref_ppc_xer_ca_w();

	mask_reg = alloc_tmp_integer_reg();
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_MOV(31, ca_reg));
	emit_branch(COMPOSE_BGE(rs_reg, 0), end);

	/* push_alloc(); */

	emit(COMPOSE_NOT(31, mask_reg));
	emit(COMPOSE_SLL(mask_reg, amount_reg, mask_reg));
	emit(COMPOSE_BIC(rs_reg, mask_reg, tmp_reg));
	emit(COMPOSE_CMPLT(31, tmp_reg, ca_reg));

	/* pop_alloc(); */

	free_tmp_integer_reg(tmp_reg);
	free_tmp_integer_reg(mask_reg);

	emit_label(end);
	free_label(end);

	unref_integer_reg(ca_reg);
	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(amount_reg);
    }
}

static void
handle_srawd_insn (word_32 insn, word_32 pc)
{
    handle_sraw_insn(insn, pc);
}

static void
handle_srawi_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA) && KILL_XER_CA)
    {
	reg_t rs_reg, ra_reg, ca_reg, mask_reg, tmp_reg;
	label_t no_ca_set = alloc_label();

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ca_reg = ref_ppc_xer_ca_w();

	tmp_reg = alloc_tmp_integer_reg();
	if (FIELD_SH > 8)
	    mask_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_MOV(31, ca_reg));
	emit_branch(COMPOSE_BGE(rs_reg, 0), no_ca_set);

	/* push_alloc(); */

	if (FIELD_SH <= 8)
	    emit(COMPOSE_AND_IMM(rs_reg, (1 << FIELD_SH) - 1, tmp_reg));
	else
	{
	    emit_load_integer_32(mask_reg, (1 << FIELD_SH) - 1);
	    emit(COMPOSE_AND(rs_reg, mask_reg, tmp_reg));
	}

	emit(COMPOSE_CMPLT(31, tmp_reg, ca_reg));

	/* pop_alloc(); */

	if (FIELD_SH > 8)
	    free_tmp_integer_reg(mask_reg);
	free_tmp_integer_reg(tmp_reg);

	emit_label(no_ca_set);
	free_label(no_ca_set);

	unref_integer_reg(ca_reg);

	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_SRA_IMM(rs_reg, FIELD_SH, ra_reg));

	unref_integer_reg(rs_reg);

	if (FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
    else if (KILL_GPR(FIELD_RA))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_SRA_IMM(rs_reg, FIELD_SH, ra_reg));

	unref_integer_reg(rs_reg);

	if (FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
    else if (KILL_XER_CA)
    {
	reg_t rs_reg, ca_reg, mask_reg, tmp_reg;
	label_t end = alloc_label();

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ca_reg = ref_ppc_xer_ca_w();

	tmp_reg = alloc_tmp_integer_reg();
	if (FIELD_SH > 8)
	    mask_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_MOV(31, ca_reg));
	emit_branch(COMPOSE_BGE(rs_reg, 0), end);

	/* push_alloc(); */

	if (FIELD_SH <= 8)
	    emit(COMPOSE_AND_IMM(rs_reg, (1 << FIELD_SH) - 1, tmp_reg));
	else
	{
	    emit_load_integer_32(mask_reg, (1 << FIELD_SH) - 1);
	    emit(COMPOSE_BIC(rs_reg, mask_reg, tmp_reg));
	}

	emit(COMPOSE_CMPLT(31, tmp_reg, ca_reg));

	/* pop_alloc(); */

	if (FIELD_SH > 8)
	    free_tmp_integer_reg(mask_reg);
	free_tmp_integer_reg(tmp_reg);

	emit_label(end);
	free_label(end);

	unref_integer_reg(ca_reg);
	unref_integer_reg(rs_reg);
    }
}

static void
handle_srawid_insn (word_32 insn, word_32 pc)
{
    handle_srawi_insn(insn, pc);
}

static void
handle_srw_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	reg_t rb_reg, rs_reg, ra_reg, tmp_reg;

	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 31, tmp_reg));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_ZAPNOT_IMM(rs_reg, 15, ra_reg));

	unref_integer_reg(rs_reg);

	emit(COMPOSE_SRL(ra_reg, tmp_reg, ra_reg));

	free_tmp_integer_reg(tmp_reg);

	emit(COMPOSE_ADDL(ra_reg, 31, ra_reg));

	if (FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
}

static void
handle_srwd_insn (word_32 insn, word_32 pc)
{
    handle_srw_insn(insn, pc);
}

static void
handle_stb_insn (word_32 insn, word_32 pc)
{
    reg_t rs_reg;

    if (FIELD_RA == 0)
    {
	rs_reg = ref_ppc_gpr_r(FIELD_RD);

	emit(COMPOSE_STB(rs_reg, FIELD_D ^ 3, 31));
    }
    else
    {
	reg_t ra_reg, addr_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_LDA(addr_reg, FIELD_D, ra_reg));

	unref_integer_reg(ra_reg);

	emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));

	rs_reg = ref_ppc_gpr_r(FIELD_RD);

	emit(COMPOSE_STB(rs_reg, 0, addr_reg));

	free_tmp_integer_reg(addr_reg);
    }

    unref_integer_reg(rs_reg);
}

static void
handle_stbu_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg, rs_reg, addr_reg;

    assert(FIELD_RS != FIELD_RA);

    ra_reg = ref_ppc_gpr_rw(FIELD_RA);

    emit(COMPOSE_LDA(ra_reg, FIELD_D, ra_reg));

    addr_reg = alloc_tmp_integer_reg();

    emit(COMPOSE_XOR_IMM(ra_reg, 3, addr_reg));

    unref_integer_reg(ra_reg);
    rs_reg = ref_ppc_gpr_r(FIELD_RS);

    emit(COMPOSE_STB(rs_reg, 0, addr_reg));

    unref_integer_reg(rs_reg);
    free_tmp_integer_reg(addr_reg);
}

static void
handle_stbx_insn (word_32 insn, word_32 pc)
{
    reg_t rb_reg, rs_reg, addr_reg;

    rb_reg = ref_ppc_gpr_r(FIELD_RB);

    if (FIELD_RA == 0)
    {
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_XOR_IMM(rb_reg, 3, addr_reg));

	unref_integer_reg(rb_reg);
	rs_reg = ref_ppc_gpr_w(FIELD_RS);

	emit(COMPOSE_STB(rs_reg, 0, addr_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(addr_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);

	emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));

	rs_reg = ref_ppc_gpr_r(FIELD_RS);

	emit(COMPOSE_STB(rs_reg, 0, addr_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(addr_reg);
    }
}

static void
handle_stfd_insn (word_32 insn, word_32 pc)
{
    reg_t addr_reg, bits_reg, frs_reg;

    addr_reg = alloc_tmp_integer_reg();

    if (FIELD_RA == 0)
	emit(COMPOSE_LDA(addr_reg, FIELD_D, 31));
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);

	emit(COMPOSE_LDA(addr_reg, FIELD_D, ra_reg));

	unref_integer_reg(ra_reg);
    }

    frs_reg = ref_ppc_fpr_r(FIELD_FRS);
    bits_reg = alloc_tmp_integer_reg();

    gen_float_to_bits(frs_reg, bits_reg, 0);

    unref_float_reg(frs_reg);

    emit_store_mem_64(bits_reg, addr_reg);

    free_tmp_integer_reg(bits_reg);
    free_tmp_integer_reg(addr_reg);
}

static void
handle_stfdu_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg, frs_reg, bits_reg;

    if (FIELD_D == 0)
	ra_reg = ref_ppc_gpr_r(FIELD_RA);
    else
    {
	ra_reg = ref_ppc_gpr_rw(FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, FIELD_D, ra_reg));
    }

    frs_reg = ref_ppc_fpr_r(FIELD_FRS);
    bits_reg = alloc_tmp_integer_reg();

    gen_float_to_bits(frs_reg, bits_reg, 0);

    unref_float_reg(frs_reg);

    emit_store_mem_64(bits_reg, ra_reg);

    free_tmp_integer_reg(bits_reg);
    unref_integer_reg(ra_reg);
}

static void
handle_stfdx_insn (word_32 insn, word_32 pc)
{
    reg_t addr_reg, bits_reg, frs_reg;

    if (FIELD_RA == 0)
	addr_reg = ref_ppc_gpr_r(FIELD_RB);
    else
    {
	reg_t ra_reg, rb_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);

	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDQ(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
    }

    bits_reg = alloc_tmp_integer_reg();
    frs_reg = ref_ppc_fpr_r(FIELD_FRS);

    gen_float_to_bits(frs_reg, bits_reg, 0);

    emit_store_mem_64(bits_reg, addr_reg);

    unref_float_reg(frs_reg);
    free_tmp_integer_reg(bits_reg);
    if (FIELD_RA == 0)
	unref_integer_reg(addr_reg);
    else
	free_tmp_integer_reg(addr_reg);
}

static void
handle_stfs_insn (word_32 insn, word_32 pc)
{
    reg_t frs_reg;

    if (FIELD_RA == 0)
    {
	frs_reg = ref_ppc_fpr_r(FIELD_FRS);

	emit(COMPOSE_STS(frs_reg, FIELD_D, 31));

	unref_float_reg(frs_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	frs_reg = ref_ppc_fpr_r(FIELD_FRS);

	emit(COMPOSE_STS(frs_reg, FIELD_D, ra_reg));

	unref_float_reg(frs_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_stfsx_insn (word_32 insn, word_32 pc)
{
    reg_t rb_reg, frs_reg, addr_reg;

    rb_reg = ref_ppc_gpr_r(FIELD_RB);

    if (FIELD_RA == 0)
    {
	frs_reg = ref_ppc_fpr_w(FIELD_FRS);

	emit(COMPOSE_STS(frs_reg, 0, rb_reg));

	unref_float_reg(frs_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
	frs_reg = ref_ppc_fpr_r(FIELD_FRS);

	emit(COMPOSE_STS(frs_reg, 0, addr_reg));

	unref_float_reg(frs_reg);
	free_tmp_integer_reg(addr_reg);
    }
}

static void
handle_sth_insn (word_32 insn, word_32 pc)
{
    reg_t rs_reg;

    if (FIELD_RA == 0)
    {
	rs_reg = ref_ppc_gpr_r(FIELD_RD);

	emit(COMPOSE_STW(rs_reg, FIELD_D ^ 2, 31));
    }
    else
    {
	reg_t ra_reg, addr_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_LDA(addr_reg, FIELD_D, ra_reg));

	unref_integer_reg(ra_reg);

	emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));

	rs_reg = ref_ppc_gpr_r(FIELD_RD);

	emit(COMPOSE_STW(rs_reg, 0, addr_reg));

	free_tmp_integer_reg(addr_reg);
    }

    unref_integer_reg(rs_reg);
}

static void
handle_sthu_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg, rs_reg, addr_reg;

    assert(FIELD_RS != FIELD_RA);

    ra_reg = ref_ppc_gpr_rw(FIELD_RA);

    emit(COMPOSE_LDA(ra_reg, FIELD_D, ra_reg));

    addr_reg = alloc_tmp_integer_reg();

    emit(COMPOSE_XOR_IMM(ra_reg, 2, addr_reg));

    unref_integer_reg(ra_reg);
    rs_reg = ref_ppc_gpr_r(FIELD_RS);

    emit(COMPOSE_STW(rs_reg, 0, addr_reg));

    unref_integer_reg(rs_reg);
    free_tmp_integer_reg(addr_reg);
}

static void
handle_sthx_insn (word_32 insn, word_32 pc)
{
    reg_t rb_reg, rs_reg, addr_reg;

    rb_reg = ref_ppc_gpr_r(FIELD_RB);

    if (FIELD_RA == 0)
    {
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_XOR_IMM(rb_reg, 2, addr_reg));

	unref_integer_reg(rb_reg);
	rs_reg = ref_ppc_gpr_w(FIELD_RS);

	emit(COMPOSE_STW(rs_reg, 0, addr_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(addr_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);

	emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));

	rs_reg = ref_ppc_gpr_r(FIELD_RS);

	emit(COMPOSE_STW(rs_reg, 0, addr_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(addr_reg);
    }
}

static void
handle_stw_insn (word_32 insn, word_32 pc)
{
    reg_t rs_reg;

    if (FIELD_RA == 0)
    {
	rs_reg = ref_ppc_gpr_r(FIELD_RS);

	emit(COMPOSE_STL(rs_reg, FIELD_D, 31));

	unref_integer_reg(rs_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rs_reg = ref_ppc_gpr_r(FIELD_RS);

	emit(COMPOSE_STL(rs_reg, FIELD_D, ra_reg));

	unref_integer_reg(rs_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_stwu_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg, rs_reg;

    if (KILL_GPR(FIELD_RA))
    {
	ra_reg = ref_ppc_gpr_rw(FIELD_RA);
	rs_reg = ref_ppc_gpr_r(FIELD_RS);

	emit(COMPOSE_STL(rs_reg, FIELD_D, ra_reg));

	unref_integer_reg(rs_reg);

	emit(COMPOSE_LDA(ra_reg, FIELD_D, ra_reg));

	unref_integer_reg(ra_reg);
    }
    else
    {
	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rs_reg = ref_ppc_gpr_r(FIELD_RS);

	emit(COMPOSE_STL(rs_reg, FIELD_D, ra_reg));

	unref_integer_reg(rs_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_stwux_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg, rb_reg, rs_reg;

    assert(FIELD_RA != FIELD_RS);

    rb_reg = ref_ppc_gpr_r(FIELD_RB);
    ra_reg = ref_ppc_gpr_rw(FIELD_RA);

    emit(COMPOSE_ADDL(ra_reg, rb_reg, ra_reg));

    unref_integer_reg(rb_reg);
    rs_reg = ref_ppc_gpr_r(FIELD_RS);

    emit(COMPOSE_STL(rs_reg, 0, ra_reg));

    unref_integer_reg(rs_reg);
    unref_integer_reg(ra_reg);
}

static void
handle_stwx_insn (word_32 insn, word_32 pc)
{
    reg_t rb_reg, rs_reg, addr_reg;

    rb_reg = ref_ppc_gpr_r(FIELD_RB);

    if (FIELD_RA == 0)
    {
	rs_reg = ref_ppc_gpr_w(FIELD_RS);

	emit(COMPOSE_STL(rs_reg, 0, rb_reg));

	unref_integer_reg(rs_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
	rs_reg = ref_ppc_gpr_r(FIELD_RS);

	emit(COMPOSE_STL(rs_reg, 0, addr_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(addr_reg);
    }
}

static void
handle_subf_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_SUBL(rb_reg, ra_reg, rd_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);

	if (FIELD_RC)
	    gen_rc_code(rd_reg);

	unref_integer_reg(rd_reg);
    }
}

static void
handle_subfd_insn (word_32 insn, word_32 pc)
{
    handle_subf_insn(insn, pc);
}

static void
handle_subfc_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD) && KILL_XER_CA)
    {
	reg_t ra_reg, rb_reg, rd_reg, ca_reg, tmp_reg;
	label_t end = alloc_label();

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	ca_reg = ref_ppc_xer_ca_w();

	emit(COMPOSE_CMPLE(ra_reg, rb_reg, ca_reg));

	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_EQV(rb_reg, ra_reg, tmp_reg));
	emit_branch(COMPOSE_BLT(tmp_reg, 0), end);

	free_tmp_integer_reg(tmp_reg);

	emit(COMPOSE_CMPLT(rb_reg, ra_reg, ca_reg));

	emit_label(end);
	free_label(end);

	unref_integer_reg(ca_reg);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_SUBL(rb_reg, ra_reg, rd_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);

	if (FIELD_RC)
	    gen_rc_code(rd_reg);

	unref_integer_reg(rd_reg);
    }
    else if (KILL_GPR(FIELD_RD))
    {
	handle_subf_insn(insn, pc);
    }
    else if (KILL_XER_CA)
    {
	reg_t ra_reg, rb_reg, ca_reg, tmp_reg;
	label_t end = alloc_label();

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	rb_reg = ref_ppc_gpr_r(FIELD_RB);
	ca_reg = ref_ppc_xer_ca_w();

	emit(COMPOSE_CMPLE(ra_reg, rb_reg, ca_reg));

	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_EQV(rb_reg, ra_reg, tmp_reg));
	emit_branch(COMPOSE_BLT(tmp_reg, 0), end);

	free_tmp_integer_reg(tmp_reg);

	emit(COMPOSE_CMPLT(rb_reg, ra_reg, ca_reg));

	emit_label(end);
	free_label(end);

	unref_integer_reg(ca_reg);
	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_subfcd_insn (word_32 insn, word_32 pc)
{
    handle_subfc_insn(insn, pc);
}

static void
handle_subfe_insn (word_32 insn, word_32 pc)
{
    if (FIELD_RA == FIELD_RB)
    {
	if (KILL_GPR(FIELD_RD))
	{
	    reg_t rd_reg, ca_reg;

	    ca_reg = ref_ppc_xer_ca_r();
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_SUBL_IMM(ca_reg, 1, rd_reg));

	    unref_integer_reg(ca_reg);

	    if (FIELD_RC)
		gen_rc_code(rd_reg);

	    unref_integer_reg(rd_reg);
	}
    }
    else
    {
	if (KILL_GPR(FIELD_RD) && KILL_XER_CA)
	{
	    reg_t ra_reg, rb_reg, rd_reg, ca_reg, tmp_reg;

	    ra_reg = ref_ppc_gpr_r(FIELD_RA);
	    if (FIELD_RD == FIELD_RA)
	    {
		tmp_reg = alloc_tmp_integer_reg();

		emit(COMPOSE_MOV(ra_reg, tmp_reg));

		unref_integer_reg(ra_reg);

		ra_reg = tmp_reg;
	    }

	    rb_reg = ref_ppc_gpr_r(FIELD_RB);
	    if (FIELD_RD == FIELD_RB)
	    {
		tmp_reg = alloc_tmp_integer_reg();

		emit(COMPOSE_MOV(rb_reg, tmp_reg));

		unref_integer_reg(rb_reg);

		rb_reg = tmp_reg;
	    }

	    ca_reg = ref_ppc_xer_ca_rw();
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_XOR_IMM(ca_reg, 1, rd_reg));
	    emit(COMPOSE_ADDL(ra_reg, rd_reg, rd_reg));
	    emit(COMPOSE_SUBL(rb_reg, rd_reg, rd_reg));

	    tmp_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_NOT(ra_reg, tmp_reg));
	    emit(COMPOSE_ZAPNOT_IMM(tmp_reg, 15, tmp_reg));
	    emit(COMPOSE_ADDQ(tmp_reg, ca_reg, ca_reg));
	    emit(COMPOSE_ZAPNOT_IMM(rb_reg, 15, tmp_reg));
	    emit(COMPOSE_ADDQ(tmp_reg, ca_reg, ca_reg));

	    free_tmp_integer_reg(tmp_reg);

	    emit(COMPOSE_SRL_IMM(ca_reg, 32, ca_reg));
	    emit(COMPOSE_AND_IMM(ca_reg, 1, ca_reg));

	    unref_integer_reg(ca_reg);
	    if (FIELD_RD == FIELD_RB)
		free_tmp_integer_reg(rb_reg);
	    else
		unref_integer_reg(rb_reg);
	    if (FIELD_RD == FIELD_RA)
		free_tmp_integer_reg(ra_reg);
	    else
		unref_integer_reg(ra_reg);

	    if (FIELD_RC)
		gen_rc_code(rd_reg);

	    unref_integer_reg(rd_reg);
	}
	else if (KILL_GPR(FIELD_RD))
	{
	    reg_t ra_reg, rb_reg, rd_reg, ca_reg;

	    assert(FIELD_RD != FIELD_RA && FIELD_RD != FIELD_RB);

	    ca_reg = ref_ppc_xer_ca_r();
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);

	    emit(COMPOSE_XOR_IMM(ca_reg, 1, rd_reg));

	    unref_integer_reg(ca_reg);
	    ra_reg = ref_ppc_gpr_r(FIELD_RA);

	    emit(COMPOSE_ADDL(ra_reg, rd_reg, rd_reg));

	    rb_reg = ref_ppc_gpr_r(FIELD_RB);

	    emit(COMPOSE_SUBL(rb_reg, rd_reg, rd_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);

	    if (FIELD_RC)
		gen_rc_code(rd_reg);

	    unref_integer_reg(rd_reg);

	}
	else if (KILL_XER_CA)
	{
	    reg_t ra_reg, rb_reg, ca_reg, tmp_reg, rd_reg;

	    ca_reg = ref_ppc_xer_ca_rw();
	    rd_reg = ref_ppc_gpr_w(FIELD_RD);
	    tmp_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_NOT(ra_reg, tmp_reg));
	    emit(COMPOSE_ZAPNOT_IMM(tmp_reg, 15, tmp_reg));
	    emit(COMPOSE_ADDQ(tmp_reg, ca_reg, ca_reg));
	    emit(COMPOSE_ZAPNOT_IMM(rb_reg, 15, tmp_reg));
	    emit(COMPOSE_ADDQ(tmp_reg, ca_reg, ca_reg));

	    free_tmp_integer_reg(tmp_reg);

	    emit(COMPOSE_SRL_IMM(ca_reg, 32, ca_reg));
	    emit(COMPOSE_AND_IMM(ca_reg, 1, ca_reg));

	    unref_integer_reg(ca_reg);
	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);
	}
    }
}

static void
handle_subfic_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RD) && KILL_XER_CA)
    {
	reg_t ra_reg, rd_reg, ca_reg, tmp_reg;
	label_t end = alloc_label();

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	ca_reg = ref_ppc_xer_ca_w();

	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, SEX32(FIELD_SIMM, 16));

	emit(COMPOSE_CMPLE(ra_reg, tmp_reg, ca_reg));
	if (FIELD_SIMM & 0x8000)
	    emit_branch(COMPOSE_BLT(ra_reg, 0), end);
	else
	    emit_branch(COMPOSE_BGE(ra_reg, 0), end);

	emit(COMPOSE_CMPLT(tmp_reg, ra_reg, ca_reg));

	emit_label(end);
	free_label(end);

	unref_integer_reg(ca_reg);
	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_SUBL(tmp_reg, ra_reg, rd_reg));

	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(ra_reg);
	unref_integer_reg(rd_reg);
    }
    else if (KILL_GPR(FIELD_RD))
    {
	reg_t ra_reg, rd_reg, tmp_reg;

	ra_reg = ref_ppc_gpr_r(FIELD_RA);

	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, SEX32(FIELD_SIMM, 16));

	rd_reg = ref_ppc_gpr_w(FIELD_RD);

	emit(COMPOSE_SUBL(tmp_reg, ra_reg, rd_reg));

	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(ra_reg);
	unref_integer_reg(rd_reg);
    }
    else if (KILL_XER_CA)
    {
	reg_t ra_reg, ca_reg, tmp_reg;
	label_t end = alloc_label();

	ra_reg = ref_ppc_gpr_r(FIELD_RA);
	ca_reg = ref_ppc_xer_ca_w();

	tmp_reg = alloc_tmp_integer_reg();
	emit_load_integer_32(tmp_reg, SEX32(FIELD_SIMM, 16));

	emit(COMPOSE_CMPLE(ra_reg, tmp_reg, ca_reg));
	if (FIELD_SIMM & 0x8000)
	    emit_branch(COMPOSE_BLT(ra_reg, 0), end);
	else
	    emit_branch(COMPOSE_BGE(ra_reg, 0), end);

	emit(COMPOSE_CMPLT(tmp_reg, ra_reg, ca_reg));

	emit_label(end);
	free_label(end);

	unref_integer_reg(ca_reg);
	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_sync_insn (word_32 insn, word_32 pc)
{
}

static void
handle_xor_insn (word_32 insn, word_32 pc)
{
    gen_simple_xo1_rc_insn(insn, emit_xor);
}

static void
handle_xord_insn (word_32 insn, word_32 pc)
{
    handle_xor_insn(insn, pc);
}

static void
handle_xori_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	if ((FIELD_UIMM & 0xff00) == 0)
	{
	    reg_t ra_reg, rs_reg;

	    rs_reg = ref_ppc_gpr_r(FIELD_RS);
	    ra_reg = ref_ppc_gpr_w(FIELD_RA);

	    emit(COMPOSE_XOR_IMM(rs_reg, FIELD_UIMM, ra_reg));

	    unref_integer_reg(ra_reg);
	    unref_integer_reg(rs_reg);
	}
	else
	{
	    reg_t ra_reg, rs_reg, tmp_reg;

	    tmp_reg = alloc_tmp_integer_reg();

	    emit_load_integer_32(tmp_reg, FIELD_UIMM);

	    rs_reg = ref_ppc_gpr_r(FIELD_RS);
	    ra_reg = ref_ppc_gpr_w(FIELD_RA);

	    emit(COMPOSE_XOR(rs_reg, tmp_reg, ra_reg));

	    unref_integer_reg(ra_reg);
	    unref_integer_reg(rs_reg);
	    free_tmp_integer_reg(tmp_reg);
	}
    }
}

static void
handle_xoris_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(FIELD_RA))
    {
	reg_t ra_reg, rs_reg, tmp_reg;

	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, FIELD_UIMM << 16);

	rs_reg = ref_ppc_gpr_r(FIELD_RS);
	ra_reg = ref_ppc_gpr_w(FIELD_RA);

	emit(COMPOSE_XOR(rs_reg, tmp_reg, ra_reg));

	unref_integer_reg(ra_reg);
	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(tmp_reg);
    }
}

void
compile_to_alpha_ppc_insn (word_32 insn, word_32 pc, int _optimize_taken_jump, label_t _taken_jump_label, word_32 _next_pc,
			   word_32 _kill_cr, word_32 _kill_xer, word_32 _kill_gpr)
{
    optimize_taken_jump = _optimize_taken_jump;
    taken_jump_label = _taken_jump_label;
    next_pc = _next_pc;
    kill_cr = _kill_cr;
    kill_xer = _kill_xer;
    kill_gpr = _kill_gpr;

#ifdef COLLECT_STATS
    ++num_translated_insns;
#endif

    switch (((insn >> 26) & 0x3F)) {
	case 27:
	    /* XORIS */
	    assert((insn & 0xFC000000) == 0x6C000000);
	    handle_xoris_insn(insn, pc);
	    break;
	case 26:
	    /* XORI */
	    assert((insn & 0xFC000000) == 0x68000000);
	    handle_xori_insn(insn, pc);
	    break;
	case 31:
	    switch (((insn >> 0) & 0x7FF)) {
		case 633:
		    /* XOR. */
		    assert((insn & 0xFC0007FF) == 0x7C000279);
		    handle_xord_insn(insn, pc);
		    break;
		case 632:
		    /* XOR */
		    assert((insn & 0xFC0007FF) == 0x7C000278);
		    handle_xor_insn(insn, pc);
		    break;
		case 1196:
		    /* SYNC */
		    assert((insn & 0xFFFFFFFF) == 0x7C0004AC);
		    handle_sync_insn(insn, pc);
		    break;
		case 272:
		    /* SUBFE */
		    assert((insn & 0xFC0007FF) == 0x7C000110);
		    handle_subfe_insn(insn, pc);
		    break;
		case 17:
		    /* SUBFC. */
		    assert((insn & 0xFC0007FF) == 0x7C000011);
		    handle_subfcd_insn(insn, pc);
		    break;
		case 16:
		    /* SUBFC */
		    assert((insn & 0xFC0007FF) == 0x7C000010);
		    handle_subfc_insn(insn, pc);
		    break;
		case 81:
		    /* SUBF. */
		    assert((insn & 0xFC0007FF) == 0x7C000051);
		    handle_subfd_insn(insn, pc);
		    break;
		case 80:
		    /* SUBF */
		    assert((insn & 0xFC0007FF) == 0x7C000050);
		    handle_subf_insn(insn, pc);
		    break;
		case 302:
		    /* STWX */
		    assert((insn & 0xFC0007FF) == 0x7C00012E);
		    handle_stwx_insn(insn, pc);
		    break;
		case 366:
		    /* STWUX */
		    assert((insn & 0xFC0007FF) == 0x7C00016E);
		    handle_stwux_insn(insn, pc);
		    break;
		case 814:
		    /* STHX */
		    assert((insn & 0xFC0007FF) == 0x7C00032E);
		    handle_sthx_insn(insn, pc);
		    break;
		case 1326:
		    /* STFSX */
		    assert((insn & 0xFC0007FF) == 0x7C00052E);
		    handle_stfsx_insn(insn, pc);
		    break;
		case 1454:
		    /* STFDX */
		    assert((insn & 0xFC0007FF) == 0x7C0005AE);
		    handle_stfdx_insn(insn, pc);
		    break;
		case 430:
		    /* STBX */
		    assert((insn & 0xFC0007FF) == 0x7C0001AE);
		    handle_stbx_insn(insn, pc);
		    break;
		case 1073:
		    /* SRW. */
		    assert((insn & 0xFC0007FF) == 0x7C000431);
		    handle_srwd_insn(insn, pc);
		    break;
		case 1072:
		    /* SRW */
		    assert((insn & 0xFC0007FF) == 0x7C000430);
		    handle_srw_insn(insn, pc);
		    break;
		case 1649:
		    /* SRAWI. */
		    assert((insn & 0xFC0007FF) == 0x7C000671);
		    handle_srawid_insn(insn, pc);
		    break;
		case 1648:
		    /* SRAWI */
		    assert((insn & 0xFC0007FF) == 0x7C000670);
		    handle_srawi_insn(insn, pc);
		    break;
		case 1585:
		    /* SRAW. */
		    assert((insn & 0xFC0007FF) == 0x7C000631);
		    handle_srawd_insn(insn, pc);
		    break;
		case 1584:
		    /* SRAW */
		    assert((insn & 0xFC0007FF) == 0x7C000630);
		    handle_sraw_insn(insn, pc);
		    break;
		case 48:
		    /* SLW */
		    assert((insn & 0xFC0007FF) == 0x7C000030);
		    handle_slw_insn(insn, pc);
		    break;
		case 825:
		    /* ORC. */
		    assert((insn & 0xFC0007FF) == 0x7C000339);
		    handle_orcd_insn(insn, pc);
		    break;
		case 824:
		    /* ORC */
		    assert((insn & 0xFC0007FF) == 0x7C000338);
		    handle_orc_insn(insn, pc);
		    break;
		case 889:
		    /* OR. */
		    assert((insn & 0xFC0007FF) == 0x7C000379);
		    handle_ord_insn(insn, pc);
		    break;
		case 888:
		    /* OR */
		    assert((insn & 0xFC0007FF) == 0x7C000378);
		    handle_or_insn(insn, pc);
		    break;
		case 249:
		    /* NOR. */
		    assert((insn & 0xFC0007FF) == 0x7C0000F9);
		    handle_nord_insn(insn, pc);
		    break;
		case 248:
		    /* NOR */
		    assert((insn & 0xFC0007FF) == 0x7C0000F8);
		    handle_nor_insn(insn, pc);
		    break;
		case 209:
		    /* NEG. */
		    assert((insn & 0xFC00FFFF) == 0x7C0000D1);
		    handle_negd_insn(insn, pc);
		    break;
		case 208:
		    /* NEG */
		    assert((insn & 0xFC00FFFF) == 0x7C0000D0);
		    handle_neg_insn(insn, pc);
		    break;
		case 953:
		    /* NAND. */
		    assert((insn & 0xFC0007FF) == 0x7C0003B9);
		    handle_nandd_insn(insn, pc);
		    break;
		case 952:
		    /* NAND */
		    assert((insn & 0xFC0007FF) == 0x7C0003B8);
		    handle_nand_insn(insn, pc);
		    break;
		case 471:
		    /* MULLW. */
		    assert((insn & 0xFC0007FF) == 0x7C0001D7);
		    handle_mullwd_insn(insn, pc);
		    break;
		case 470:
		    /* MULLW */
		    assert((insn & 0xFC0007FF) == 0x7C0001D6);
		    handle_mullw_insn(insn, pc);
		    break;
		case 22:
		    /* MULHWU */
		    assert((insn & 0xFC0007FF) == 0x7C000016);
		    handle_mulhwu_insn(insn, pc);
		    break;
		case 150:
		    /* MULHW */
		    assert((insn & 0xFC0007FF) == 0x7C000096);
		    handle_mulhw_insn(insn, pc);
		    break;
		case 934:
		    switch (((insn >> 11) & 0x3FF)) {
			case 256:
			    /* MTLR */
			    assert((insn & 0xFC1FFFFF) == 0x7C0803A6);
			    handle_mtlr_insn(insn, pc);
			    break;
			case 288:
			    /* MTCTR */
			    assert((insn & 0xFC1FFFFF) == 0x7C0903A6);
			    handle_mtctr_insn(insn, pc);
			    break;
			default:
			    assert(0);
		    }
		    break;
		case 288:
		    /* MTCRF */
		    assert((insn & 0xFC100FFF) == 0x7C000120);
		    handle_mtcrf_insn(insn, pc);
		    break;
		case 678:
		    switch (((insn >> 11) & 0x3FF)) {
			case 32:
			    /* MFXER */
			    assert((insn & 0xFC1FFFFF) == 0x7C0102A6);
			    handle_mfxer_insn(insn, pc);
			    break;
			case 256:
			    /* MFLR */
			    assert((insn & 0xFC1FFFFF) == 0x7C0802A6);
			    handle_mflr_insn(insn, pc);
			    break;
			case 288:
			    /* MFCTR */
			    assert((insn & 0xFC1FFFFF) == 0x7C0902A6);
			    handle_mfctr_insn(insn, pc);
			    break;
			default:
			    assert(0);
		    }
		    break;
		case 38:
		    /* MFCR */
		    assert((insn & 0xFC1FFFFF) == 0x7C000026);
		    handle_mfcr_insn(insn, pc);
		    break;
		case 46:
		    /* LWZX */
		    assert((insn & 0xFC0007FF) == 0x7C00002E);
		    handle_lwzx_insn(insn, pc);
		    break;
		case 110:
		    /* LWZUX */
		    assert((insn & 0xFC0007FF) == 0x7C00006E);
		    handle_lwzux_insn(insn, pc);
		    break;
		case 558:
		    /* LHZX */
		    assert((insn & 0xFC0007FF) == 0x7C00022E);
		    handle_lhzx_insn(insn, pc);
		    break;
		case 686:
		    /* LHAX */
		    assert((insn & 0xFC0007FF) == 0x7C0002AE);
		    handle_lhax_insn(insn, pc);
		    break;
		case 1070:
		    /* LFSX */
		    assert((insn & 0xFC0007FF) == 0x7C00042E);
		    handle_lfsx_insn(insn, pc);
		    break;
		case 1198:
		    /* LFDX */
		    assert((insn & 0xFC0007FF) == 0x7C0004AE);
		    handle_lfdx_insn(insn, pc);
		    break;
		case 174:
		    /* LBZX */
		    assert((insn & 0xFC0007FF) == 0x7C0000AE);
		    handle_lbzx_insn(insn, pc);
		    break;
		case 238:
		    /* LBZUX */
		    assert((insn & 0xFC0007FF) == 0x7C0000EE);
		    handle_lbzux_insn(insn, pc);
		    break;
		case 1964:
		    /* ICBI */
		    assert((insn & 0xFFE007FF) == 0x7C0007AC);
		    handle_icbi_insn(insn, pc);
		    break;
		case 1845:
		    /* EXTSH. */
		    assert((insn & 0xFC00FFFF) == 0x7C000735);
		    handle_extshd_insn(insn, pc);
		    break;
		case 1844:
		    /* EXTSH */
		    assert((insn & 0xFC00FFFF) == 0x7C000734);
		    handle_extsh_insn(insn, pc);
		    break;
		case 1908:
		    /* EXTSB */
		    assert((insn & 0xFC00FFFF) == 0x7C000774);
		    handle_extsb_insn(insn, pc);
		    break;
		case 569:
		    /* EQV. */
		    assert((insn & 0xFC0007FF) == 0x7C000239);
		    handle_eqvd_insn(insn, pc);
		    break;
		case 568:
		    /* EQV */
		    assert((insn & 0xFC0007FF) == 0x7C000238);
		    handle_eqv_insn(insn, pc);
		    break;
		case 918:
		    /* DIVWU */
		    assert((insn & 0xFC0007FF) == 0x7C000396);
		    handle_divwu_insn(insn, pc);
		    break;
		case 982:
		    /* DIVW */
		    assert((insn & 0xFC0007FF) == 0x7C0003D6);
		    handle_divw_insn(insn, pc);
		    break;
		case 2028:
		    /* DCBZ */
		    assert((insn & 0xFFE007FF) == 0x7C0007EC);
		    handle_dcbz_insn(insn, pc);
		    break;
		case 108:
		    /* DCBST */
		    assert((insn & 0xFFE007FF) == 0x7C00006C);
		    handle_dcbst_insn(insn, pc);
		    break;
		case 52:
		    /* CNTLZW */
		    assert((insn & 0xFC00FFFF) == 0x7C000034);
		    handle_cntlzw_insn(insn, pc);
		    break;
		case 0:
		    /* CMPW */
		    assert((insn & 0xFC6007FF) == 0x7C000000);
		    handle_cmpw_insn(insn, pc);
		    break;
		case 64:
		    /* CMPLW */
		    assert((insn & 0xFC6007FF) == 0x7C000040);
		    handle_cmplw_insn(insn, pc);
		    break;
		case 121:
		    /* ANDC. */
		    assert((insn & 0xFC0007FF) == 0x7C000079);
		    handle_andcd_insn(insn, pc);
		    break;
		case 120:
		    /* ANDC */
		    assert((insn & 0xFC0007FF) == 0x7C000078);
		    handle_andc_insn(insn, pc);
		    break;
		case 57:
		    /* AND. */
		    assert((insn & 0xFC0007FF) == 0x7C000039);
		    handle_andd_insn(insn, pc);
		    break;
		case 56:
		    /* AND */
		    assert((insn & 0xFC0007FF) == 0x7C000038);
		    handle_and_insn(insn, pc);
		    break;
		case 404:
		    /* ADDZE */
		    assert((insn & 0xFC00FFFF) == 0x7C000194);
		    handle_addze_insn(insn, pc);
		    break;
		case 277:
		    /* ADDE. */
		    assert((insn & 0xFC0007FF) == 0x7C000115);
		    handle_added_insn(insn, pc);
		    break;
		case 276:
		    /* ADDE */
		    assert((insn & 0xFC0007FF) == 0x7C000114);
		    handle_adde_insn(insn, pc);
		    break;
		case 21:
		    /* ADDC. */
		    assert((insn & 0xFC0007FF) == 0x7C000015);
		    handle_addcd_insn(insn, pc);
		    break;
		case 20:
		    /* ADDC */
		    assert((insn & 0xFC0007FF) == 0x7C000014);
		    handle_addc_insn(insn, pc);
		    break;
		case 533:
		    /* ADD. */
		    assert((insn & 0xFC0007FF) == 0x7C000215);
		    handle_addd_insn(insn, pc);
		    break;
		case 532:
		    /* ADD */
		    assert((insn & 0xFC0007FF) == 0x7C000214);
		    handle_add_insn(insn, pc);
		    break;
		default:
		    assert(0);
	    }
	    break;
	case 8:
	    /* SUBFIC */
	    assert((insn & 0xFC000000) == 0x20000000);
	    handle_subfic_insn(insn, pc);
	    break;
	case 37:
	    /* STWU */
	    assert((insn & 0xFC000000) == 0x94000000);
	    handle_stwu_insn(insn, pc);
	    break;
	case 36:
	    /* STW */
	    assert((insn & 0xFC000000) == 0x90000000);
	    handle_stw_insn(insn, pc);
	    break;
	case 45:
	    /* STHU */
	    assert((insn & 0xFC000000) == 0xB4000000);
	    handle_sthu_insn(insn, pc);
	    break;
	case 44:
	    /* STH */
	    assert((insn & 0xFC000000) == 0xB0000000);
	    handle_sth_insn(insn, pc);
	    break;
	case 52:
	    /* STFS */
	    assert((insn & 0xFC000000) == 0xD0000000);
	    handle_stfs_insn(insn, pc);
	    break;
	case 55:
	    /* STFDU */
	    assert((insn & 0xFC000000) == 0xDC000000);
	    handle_stfdu_insn(insn, pc);
	    break;
	case 54:
	    /* STFD */
	    assert((insn & 0xFC000000) == 0xD8000000);
	    handle_stfd_insn(insn, pc);
	    break;
	case 39:
	    /* STBU */
	    assert((insn & 0xFC000000) == 0x9C000000);
	    handle_stbu_insn(insn, pc);
	    break;
	case 38:
	    /* STB */
	    assert((insn & 0xFC000000) == 0x98000000);
	    handle_stb_insn(insn, pc);
	    break;
	case 17:
	    /* SC */
	    assert((insn & 0xFFFFFFFF) == 0x44000002);
	    handle_sc_insn(insn, pc);
	    break;
	case 21:
	    switch (((insn >> 0) & 0x1)) {
		case 1:
		    /* RLWINM. */
		    assert((insn & 0xFC000001) == 0x54000001);
		    handle_rlwinmd_insn(insn, pc);
		    break;
		case 0:
		    /* RLWINM */
		    assert((insn & 0xFC000001) == 0x54000000);
		    handle_rlwinm_insn(insn, pc);
		    break;
		default:
		    assert(0);
	    }
	    break;
	case 20:
	    /* RLWIMI */
	    assert((insn & 0xFC000001) == 0x50000000);
	    handle_rlwimi_insn(insn, pc);
	    break;
	case 25:
	    /* ORIS */
	    assert((insn & 0xFC000000) == 0x64000000);
	    handle_oris_insn(insn, pc);
	    break;
	case 24:
	    /* ORI */
	    assert((insn & 0xFC000000) == 0x60000000);
	    handle_ori_insn(insn, pc);
	    break;
	case 7:
	    /* MULLI */
	    assert((insn & 0xFC000000) == 0x1C000000);
	    handle_mulli_insn(insn, pc);
	    break;
	case 63:
	    switch (((insn >> 0) & 0x3F)) {
		case 12:
		    switch (((insn >> 6) & 0x3F)) {
			case 4:
			    /* MTFSFI */
			    assert((insn & 0xFC7F0FFF) == 0xFC00010C);
			    handle_mtfsfi_insn(insn, pc);
			    break;
			case 2:
			    /* MTFSB0 */
			    assert((insn & 0xFC1FFFFF) == 0xFC00008C);
			    handle_mtfsb0_insn(insn, pc);
			    break;
			default:
			    assert(0);
		    }
		    break;
		case 14:
		    switch (((insn >> 6) & 0x1F)) {
			case 22:
			    /* MTFSF */
			    assert((insn & 0xFFFF07FF) == 0xFDFE058E);
			    handle_mtfsf_insn(insn, pc);
			    break;
			case 18:
			    /* MFFS */
			    assert((insn & 0xFC1FFFFF) == 0xFC00048E);
			    handle_mffs_insn(insn, pc);
			    break;
			default:
			    assert(0);
		    }
		    break;
		case 40:
		    /* FSUB */
		    assert((insn & 0xFC0007FF) == 0xFC000028);
		    handle_fsub_insn(insn, pc);
		    break;
		case 24:
		    /* FRSP */
		    assert((insn & 0xFC1F07FF) == 0xFC000018);
		    handle_frsp_insn(insn, pc);
		    break;
		case 60:
		    /* FNMSUB */
		    assert((insn & 0xFC00003F) == 0xFC00003C);
		    handle_fnmsub_insn(insn, pc);
		    break;
		case 16:
		    switch (((insn >> 6) & 0x1F)) {
			case 1:
			    /* FNEG */
			    assert((insn & 0xFC1F07FF) == 0xFC000050);
			    handle_fneg_insn(insn, pc);
			    break;
			case 2:
			    /* FMR */
			    assert((insn & 0xFC1F07FF) == 0xFC000090);
			    handle_fmr_insn(insn, pc);
			    break;
			case 8:
			    /* FABS */
			    assert((insn & 0xFC1F07FF) == 0xFC000210);
			    handle_fabs_insn(insn, pc);
			    break;
			default:
			    assert(0);
		    }
		    break;
		case 50:
		    /* FMUL */
		    assert((insn & 0xFC00F83F) == 0xFC000032);
		    handle_fmul_insn(insn, pc);
		    break;
		case 56:
		    /* FMSUB */
		    assert((insn & 0xFC00003F) == 0xFC000038);
		    handle_fmsub_insn(insn, pc);
		    break;
		case 58:
		    /* FMADD */
		    assert((insn & 0xFC00003F) == 0xFC00003A);
		    handle_fmadd_insn(insn, pc);
		    break;
		case 36:
		    /* FDIV */
		    assert((insn & 0xFC0007FF) == 0xFC000024);
		    handle_fdiv_insn(insn, pc);
		    break;
		case 30:
		    /* FCTIWZ */
		    assert((insn & 0xFC1F07FF) == 0xFC00001E);
		    handle_fctiwz_insn(insn, pc);
		    break;
		case 0:
		    /* FCMPU */
		    assert((insn & 0xFC6007FF) == 0xFC000000);
		    handle_fcmpu_insn(insn, pc);
		    break;
		case 42:
		    /* FADD */
		    assert((insn & 0xFC0007FF) == 0xFC00002A);
		    handle_fadd_insn(insn, pc);
		    break;
		default:
		    assert(0);
	    }
	    break;
	case 19:
	    switch (((insn >> 0) & 0x7FF)) {
		case 0:
		    /* MCRF */
		    assert((insn & 0xFC63FFFF) == 0x4C000000);
		    handle_mcrf_insn(insn, pc);
		    break;
		case 300:
		    /* ISYNC */
		    assert((insn & 0xFFFFFFFF) == 0x4C00012C);
		    handle_isync_insn(insn, pc);
		    break;
		case 386:
		    /* CRXOR */
		    assert((insn & 0xFC0007FF) == 0x4C000182);
		    handle_crxor_insn(insn, pc);
		    break;
		case 898:
		    /* CROR */
		    assert((insn & 0xFC0007FF) == 0x4C000382);
		    handle_cror_insn(insn, pc);
		    break;
		case 66:
		    /* CRNOR */
		    assert((insn & 0xFC0007FF) == 0x4C000042);
		    handle_crnor_insn(insn, pc);
		    break;
		case 578:
		    /* CREQV */
		    assert((insn & 0xFC0007FF) == 0x4C000242);
		    handle_creqv_insn(insn, pc);
		    break;
		case 32:
		    switch (((insn >> 11) & 0x1F)) {
			case 0:
			    switch (((insn >> 21) & 0x1F)) {
				case 5:
				    /* BNELR+ */
				    assert((insn & 0xFFE0FFFF) == 0x4CA00020);
				    handle_bnelrp_insn(insn, pc);
				    break;
				case 4:
				    /* BNELR */
				    assert((insn & 0xFFE0FFFF) == 0x4C800020);
				    handle_bnelr_insn(insn, pc);
				    break;
				case 20:
				    /* BLR */
				    assert((insn & 0xFFE0FFFF) == 0x4E800020);
				    handle_blr_insn(insn, pc);
				    break;
				case 12:
				    /* BEQLR */
				    assert((insn & 0xFFE0FFFF) == 0x4D800020);
				    handle_beqlr_insn(insn, pc);
				    break;
				default:
				    assert(0);
			    }
			    break;
			default:
			    assert(0);
		    }
		    break;
		case 33:
		    /* BLRL */
		    assert((insn & 0xFFE0FFFF) == 0x4E800021);
		    handle_blrl_insn(insn, pc);
		    break;
		case 1056:
		    /* BCTR */
		    assert((insn & 0xFFE0FFFF) == 0x4E800420);
		    handle_bctr_insn(insn, pc);
		    break;
		default:
		    assert(0);
	    }
	    break;
	case 33:
	    /* LWZU */
	    assert((insn & 0xFC000000) == 0x84000000);
	    handle_lwzu_insn(insn, pc);
	    break;
	case 32:
	    /* LWZ */
	    assert((insn & 0xFC000000) == 0x80000000);
	    handle_lwz_insn(insn, pc);
	    break;
	case 41:
	    /* LHZU */
	    assert((insn & 0xFC000000) == 0xA4000000);
	    handle_lhzu_insn(insn, pc);
	    break;
	case 40:
	    /* LHZ */
	    assert((insn & 0xFC000000) == 0xA0000000);
	    handle_lhz_insn(insn, pc);
	    break;
	case 43:
	    /* LHAU */
	    assert((insn & 0xFC000000) == 0xAC000000);
	    handle_lhau_insn(insn, pc);
	    break;
	case 42:
	    /* LHA */
	    assert((insn & 0xFC000000) == 0xA8000000);
	    handle_lha_insn(insn, pc);
	    break;
	case 48:
	    /* LFS */
	    assert((insn & 0xFC000000) == 0xC0000000);
	    handle_lfs_insn(insn, pc);
	    break;
	case 50:
	    /* LFD */
	    assert((insn & 0xFC000000) == 0xC8000000);
	    handle_lfd_insn(insn, pc);
	    break;
	case 35:
	    /* LBZU */
	    assert((insn & 0xFC000000) == 0x8C000000);
	    handle_lbzu_insn(insn, pc);
	    break;
	case 34:
	    /* LBZ */
	    assert((insn & 0xFC000000) == 0x88000000);
	    handle_lbz_insn(insn, pc);
	    break;
	case 59:
	    switch (((insn >> 0) & 0x3F)) {
		case 40:
		    /* FSUBS */
		    assert((insn & 0xFC0007FF) == 0xEC000028);
		    handle_fsubs_insn(insn, pc);
		    break;
		case 50:
		    /* FMULS */
		    assert((insn & 0xFC00F83F) == 0xEC000032);
		    handle_fmuls_insn(insn, pc);
		    break;
		case 56:
		    /* FMSUBS */
		    assert((insn & 0xFC00003F) == 0xEC000038);
		    handle_fmsubs_insn(insn, pc);
		    break;
		case 58:
		    /* FMADDS */
		    assert((insn & 0xFC00003F) == 0xEC00003A);
		    handle_fmadds_insn(insn, pc);
		    break;
		case 36:
		    /* FDIVS */
		    assert((insn & 0xFC0007FF) == 0xEC000024);
		    handle_fdivs_insn(insn, pc);
		    break;
		case 42:
		    /* FADDS */
		    assert((insn & 0xFC0007FF) == 0xEC00002A);
		    handle_fadds_insn(insn, pc);
		    break;
		default:
		    assert(0);
	    }
	    break;
	case 11:
	    /* CMPWI */
	    assert((insn & 0xFC600000) == 0x2C000000);
	    handle_cmpwi_insn(insn, pc);
	    break;
	case 10:
	    /* CMPLWI */
	    assert((insn & 0xFC600000) == 0x28000000);
	    handle_cmplwi_insn(insn, pc);
	    break;
	case 16:
	    switch (((insn >> 21) & 0x1F)) {
		case 5:
		    /* BNE- */
		    assert((insn & 0xFFE00003) == 0x40A00000);
		    handle_bne__insn(insn, pc);
		    break;
		case 4:
		    /* BNE */
		    assert((insn & 0xFFE00003) == 0x40800000);
		    handle_bne_insn(insn, pc);
		    break;
		case 13:
		    /* BEQ+ */
		    assert((insn & 0xFFE00003) == 0x41A00000);
		    handle_beqp_insn(insn, pc);
		    break;
		case 12:
		    /* BEQ */
		    assert((insn & 0xFFE00003) == 0x41800000);
		    handle_beq_insn(insn, pc);
		    break;
		case 18:
		    /* BDZ */
		    assert((insn & 0xFFE00003) == 0x42400000);
		    handle_bdz_insn(insn, pc);
		    break;
		case 16:
		    /* BDNZ */
		    assert((insn & 0xFFE00003) == 0x42000000);
		    handle_bdnz_insn(insn, pc);
		    break;
		default:
		    assert(0);
	    }
	    break;
	case 18:
	    switch (((insn >> 0) & 0x3)) {
		case 1:
		    /* BL */
		    assert((insn & 0xFC000003) == 0x48000001);
		    handle_bl_insn(insn, pc);
		    break;
		case 0:
		    /* B */
		    assert((insn & 0xFC000003) == 0x48000000);
		    handle_b_insn(insn, pc);
		    break;
		default:
		    assert(0);
	    }
	    break;
	case 29:
	    /* ANDIS. */
	    assert((insn & 0xFC000000) == 0x74000000);
	    handle_andisd_insn(insn, pc);
	    break;
	case 28:
	    /* ANDI. */
	    assert((insn & 0xFC000000) == 0x70000000);
	    handle_andid_insn(insn, pc);
	    break;
	case 15:
	    /* ADDIS */
	    assert((insn & 0xFC000000) == 0x3C000000);
	    handle_addis_insn(insn, pc);
	    break;
	case 13:
	    /* ADDIC. */
	    assert((insn & 0xFC000000) == 0x34000000);
	    handle_addicd_insn(insn, pc);
	    break;
	case 12:
	    /* ADDIC */
	    assert((insn & 0xFC000000) == 0x30000000);
	    handle_addic_insn(insn, pc);
	    break;
	case 14:
	    /* ADDI */
	    assert((insn & 0xFC000000) == 0x38000000);
	    handle_addi_insn(insn, pc);
	    break;
	default:
	    assert(0);
    }
}
