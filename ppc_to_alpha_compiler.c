/*
 * ppc_to_alpha_compiler.c
 *
 * bintrans
 *
 * Copyright (C) 2001-2004 Mark Probst
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

#ifdef COLLECT_STATS
#define GEN_CRF0_BITS(n)           num_generated_crf0_bits += n
#define GEN_CRF0_BIT()             GEN_CRF0_BITS(1)
#define GEN_CRFX_BITS(n)           num_generated_crfx_bits += n
#define GEN_CRFX_BIT()             GEN_CRFX_BITS(1)
#else
#define GEN_CRF0_BIT()
#define GEN_CRFX_BIT()
#define GEN_CRFX_BITS(n)
#endif

#define compose_width_zapnot(ra,w,rs)   COMPOSE_ZAPNOT_IMM((ra),(1<<(w))-1,(rs))

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

#ifdef USE_MLGEN
#include "mlgen_macros.h"
#include "mlgen_ppc_to_alpha.h"
#endif

static void
gen_rc_code (reg_t reg)
{
    if (KILL_CRFB(0))
    {
	reg_t bit_reg;

	bit_reg = ref_ppc_crf0_w(0);

	emit(COMPOSE_CMPLT(reg, 31, bit_reg));

	unref_integer_reg(bit_reg);

	GEN_CRF0_BIT();
    }
    if (KILL_CRFB(1))
    {
	reg_t bit_reg;

	bit_reg = ref_ppc_crf0_w(1);

	emit(COMPOSE_CMPLT(31, reg, bit_reg));

	unref_integer_reg(bit_reg);

	GEN_CRF0_BIT();
    }
    if (KILL_CRFB(2))
    {
	reg_t bit_reg;

	bit_reg = ref_ppc_crf0_w(2);

	emit(COMPOSE_CMPEQ(reg, 31, bit_reg));

	unref_integer_reg(bit_reg);

	GEN_CRF0_BIT();
    }
    if (KILL_CRFB(3))
    {
	reg_t so_reg, bit_reg;

	so_reg = ref_ppc_xer_so_r();
	bit_reg = ref_ppc_crf0_w(3);

	emit(COMPOSE_MOV(so_reg, bit_reg));

	unref_integer_reg(bit_reg);
	unref_integer_reg(so_reg);

	GEN_CRF0_BIT();
    }
}

static void
gen_simple_xo9_rc_insn (word_32 insn, void (*emitter) (reg_t, reg_t, reg_t))
{
    reg_t rd_reg, ra_reg, rb_reg;

    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

    emitter(ra_reg, rb_reg, rd_reg);

    unref_integer_reg(ra_reg);
    unref_integer_reg(rb_reg);

    if (PPC_FIELD_RC)
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
handle_addo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_addod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_addc_insn (word_32 insn, word_32 pc)
{
    if (KILL_XER_CA)
    {
	reg_t ra_reg, rb_reg, ca_reg, tmp_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	ca_reg = ref_ppc_xer_ca_w();

	emit(COMPOSE_ZAPNOT_IMM(ra_reg, 15, ca_reg));

	unref_integer_reg(ra_reg);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(rb_reg, 15, tmp_reg));

	unref_integer_reg(rb_reg);

	emit(COMPOSE_ADDQ(ca_reg, tmp_reg, ca_reg));

	free_tmp_integer_reg(tmp_reg);

	if (KILL_GPR(PPC_FIELD_RD))
	{
	    reg_t rd_reg;

	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_ADDL(ca_reg, 31, rd_reg));

	    if (PPC_FIELD_RC)
		gen_rc_code(rd_reg);

	    unref_integer_reg(rd_reg);
	}

	emit(COMPOSE_SRL_IMM(ca_reg, 32, ca_reg));

	unref_integer_reg(ca_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

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
handle_addco_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_addcod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}


static void
handle_adde_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD) && KILL_XER_CA)
    {
	reg_t ra_reg, rb_reg, rd_reg, ca_reg, tmp_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(ra_reg, 15, tmp_reg));

	unref_integer_reg(ra_reg);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_ZAPNOT_IMM(rb_reg, 15, rd_reg));

	unref_integer_reg(rb_reg);

	emit(COMPOSE_ADDQ(tmp_reg, rd_reg, rd_reg));

	free_tmp_integer_reg(tmp_reg);
	ca_reg = ref_ppc_xer_ca_rw();

	emit(COMPOSE_ADDQ(rd_reg, ca_reg, rd_reg));
	emit(COMPOSE_SRL_IMM(rd_reg, 32, ca_reg));

	unref_integer_reg(ca_reg);

	emit(COMPOSE_ADDL(rd_reg, 31, rd_reg));

	if (PPC_FIELD_RC)
	    gen_rc_code(rd_reg);

	unref_integer_reg(rd_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg, ca_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, rd_reg));

	unref_integer_reg(ra_reg);
	unref_integer_reg(rb_reg);
	ca_reg = ref_ppc_xer_ca_r();

	emit(COMPOSE_ADDL(rd_reg, ca_reg, rd_reg));

	unref_integer_reg(ca_reg);

	if (PPC_FIELD_RC)
	    gen_rc_code(rd_reg);

	unref_integer_reg(rd_reg);
    }
    else if (KILL_XER_CA)
    {
	reg_t ra_reg, rb_reg, tmp1_reg, tmp2_reg, ca_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	tmp1_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(ra_reg, 15, tmp1_reg));

	unref_integer_reg(ra_reg);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
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

	bt_assert(!PPC_FIELD_RC);
    }
}

static void
handle_added_insn (word_32 insn, word_32 pc)
{
    handle_adde_insn(insn, pc);
}

static void
handle_addeo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}


static void
handle_addeod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_addi_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rd_reg;

	if (PPC_FIELD_RA == 0)
	{
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDA(rd_reg, PPC_FIELD_SIMM, 31));

	    unref_integer_reg(rd_reg);
	}
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    if ((PPC_FIELD_SIMM & 0xff00) == 0)
		emit(COMPOSE_ADDL_IMM(ra_reg, PPC_FIELD_SIMM, rd_reg));
	    else if ((PPC_FIELD_SIMM & 0xff00) == 0xff00 && (PPC_FIELD_SIMM & 0x00ff) != 0)
		emit(COMPOSE_SUBL_IMM(ra_reg, (~(PPC_FIELD_SIMM & 0xff) & 0xff) + 1, rd_reg));
	    else
	    {
		emit(COMPOSE_LDA(rd_reg, PPC_FIELD_SIMM, ra_reg));
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

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	tmp1_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(ra_reg, 15, tmp1_reg));

	unref_integer_reg(ra_reg);
	tmp2_reg = alloc_tmp_integer_reg();

	emit_load_integer_64(tmp2_reg, SEX32(PPC_FIELD_SIMM, 16));

	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_ADDQ(tmp1_reg, tmp2_reg, tmp1_reg));

	free_tmp_integer_reg(tmp2_reg);
	ca_reg = ref_ppc_xer_ca_w();

	emit(COMPOSE_SRL_IMM(tmp1_reg, 32, ca_reg));

	unref_integer_reg(ca_reg);

	if (KILL_GPR(PPC_FIELD_RD))
	{
	    emit(COMPOSE_ADDL(tmp1_reg, 31, rd_reg));

	    if (result_reg != 0)
		*result_reg = rd_reg;
	    else
		unref_integer_reg(rd_reg);
	}
	else
	    bt_assert(result_reg == 0);

	free_tmp_integer_reg(tmp1_reg);
    }
    else
    {
	reg_t rd_reg;

	bt_assert(KILL_GPR(PPC_FIELD_RD));

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	if ((PPC_FIELD_SIMM & 0xff00) == 0)
	    emit(COMPOSE_ADDL_IMM(ra_reg, PPC_FIELD_SIMM, rd_reg));
	else
	{
	    emit(COMPOSE_LDA(rd_reg, PPC_FIELD_SIMM, ra_reg));
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
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rd_reg;

	if (PPC_FIELD_RA == 0)
	{
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDAH(rd_reg, PPC_FIELD_SIMM, 31));

	    unref_integer_reg(rd_reg);
	}
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDAH(rd_reg, PPC_FIELD_SIMM, ra_reg));

	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_ADDL(rd_reg, 31, rd_reg));

	    unref_integer_reg(rd_reg);
	}
    }
}

static void
handle_addme_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_addmed_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_addmeo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_addmeod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_addze_insn (word_32 insn, word_32 pc)
{
    if (KILL_XER_CA)
    {
	reg_t ra_reg, rd_reg, ca_reg, tmp_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	ca_reg = ref_ppc_xer_ca_rw();
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_ADDL(ra_reg, ca_reg, rd_reg));

	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_CMPEQ(rd_reg, 31, tmp_reg));

	unref_integer_reg(rd_reg);

	emit(COMPOSE_AND(tmp_reg, ca_reg, ca_reg));

	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(ca_reg);
	unref_integer_reg(ra_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rd_reg, ca_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	ca_reg = ref_ppc_xer_ca_r();
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_ADDL(ra_reg, ca_reg, rd_reg));

	unref_integer_reg(rd_reg);
	unref_integer_reg(ca_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_addzed_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_addzeo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_addzeod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
gen_simple_xo1_rc_insn (word_32 insn, void (*emitter) (reg_t, reg_t, reg_t))
{
    reg_t ra_reg, rs_reg, rb_reg;

    rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
    ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

    emitter(rs_reg, rb_reg, ra_reg);

    unref_integer_reg(rb_reg);
    unref_integer_reg(rs_reg);

    if (PPC_FIELD_RC)
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
    if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	if ((PPC_FIELD_UIMM & 0xff00) == 0)
	    emit(COMPOSE_AND_IMM(rs_reg, PPC_FIELD_UIMM, ra_reg));
	else
	{
	    reg_t tmp_reg;

	    tmp_reg = alloc_tmp_integer_reg();

	    emit_load_integer_32(tmp_reg, PPC_FIELD_UIMM);
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
    if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t rs_reg, ra_reg, tmp_reg;

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, PPC_FIELD_UIMM << 16);
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
	bt_assert(pc + (SEX32(PPC_FIELD_LI, 24) << 2) == next_pc);
    else
    {
	word_32 target = pc + (SEX32(PPC_FIELD_LI, 24) << 2);

	emit_freeze_save();

	emit_start_direct_jump(1);

	emit_store_regs(EMU_INSN_EPILOGUE, target);

	emit_direct_jump(target);
    }
}

static void
gen_indirect_branch (void)
{
    bt_assert(!optimize_taken_jump);

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
	word_32 target = pc + (SEX32(PPC_FIELD_BD, 14) << 2);

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
	word_32 target = pc + (SEX32(PPC_FIELD_BD, 14) << 2);
	label_t alt_label = alloc_label();

	if (target == pc + 4)
	{
	    if (unref_cond_reg)
		unref_integer_reg(cond_reg);
	}
	else
	{
	    bt_assert(target != pc + 4);
	    bt_assert(next_pc == target || next_pc == pc + 4);

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
    if (PPC_FIELD_BI < 4)
    {
	reg_t crfb_reg;

	crfb_reg = ref_ppc_crf0_r(PPC_FIELD_BI);

	gen_cond_branch(insn, pc, eq ? emit_bne : emit_beq, eq ? emit_beq : emit_bne, crfb_reg, 1);
    }
    else
    {
	reg_t cr_reg, tmp_reg;

	cr_reg = ref_ppc_spr_r(SPR_CR);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_SRL_IMM(cr_reg, 31 - PPC_FIELD_BI, tmp_reg));

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

    if (PPC_FIELD_BI < 4)
	cond_reg = ref_ppc_crf0_r(PPC_FIELD_BI);
    else
    {
	reg_t cr_reg;

	cr_reg = ref_ppc_spr_r(SPR_CR);
	cond_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_SRL_IMM(cr_reg, 31 - PPC_FIELD_BI, cond_reg));

	unref_integer_reg(cr_reg);
    }

    if (NO_DYNAMO_TRACES || next_pc == NO_FOREIGN_ADDR)
    {
	emit_freeze_save();

	if (eq)
	    emit_branch(COMPOSE_BLBC(cond_reg, 0), end_label);
	else
	    emit_branch(COMPOSE_BLBS(cond_reg, 0), end_label);

	if (PPC_FIELD_BI < 4)
	    unref_integer_reg(cond_reg);
	else
	    free_tmp_integer_reg(cond_reg);

	lr_reg = ref_ppc_spr_r(SPR_LR);

	emit(COMPOSE_BIC_IMM(lr_reg, 3, JUMP_TARGET_REG));

	unref_integer_reg(lr_reg);

	bt_assert(!optimize_taken_jump);

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

	if (PPC_FIELD_BI < 4)
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

	    GEN_CRF0_BIT();
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

	    GEN_CRF0_BIT();
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

	    GEN_CRF0_BIT();
	}
	if (KILL_CRFB(3))
	{
	    reg_t bit_reg, so_reg;

	    so_reg = ref_ppc_xer_so_r();
	    bit_reg = ref_ppc_crf0_w(3);

	    emit(COMPOSE_MOV(so_reg, bit_reg));

	    unref_integer_reg(bit_reg);
	    unref_integer_reg(so_reg);

	    GEN_CRF0_BIT();
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

	GEN_CRFX_BITS(4);
    }
}

static void
gen_cmp_regs_insn (word_32 insn, int is_unsigned)
{
    reg_t ra_reg, rb_reg;

    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

    gen_cmp_insn(ra_reg, rb_reg, PPC_FIELD_CRFD, is_unsigned, 0, 0);

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

    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);

    if ((PPC_FIELD_UIMM & 0xff00) == 0)
	gen_cmp_insn(ra_reg, 0, PPC_FIELD_CRFD, 1, 1, PPC_FIELD_UIMM);
    else
    {
	reg_t imm_reg;

	imm_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(imm_reg, PPC_FIELD_UIMM);

	gen_cmp_insn(ra_reg, imm_reg, PPC_FIELD_CRFD, 1, 0, 0);

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

    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);

    if ((PPC_FIELD_SIMM & 0xff00) == 0)
	gen_cmp_insn(ra_reg, 0, PPC_FIELD_CRFD, 0, 1, PPC_FIELD_SIMM);
    else
    {
	reg_t imm_reg;

	imm_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_LDA(imm_reg, PPC_FIELD_SIMM, 31));

	gen_cmp_insn(ra_reg, imm_reg, PPC_FIELD_CRFD, 0, 0, 0);

	free_tmp_integer_reg(imm_reg);
    }

    unref_integer_reg(ra_reg);
}

static void
handle_cntlzw_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

	emit(COMPOSE_MOV(rs_reg, 16));

	unref_integer_reg(rs_reg);

	emit(COMPOSE_LDQ(0, LEADING_ZEROS_CONST * 4, CONSTANT_AREA_REG));
	emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, C_STUB_CONST * 4, CONSTANT_AREA_REG));
	emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));

	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	emit(COMPOSE_MOV(0, ra_reg));

	unref_integer_reg(ra_reg);
    }
}

static void
gen_crop_insn (word_32 insn, void (*emitter) (reg_t, reg_t, reg_t))
{
    reg_t bit_a_reg, bit_b_reg;
    int need_mask = 0;

    if (PPC_FIELD_CRBA < 4)
	bit_a_reg = ref_ppc_crf0_r(PPC_FIELD_CRBA);
    else
    {
	reg_t cr_reg;

	cr_reg = ref_ppc_spr_r(SPR_CR);
	bit_a_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_SRL_IMM(cr_reg, 31 - PPC_FIELD_CRBA, bit_a_reg));

	unref_integer_reg(cr_reg);

	need_mask = 1;
    }

    if (PPC_FIELD_CRBB < 4)
	bit_b_reg = ref_ppc_crf0_r(PPC_FIELD_CRBB);
    else
    {
	reg_t cr_reg;

	cr_reg = ref_ppc_spr_r(SPR_CR);
	bit_b_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_SRL_IMM(cr_reg, 31 - PPC_FIELD_CRBB, bit_b_reg));

	unref_integer_reg(cr_reg);

	need_mask = 1;
    }

    if (PPC_FIELD_CRBD < 4)
    {
	reg_t crfb_reg;

	crfb_reg = ref_ppc_crf0_w(PPC_FIELD_CRBD);

	emitter(bit_a_reg, bit_b_reg, crfb_reg);
	if (need_mask)
	    emit(COMPOSE_AND_IMM(crfb_reg, 1, crfb_reg));

	unref_integer_reg(crfb_reg);

	GEN_CRF0_BIT();
    }
    else
    {
	reg_t cr_reg, mask_reg;

	emitter(bit_a_reg, bit_b_reg, bit_a_reg);
	if (need_mask)
	    emit(COMPOSE_AND_IMM(bit_a_reg, 1, bit_a_reg));

	cr_reg = ref_ppc_spr_rw(SPR_CR);
	mask_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(mask_reg, 1 << (31 - PPC_FIELD_CRBD));
	emit(COMPOSE_SLL_IMM(bit_a_reg, 31 - PPC_FIELD_CRBD, bit_a_reg));
	emit(COMPOSE_BIC(cr_reg, mask_reg, cr_reg));

	free_tmp_integer_reg(mask_reg);

	emit(COMPOSE_BIS(cr_reg, bit_a_reg, cr_reg));

	unref_integer_reg(cr_reg);

	GEN_CRFX_BIT();
    }

    if (PPC_FIELD_CRBB < 4)
	unref_integer_reg(bit_b_reg);
    else
	free_tmp_integer_reg(bit_b_reg);

    if (PPC_FIELD_CRBA < 4)
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
handle_crand_insn (word_32 insn, word_32 pc)
{
    /* FIXME: implement */
    bt_assert(0);
}

static void
handle_crandc_insn (word_32 insn, word_32 pc)
{
    /* FIXME: implement */
    bt_assert(0);
}

static void
handle_crnand_insn (word_32 insn, word_32 pc)
{
    /* FIXME: implement */
    bt_assert(0);
}

static void
handle_crorc_insn (word_32 insn, word_32 pc)
{
    /* FIXME: implement */
    bt_assert(0);
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

    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

    if (PPC_FIELD_RA == 0)
    {
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_BIC_IMM(rb_reg, 31, addr_reg));

	unref_integer_reg(rb_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);
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

    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

    emit(COMPOSE_MOV(ra_reg, 16));
    emit(COMPOSE_MOV(rb_reg, 17));

    unref_integer_reg(rb_reg);
    unref_integer_reg(ra_reg);

    emit(COMPOSE_LDQ(0, const_index * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, C_STUB_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));

    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

    emit(COMPOSE_MOV(0, rd_reg));

    unref_integer_reg(rd_reg);
}

static void
handle_divw_insn (word_32 insn, word_32 pc)
{
    gen_div_insn(insn, DIV_SIGNED_32_CONST);
}

static void
handle_divwd_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_divwo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_divwod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_divwu_insn (word_32 insn, word_32 pc)
{
    gen_div_insn(insn, DIV_UNSIGNED_32_CONST);
}

static void
handle_divwud_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_divwuo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_divwuod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
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
    if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	emit(COMPOSE_SEXTB(rs_reg, ra_reg));

	unref_integer_reg(ra_reg);
	unref_integer_reg(rs_reg);
    }
}

static void
handle_extsh_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

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

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t frb_reg, frd_reg;

	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

	emit(COMPOSE_CPYS(31, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
    }
}

static void
handle_fadd_insn (word_32 insn, word_32 pc)
{
    announce("fadd");

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frd_reg;

	fra_reg = ref_ppc_fpr_r(PPC_FIELD_FRA);
	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

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

    fra_reg = ref_ppc_fpr_r(PPC_FIELD_FRA);
    frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);

    if (PPC_FIELD_CRFD == 0)
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

	GEN_CRF0_BITS(4);
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

	emit_load_integer_32(tmp_reg, 15 << ((7 - PPC_FIELD_CRFD) * 4));

	cr_reg = ref_ppc_spr_rw(SPR_CR);

	emit(COMPOSE_BIC(cr_reg, tmp_reg, cr_reg));

	free_tmp_integer_reg(tmp_reg);

	emit(COMPOSE_SLL_IMM(crf_reg, (7 - PPC_FIELD_CRFD) * 4, crf_reg));
	emit(COMPOSE_BIS(cr_reg, crf_reg, cr_reg));

	unref_integer_reg(cr_reg);
	free_tmp_integer_reg(crf_reg);

	GEN_CRFX_BITS(4);
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

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t frb_reg, frd_reg, tmp_reg;

	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

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

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frd_reg;

	fra_reg = ref_ppc_fpr_r(PPC_FIELD_FRA);
	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

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

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frc_reg, frd_reg, tmp_reg;

	fra_reg = ref_ppc_fpr_r(PPC_FIELD_FRA);
	frc_reg = ref_ppc_fpr_r(PPC_FIELD_FRC);
	tmp_reg = alloc_tmp_float_reg();

	emit(COMPOSE_MULT(fra_reg, frc_reg, tmp_reg));

	unref_float_reg(frc_reg);
	unref_float_reg(fra_reg);

	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

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

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t frb_reg, frd_reg;

	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

	emit(COMPOSE_CPYS(frb_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
    }
}

static void
handle_fmsub_insn (word_32 insn, word_32 pc)
{
    announce("fmsub");

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frc_reg, frd_reg, tmp_reg;

	fra_reg = ref_ppc_fpr_r(PPC_FIELD_FRA);
	frc_reg = ref_ppc_fpr_r(PPC_FIELD_FRC);
	tmp_reg = alloc_tmp_float_reg();

	emit(COMPOSE_MULT(fra_reg, frc_reg, tmp_reg));

	unref_float_reg(frc_reg);
	unref_float_reg(fra_reg);

	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

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

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t fra_reg, frc_reg, frd_reg;

	fra_reg = ref_ppc_fpr_r(PPC_FIELD_FRA);
	frc_reg = ref_ppc_fpr_r(PPC_FIELD_FRC);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

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

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t frb_reg, frd_reg;

	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

	emit(COMPOSE_CPYSN(frb_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
    }
}

static void
handle_fnmsub_insn (word_32 insn, word_32 pc)
{
    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frc_reg, frd_reg, tmp_reg;

	fra_reg = ref_ppc_fpr_r(PPC_FIELD_FRA);
	frc_reg = ref_ppc_fpr_r(PPC_FIELD_FRC);
	tmp_reg = alloc_tmp_float_reg();

	emit(COMPOSE_MULT(fra_reg, frc_reg, tmp_reg));

	unref_float_reg(frc_reg);
	unref_float_reg(fra_reg);

	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

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

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t frb_reg, frd_reg;

	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

	emit(COMPOSE_CPYS(frb_reg, frb_reg, frd_reg));

	unref_float_reg(frd_reg);
	unref_float_reg(frb_reg);
    }
}

static void
handle_fsub_insn (word_32 insn, word_32 pc)
{
    announce("fsub");

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t fra_reg, frb_reg, frd_reg;

	fra_reg = ref_ppc_fpr_r(PPC_FIELD_FRA);
	frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

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
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rd_reg;

	if (PPC_FIELD_RA == 0)
	{
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDBU(rd_reg, PPC_FIELD_D ^ 3, 31));
	}
	else
	{
	    reg_t ra_reg, addr_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_LDA(addr_reg, PPC_FIELD_D, ra_reg));

	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));

	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDBU(rd_reg, 0, addr_reg));

	    free_tmp_integer_reg(addr_reg);
	}

	unref_integer_reg(rd_reg);
    }
}

static void
handle_lbzu_insn (word_32 insn, word_32 pc)
{
    bt_assert(PPC_FIELD_RD != PPC_FIELD_RA);

    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rd_reg, addr_reg;

	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, PPC_FIELD_D, ra_reg));

	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_XOR_IMM(ra_reg, 3, addr_reg));

	unref_integer_reg(ra_reg);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_LDBU(rd_reg, 0, addr_reg));

	unref_integer_reg(rd_reg);
	free_tmp_integer_reg(addr_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, PPC_FIELD_D ^ 3, ra_reg));

	unref_integer_reg(ra_reg);
    }
}

static void
handle_lbzux_insn (word_32 insn, word_32 pc)
{
    bt_assert(PPC_FIELD_RD != PPC_FIELD_RA && PPC_FIELD_RD != PPC_FIELD_RB);

    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg, addr_reg;

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, ra_reg));

	unref_integer_reg(rb_reg);

	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_XOR_IMM(ra_reg, 3, addr_reg));

	unref_integer_reg(ra_reg);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_LDBU(rd_reg, 0, addr_reg));

	unref_integer_reg(rd_reg);
	free_tmp_integer_reg(addr_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t ra_reg, rb_reg;

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, ra_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_lbzx_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rb_reg, rd_reg, addr_reg;

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

	if (PPC_FIELD_RA == 0)
	{
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_XOR_IMM(rb_reg, 3, addr_reg));

	    unref_integer_reg(rb_reg);
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDBU(rd_reg, 0, addr_reg));

	    unref_integer_reg(rd_reg);
	    free_tmp_integer_reg(addr_reg);
	}
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));

	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

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

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t addr_reg, bits_reg, frd_reg;

	addr_reg = alloc_tmp_integer_reg();

	if (PPC_FIELD_RA == 0)
	    emit(COMPOSE_LDA(addr_reg, PPC_FIELD_D, 31));
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);

	    emit(COMPOSE_LDA(addr_reg, PPC_FIELD_D, ra_reg));

	    unref_integer_reg(ra_reg);
	}

	bits_reg = alloc_tmp_integer_reg();

	emit_load_mem_64(bits_reg, addr_reg);

	free_tmp_integer_reg(addr_reg);

	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

	gen_bits_to_float(bits_reg, frd_reg, 0);

	unref_float_reg(frd_reg);
	free_tmp_integer_reg(bits_reg);
    }
}

static void
handle_lfdx_insn (word_32 insn, word_32 pc)
{
    announce("lfdx");

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t addr_reg, bits_reg, frd_reg;

	if (PPC_FIELD_RA == 0)
	    addr_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	else
	{
	    reg_t ra_reg, rb_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_ADDQ(ra_reg, rb_reg, addr_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);
	}

	bits_reg = alloc_tmp_integer_reg();

	emit_load_mem_64(bits_reg, addr_reg);

	if (PPC_FIELD_RA == 0)
	    unref_integer_reg(addr_reg);
	else
	    free_tmp_integer_reg(addr_reg);

	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

	gen_bits_to_float(bits_reg, frd_reg, 0);

	unref_float_reg(frd_reg);
	free_tmp_integer_reg(bits_reg);
    }
}

static void
handle_lfs_insn (word_32 insn, word_32 pc)
{
    announce("lfs");

    if (KILL_FPR(PPC_FIELD_FRD))
    {
	reg_t frd_reg;

	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

	if (PPC_FIELD_RA == 0)
	    emit(COMPOSE_LDS(frd_reg, PPC_FIELD_D, 31));
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);

	    emit(COMPOSE_LDS(frd_reg, PPC_FIELD_D, ra_reg));

	    unref_integer_reg(ra_reg);
	}

	unref_float_reg(frd_reg);
    }
}

static void
handle_lfsx_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	if (PPC_FIELD_RA == 0)
	{
	    reg_t rb_reg, frd_reg;

	    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	    frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

	    emit(COMPOSE_LDS(frd_reg, 0, rb_reg));

	    unref_integer_reg(frd_reg);
	    unref_float_reg(rb_reg);
	}
	else
	{
	    reg_t ra_reg, rb_reg, addr_reg, frd_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);

	    frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

	    emit(COMPOSE_LDS(frd_reg, 0, addr_reg));

	    unref_float_reg(frd_reg);
	    free_tmp_integer_reg(addr_reg);
	}
    }
}

static void
gen_load_half_imm_insn (word_32 insn, int sex)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rd_reg;

	if (PPC_FIELD_RA == 0)
	{
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDWU(rd_reg, PPC_FIELD_D ^ 2, 31));
	}
	else
	{
	    reg_t ra_reg, addr_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_LDA(addr_reg, PPC_FIELD_D, ra_reg));

	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));

	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

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
    bt_assert(PPC_FIELD_RD != PPC_FIELD_RA);

    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rd_reg, addr_reg;

	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, PPC_FIELD_D, ra_reg));

	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_XOR_IMM(ra_reg, 2, addr_reg));

	unref_integer_reg(ra_reg);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_LDWU(rd_reg, 0, addr_reg));

	free_tmp_integer_reg(addr_reg);

	if (sex)
	    emit(COMPOSE_SEXTW(rd_reg, rd_reg));

	unref_integer_reg(rd_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, PPC_FIELD_D ^ 2, ra_reg));

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
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rb_reg, rd_reg, addr_reg;

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

	if (PPC_FIELD_RA == 0)
	{
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_XOR_IMM(rb_reg, 2, addr_reg));

	    unref_integer_reg(rb_reg);
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDWU(rd_reg, 0, addr_reg));

	    free_tmp_integer_reg(addr_reg);

	    if (sex)
		emit(COMPOSE_SEXTW(rd_reg, rd_reg));

	    unref_integer_reg(rd_reg);
	}
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));

	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

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
handle_lhbrx_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
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
handle_lwbrx_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_lwz_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rd_reg;

	if (PPC_FIELD_RA == 0)
	{
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDL(rd_reg, PPC_FIELD_D, 31));

	    unref_integer_reg(rd_reg);
	}
	else
	{
	    reg_t ra_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDL(rd_reg, PPC_FIELD_D, ra_reg));

	    unref_integer_reg(rd_reg);
	    unref_integer_reg(ra_reg);
	}
    }
}

static void
handle_lwzu_insn (word_32 insn, word_32 pc)
{
    bt_assert(PPC_FIELD_RD != PPC_FIELD_RA);

    if (KILL_GPR(PPC_FIELD_RD) || KILL_GPR(PPC_FIELD_RA))
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, PPC_FIELD_D, ra_reg));

	if (KILL_GPR(PPC_FIELD_RD))
	{
	    reg_t rd_reg;

	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDL(rd_reg, 0, ra_reg));

	    unref_integer_reg(rd_reg);
	}

	unref_integer_reg(ra_reg);
    }
}

static void
handle_lwzux_insn (word_32 insn, word_32 pc)
{
    bt_assert(PPC_FIELD_RD != PPC_FIELD_RA && PPC_FIELD_RD != PPC_FIELD_RB);

    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg;

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, ra_reg));

	unref_integer_reg(rb_reg);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_LDL(rd_reg, 0, ra_reg));

	unref_integer_reg(ra_reg);
	unref_integer_reg(rd_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t ra_reg, rb_reg;

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

	emit(COMPOSE_ADDL(ra_reg, rb_reg, ra_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_lwzx_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	if (PPC_FIELD_RA == 0)
	{
	    reg_t rb_reg, rd_reg;

	    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDL(rd_reg, 0, rb_reg));

	    unref_integer_reg(rd_reg);
	    unref_integer_reg(rb_reg);
	}
	else
	{
	    reg_t ra_reg, rb_reg, addr_reg, rd_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	    addr_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);

	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_LDL(rd_reg, 0, addr_reg));

	    unref_integer_reg(rd_reg);
	    free_tmp_integer_reg(addr_reg);
	}
    }
}

static void
handle_mcrf_insn (word_32 insn, word_32 pc)
{
    if (PPC_FIELD_CRFD != PPC_FIELD_CRFS)
    {
	if (PPC_FIELD_CRFD == 0)
	{
	    reg_t cr_reg;

	    cr_reg = ref_ppc_spr_r(SPR_CR);

	    if (KILL_CRFB(0))
	    {
		reg_t bit_reg;

		bit_reg = ref_ppc_crf0_w(0);

		emit(COMPOSE_SRL_IMM(cr_reg, 31 - (PPC_FIELD_CRFS * 4 + 0), bit_reg));
		emit(COMPOSE_AND_IMM(bit_reg, 1, bit_reg));

		unref_integer_reg(bit_reg);

		GEN_CRF0_BIT();
	    }
	    if (KILL_CRFB(1))
	    {
		reg_t bit_reg;

		bit_reg = ref_ppc_crf0_w(1);

		emit(COMPOSE_SRL_IMM(cr_reg, 31 - (PPC_FIELD_CRFS * 4 + 1), bit_reg));
		emit(COMPOSE_AND_IMM(bit_reg, 1, bit_reg));

		unref_integer_reg(bit_reg);

		GEN_CRF0_BIT();
	    }
	    if (KILL_CRFB(2))
	    {
		reg_t bit_reg;

		bit_reg = ref_ppc_crf0_w(2);

		emit(COMPOSE_SRL_IMM(cr_reg, 31 - (PPC_FIELD_CRFS * 4 + 2), bit_reg));
		emit(COMPOSE_AND_IMM(bit_reg, 1, bit_reg));

		unref_integer_reg(bit_reg);

		GEN_CRF0_BIT();
	    }
	    if (KILL_CRFB(3))
	    {
		reg_t bit_reg;

		bit_reg = ref_ppc_crf0_w(3);

		emit(COMPOSE_SRL_IMM(cr_reg, 31 - (PPC_FIELD_CRFS * 4 + 3), bit_reg));
		emit(COMPOSE_AND_IMM(bit_reg, 1, bit_reg));

		unref_integer_reg(bit_reg);

		GEN_CRF0_BIT();
	    }

	    unref_integer_reg(cr_reg);
	}
	else
	{
	    if (PPC_FIELD_CRFS == 0)
	    {
		reg_t field_reg, mask_reg, cr_reg;
		int have_set = 0;

		field_reg = alloc_tmp_integer_reg();

		if (KILL_CRFB(4 * PPC_FIELD_CRFD + 0))
		{
		    reg_t bit_reg;

		    bit_reg = ref_ppc_crf0_r(0);

		    emit(COMPOSE_SLL_IMM(bit_reg, 3, field_reg));

		    unref_integer_reg(bit_reg);

		    have_set = 1;

		    GEN_CRFX_BIT();
		}
		if (KILL_CRFB(4 * PPC_FIELD_CRFD + 1))
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

		    GEN_CRFX_BIT();
		}
		if (KILL_CRFB(4 * PPC_FIELD_CRFD + 2))
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

		    GEN_CRFX_BIT();
		}
		if (KILL_CRFB(4 * PPC_FIELD_CRFD + 3))
		{
		    reg_t bit_reg;

		    bit_reg = ref_ppc_crf0_r(3);

		    if (have_set)
			emit(COMPOSE_BIS(field_reg, bit_reg, field_reg));
		    else
			emit(COMPOSE_MOV(bit_reg, field_reg));

		    unref_integer_reg(bit_reg);

		    GEN_CRFX_BIT();
		}

		mask_reg = alloc_tmp_integer_reg();

		emit_load_integer_32(mask_reg, 15 << ((7 - PPC_FIELD_CRFD) * 4));

		cr_reg = ref_ppc_spr_rw(SPR_CR);

		emit(COMPOSE_BIC(cr_reg, mask_reg, cr_reg));

		free_tmp_integer_reg(mask_reg);

		emit(COMPOSE_SLL_IMM(field_reg, ((7 - PPC_FIELD_CRFD) * 4), field_reg));
		emit(COMPOSE_BIS(cr_reg, field_reg, cr_reg));

		unref_integer_reg(cr_reg);
		free_tmp_integer_reg(field_reg);
	    }
	    else
	    {
		reg_t cr_reg, mask_reg, field_reg;

		cr_reg = ref_ppc_spr_rw(SPR_CR);
		field_reg = alloc_tmp_integer_reg();

		emit(COMPOSE_SRL_IMM(cr_reg, ((7 - PPC_FIELD_CRFS) * 4), field_reg));
		emit(COMPOSE_AND_IMM(field_reg, 15, field_reg));
		emit(COMPOSE_SLL_IMM(field_reg, ((7 - PPC_FIELD_CRFD) * 4), field_reg));

		mask_reg = alloc_tmp_integer_reg();

		emit_load_integer_32(mask_reg, 15 << ((7 - PPC_FIELD_CRFD) * 4));

		emit(COMPOSE_BIC(cr_reg, mask_reg, cr_reg));

		free_tmp_integer_reg(mask_reg);

		emit(COMPOSE_BIS(cr_reg, field_reg, cr_reg));

		free_tmp_integer_reg(field_reg);
		unref_integer_reg(cr_reg);

		GEN_CRFX_BITS(4);
	    }
	}
    }
}

static void
handle_mcrxr_insn (word_32 insn, word_32 pc)
{
    /* FIXME: implement */
    bt_assert(0);
}

static void
handle_mfcr_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rd_reg, cr_reg, tmp_reg;

	cr_reg = ref_ppc_spr_r(SPR_CR);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

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
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rd_reg, ctr_reg;

	ctr_reg = ref_ppc_spr_r(SPR_CTR);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_MOV(ctr_reg, rd_reg));

	unref_integer_reg(rd_reg);
	unref_integer_reg(ctr_reg);
    }
}

static void
handle_mffs_insn (word_32 insn, word_32 pc)
{
    if (KILL_FPR(PPC_FIELD_RD))
    {
	reg_t fpscr_reg, frd_reg;

	fpscr_reg = ref_ppc_spr_r(SPR_FPSCR);
	frd_reg = ref_ppc_fpr_w(PPC_FIELD_FRD);

	gen_bits_to_float(fpscr_reg, frd_reg, 1);

	unref_float_reg(frd_reg);
	unref_integer_reg(fpscr_reg);
    }
}

static void
handle_mflr_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rd_reg, lr_reg;

	lr_reg = ref_ppc_spr_r(SPR_LR);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_MOV(lr_reg, rd_reg));

	unref_integer_reg(rd_reg);
	unref_integer_reg(lr_reg);
    }
}

static void
handle_mfxer_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_mtcrf_insn (word_32 insn, word_32 pc)
{
    reg_t mask_reg, rs_reg, cr_reg;

    rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

    if (PPC_FIELD_CRM & 0x80)
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

		GEN_CRF0_BIT();
	    }
    }

    /* FIXME: we don't have to do all the following if PPC_FIELD_CRM ==
       0x80 */

    mask_reg = alloc_tmp_integer_reg();

    emit_load_integer_32(mask_reg, maskmask(4, 8, PPC_FIELD_CRM & 0x7f));

    cr_reg = ref_ppc_spr_rw(SPR_CR);

    emit(COMPOSE_BIC(cr_reg, mask_reg, cr_reg));
    emit(COMPOSE_AND(rs_reg, mask_reg, mask_reg));

    unref_integer_reg(rs_reg);

    emit(COMPOSE_BIS(cr_reg, mask_reg, cr_reg));

    free_tmp_integer_reg(mask_reg);
    unref_integer_reg(cr_reg);

    GEN_CRFX_BITS(4);
}

static void
handle_mtctr_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rs_reg, ctr_reg;

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RD);
	ctr_reg = ref_ppc_spr_w(SPR_CTR);

	emit(COMPOSE_MOV(rs_reg, ctr_reg));

	unref_integer_reg(ctr_reg);
	unref_integer_reg(rs_reg);
    }
}

static void
handle_mtfsb0_insn (word_32 insn, word_32 pc)
{
    if (PPC_FIELD_CRBD < 4)
    {
	if (KILL_CRFB(PPC_FIELD_CRBD))
	{
	    reg_t bit_reg;

	    bit_reg = ref_ppc_crf0_w(PPC_FIELD_CRBD);

	    emit(COMPOSE_BIS(31, 31, bit_reg));

	    unref_integer_reg(bit_reg);

	    GEN_CRF0_BIT();
	}
    }
    else
    {
	reg_t cr_reg, tmp_reg;

	cr_reg = ref_ppc_spr_rw(SPR_CR);
	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, 1 << (31 - PPC_FIELD_CRBD));
	emit(COMPOSE_BIC(cr_reg, tmp_reg, cr_reg));

	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(cr_reg);

	GEN_CRFX_BIT();
    }
}

static void
handle_mtfsf_insn (word_32 insn, word_32 pc)
{
    reg_t fpscr_reg, frb_reg;

    frb_reg = ref_ppc_fpr_r(PPC_FIELD_FRB);
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

    emit_load_integer_32(tmp_reg, 15 << (7 - PPC_FIELD_CRFD));
    emit(COMPOSE_BIC(fpscr_reg, tmp_reg, fpscr_reg));
    emit_load_integer_32(tmp_reg, PPC_FIELD_IMM << (7 - PPC_FIELD_CRFD));
    emit(COMPOSE_BIS(fpscr_reg, tmp_reg, fpscr_reg));

    free_tmp_integer_reg(tmp_reg);
    unref_integer_reg(fpscr_reg);
}

static void
handle_mtlr_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rs_reg, lr_reg;

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RD);
	lr_reg = ref_ppc_spr_w(SPR_LR);

	emit(COMPOSE_MOV(rs_reg, lr_reg));

	unref_integer_reg(lr_reg);
	unref_integer_reg(rs_reg);
    }
}

static void
handle_mtxer_insn (word_32 insn, word_32 pc)
{
    /* FIXME: implement */
    bt_assert(0);
}

static void
handle_mulhw_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

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
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg, zapped_ra_reg, zapped_rb_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	zapped_ra_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(ra_reg, 15, zapped_ra_reg));

	unref_integer_reg(ra_reg);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	zapped_rb_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ZAPNOT_IMM(rb_reg, 15, zapped_rb_reg));

	unref_integer_reg(rb_reg);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

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
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rd_reg, ra_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	if ((PPC_FIELD_SIMM & 0xff00) == 0)
	    emit(COMPOSE_MULL_IMM(ra_reg, PPC_FIELD_SIMM, rd_reg));
	else
	{
	    reg_t tmp_reg;

	    tmp_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_LDA(tmp_reg, PPC_FIELD_SIMM, 31));

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
handle_mullwo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_mullwod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
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

    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

    emit(COMPOSE_NEGL(ra_reg, rd_reg));

    unref_integer_reg(ra_reg);

    if (PPC_FIELD_RC)
	gen_rc_code(rd_reg);

    unref_integer_reg(rd_reg);
}

static void
handle_negd_insn (word_32 insn, word_32 pc)
{
    handle_neg_insn(insn, pc);
}

static void
handle_nego_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_negod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
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
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	if ((PPC_FIELD_UIMM & 0xff00) == 0)
	    emit(COMPOSE_BIS_IMM(rs_reg, PPC_FIELD_UIMM, ra_reg));
	else
	{
	    reg_t tmp_reg;

	    tmp_reg = alloc_tmp_integer_reg();

	    emit_load_integer_32(tmp_reg, PPC_FIELD_UIMM);
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
    if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t ra_reg, rs_reg, tmp_reg;

	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_LDAH(tmp_reg, PPC_FIELD_UIMM, 31));

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

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
    if (KILL_GPR(PPC_FIELD_RA))
    {
	word_32 mask = mask_32(31 - PPC_FIELD_ME, 31 - PPC_FIELD_MB);
	reg_t ra_reg, rs_reg, mask_reg, tmp_reg;

	tmp_reg = alloc_tmp_integer_reg();

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

	if (PPC_FIELD_SH == 0)
	{
	    mask_reg = alloc_tmp_integer_reg();
	    emit_load_integer_32(mask_reg, mask);

	    emit(COMPOSE_AND(rs_reg, mask_reg, tmp_reg));
	}
	else
	{
	    gen_rotl(rs_reg, tmp_reg, PPC_FIELD_SH);

	    mask_reg = alloc_tmp_integer_reg();
	    emit_load_integer_32(mask_reg, mask);

	    emit(COMPOSE_AND(tmp_reg, mask_reg, tmp_reg));
	}

	unref_integer_reg(rs_reg);

	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

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
#ifndef USE_MLGEN
    if (KILL_GPR(PPC_FIELD_RA))
    {
	word_32 mask = mask_32(31 - PPC_FIELD_ME, 31 - PPC_FIELD_MB);
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	if (PPC_FIELD_MB == 24 && PPC_FIELD_ME == 31 && (PPC_FIELD_SH & 7) == 0)
	{
	    emit(COMPOSE_EXTBL_IMM(rs_reg, PPC_FIELD_SH == 0 ? 0 : 4 - (PPC_FIELD_SH >> 3), ra_reg));

	    unref_integer_reg(rs_reg);
	}
	else if (PPC_FIELD_MB == 16 && PPC_FIELD_ME == 31 && (PPC_FIELD_SH & 7) == 0 && PPC_FIELD_SH != 8)
	{
	    emit(COMPOSE_EXTWL_IMM(rs_reg, PPC_FIELD_SH == 0 ? 0 : 4 - (PPC_FIELD_SH >> 3), ra_reg));

	    unref_integer_reg(rs_reg);
	}
	else if (PPC_FIELD_SH == 0)
	{
	    gen_and_with_const(rs_reg, ra_reg, mask);

	    unref_integer_reg(rs_reg);
	}
	else if (PPC_FIELD_MB == 0 && PPC_FIELD_ME + PPC_FIELD_SH == 31)
	{
	    emit(COMPOSE_SLL_IMM(rs_reg, PPC_FIELD_SH, ra_reg));

	    unref_integer_reg(rs_reg);

	    emit(COMPOSE_ADDL(ra_reg, 31, ra_reg));
	}
	else if (PPC_FIELD_ME == 31 && (32 - PPC_FIELD_MB) <= PPC_FIELD_SH)
	{
	    bt_assert(PPC_FIELD_MB != 0);

	    emit(COMPOSE_SLL_IMM(rs_reg, 32 + PPC_FIELD_SH - (32 - PPC_FIELD_MB), ra_reg));

	    unref_integer_reg(rs_reg);

	    emit(COMPOSE_SRL_IMM(ra_reg, 32 + PPC_FIELD_MB, ra_reg));
	}
	else
	{
	    gen_rotl(rs_reg, ra_reg, PPC_FIELD_SH);

	    unref_integer_reg(rs_reg);

	    gen_and_with_const(ra_reg, ra_reg, mask);
	}

	if (PPC_FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
#else
    reg_t rs_reg, ra_reg;

    rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
    ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

    mlgen_rlwinm(PPC_FIELD_SH, PPC_FIELD_MB, PPC_FIELD_ME, ra_reg, rs_reg);

    unref_integer_reg(rs_reg);

    if (PPC_FIELD_RC)
	gen_rc_code(ra_reg);

    unref_integer_reg(ra_reg);
#endif
}

static void
handle_rlwinmd_insn (word_32 insn, word_32 pc)
{
    handle_rlwinm_insn(insn, pc);
}

static void
handle_rlwnm_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RA))
    {
	word_32 mask = mask_32(31 - PPC_FIELD_ME, 31 - PPC_FIELD_MB);
	reg_t ra_reg, rs_reg, rb_reg, tmp;

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	tmp = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 0x1f, tmp));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	emit(COMPOSE_ZAPNOT_IMM(rs_reg, 15, ra_reg));

	unref_integer_reg(rs_reg);

	emit(COMPOSE_SLL(ra_reg, tmp, ra_reg));
	emit(COMPOSE_SRL_IMM(ra_reg, 32, tmp));
	emit(COMPOSE_BIS(ra_reg, tmp, ra_reg));

	free_tmp_integer_reg(tmp);

	if (mask != 0xffffffff)
	    gen_and_with_const(ra_reg, ra_reg, mask);

	emit(COMPOSE_ADDL(ra_reg, 31, ra_reg));

	if (PPC_FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
}

static void
handle_rlwnmd_insn (word_32 insn, word_32 pc)
{
    handle_rlwnm_insn(insn, pc);
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
    if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t rb_reg, rs_reg, ra_reg, tmp_reg;

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 63, tmp_reg));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	emit(COMPOSE_SLL(rs_reg, tmp_reg, ra_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(tmp_reg);

	emit(COMPOSE_ADDL(ra_reg, 31, ra_reg));

	if (PPC_FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
}

static void
handle_slwd_insn (word_32 insn, word_32 pc)
{
    handle_slw_insn(insn, pc);
}

static void
handle_sraw_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RA) && KILL_XER_CA)
    {
	reg_t rs_reg, ra_reg, rb_reg, ca_reg, mask_reg, amount_reg, tmp_reg;
	label_t no_ca_set = alloc_label();

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	amount_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 63, amount_reg));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
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

	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	emit(COMPOSE_SRA(rs_reg, amount_reg, ra_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(amount_reg);

	if (PPC_FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t rb_reg, rs_reg, ra_reg, tmp_reg;

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 63, tmp_reg));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	emit(COMPOSE_SRA(rs_reg, tmp_reg, ra_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(tmp_reg);

	if (PPC_FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
    else if (KILL_XER_CA)
    {
	reg_t rs_reg, rb_reg, ca_reg, mask_reg, amount_reg, tmp_reg;
	label_t end = alloc_label();

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	amount_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 63, amount_reg));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
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
    if (KILL_GPR(PPC_FIELD_RA) && KILL_XER_CA)
    {
	reg_t rs_reg, ra_reg, ca_reg, mask_reg, tmp_reg;
	label_t no_ca_set = alloc_label();

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ca_reg = ref_ppc_xer_ca_w();

	tmp_reg = alloc_tmp_integer_reg();
	if (PPC_FIELD_SH > 8)
	    mask_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_MOV(31, ca_reg));
	emit_branch(COMPOSE_BGE(rs_reg, 0), no_ca_set);

	/* push_alloc(); */

	if (PPC_FIELD_SH <= 8)
	    emit(COMPOSE_AND_IMM(rs_reg, (1 << PPC_FIELD_SH) - 1, tmp_reg));
	else
	{
	    emit_load_integer_32(mask_reg, (1 << PPC_FIELD_SH) - 1);
	    emit(COMPOSE_AND(rs_reg, mask_reg, tmp_reg));
	}

	emit(COMPOSE_CMPLT(31, tmp_reg, ca_reg));

	/* pop_alloc(); */

	if (PPC_FIELD_SH > 8)
	    free_tmp_integer_reg(mask_reg);
	free_tmp_integer_reg(tmp_reg);

	emit_label(no_ca_set);
	free_label(no_ca_set);

	unref_integer_reg(ca_reg);

	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	emit(COMPOSE_SRA_IMM(rs_reg, PPC_FIELD_SH, ra_reg));

	unref_integer_reg(rs_reg);

	if (PPC_FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t rs_reg, ra_reg;

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	emit(COMPOSE_SRA_IMM(rs_reg, PPC_FIELD_SH, ra_reg));

	unref_integer_reg(rs_reg);

	if (PPC_FIELD_RC)
	    gen_rc_code(ra_reg);

	unref_integer_reg(ra_reg);
    }
    else if (KILL_XER_CA)
    {
	reg_t rs_reg, ca_reg, mask_reg, tmp_reg;
	label_t end = alloc_label();

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ca_reg = ref_ppc_xer_ca_w();

	tmp_reg = alloc_tmp_integer_reg();
	if (PPC_FIELD_SH > 8)
	    mask_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_MOV(31, ca_reg));
	emit_branch(COMPOSE_BGE(rs_reg, 0), end);

	/* push_alloc(); */

	if (PPC_FIELD_SH <= 8)
	    emit(COMPOSE_AND_IMM(rs_reg, (1 << PPC_FIELD_SH) - 1, tmp_reg));
	else
	{
	    emit_load_integer_32(mask_reg, (1 << PPC_FIELD_SH) - 1);
	    emit(COMPOSE_BIC(rs_reg, mask_reg, tmp_reg));
	}

	emit(COMPOSE_CMPLT(31, tmp_reg, ca_reg));

	/* pop_alloc(); */

	if (PPC_FIELD_SH > 8)
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
    if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t rb_reg, rs_reg, ra_reg, tmp_reg;

	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	tmp_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_AND_IMM(rb_reg, 31, tmp_reg));

	unref_integer_reg(rb_reg);

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	emit(COMPOSE_ZAPNOT_IMM(rs_reg, 15, ra_reg));

	unref_integer_reg(rs_reg);

	emit(COMPOSE_SRL(ra_reg, tmp_reg, ra_reg));

	free_tmp_integer_reg(tmp_reg);

	emit(COMPOSE_ADDL(ra_reg, 31, ra_reg));

	if (PPC_FIELD_RC)
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

    if (PPC_FIELD_RA == 0)
    {
	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RD);

	emit(COMPOSE_STB(rs_reg, PPC_FIELD_D ^ 3, 31));
    }
    else
    {
	reg_t ra_reg, addr_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_LDA(addr_reg, PPC_FIELD_D, ra_reg));

	unref_integer_reg(ra_reg);

	emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RD);

	emit(COMPOSE_STB(rs_reg, 0, addr_reg));

	free_tmp_integer_reg(addr_reg);
    }

    unref_integer_reg(rs_reg);
}

static void
handle_stbu_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg, rs_reg, addr_reg;

    bt_assert(PPC_FIELD_RS != PPC_FIELD_RA);

    ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

    emit(COMPOSE_LDA(ra_reg, PPC_FIELD_D, ra_reg));

    addr_reg = alloc_tmp_integer_reg();

    emit(COMPOSE_XOR_IMM(ra_reg, 3, addr_reg));

    unref_integer_reg(ra_reg);
    rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

    emit(COMPOSE_STB(rs_reg, 0, addr_reg));

    unref_integer_reg(rs_reg);
    free_tmp_integer_reg(addr_reg);
}

static void
handle_stbx_insn (word_32 insn, word_32 pc)
{
    reg_t rb_reg, rs_reg, addr_reg;

    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

    if (PPC_FIELD_RA == 0)
    {
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_XOR_IMM(rb_reg, 3, addr_reg));

	unref_integer_reg(rb_reg);
	rs_reg = ref_ppc_gpr_w(PPC_FIELD_RS);

	emit(COMPOSE_STB(rs_reg, 0, addr_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(addr_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);

	emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

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

    if (PPC_FIELD_RA == 0)
	emit(COMPOSE_LDA(addr_reg, PPC_FIELD_D, 31));
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);

	emit(COMPOSE_LDA(addr_reg, PPC_FIELD_D, ra_reg));

	unref_integer_reg(ra_reg);
    }

    frs_reg = ref_ppc_fpr_r(PPC_FIELD_FRS);
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

    if (PPC_FIELD_D == 0)
	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
    else
    {
	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

	emit(COMPOSE_LDA(ra_reg, PPC_FIELD_D, ra_reg));
    }

    frs_reg = ref_ppc_fpr_r(PPC_FIELD_FRS);
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

    if (PPC_FIELD_RA == 0)
	addr_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
    else
    {
	reg_t ra_reg, rb_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDQ(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
    }

    bits_reg = alloc_tmp_integer_reg();
    frs_reg = ref_ppc_fpr_r(PPC_FIELD_FRS);

    gen_float_to_bits(frs_reg, bits_reg, 0);

    emit_store_mem_64(bits_reg, addr_reg);

    unref_float_reg(frs_reg);
    free_tmp_integer_reg(bits_reg);
    if (PPC_FIELD_RA == 0)
	unref_integer_reg(addr_reg);
    else
	free_tmp_integer_reg(addr_reg);
}

static void
handle_stfs_insn (word_32 insn, word_32 pc)
{
    reg_t frs_reg;

    if (PPC_FIELD_RA == 0)
    {
	frs_reg = ref_ppc_fpr_r(PPC_FIELD_FRS);

	emit(COMPOSE_STS(frs_reg, PPC_FIELD_D, 31));

	unref_float_reg(frs_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	frs_reg = ref_ppc_fpr_r(PPC_FIELD_FRS);

	emit(COMPOSE_STS(frs_reg, PPC_FIELD_D, ra_reg));

	unref_float_reg(frs_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_stfsx_insn (word_32 insn, word_32 pc)
{
    reg_t rb_reg, frs_reg, addr_reg;

    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

    if (PPC_FIELD_RA == 0)
    {
	frs_reg = ref_ppc_fpr_w(PPC_FIELD_FRS);

	emit(COMPOSE_STS(frs_reg, 0, rb_reg));

	unref_float_reg(frs_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
	frs_reg = ref_ppc_fpr_r(PPC_FIELD_FRS);

	emit(COMPOSE_STS(frs_reg, 0, addr_reg));

	unref_float_reg(frs_reg);
	free_tmp_integer_reg(addr_reg);
    }
}

static void
handle_sth_insn (word_32 insn, word_32 pc)
{
    reg_t rs_reg;

    if (PPC_FIELD_RA == 0)
    {
	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RD);

	emit(COMPOSE_STW(rs_reg, PPC_FIELD_D ^ 2, 31));
    }
    else
    {
	reg_t ra_reg, addr_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_LDA(addr_reg, PPC_FIELD_D, ra_reg));

	unref_integer_reg(ra_reg);

	emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RD);

	emit(COMPOSE_STW(rs_reg, 0, addr_reg));

	free_tmp_integer_reg(addr_reg);
    }

    unref_integer_reg(rs_reg);
}

static void
handle_sthbrx_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_sthu_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg, rs_reg, addr_reg;

    bt_assert(PPC_FIELD_RS != PPC_FIELD_RA);

    ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

    emit(COMPOSE_LDA(ra_reg, PPC_FIELD_D, ra_reg));

    addr_reg = alloc_tmp_integer_reg();

    emit(COMPOSE_XOR_IMM(ra_reg, 2, addr_reg));

    unref_integer_reg(ra_reg);
    rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

    emit(COMPOSE_STW(rs_reg, 0, addr_reg));

    unref_integer_reg(rs_reg);
    free_tmp_integer_reg(addr_reg);
}

static void
handle_sthx_insn (word_32 insn, word_32 pc)
{
    reg_t rb_reg, rs_reg, addr_reg;

    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

    if (PPC_FIELD_RA == 0)
    {
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_XOR_IMM(rb_reg, 2, addr_reg));

	unref_integer_reg(rb_reg);
	rs_reg = ref_ppc_gpr_w(PPC_FIELD_RS);

	emit(COMPOSE_STW(rs_reg, 0, addr_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(addr_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);

	emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

	emit(COMPOSE_STW(rs_reg, 0, addr_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(addr_reg);
    }
}

static void
handle_stw_insn (word_32 insn, word_32 pc)
{
    reg_t rs_reg;

    if (PPC_FIELD_RA == 0)
    {
	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

	emit(COMPOSE_STL(rs_reg, PPC_FIELD_D, 31));

	unref_integer_reg(rs_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

	emit(COMPOSE_STL(rs_reg, PPC_FIELD_D, ra_reg));

	unref_integer_reg(rs_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_stwbrx_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_stwu_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg, rs_reg;

    if (KILL_GPR(PPC_FIELD_RA))
    {
	ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);
	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

	emit(COMPOSE_STL(rs_reg, PPC_FIELD_D, ra_reg));

	unref_integer_reg(rs_reg);

	emit(COMPOSE_LDA(ra_reg, PPC_FIELD_D, ra_reg));

	unref_integer_reg(ra_reg);
    }
    else
    {
	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

	emit(COMPOSE_STL(rs_reg, PPC_FIELD_D, ra_reg));

	unref_integer_reg(rs_reg);
	unref_integer_reg(ra_reg);
    }
}

static void
handle_stwux_insn (word_32 insn, word_32 pc)
{
    reg_t ra_reg, rb_reg, rs_reg;

    bt_assert(PPC_FIELD_RA != PPC_FIELD_RS);

    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
    ra_reg = ref_ppc_gpr_rw(PPC_FIELD_RA);

    emit(COMPOSE_ADDL(ra_reg, rb_reg, ra_reg));

    unref_integer_reg(rb_reg);
    rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

    emit(COMPOSE_STL(rs_reg, 0, ra_reg));

    unref_integer_reg(rs_reg);
    unref_integer_reg(ra_reg);
}

static void
handle_stwx_insn (word_32 insn, word_32 pc)
{
    reg_t rb_reg, rs_reg, addr_reg;

    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

    if (PPC_FIELD_RA == 0)
    {
	rs_reg = ref_ppc_gpr_w(PPC_FIELD_RS);

	emit(COMPOSE_STL(rs_reg, 0, rb_reg));

	unref_integer_reg(rs_reg);
    }
    else
    {
	reg_t ra_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	addr_reg = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDL(ra_reg, rb_reg, addr_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);
	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);

	emit(COMPOSE_STL(rs_reg, 0, addr_reg));

	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(addr_reg);
    }
}

static void
handle_subf_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rb_reg, rd_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_SUBL(rb_reg, ra_reg, rd_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);

	if (PPC_FIELD_RC)
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
handle_subfo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfc_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD) && KILL_XER_CA)
    {
	reg_t ra_reg, rb_reg, rd_reg, ca_reg, tmp_reg;
	label_t end = alloc_label();

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
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
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_SUBL(rb_reg, ra_reg, rd_reg));

	unref_integer_reg(rb_reg);
	unref_integer_reg(ra_reg);

	if (PPC_FIELD_RC)
	    gen_rc_code(rd_reg);

	unref_integer_reg(rd_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RD))
    {
	handle_subf_insn(insn, pc);
    }
    else if (KILL_XER_CA)
    {
	reg_t ra_reg, rb_reg, ca_reg, tmp_reg;
	label_t end = alloc_label();

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
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
handle_subfco_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfcod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfe_insn (word_32 insn, word_32 pc)
{
    if (PPC_FIELD_RA == PPC_FIELD_RB)
    {
	if (KILL_GPR(PPC_FIELD_RD))
	{
	    reg_t rd_reg, ca_reg;

	    ca_reg = ref_ppc_xer_ca_r();
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_SUBL_IMM(ca_reg, 1, rd_reg));

	    unref_integer_reg(ca_reg);

	    if (PPC_FIELD_RC)
		gen_rc_code(rd_reg);

	    unref_integer_reg(rd_reg);
	}
    }
    else
    {
	if (KILL_GPR(PPC_FIELD_RD) && KILL_XER_CA)
	{
	    reg_t ra_reg, rb_reg, rd_reg, ca_reg, tmp_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	    if (PPC_FIELD_RD == PPC_FIELD_RA)
	    {
		tmp_reg = alloc_tmp_integer_reg();

		emit(COMPOSE_MOV(ra_reg, tmp_reg));

		unref_integer_reg(ra_reg);

		ra_reg = tmp_reg;
	    }

	    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	    if (PPC_FIELD_RD == PPC_FIELD_RB)
	    {
		tmp_reg = alloc_tmp_integer_reg();

		emit(COMPOSE_MOV(rb_reg, tmp_reg));

		unref_integer_reg(rb_reg);

		rb_reg = tmp_reg;
	    }

	    ca_reg = ref_ppc_xer_ca_rw();
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

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
	    if (PPC_FIELD_RD == PPC_FIELD_RB)
		free_tmp_integer_reg(rb_reg);
	    else
		unref_integer_reg(rb_reg);
	    if (PPC_FIELD_RD == PPC_FIELD_RA)
		free_tmp_integer_reg(ra_reg);
	    else
		unref_integer_reg(ra_reg);

	    if (PPC_FIELD_RC)
		gen_rc_code(rd_reg);

	    unref_integer_reg(rd_reg);
	}
	else if (KILL_GPR(PPC_FIELD_RD))
	{
	    reg_t ra_reg, rb_reg, rd_reg, ca_reg, tmp_reg;

	    ca_reg = ref_ppc_xer_ca_r();
	    tmp_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_XOR_IMM(ca_reg, 1, tmp_reg));

	    unref_integer_reg(ca_reg);
	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);

	    emit(COMPOSE_ADDL(ra_reg, tmp_reg, tmp_reg));

	    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);
	    rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	    emit(COMPOSE_SUBL(rb_reg, tmp_reg, rd_reg));

	    unref_integer_reg(rb_reg);
	    unref_integer_reg(ra_reg);
	    free_tmp_integer_reg(tmp_reg);

	    if (PPC_FIELD_RC)
		gen_rc_code(rd_reg);

	    unref_integer_reg(rd_reg);
	}
	else if (KILL_XER_CA)
	{
	    reg_t ra_reg, rb_reg, ca_reg, tmp_reg;

	    ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	    tmp_reg = alloc_tmp_integer_reg();

	    emit(COMPOSE_NOT(ra_reg, tmp_reg));

	    unref_integer_reg(ra_reg);

	    emit(COMPOSE_ZAPNOT_IMM(tmp_reg, 15, tmp_reg));

	    ca_reg = ref_ppc_xer_ca_rw();

	    emit(COMPOSE_ADDQ(tmp_reg, ca_reg, ca_reg));

	    rb_reg = ref_ppc_gpr_r(PPC_FIELD_RB);

	    emit(COMPOSE_ZAPNOT_IMM(rb_reg, 15, tmp_reg));

	    unref_integer_reg(rb_reg);

	    emit(COMPOSE_ADDQ(tmp_reg, ca_reg, ca_reg));

	    free_tmp_integer_reg(tmp_reg);

	    emit(COMPOSE_SRL_IMM(ca_reg, 32, ca_reg));
	    emit(COMPOSE_AND_IMM(ca_reg, 1, ca_reg));

	    unref_integer_reg(ca_reg);
	}
    }
}

static void
handle_subfed_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfeo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfeod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfic_insn (word_32 insn, word_32 pc)
{
    if (KILL_GPR(PPC_FIELD_RD) && KILL_XER_CA)
    {
	reg_t ra_reg, rd_reg, ca_reg, tmp_reg;
	label_t end = alloc_label();

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	ca_reg = ref_ppc_xer_ca_w();

	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, SEX32(PPC_FIELD_SIMM, 16));

	emit(COMPOSE_CMPLE(ra_reg, tmp_reg, ca_reg));
	if (PPC_FIELD_SIMM & 0x8000)
	    emit_branch(COMPOSE_BLT(ra_reg, 0), end);
	else
	    emit_branch(COMPOSE_BGE(ra_reg, 0), end);

	emit(COMPOSE_CMPLT(tmp_reg, ra_reg, ca_reg));

	emit_label(end);
	free_label(end);

	unref_integer_reg(ca_reg);
	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_SUBL(tmp_reg, ra_reg, rd_reg));

	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(ra_reg);
	unref_integer_reg(rd_reg);
    }
    else if (KILL_GPR(PPC_FIELD_RD))
    {
	reg_t ra_reg, rd_reg, tmp_reg;

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);

	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, SEX32(PPC_FIELD_SIMM, 16));

	rd_reg = ref_ppc_gpr_w(PPC_FIELD_RD);

	emit(COMPOSE_SUBL(tmp_reg, ra_reg, rd_reg));

	free_tmp_integer_reg(tmp_reg);
	unref_integer_reg(ra_reg);
	unref_integer_reg(rd_reg);
    }
    else if (KILL_XER_CA)
    {
	reg_t ra_reg, ca_reg, tmp_reg;
	label_t end = alloc_label();

	ra_reg = ref_ppc_gpr_r(PPC_FIELD_RA);
	ca_reg = ref_ppc_xer_ca_w();

	tmp_reg = alloc_tmp_integer_reg();
	emit_load_integer_32(tmp_reg, SEX32(PPC_FIELD_SIMM, 16));

	emit(COMPOSE_CMPLE(ra_reg, tmp_reg, ca_reg));
	if (PPC_FIELD_SIMM & 0x8000)
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
handle_subfme_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfmed_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfmeo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfmeod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfze_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfzed_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfzeo_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
}

static void
handle_subfzeod_insn (word_32 insn, word_32 pc)
{
    bt_assert(0);
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
    if (KILL_GPR(PPC_FIELD_RA))
    {
	if ((PPC_FIELD_UIMM & 0xff00) == 0)
	{
	    reg_t ra_reg, rs_reg;

	    rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	    ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	    emit(COMPOSE_XOR_IMM(rs_reg, PPC_FIELD_UIMM, ra_reg));

	    unref_integer_reg(ra_reg);
	    unref_integer_reg(rs_reg);
	}
	else
	{
	    reg_t ra_reg, rs_reg, tmp_reg;

	    tmp_reg = alloc_tmp_integer_reg();

	    emit_load_integer_32(tmp_reg, PPC_FIELD_UIMM);

	    rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	    ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

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
    if (KILL_GPR(PPC_FIELD_RA))
    {
	reg_t ra_reg, rs_reg, tmp_reg;

	tmp_reg = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp_reg, PPC_FIELD_UIMM << 16);

	rs_reg = ref_ppc_gpr_r(PPC_FIELD_RS);
	ra_reg = ref_ppc_gpr_w(PPC_FIELD_RA);

	emit(COMPOSE_XOR(rs_reg, tmp_reg, ra_reg));

	unref_integer_reg(ra_reg);
	unref_integer_reg(rs_reg);
	free_tmp_integer_reg(tmp_reg);
    }
}

#define SKELETON_FUNC_NAME     compile_to_alpha_ppc_insn
#define SKELETON_FUNC_ARGS     , int _optimize_taken_jump, label_t _taken_jump_label, word_32 _next_pc, \
                               word_32 _kill_cr, word_32 _kill_xer, word_32 _kill_gpr
#ifdef COLLECT_STATS
#define SKELETON_PRE_DECODE \
    optimize_taken_jump = _optimize_taken_jump; \
    taken_jump_label = _taken_jump_label; \
    next_pc = _next_pc; \
    kill_cr = _kill_cr; \
    kill_xer = _kill_xer; \
    kill_gpr = _kill_gpr; \
    ++num_translated_insns;
#else
#define SKELETON_PRE_DECODE \
    optimize_taken_jump = _optimize_taken_jump; \
    taken_jump_label = _taken_jump_label; \
    next_pc = _next_pc; \
    kill_cr = _kill_cr; \
    kill_xer = _kill_xer; \
    kill_gpr = _kill_gpr;
#endif

#include "ppc_skeleton.c"
