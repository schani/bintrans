/*
 * compiler.h
 *
 * bintrans
 *
 * Copyright (C) 2001,2002 Mark Probst
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

#define MAX_CODE_INSNS        700000
#define MAX_TRACE_INSNS          512
#ifdef DYNAMO_TRACES
#define MAX_ALT_CODE_INSNS    300000
#else
#define MAX_ALT_CODE_INSNS         0
#endif

extern word_32 code_area[MAX_CODE_INSNS + MAX_ALT_CODE_INSNS];

typedef word_32 reg_t;
typedef int label_t;

#define NO_REG           ((reg_t)-1)
#define NO_FOREIGN_ADDR  ((word_32)-1)

extern interpreter_t *compiler_intp;
#ifdef CROSSDEBUGGER
extern interpreter_t *debugger_intp;
#endif

reg_t ref_integer_reg (int foreign_reg, int reading, int writing);
void unref_integer_reg (reg_t reg);

reg_t ref_float_reg (int foreign_reg, int reading, int writing);
void unref_float_reg (reg_t reg);

void emit (word_32 insn);

#define ref_integer_reg_for_reading(f)                    ref_integer_reg(f,1,0)
#define ref_integer_reg_for_writing(f)                    ref_integer_reg(f,0,1)
#define ref_integer_reg_for_reading_and_writing(f)        ref_integer_reg(f,1,1)

#define ref_gpr_reg_for_reading                           ref_integer_reg_for_reading
#define ref_gpr_reg_for_writing                           ref_integer_reg_for_writing

#define unref_gpr_reg(f)                                  unref_integer_reg(f)

#define ref_float_reg_for_reading(f)                      ref_float_reg(f,1,0)
#define ref_float_reg_for_writing(f)                      ref_float_reg(f,0,1)
#define ref_float_reg_for_reading_and_writing(f)          ref_float_reg(f,1,1)

#define emit_store_mem_32(val,addr)    emit(COMPOSE_STL((val),0,(addr)))
#define emit_load_mem_32(val,addr)     emit(COMPOSE_LDL((val),0,(addr)))

#define NEED_NATIVE        0x1000

#define FIELD_REG_BIT      0x80000000

#ifdef CROSSDEBUGGER
void compare_register_sets (void);
#endif

/* liveness.c */
#ifdef EMU_I386
typedef struct
{
    word_32 flags_live;
    word_32 flags_killed;
} i386_insn_t;

#define MAX_BLOCK_INSNS          1024
#define MAX_AFTER_BRANCH_INSNS     30

extern i386_insn_t block_insns[MAX_TRACE_INSNS + MAX_AFTER_BRANCH_INSNS];

int compute_liveness (interpreter_t *intp, word_32 addr, word_32 *addrs);
void print_liveness (interpreter_t *intp, word_32 *addrs, int num_block_insns);
#endif

#if defined(EMU_PPC)
typedef struct
{
    word_32 live_cr;
    word_32 live_xer;
    word_32 live_gpr;
    word_32 killed_cr;
    word_32 killed_xer;
    word_32 killed_gpr;
} ppc_insn_t;

extern ppc_insn_t block_insns[MAX_TRACE_INSNS];

int compute_iterative_liveness (interpreter_t *intp, word_32 addr, word_32 *addrs, word_32 *live_cr, word_32 *live_xer, word_32 *live_gpr);

#if defined(DYNAMO_TRACES)
void compute_liveness_for_trace (interpreter_t *intp, word_32 *addrs, int length);
#endif
#if defined(COLLECT_LIVENESS)
void load_liveness_info (void);
void save_liveness_info (void);
#endif
#endif

/* ppc_compiler.c */
void compile_ppc_insn (word_32 insn, word_32 pc, int optimize_taken_jump, label_t taken_jump_label);

addr_t compile_basic_block (word_32 addr, int as_trace, unsigned char *preferred_alloced_integer_regs);
addr_t compile_loop_trace (word_32 addr, int length, int bits);
addr_t compile_trace (word_32 *addrs, int length, unsigned char *preferred_alloced_integer_regs);

/* ppc_to_alpha_compiler.c */
void compile_to_alpha_ppc_insn (word_32 insn, word_32 pc, int optimize_taken_jump, label_t taken_jump_label, word_32 next_pc,
				word_32 kill_cr, word_32 kill_xer, word_32 kill_gpr);

/* ppc_jump_analyzer.c */
void jump_analyze_ppc_insn (word_32 insn, word_32 pc, int *_num_targets, word_32 *targets, int *_can_fall_through, int *_can_jump_indirectly);

/* ppc_livenesser.c */
void liveness_ppc_insn (word_32 insn, word_32 pc,
			word_32 *_live_CR, word_32 *_killed_CR,
			word_32 *_live_XER, word_32 *_killed_XER,
			word_32 *_live_GPR, word_32 *_killed_GPR);

/* ppc_killer.c */
void kill_ppc_insn (word_32 insn, word_32 pc, word_32 *_killed_CR, word_32 *_killed_XER, word_32 *_killed_GPR);

/* ppc_consumer.c */
void consume_ppc_insn (word_32 insn, word_32 pc, word_32 *_consumed_CR, word_32 *_consumed_XER, word_32 *_consumed_GPR);
