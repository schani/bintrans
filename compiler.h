/*
 * compiler.h
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

#define MAX_CODE_INSNS     600000

extern word_32 code_area[MAX_CODE_INSNS];

typedef word_32 reg_t;
typedef int label_t;

#define NO_REG         ((reg_t)-1)

extern interpreter_t *compiler_intp;

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

/* ppc_compiler.c */
void compile_ppc_insn (word_32 insn, word_32 pc, int optimize_taken_jump, label_t taken_jump_label);

word_64 compile_basic_block (word_32 addr, int as_trace);
word_64 compile_trace (word_32 addr, int length, int bits);

/* ppc_to_alpha_compiler.c */
void compile_to_alpha_ppc_insn (word_32 insn, word_32 pc, int optimize_taken_jump, label_t taken_jump_label);

/* ppc_jump_analyzer.c */
void jump_analyze_ppc_insn (word_32 insn, word_32 pc, int *_num_targets, word_32 *targets, int *_can_fall_through, int *_can_jump_indirectly);
