/*
 * compiler.c
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

#include <assert.h>
#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>
#include <string.h>
#include <setjmp.h>

#include "bintrans.h"

/* #define NO_REGISTER_CACHING */
/* #define NO_PATCHING */
/* #define CHECK_TMP_REGS */
#define PRESERVE_CONSTANTS

#ifdef NEED_COMPILER
#include "compiler.h"
#include "fragment_hash.h"

#if defined(ARCH_ALPHA)
#include "alpha_composer.h"
#include "alpha_disassembler.c"
#elif defined(ARCH_PPC)
#include "ppc_composer.h"
#include "ppc_disassembler.c"
#else
#error unsupported target architecture
#endif

#define MAX_LABELS             128
#define MAX_BACKPATCHES          8
#define MAX_CONSTANTS         8192

#define MAX_UNRESOLVED_JUMPS 65536 /* should be a lot less if we actually do resolve branches */

#define MAX_FRAGMENT_EMU_INSNS          512
#define MAX_FRAGMENT_INSNS             1024
#define MAX_REG_FIELDS                 1024
#define MAX_REG_LOCKS                  2048
#define MAX_FRAGMENT_UNRESOLVED_JUMPS    64

#define NUM_AFTER_JUMP_INSN_SLOTS         0

#if defined(ARCH_ALPHA)
#define JUMP_TARGET_REG        16
#define RETURN_ADDR_REG        26
#define CONSTANT_AREA_REG      27
#define PROCEDURE_VALUE_REG    27
#elif defined(ARCH_PPC)
#define JUMP_TARGET_REG         3
#define CONSTANT_AREA_REG      31
#else
#error unsupported target architecture
#endif

#if defined(ARCH_ALPHA)
#define NUM_REG_CONSTANTS          (NUM_EMU_REGISTERS * 2 + 2) /* registers + scratch area */

#define DIRECT_DISPATCHER_CONST    (NUM_REG_CONSTANTS + 0)
#define INDIRECT_DISPATCHER_CONST  (NUM_REG_CONSTANTS + 2)
#define SYSTEM_CALL_CONST          (NUM_REG_CONSTANTS + 4)
#define ISYNC_CONST                (NUM_REG_CONSTANTS + 6)
#define C_STUB_CONST               (NUM_REG_CONSTANTS + 8)
#define LEADING_ZEROS_CONST        (NUM_REG_CONSTANTS + 10)
#define DIV_UNSIGNED_64_CONST      (NUM_REG_CONSTANTS + 12)
#define DIV_SIGNED_64_CONST        (NUM_REG_CONSTANTS + 14)
#define MOD_UNSIGNED_64_CONST      (NUM_REG_CONSTANTS + 16)
#define MOD_SIGNED_64_CONST        (NUM_REG_CONSTANTS + 18)
#define DIV_UNSIGNED_32_CONST      (NUM_REG_CONSTANTS + 20)
#define DIV_SIGNED_32_CONST        (NUM_REG_CONSTANTS + 22)
#define MOD_UNSIGNED_32_CONST      (NUM_REG_CONSTANTS + 24)
#define MOD_SIGNED_32_CONST        (NUM_REG_CONSTANTS + 26)
#define DIV_UNSIGNED_16_CONST      (NUM_REG_CONSTANTS + 28)
#define DIV_SIGNED_16_CONST        (NUM_REG_CONSTANTS + 30)
#define MOD_UNSIGNED_16_CONST      (NUM_REG_CONSTANTS + 32)
#define MOD_SIGNED_16_CONST        (NUM_REG_CONSTANTS + 34)
#define DIV_UNSIGNED_8_CONST       (NUM_REG_CONSTANTS + 36)
#define DIV_SIGNED_8_CONST         (NUM_REG_CONSTANTS + 38)
#define MOD_UNSIGNED_8_CONST       (NUM_REG_CONSTANTS + 40)
#define MOD_SIGNED_8_CONST         (NUM_REG_CONSTANTS + 42)
#ifdef COUNT_INSNS
#define EMULATED_INSNS_CONST       (NUM_REG_CONSTANTS + 44)
#define NATIVE_INSNS_CONST         (NUM_REG_CONSTANTS + 46)
#define FRAGMENTS_CONST            (NUM_REG_CONSTANTS + 48)
#define PREMATURE_EXITS_CONST      (NUM_REG_CONSTANTS + 50)
#define END_EXITS_CONST            (NUM_REG_CONSTANTS + 52)
#define EMULATED_TRACE_INSNS_CONST (NUM_REG_CONSTANTS + 54)
#ifdef SYNC_BLOCKS
#define EMPTY_SYNC_CONST           (NUM_REG_CONSTANTS + 56)
#define STANDARD_SYNC_CONST        (NUM_REG_CONSTANTS + 58)
#define CUSTOM_SYNC_CONST          (NUM_REG_CONSTANTS + 60)
#endif
#endif
#elif defined(ARCH_PPC)
#define NUM_REG_CONSTANTS          (NUM_EMU_REGISTERS + 1) /* registers + scratch area */

#define DIRECT_DISPATCHER_CONST    (NUM_REG_CONSTANTS + 0)
#define INDIRECT_DISPATCHER_CONST  (NUM_REG_CONSTANTS + 1)
#define SYSTEM_CALL_CONST          (NUM_REG_CONSTANTS + 2)
#define ISYNC_CONST                (NUM_REG_CONSTANTS + 3)
#define C_STUB_CONST               (NUM_REG_CONSTANTS + 4)
#define REPNE_SCASB_CONST          (NUM_REG_CONSTANTS + 5)
#define REP_MOVSD_CONST            (NUM_REG_CONSTANTS + 6)
#else
#error unsupported target architecture
#endif

typedef struct
{
    int begin;
    int end;
} reg_range_t;

#if defined(ARCH_ALPHA)
#if defined(EMU_PPC)
#define NEED_REGISTER_ALLOCATOR
reg_range_t integer_reg_range[] = { { 4, 16 }, { 18, 26 }, { 0, 0 } };
#elif defined(EMU_I386)
/* reg_range_t integer_reg_range[] = { { 15, 16 }, { 18, 26 }, { 0, 0 } }; */
#define FIRST_NATIVE_INTEGER_HOST_REG           1
#define NUM_NATIVE_INTEGER_HOST_REGS           14 /* FIXME: this should be in the machine definition (NUM_INTEGER_EMU_REGISTERS) */
#endif
#define FIRST_TMP_INTEGER_REG   1
#define NUM_TMP_INTEGER_REGS    3
#elif defined(ARCH_PPC)
#if defined(EMU_I386)
#define FIRST_NATIVE_INTEGER_HOST_REG          14
#define NUM_NATIVE_INTEGER_HOST_REGS           10
#else
#error unsupported foreign architecture
#endif
#define FIRST_TMP_INTEGER_REG                  10
#define NUM_TMP_INTEGER_REGS                    3
#else
#error unsupported target architecture
#endif

#ifndef NEED_REGISTER_ALLOCATOR
#define note_insn_reg(a,b)
#endif

#if defined(EMU_PPC) && defined(FAST_PPC_FPR)
#define FIRST_FLOAT_REG        28
#else
#define FIRST_FLOAT_REG         2
#endif

#define FIRST_TMP_FLOAT_REG     1
#define NUM_TMP_FLOAT_REGS      1

#ifdef NEED_REGISTER_ALLOCATOR
int native_integer_regs[NUM_FREE_INTEGER_REGS];
int native_float_regs[NUM_FREE_FLOAT_REGS];
#endif

#define REG_TYPE_INTEGER        1
#define REG_TYPE_FLOAT          2

#define MAX_ALLOC_DEPTH         4

typedef struct
{
    int emitted;
    int insn_num;
    int num_backpatches;
    int backpatches[MAX_BACKPATCHES];
} label_info_t;

typedef struct
{
    reg_t native_reg;
    int free;
} tmp_register_t;

#define DONT_KNOW           -1

#define REG_LOCK_READ         1
#define REG_LOCK_WRITE        2
#define REG_LOCK_READ_WRITE   3
#define REG_LOCK_UNLOCK       4

typedef struct
{
    int type;
    reg_t reg;
    int insn_num;
} reg_lock_t;

typedef struct
{
    int insn_num;
    reg_t reg;
    int shift;
} reg_field_t;

#define EMU_INSN_INSN                     1
#define EMU_INSN_EPILOGUE                 2
#define EMU_INSN_INTERLUDE                3
#define EMU_INSN_STORE_REGS               4
#define EMU_INSN_CONTINUE                 5
#define EMU_INSN_DIRECT_JUMP              6
#define EMU_INSN_DIRECT_JUMP_NO_SYNC      7
#define EMU_INSN_BEGIN_ALT                8
#define EMU_INSN_END_ALT                  9
#define EMU_INSN_FREEZE_SAVE             10

typedef struct
{
    int type;
    word_32 addr;
    int native_index;
} emu_insn_t;

#define ALLOCED_REG_NONE      0xff
#define ALLOCED_REG_REG_MASK  0x7f
#define ALLOCED_REG_DIRTY     0x80
#define ALLOCED_REG_SAVED     0x80

typedef struct
{
    addr_t addr;
    word_32 target;
#ifdef SYNC_BLOCKS
    addr_t start;
    unsigned char alloced_integer_regs[NUM_FREE_INTEGER_REGS];
    unsigned char alloced_float_regs[NUM_FREE_FLOAT_REGS];
#endif
} unresolved_jump_t;

typedef struct
{
    int insn_num;
    word_32 target;
} fragment_unresolved_jump_t;

#ifdef COLLECT_STATS
typedef struct
{
    int num_translated;
    int num_generated_insns;
    int num_generated_consts;
} insn_info_t;

insn_info_t insn_infos[NUM_INSNS];

#ifdef COLLECT_PPC_FPR_STATS
unsigned int fpr_uses[32];
#endif
#endif

word_32 fragment_insns[MAX_FRAGMENT_INSNS];
int num_fragment_insns;

emu_insn_t fragment_emu_insns[MAX_FRAGMENT_EMU_INSNS];
int num_fragment_emu_insns;

reg_lock_t reg_locks[MAX_REG_LOCKS];
int num_reg_locks;

#ifdef NEED_REGISTER_ALLOCATOR
reg_field_t reg_fields[MAX_REG_FIELDS];
int num_reg_fields;
#endif

label_info_t label_infos[MAX_LABELS];
int num_labels;

#ifdef SYNC_BLOCKS
fragment_unresolved_jump_t fragment_unresolved_jumps[MAX_FRAGMENT_UNRESOLVED_JUMPS];
int num_fragment_unresolved_jumps;

unresolved_jump_t unresolved_jumps[MAX_UNRESOLVED_JUMPS];
#endif

word_32 code_area[MAX_CODE_INSNS + MAX_ALT_CODE_INSNS];
word_32 *emit_loc;
#ifdef DYNAMO_TRACES
int emit_alt = 0;
word_32 *alt_emit_loc;
#endif

#if defined(ARCH_ALPHA)
int num_constants = NUM_EMU_REGISTERS * 2 + 2;
#elif defined(ARCH_PPC)
int num_constants = NUM_EMU_REGISTERS + 1;
#else
#error unsupported target architecture
#endif
int old_num_constants;
int num_constants_init;
word_32 constant_area[NUM_EMU_REGISTERS * 2 + 2 + MAX_CONSTANTS] __attribute__ ((aligned (8)));

tmp_register_t tmp_integer_regs[NUM_TMP_INTEGER_REGS];
tmp_register_t tmp_float_regs[NUM_TMP_FLOAT_REGS];

#ifdef NEED_REGISTER_ALLOCATOR
typedef struct
{
    int type;
    int used;
    int locked;
    int dirty;
    int first_insn_num;
    int last_insn_num;
    int live_at_start;
    int live_at_end;
    int is_saved;
#ifdef DYNAMO_TRACES
    int is_saved_freeze;
#endif
    reg_t preferred_native_reg;
    reg_t native_reg;
} emu_register_t;

emu_register_t emu_regs[NUM_EMU_REGISTERS];

reg_t alloced_integer_regs[NUM_NATIVE_INTEGER_REGS];
reg_t alloced_float_regs[NUM_NATIVE_FLOAT_REGS];

int all_regs_must_be_saved;
#endif

int have_jumped = 0;
int generated_insn_index;

interpreter_t *compiler_intp = 0;
#ifdef CROSSDEBUGGER
interpreter_t *debugger_intp = 0;
#endif

#ifdef COLLECT_STATS
#ifdef COMPILER_THRESHOLD
unsigned long num_patched_direct_jumps = 0;
#else
unsigned long num_direct_jumps = 0;
#endif
unsigned long num_direct_and_indirect_jumps = 0;
unsigned long num_translated_blocks = 0;
unsigned long num_translated_traces = 0;
unsigned long num_translated_insns = 0;
unsigned long num_translated_trace_insns = 0;
unsigned long num_load_store_reg_insns = 0;
unsigned long num_stub_calls = 0;
unsigned long num_loop_profiler_calls = 0;
unsigned long num_const_adds = 0;
unsigned long num_isyncs = 0;
unsigned long num_sync_blocks = 0;
unsigned long num_sync_blocks_in_situ = 0;
unsigned long num_empty_sync_blocks = 0;
#endif

fragment_hash_entry_t fragment_entry;
fragment_hash_supplement_t fragment_entry_supplement;

void (*branch_profile_func) (void) = 0;

jmp_buf compiler_restart;

#ifdef MEASURE_TIME
long compiler_time = 0;
struct timeval measure_start;

void
start_timer (void)
{
    gettimeofday(&measure_start, 0);
}

void
stop_timer (void)
{
    struct timeval stop;

    gettimeofday(&stop, 0);

    compiler_time += (stop.tv_sec - measure_start.tv_sec) * 1000000 + stop.tv_usec - measure_start.tv_usec;
}
#else
#define start_timer()
#define stop_timer()
#endif

void
emit (word_32 insn)
{
    assert(num_fragment_insns < MAX_FRAGMENT_INSNS);

    fragment_insns[num_fragment_insns++] = insn;

    /*
    assert(emit_loc < code_area + MAX_CODE_INSNS);
    *emit_loc++ = insn;
    */
}

void
start_emu_insn (int type, word_32 addr)
{
    assert(num_fragment_emu_insns < MAX_FRAGMENT_EMU_INSNS);

    fragment_emu_insns[num_fragment_emu_insns].type = type;
    fragment_emu_insns[num_fragment_emu_insns].addr = addr;
    fragment_emu_insns[num_fragment_emu_insns].native_index = num_fragment_insns;

    ++num_fragment_emu_insns;
}

#ifdef NEED_REGISTER_ALLOCATOR
void
emit_store_regs (int follow_type, word_32 addr)
{
    start_emu_insn(EMU_INSN_STORE_REGS, addr);
    emit(0);
    start_emu_insn(follow_type, 0);
}

void
note_insn_reg (reg_t reg, int shift)
{
    assert(num_reg_fields < MAX_REG_FIELDS);
    assert(reg & FIELD_REG_BIT);

    reg_fields[num_reg_fields].insn_num = num_fragment_insns;
    reg_fields[num_reg_fields].reg = reg & ~FIELD_REG_BIT;
    reg_fields[num_reg_fields].shift = shift;

    ++num_reg_fields;
}
#endif

void
disassemble_target_code (addr_t start, addr_t end)
{
    addr_t x;

    for (x = start; x < end; x += 4)
    {
	printf("%016lx  ", x);
	disassemble_target_insn(*(word_32*)x, x);
	printf("\n");
    }
}

void
dump_fragment_code (word_32 *addrs, addr_t start, addr_t end, addr_t alt_start, addr_t alt_end)
{
#ifdef DUMP_CODE
    int i;
    addr_t index = start;
    addr_t alt_index = alt_start;
    int in_alt = 0;

    for (i = 0; i < num_fragment_emu_insns; ++i)
    {
	addr_t last;

	printf("++++++++++++++++\n");

	switch (fragment_emu_insns[i].type)
	{
	    case EMU_INSN_INSN :
		printf("%08x  ", addrs[fragment_emu_insns[i].addr]);
#if defined(EMU_PPC)
		disassemble_ppc_insn(mem_get_32(compiler_intp, addrs[fragment_emu_insns[i].addr]), addrs[fragment_emu_insns[i].addr]);
#elif defined(EMU_I386)
		compiler_intp->pc = addrs[fragment_emu_insns[i].addr];
		disassemble_i386_insn(compiler_intp);
		printf("       0x%08x", block_insns[i - 1].flags_killed);
#endif
		break;

	    case EMU_INSN_EPILOGUE :
		printf("epilogue");
		break;

	    case EMU_INSN_STORE_REGS :
		printf("store regs (%08x)", fragment_emu_insns[i].addr);
		break;

	    case EMU_INSN_CONTINUE :
		printf("continue");
		break;

	    case EMU_INSN_FREEZE_SAVE :
		printf("freeze save");
		break;

	    case EMU_INSN_BEGIN_ALT :
#ifdef DYNAMO_TRACES
		printf("begin alt");
		in_alt = 1;
#else
		assert(0);
#endif
		break;

	    case EMU_INSN_END_ALT :
#ifdef DYNAMO_TRACES
		printf("end alt");
		in_alt = 0;
#else
		assert(0);
#endif
		break;

	    case EMU_INSN_DIRECT_JUMP :
	    case EMU_INSN_DIRECT_JUMP_NO_SYNC :
		printf("direct jump");
		break;
	}

	printf("\n- - - - - - - - \n");

	if (i == num_fragment_emu_insns - 1)
	{
	    assert(!in_alt);
	    last = end;
	}
	else
	{
	    if (!in_alt && fragment_emu_insns[i + 1].type != EMU_INSN_BEGIN_ALT)
		last = start + fragment_emu_insns[i + 1].native_index * 4;
	    else if (in_alt && fragment_emu_insns[i + 1].type != EMU_INSN_END_ALT)
		last = alt_start + fragment_emu_insns[i + 1].native_index * 4;
	    else
	    {
		int j;

		for (j = i + 2; j < num_fragment_emu_insns; ++j)
		    if (fragment_emu_insns[j].type == EMU_INSN_BEGIN_ALT
			|| fragment_emu_insns[j].type == EMU_INSN_END_ALT)
			break;

		if (j == num_fragment_emu_insns)
		{
		    if (!in_alt)
			last = end;
		    else
			last = alt_end;
		}
		else
		    last = (in_alt ? alt_start : start) + fragment_emu_insns[j].native_index * 4;
	    }
	}

	disassemble_target_code(in_alt ? alt_index : index, last);

	if (!in_alt)
	    index = last;
	else
	    alt_index = last;
    }

    for (i = old_num_constants; i < num_constants; ++i)
	printf("%4d    %08x\n", i * 4, constant_area[i]);
#endif
}

void
reinit_compiler (void)
{
    int i;

    num_constants = num_constants_init;
    emit_loc = code_area;
#ifdef DYNAMO_TRACES
    alt_emit_loc = code_area + MAX_CODE_INSNS;
#endif

#ifdef SYNC_BLOCKS
    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	unresolved_jumps[i].addr = 0;
#endif

    init_fragment_hash();
}

void
direct_emit (word_32 insn)
{
#ifdef DYNAMO_TRACES
    if (emit_alt)
    {
	assert(alt_emit_loc <= code_area + MAX_CODE_INSNS + MAX_ALT_CODE_INSNS);

	if (alt_emit_loc == code_area + MAX_CODE_INSNS + MAX_ALT_CODE_INSNS)
	{
	    printf("flushing cache (alt_emit_loc)\n");
	    reinit_compiler();
	    longjmp(compiler_restart, 1);
	}

	*alt_emit_loc++ = insn;
    }
    else
#endif
    {
	assert(emit_loc <= code_area + MAX_CODE_INSNS);

	if (emit_loc == code_area + MAX_CODE_INSNS)
	{
	    printf("flushing cache (emit_loc)\n");
	    reinit_compiler();
	    longjmp(compiler_restart, 1);
	}

	*emit_loc++ = insn;
    }
}

#ifdef ARCH_ALPHA
void
load_reg (reg_t native_reg, reg_t foreign_reg)
{
#ifdef COLLECT_STATS
    ++num_load_store_reg_insns;
#endif

    switch (emu_regs[foreign_reg].type)
    {
	case REG_TYPE_INTEGER :
	    direct_emit(COMPOSE_LDL(native_reg, foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	case REG_TYPE_FLOAT :
	    direct_emit(COMPOSE_LDT(native_reg, foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	default :
	    assert(0);
    }
}

void
store_reg (reg_t native_reg, reg_t foreign_reg)
{
#ifdef COLLECT_STATS
    ++num_load_store_reg_insns;
#endif

    switch (emu_regs[foreign_reg].type)
    {
	case REG_TYPE_INTEGER :
	    direct_emit(COMPOSE_STL(native_reg, foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	case REG_TYPE_FLOAT :
	    direct_emit(COMPOSE_STT(native_reg, foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	default:
	    assert(0);
    }
}
#endif

#ifdef NEED_REGISTER_ALLOCATOR
reg_t
ref_reg (int foreign_reg, int reading, int writing, int type)
{
    assert(foreign_reg >= 0 && foreign_reg < NUM_EMU_REGISTERS);
    assert(emu_regs[foreign_reg].type == type);
    assert(reading || writing);

    if (!emu_regs[foreign_reg].used)
    {
	emu_regs[foreign_reg].used = 1;
	emu_regs[foreign_reg].live_at_start = reading;
	emu_regs[foreign_reg].live_at_end = 0;
	emu_regs[foreign_reg].first_insn_num = num_fragment_insns;
    }

    if (emu_regs[foreign_reg].locked)
    {
	int i;

	assert(num_reg_locks > 0);

	for (i = num_reg_locks - 1; i >= 0; --i)
	    if (reg_locks[i].reg == foreign_reg)
	    {
		if (!(reg_locks[i].type & REG_LOCK_READ))
		    assert(!reading);
		if (writing)
		    reg_locks[i].type |= REG_LOCK_WRITE;
		break;
	    }

	assert(i >= 0);
    }
    else
    {
	reg_locks[num_reg_locks].reg = foreign_reg;
	reg_locks[num_reg_locks].type = (reading ? REG_LOCK_READ : 0) | (writing ? REG_LOCK_WRITE : 0);
	reg_locks[num_reg_locks].insn_num = num_fragment_insns;

	++num_reg_locks;
    }

    ++emu_regs[foreign_reg].locked;

    emu_regs[foreign_reg].live_at_end = emu_regs[foreign_reg].live_at_end || writing;

    return foreign_reg | FIELD_REG_BIT;
}

void
unref_reg (reg_t reg)
{
    int foreign_reg;

    assert(reg | FIELD_REG_BIT);

    foreign_reg = reg & ~FIELD_REG_BIT;
    assert(foreign_reg >= 0 && foreign_reg < NUM_EMU_REGISTERS);

    assert(emu_regs[foreign_reg].used);
    assert(emu_regs[foreign_reg].locked > 0);

    emu_regs[foreign_reg].last_insn_num = num_fragment_insns;

    if (--emu_regs[foreign_reg].locked == 0)
    {
	reg_locks[num_reg_locks].reg = foreign_reg;
	reg_locks[num_reg_locks].type = REG_LOCK_UNLOCK;
	reg_locks[num_reg_locks].insn_num = num_fragment_insns;

	++num_reg_locks;
    }
}
#endif

reg_t
ref_integer_reg (int foreign_reg, int reading, int writing)
{
    assert(foreign_reg >= 0 && foreign_reg < NUM_EMU_REGISTERS);
#ifdef EMU_I386
    assert(foreign_reg < NUM_NATIVE_INTEGER_HOST_REGS);
    return foreign_reg + FIRST_NATIVE_INTEGER_HOST_REG;
#else
    return ref_reg(foreign_reg, reading, writing, REG_TYPE_INTEGER);
#endif
}

void
unref_integer_reg (reg_t reg)
{
#ifdef EMU_I386
    assert(reg >= FIRST_NATIVE_INTEGER_HOST_REG && reg < FIRST_NATIVE_INTEGER_HOST_REG + NUM_NATIVE_INTEGER_HOST_REGS);
#else
    unref_reg(reg);
#endif
}

#ifdef NEED_REGISTER_ALLOCATOR
reg_t
ref_float_reg (int foreign_reg, int reading, int writing)
{
    assert(foreign_reg >= 0 && foreign_reg < NUM_EMU_REGISTERS);

#if defined(EMU_PPC) && defined(FAST_PPC_FPR)
    {
	int fpr_num = foreign_reg - 5 - 32;

	assert(foreign_reg >= 5 + 32 && foreign_reg < 5 + 32 + 32);

	if (fpr_num < 14)
	    return fpr_num;
	if (fpr_num >= 18)
	    return fpr_num - 4;
    }
#else
    return ref_reg(foreign_reg, reading, writing, REG_TYPE_FLOAT);
#endif
}

void
unref_float_reg (reg_t reg)
{
#if defined(EMU_PPC) && defined(FAST_PPC_FPR)
    if (reg < 28)
	return;
#else
    unref_reg(reg);
#endif
}
#endif

#ifdef NEED_REGISTER_ALLOCATOR
void
set_all_regs_must_be_saved (void)
{
    all_regs_must_be_saved = 1;
}
#endif

reg_t
alloc_tmp_reg (tmp_register_t *tmp_regs, int num_regs)
{
    int i;

    for (i = 0; i < num_regs; ++i)
	if (tmp_regs[i].free)
	{
	    tmp_regs[i].free = 0;
	    return tmp_regs[i].native_reg;
	}

    assert(0);

    return -1;
}

void
free_tmp_reg (tmp_register_t *tmp_regs, int num_regs, reg_t native_reg)
{
    int i;

    for (i = 0; i < num_regs; ++i)
	if (tmp_regs[i].native_reg == native_reg)
	{
	    assert(!tmp_regs[i].free);
	    tmp_regs[i].free = 1;
	    return;
	}

    assert(0);
}

reg_t
alloc_tmp_integer_reg (void)
{
    return alloc_tmp_reg(tmp_integer_regs, NUM_TMP_INTEGER_REGS);
}

void
free_tmp_integer_reg (reg_t native_reg)
{
    free_tmp_reg(tmp_integer_regs, NUM_TMP_INTEGER_REGS, native_reg);
}

int
is_tmp_integer_reg (reg_t reg)
{
    int i;

    for (i = 0; i < NUM_TMP_INTEGER_REGS; ++i)
	if (tmp_integer_regs[i].native_reg == reg)
	    return 1;
    return 0;
}

reg_t
alloc_tmp_float_reg (void)
{
    return alloc_tmp_reg(tmp_float_regs, NUM_TMP_FLOAT_REGS);
}

void
free_tmp_float_reg (reg_t native_reg)
{
    free_tmp_reg(tmp_float_regs, NUM_TMP_FLOAT_REGS, native_reg);
}

#if defined(ARCH_ALPHA)
void
emit_load_integer_32 (reg_t reg, word_32 val)
{
    if ((val >> 15) == 0 || (val >> 15) == 0x1ffff)
	emit(COMPOSE_LDA(reg, val & 0xffff, 31));
    else if ((val & 0xffff) == 0)
	emit(COMPOSE_LDAH(reg, val >> 16, 31));
    else
    {
#if defined(EMU_I386) || defined(PRESERVE_CONSTANTS)
	assert(num_constants < MAX_CONSTANTS);

	emit(COMPOSE_LDL(reg, num_constants * 4, CONSTANT_AREA_REG));
	constant_area[num_constants++] = val;
#else
	emit(COMPOSE_LDA(reg, val & 0xffff, 31));
	if (val & 0x8000)
	    emit(COMPOSE_ZAPNOT_IMM(reg, 3, reg));
	emit(COMPOSE_LDAH(reg, val >> 16, reg));
#endif
    }
}
#elif defined(ARCH_PPC)
void
emit_load_integer_32 (reg_t reg, word_32 val)
{
    if ((val >> 15) == 0 || (val >> 15) == 0x1ffff)
	emit(COMPOSE_LI(reg, val & 0xffff));
    else if ((val & 0xffff) == 0)
	emit(COMPOSE_LIS(reg, val >> 16));
    else
    {
	emit(COMPOSE_LIS(reg, val >> 16));
	emit(COMPOSE_ORI(reg, reg, val & 0xffff));
    }
    
}
#else
#error unsupported target architecture
#endif

#ifdef ARCH_ALPHA
void
emit_load_integer_64 (reg_t reg, word_64 val)
{
    if ((val >> 31) == 0 || (val >> 31) == 0x1ffffffff)
	emit_load_integer_32(reg, (word_32)val);
#if defined(EMU_I386) || defined(PRESERVE_CONSTANTS)
    else if ((val >> 32) == 0)
    {
	if ((val >> 15) == 0 || (val >> 15) == 0x1ffff)
	{
	    emit(COMPOSE_LDA(reg, val & 0xffff, 31));
	    emit(COMPOSE_ZAPNOT_IMM(reg, 15, reg));
	}
	else
	{
	    emit_load_integer_32(reg, (word_32)val);
	    emit(COMPOSE_ZAPNOT_IMM(reg, 15, reg));
	}
    }
#endif
    else
    {
	assert(num_constants + 2 < MAX_CONSTANTS);

	if (num_constants % 2 == 1)
	    constant_area[num_constants++] = 0xdeadbeef;

	emit(COMPOSE_LDQ(reg, num_constants * 4, CONSTANT_AREA_REG));
	constant_area[num_constants++] = val & 0xffffffff;
	constant_area[num_constants++] = val >> 32;

	/*
	printf("  load $%u,0x%lx\n", reg, val);
	assert(0);
	*/
    }
}
#endif

label_t
alloc_label (void)
{
    assert(num_labels < MAX_LABELS);

    label_infos[num_labels].emitted = 0;
    label_infos[num_labels].num_backpatches = 0;

    return num_labels++;
}

void
free_label (label_t label)
{
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(l->emitted);

    /*
    int i;
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(!l->free);

    for (i = 0; i < l->num_backpatches; ++i)
    {
	sword_64 disp = l->address - l->backpatches[i] - 4;
	word_32 field;

	assert(disp >= -(1 << 22) && disp < (1 << 22));
	field = (disp >> 2) & 0x1fffff;

	*(word_32*)(l->backpatches[i]) |= field;
    }

    label_infos[label].free = 1;
    */
}

void
emit_label (label_t label)
{
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(!l->emitted);

    l->insn_num = num_fragment_insns;
    l->emitted = 1;

    /* printf("label %d:\n", label); */
}

void
emit_branch (word_32 insn, label_t label)
{
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(l->num_backpatches < MAX_BACKPATCHES);
    l->backpatches[l->num_backpatches++] = num_fragment_insns;

    emit(insn);
}

void
emit_start_direct_jump (int sync_block)
{
#ifdef SYNC_BLOCKS
    if (sync_block)
	start_emu_insn(EMU_INSN_DIRECT_JUMP, 0);
    else
	start_emu_insn(EMU_INSN_DIRECT_JUMP_NO_SYNC, 0);

    start_emu_insn(EMU_INSN_CONTINUE, 0);
#endif
}

#ifdef DYNAMO_TRACES
#ifdef SYNC_BLOCKS
void
emit_freeze_save (void)
{
    start_emu_insn(EMU_INSN_FREEZE_SAVE, 0);
    start_emu_insn(EMU_INSN_CONTINUE, 0);
}
#else
#define emit_freeze_save()
#endif

void
emit_begin_alt (void)
{
    start_emu_insn(EMU_INSN_BEGIN_ALT, 0);
    start_emu_insn(EMU_INSN_CONTINUE, 0);
}

void
emit_end_alt (void)
{
    start_emu_insn(EMU_INSN_END_ALT, 0);
    start_emu_insn(EMU_INSN_CONTINUE, 0);
}
#else
#define emit_freeze_save()
#define emit_begin_alt()
#define emit_end_alt()
#endif

void
emit_direct_jump (word_32 target)
{
    int i;

    if (branch_profile_func != 0)
	branch_profile_func();

#if defined(ARCH_ALPHA)
    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, DIRECT_DISPATCHER_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));
    emit(target);
#elif defined(ARCH_PPC)
    emit(COMPOSE_LWZ(0, DIRECT_DISPATCHER_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_MTLR(0));
    emit(COMPOSE_BLRL());
    emit(target);
#else
#error unsupported target architecture
#endif

#ifdef SYNC_BLOCKS
    assert(num_fragment_unresolved_jumps < MAX_FRAGMENT_UNRESOLVED_JUMPS);

    fragment_unresolved_jumps[num_fragment_unresolved_jumps].insn_num = num_fragment_insns;
    fragment_unresolved_jumps[num_fragment_unresolved_jumps].target = target;

    ++num_fragment_unresolved_jumps;
#endif

    for (i = 0; i < NUM_AFTER_JUMP_INSN_SLOTS; ++i)
	emit(0);

    have_jumped = 1;
}

void
emit_indirect_jump (void)
{
    if (branch_profile_func != 0)
	branch_profile_func();

#if defined(ARCH_ALPHA)
    /* we assume that the target address is already in $7 */

    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, INDIRECT_DISPATCHER_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(0, PROCEDURE_VALUE_REG)); /* FIXME: should be 31! */
#elif defined(ARCH_PPC)
    /* we assume that the target address is already in r3 */

    emit(COMPOSE_LWZ(0, INDIRECT_DISPATCHER_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_MTLR(0));
    emit(COMPOSE_BLR());
#else
#error unsupported target architecture
#endif

    have_jumped = 1;
}

void
emit_isync (void)
{
#if defined(ARCH_ALPHA)
    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, ISYNC_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(31, PROCEDURE_VALUE_REG));
#elif defined(ARCH_PPC)
    assert(0);
#else
#error unsupported target architecture
#endif

    have_jumped = 1;
}

void
emit_system_call (void)
{
#if defined(ARCH_ALPHA)
    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, SYSTEM_CALL_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));
#elif defined(ARCH_PPC)
    emit(COMPOSE_LWZ(0, SYSTEM_CALL_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_MTLR(0));
    emit(COMPOSE_BLRL());
#else
#error unsupported target architecture
#endif
}

void
start_fragment (void)
{
    int i;

    num_fragment_insns = 0;
    num_fragment_emu_insns = 0;
    num_reg_locks = 0;
    num_labels = 0;
#ifdef SYNC_BLOCKS
    num_fragment_unresolved_jumps = 0;
#endif
    old_num_constants = num_constants;

#ifdef NEED_REGISTER_ALLOCATOR
    num_reg_fields = 0;

    all_regs_must_be_saved = 0;

    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
    {
	emu_regs[i].used = 0;
	emu_regs[i].locked = 0;
	emu_regs[i].native_reg = NO_REG;
    }
#endif
}

#ifdef NEED_REGISTER_ALLOCATOR
void
simple_reg_alloc (void)
{
    int i, ii = 0, fi = 0;

    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
	if (emu_regs[i].used && emu_regs[i].native_reg == NO_REG)
	{
	    if (emu_regs[i].type == REG_TYPE_INTEGER)
	    {
		assert(emu_regs[i].preferred_native_reg != NO_REG);

		if (alloced_integer_regs[emu_regs[i].preferred_native_reg] == NO_REG)
		    emu_regs[i].native_reg = emu_regs[i].preferred_native_reg;
		else
		{
		    while (alloced_integer_regs[native_integer_regs[ii]] != NO_REG)
			++ii;
		    assert(ii < NUM_FREE_INTEGER_REGS);
		    emu_regs[i].native_reg = native_integer_regs[ii];
		}
		alloced_integer_regs[emu_regs[i].native_reg] = i;
	    }
	    else if (emu_regs[i].type == REG_TYPE_FLOAT)
	    {
		emu_regs[i].native_reg = native_float_regs[fi++];
		assert(alloced_float_regs[emu_regs[i].native_reg] == NO_REG);
		alloced_float_regs[emu_regs[i].native_reg] = i;
	    }
	    else
		assert(0);
	}
}
#endif

#ifdef COLLECT_LIVENESS
int
emu_reg_live (reg_t j, word_32 live_cr, word_32 live_xer, word_32 live_gpr)
{
    int live = 1;

    if (j >= CLASS_FIRST_INDEX_GPR && j < CLASS_FIRST_INDEX_GPR + 32)
	live = (live_gpr >> (j - CLASS_FIRST_INDEX_GPR)) & 1;
    else if (j == REG_INDEX_CRFB0)
	live = live_cr >> 31;
    else if (j == REG_INDEX_CRFB1)
	live = (live_cr >> 30) & 1;
    else if (j == REG_INDEX_CRFB2)
	live = (live_cr >> 29) & 1;
    else if (j == REG_INDEX_CRFB3)
	live = (live_cr >> 28) & 1;
    else if (j == REG_INDEX_XER_SO)
	live = live_xer >> 31;
    else if (j == REG_INDEX_XER_CA)
	live = (live_xer >> 29) & 1;

    return live;
}
#endif

#ifdef NEED_REGISTER_ALLOCATOR
void
alloc_native_reg_for_emu_reg (reg_t emu_reg, int current_insn_num, int insn_index)
{
    int l = -1;
    reg_t native_reg;

    if (emu_regs[emu_reg].type == REG_TYPE_INTEGER)
    {
	l = alloced_integer_regs[emu_regs[emu_reg].preferred_native_reg];

	if (l == NO_REG)
	{
	    emu_regs[emu_reg].native_reg = emu_regs[emu_reg].preferred_native_reg;
	    alloced_integer_regs[emu_regs[emu_reg].preferred_native_reg] = emu_reg;

	    return;
	}

	assert(emu_regs[l].type == emu_regs[emu_reg].type);
	assert(emu_regs[l].native_reg == emu_regs[emu_reg].preferred_native_reg);
	if (emu_regs[l].locked || emu_regs[l].last_insn_num > current_insn_num)
	    l = -1;
    }

    if (l < 0)
    {
	for (l = 0; l < NUM_EMU_REGISTERS; ++l)
	    if (emu_regs[l].type == emu_regs[emu_reg].type && emu_regs[l].used
		&& !emu_regs[l].locked && emu_regs[l].native_reg != NO_REG
		&& emu_regs[l].last_insn_num <= current_insn_num)
		break;

	if (l == NUM_EMU_REGISTERS)
	    for (l = 0; l < NUM_EMU_REGISTERS; ++l)
		if (emu_regs[l].type == emu_regs[emu_reg].type && emu_regs[l].used
		    && !emu_regs[l].locked && emu_regs[l].native_reg != NO_REG)
		    break;
    }

    assert(l >= 0 && l < NUM_EMU_REGISTERS);

    if (emu_regs[l].dirty)
	assert(emu_regs[l].live_at_end);

    if (emu_regs[l].dirty
#ifdef COLLECT_LIVENESS
	&& emu_reg_live(l, block_insns[insn_index].live_cr, block_insns[insn_index].live_xer, block_insns[insn_index].live_gpr)
#endif
	)
    {
	store_reg(emu_regs[l].native_reg, l);
	if (emu_regs[l].is_saved == DONT_KNOW)
	    emu_regs[l].is_saved = 1;
    }
    else
    {
	if (emu_regs[l].is_saved == DONT_KNOW)
	    emu_regs[l].is_saved = 0;
    }

    native_reg = emu_regs[l].native_reg;

    emu_regs[l].native_reg = NO_REG;
    emu_regs[emu_reg].native_reg = native_reg;

    if (emu_regs[emu_reg].type == REG_TYPE_INTEGER)
	alloced_integer_regs[native_reg] = emu_reg;
    else if (emu_regs[emu_reg].type == REG_TYPE_FLOAT)
	alloced_float_regs[native_reg] = emu_reg;
    else
	assert(0);
}

void
execute_reg_locks (int *_rl, int insn_num, int insn_index)
{
    int rl = *_rl;

    while (rl < num_reg_locks && reg_locks[rl].insn_num == insn_num)
    {
	/* printf("  %slock %d\n", reg_locks[rl].type == REG_LOCK_UNLOCK ? "un" : "", reg_locks[rl].reg); */

	if (reg_locks[rl].type == REG_LOCK_UNLOCK)
	    emu_regs[reg_locks[rl].reg].locked = 0;
	else
	{
	    assert(!emu_regs[reg_locks[rl].reg].locked);

	    emu_regs[reg_locks[rl].reg].locked = 1;

	    if (emu_regs[reg_locks[rl].reg].native_reg == NO_REG)
	    {
		alloc_native_reg_for_emu_reg(reg_locks[rl].reg, insn_num, insn_index);

		if (reg_locks[rl].type & REG_LOCK_READ)
		    load_reg(emu_regs[reg_locks[rl].reg].native_reg, reg_locks[rl].reg);
		emu_regs[reg_locks[rl].reg].dirty = (reg_locks[rl].type & REG_LOCK_WRITE) ? 1 : 0;
	    }
	    else
		emu_regs[reg_locks[rl].reg].dirty |= (reg_locks[rl].type & REG_LOCK_WRITE) ? 1 : 0;

	    if (emu_regs[reg_locks[rl].reg].dirty)
		assert(emu_regs[reg_locks[rl].reg].live_at_end);
	}

	++rl;
    }

    *_rl = rl;
}
#endif

void
finish_fragment (word_32 *addrs, unsigned char *preferred_alloced_integer_regs)
{
    int i;
#ifdef NEED_REGISTER_ALLOCATOR
    int num_integer_regs = 0, num_float_regs = 0;
#endif
#ifdef DUMP_CODE
    addr_t start = (addr_t)emit_loc;
#ifdef DYNAMO_TRACES
    addr_t alt_start = (addr_t)alt_emit_loc;
#endif
#endif
    addr_t body_start;
    int jumps_index;
    int rf, rl;
#ifdef SYNC_BLOCKS
    addr_t direct_jump_start;
    int no_sync;
    int has_freezed_save = 0;
#endif
#ifdef DYNAMO_TRACES
#define EMIT_LOC           (emit_alt ? alt_emit_loc : emit_loc)
#define START              (emit_alt ? alt_start : body_start)
#else
#define EMIT_LOC           emit_loc
#define START              body_start
#endif

#ifdef DYNAMO_TRACES
    emit_alt = 0;
#endif

    init_fragment_hash_entry(&fragment_entry, &fragment_entry_supplement);

    fragment_entry.native_addr = (addr_t)emit_loc;

#ifdef NEED_REGISTER_ALLOCATOR
    for (i = 0; i < NUM_NATIVE_INTEGER_REGS; ++i)
	alloced_integer_regs[i] = NO_REG;
    for (i = 0; i < NUM_NATIVE_FLOAT_REGS; ++i)
	alloced_float_regs[i] = NO_REG;

    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
    {
	if (emu_regs[i].used)
	{
	    if (emu_regs[i].type == REG_TYPE_INTEGER)
		++num_integer_regs;
	    else if (emu_regs[i].type == REG_TYPE_FLOAT)
		++num_float_regs;
	    else
		assert(0);

	    emu_regs[i].dirty = 0;
	    emu_regs[i].is_saved = DONT_KNOW;
	    assert(emu_regs[i].native_reg == NO_REG);
	}
    }

    /*
    if (preferred_alloced_integer_regs != 0)
	for (i = 0; i < NUM_FREE_INTEGER_REGS; ++i)
	    if (preferred_alloced_integer_regs[i] != ALLOCED_REG_NONE)
	    {
		reg_t reg = preferred_alloced_integer_regs[i] & ALLOCED_REG_REG_MASK;

		assert(emu_regs[reg].type == REG_TYPE_INTEGER);
		if (emu_regs[reg].used && emu_regs[reg].live_at_start)
		{
		    emu_regs[reg].native_reg = native_integer_regs[i];
		    alloced_integer_regs[native_integer_regs[i]] = reg;
		}
	    }
    */

#ifdef DUMP_CODE
    printf("%d integer regs, %d float regs\n", num_integer_regs, num_float_regs);
#endif

    if (num_integer_regs <= NUM_FREE_INTEGER_REGS && num_float_regs <= NUM_FREE_FLOAT_REGS)
	simple_reg_alloc();
    else
    {
	int ii = 0, fi = 0;

	for (i = 0; i < num_reg_fields; ++i)
	{
	    reg_t reg = reg_fields[i].reg;

	    if (emu_regs[reg].native_reg == NO_REG)
	    {
		if (emu_regs[reg].type == REG_TYPE_INTEGER)
		{
		    assert(emu_regs[reg].preferred_native_reg != NO_REG);

		    if (alloced_integer_regs[emu_regs[reg].preferred_native_reg] == NO_REG)
			emu_regs[reg].native_reg = emu_regs[reg].preferred_native_reg;
		    else
		    {
			while (ii < NUM_FREE_INTEGER_REGS && alloced_integer_regs[native_integer_regs[ii]] != NO_REG)
			    ++ii;

			if (ii < NUM_FREE_INTEGER_REGS)
			    emu_regs[reg].native_reg = native_integer_regs[ii];
		    }

		    if (emu_regs[reg].native_reg != NO_REG)
			alloced_integer_regs[emu_regs[reg].native_reg] = reg;
		}
		else if (emu_regs[reg_fields[i].reg].type == REG_TYPE_FLOAT)
		{
		    if (fi < NUM_FREE_FLOAT_REGS)
			emu_regs[reg].native_reg = native_float_regs[fi++];

		    if (emu_regs[reg].native_reg != NO_REG)
			alloced_float_regs[emu_regs[reg].native_reg] = reg;
		}
		else
		    assert(0);

		if (ii == NUM_FREE_INTEGER_REGS && fi == NUM_FREE_FLOAT_REGS)
		    break;
	    }
	}
    }

    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
    {
	if (emu_regs[i].used && emu_regs[i].live_at_start && emu_regs[i].native_reg != NO_REG)
	    load_reg(emu_regs[i].native_reg, i);
    }
#endif

    body_start = (addr_t)emit_loc;
    jumps_index = 0;
    rf = 0;
    rl = 0;

#ifdef SYNC_BLOCKS
    fragment_entry_supplement.synced_native_addr = body_start;

    for (i = 0; i < NUM_FREE_INTEGER_REGS; ++i)
	if (alloced_integer_regs[native_integer_regs[i]] == NO_REG
	    || !emu_regs[alloced_integer_regs[native_integer_regs[i]]].live_at_start) /* comment out this line for fast compress */
	    fragment_entry_supplement.alloced_integer_regs[i] = ALLOCED_REG_NONE;
	else
	    fragment_entry_supplement.alloced_integer_regs[i] = alloced_integer_regs[native_integer_regs[i]];

    for (i = 0; i < NUM_FREE_FLOAT_REGS; ++i)
	if (alloced_float_regs[native_float_regs[i]] == NO_REG
	    || !emu_regs[alloced_float_regs[native_float_regs[i]]].live_at_start) /* comment out this line for fast compress */
	    fragment_entry_supplement.alloced_float_regs[i] = ALLOCED_REG_NONE;
	else
	    fragment_entry_supplement.alloced_float_regs[i] = alloced_float_regs[native_float_regs[i]];
#endif

    /* instructions are patched and emitted; register allocation happens simultaneously */
    for (i = 0; i < num_fragment_emu_insns; ++i)
    {
	int len, first, last;
	int j;

	first = fragment_emu_insns[i].native_index;

	if (i < num_fragment_emu_insns - 1)
	    len = fragment_emu_insns[i + 1].native_index - first;
	else
	    len = num_fragment_insns - first;

	last = first + len;

#ifdef DYNAMO_TRACES
	/* we handle this here so that the setting of native_index uses the correct emit_loc */
	if (fragment_emu_insns[i].type == EMU_INSN_BEGIN_ALT)
	    emit_alt = 1;
	else if (fragment_emu_insns[i].type == EMU_INSN_END_ALT)
	    emit_alt = 0;
#endif

#ifdef DUMP_CODE
	fragment_emu_insns[i].native_index = EMIT_LOC - (word_32*)START;
#endif

	/* printf("insn at %08x  %d\n", fragment_emu_insns[i].addr, fragment_emu_insns[i].type); */

	switch (fragment_emu_insns[i].type)
	{
	    case EMU_INSN_STORE_REGS :
		{
#ifdef COLLECT_LIVENESS
		    fragment_hash_entry_t *entry;
		    fragment_hash_supplement_t *supplement;
		    word_32 live_cr, live_xer, live_gpr;

		    if (fragment_emu_insns[i].addr == NO_FOREIGN_ADDR
			|| (entry = fragment_hash_get(fragment_emu_insns[i].addr, &supplement)) == 0)
			live_cr = live_xer = live_gpr = 0xffffffff;
		    else
		    {
			live_cr = supplement->live_cr;
			live_xer = supplement->live_xer;
			live_gpr = supplement->live_gpr;
		    }
#endif

		    assert(first + 1 == last);

		    assert(i > 0);
#ifdef NEED_REGISTER_ALLOCATOR
		    execute_reg_locks(&rl, first, fragment_emu_insns[i - 1].addr);
#endif

		    /* the dummy insn is replaced by its code_area index */
		    fragment_insns[first] = EMIT_LOC - code_area;

#ifdef NEED_REGISTER_ALLOCATOR
		    for (j = 0; j < NUM_EMU_REGISTERS; ++j)
			if (emu_regs[j].used && emu_regs[j].dirty && emu_regs[j].live_at_end && emu_regs[j].native_reg != NO_REG)
			{
			    int live = 1;

#ifdef COLLECT_LIVENESS
			    live = emu_reg_live(j, live_cr, live_xer, live_gpr);
#endif

			    if (live)
				store_reg(emu_regs[j].native_reg, j);
			}
#endif
		}
		break;

#ifdef SYNC_BLOCKS
	    case EMU_INSN_DIRECT_JUMP :
		direct_jump_start = (addr_t)EMIT_LOC;
		no_sync = 0;
		break;

	    case EMU_INSN_DIRECT_JUMP_NO_SYNC :
		direct_jump_start = (addr_t)EMIT_LOC;
		no_sync = 1;
		break;
#endif

#ifdef DYNAMO_TRACES
#ifdef SYNC_BLOCKS
	    case EMU_INSN_FREEZE_SAVE :
		if (!has_freezed_save)
		{
		    for (j = 0; j < NUM_EMU_REGISTERS; ++j)
			if (emu_regs[j].used)
			{
			    if (emu_regs[j].is_saved == DONT_KNOW)
				emu_regs[j].is_saved_freeze = emu_regs[j].dirty;
			    else
				emu_regs[j].is_saved_freeze = emu_regs[j].is_saved;
			}

		    has_freezed_save = 1;
		}
		break;
#endif

	    case EMU_INSN_BEGIN_ALT :
		assert(len == 0);
		break;

	    case EMU_INSN_END_ALT :
		assert(len == 0);
		break;
#endif

	    default :
		for (j = first; j < last; ++j)
		{
		    word_32 *first_insn = EMIT_LOC;

#ifdef NEED_REGISTER_ALLOCATOR
		    execute_reg_locks(&rl, j, fragment_emu_insns[i].addr);

		    while (rf < num_reg_fields && reg_fields[rf].insn_num == j)
		    {
			/* printf("  use %d\n", reg_fields[rf].reg); */

			assert(emu_regs[reg_fields[rf].reg].locked);

			fragment_insns[j] |= emu_regs[reg_fields[rf].reg].native_reg << reg_fields[rf].shift;

			++rf;
		    }
#endif

		    /*
		    printf("  %d: ", j);
		    disassemble_alpha_insn(fragment_insns[j], (word_64)EMIT_LOC);
		    printf("\n");
		    */

		    direct_emit(fragment_insns[j]);

		    /* the insn is replaced by its code_area index */
		    fragment_insns[j] = first_insn - code_area;

#ifdef SYNC_BLOCKS
		    if (jumps_index < num_fragment_unresolved_jumps && j + 1 == fragment_unresolved_jumps[jumps_index].insn_num)
		    {
			int k;
			int l;

			for (k = 0; k < MAX_UNRESOLVED_JUMPS; ++k)
			    if (unresolved_jumps[k].addr == 0)
				break;

			assert(k < MAX_UNRESOLVED_JUMPS);

			unresolved_jumps[k].addr = (addr_t)EMIT_LOC;
			unresolved_jumps[k].target = fragment_unresolved_jumps[jumps_index].target;
			unresolved_jumps[k].start = direct_jump_start;
			for (l = 0; l < NUM_FREE_INTEGER_REGS; ++l)
			{
			    if (no_sync || alloced_integer_regs[native_integer_regs[l]] == NO_REG)
				unresolved_jumps[k].alloced_integer_regs[l] = ALLOCED_REG_NONE;
			    else
			    {
				reg_t reg = alloced_integer_regs[native_integer_regs[l]];

				if (!emu_regs[reg].live_at_start)
				    assert(emu_regs[reg].live_at_end);

				/* comment out the if (only else branch remains) for fast compress */
				if (!emu_regs[reg].live_at_start && !emu_regs[reg].dirty)
				    unresolved_jumps[k].alloced_integer_regs[l] = ALLOCED_REG_NONE;
				else
				    unresolved_jumps[k].alloced_integer_regs[l] = reg | (emu_regs[reg].dirty ? ALLOCED_REG_DIRTY : 0);
			    }
			}
			for (l = 0; l < NUM_FREE_FLOAT_REGS; ++l)
			{
			    if (no_sync || alloced_float_regs[native_float_regs[l]] == NO_REG)
				unresolved_jumps[k].alloced_float_regs[l] = ALLOCED_REG_NONE;
			    else
			    {
				reg_t reg = alloced_float_regs[native_float_regs[l]];

				if (!emu_regs[reg].live_at_start)
				    assert(emu_regs[reg].live_at_end);

				/* comment out the if (only else branch remains) for fast compress */
				if (!emu_regs[reg].live_at_start && !emu_regs[reg].dirty)
				    unresolved_jumps[k].alloced_float_regs[l] = ALLOCED_REG_NONE;
				else
				    unresolved_jumps[k].alloced_float_regs[l] = reg | (emu_regs[reg].dirty ? ALLOCED_REG_DIRTY : 0);
			    }
			}

			++jumps_index;
		    }
#endif
		}
		break;
	}
    }

#ifdef SYNC_BLOCKS
    assert(has_freezed_save);
#endif

#ifdef NEED_REGISTER_ALLOCATOR
    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
	if (emu_regs[i].used && emu_regs[i].is_saved == DONT_KNOW)
	    emu_regs[i].is_saved = emu_regs[i].live_at_end;
#endif

#ifdef SYNC_BLOCKS
#ifdef DYNAMO_TRACES
#define IS_SAVED       is_saved_freeze
#else
#define IS_SAVED       is_saved
#endif
    for (i = 0; i < NUM_FREE_INTEGER_REGS; ++i)
	if (fragment_entry_supplement.alloced_integer_regs[i] != ALLOCED_REG_NONE
	    && emu_regs[fragment_entry_supplement.alloced_integer_regs[i]].IS_SAVED)
	    fragment_entry_supplement.alloced_integer_regs[i] |= ALLOCED_REG_SAVED;

    for (i = 0; i < NUM_FREE_FLOAT_REGS; ++i)
	if (fragment_entry_supplement.alloced_float_regs[i] != ALLOCED_REG_NONE
	    && emu_regs[fragment_entry_supplement.alloced_float_regs[i]].IS_SAVED)
	    fragment_entry_supplement.alloced_float_regs[i] |= ALLOCED_REG_SAVED;
#endif

    /* jumps are patched */
    for (i = 0; i < num_labels; ++i)
    {
	label_info_t *l = &label_infos[i];
	int j;

	for (j = 0; j < l->num_backpatches; ++j)
	{
	    int index = fragment_insns[l->backpatches[j]];
#if defined(ARCH_ALPHA)
	    sword_64 disp;
	    word_32 field;

	    while ((code_area[index] >> 26) == 0x28 || (code_area[index] >> 26) == 0x2c
		   || (code_area[index] >> 26) == 0x23 || (code_area[index] >> 26) == 0x27)
		++index;

	    disp = (fragment_insns[l->insn_num] - index - 1) * 4;

	    assert(disp >= -(1 << 22) && disp < (1 << 22));
	    field = (disp >> 2) & 0x1fffff;

	    code_area[index] |= field;
#elif defined(ARCH_PPC)
	    sword_32 disp;
	    word_32 field;

	    assert((code_area[index] >> 26) == 16);

	    disp = (fragment_insns[l->insn_num] - index);

	    assert(disp >= -(1 << 13) && disp < (1 << 13));
	    field = disp & 0x3fff;

	    code_area[index] |= field << 2;
#else
#error unsupported target architecture
#endif
	}
    }

#ifdef DUMP_CODE
    printf("++++++++++++++++\nprologue\n- - - - - - - - \n");

    disassemble_target_code(start, body_start);

#ifdef DYNAMO_TRACES
    dump_fragment_code(addrs, body_start, (addr_t)emit_loc, alt_start, (addr_t)alt_emit_loc);
#else
    dump_fragment_code(addrs, body_start, (addr_t)emit_loc, 0, 0);
#endif
#endif
}

#ifdef ARCH_ALPHA
void
emit_store_mem_8 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));
#endif
    emit(COMPOSE_STB(value_reg, 0, addr_reg));
}

void
emit_store_mem_64 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    reg_t tmp_reg;

    emit(COMPOSE_STL(value_reg, 4, addr_reg));
    tmp_reg = alloc_tmp_integer_reg();
    emit(COMPOSE_SRL_IMM(value_reg, 32, tmp_reg));
    emit(COMPOSE_STL(tmp_reg, 0, addr_reg));
    free_tmp_integer_reg(tmp_reg);
#else
    emit(COMPOSE_STQ(value_reg, 0, addr_reg));
#endif
}

void
emit_store_mem_16 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));
#endif
    emit(COMPOSE_STW(value_reg, 0, addr_reg));
}

void
emit_load_mem_8 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));
#endif
    emit(COMPOSE_LDBU(value_reg, 0, addr_reg));
}

void
emit_load_mem_16 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));
#endif
    emit(COMPOSE_LDWU(value_reg, 0, addr_reg));
}

void
emit_load_mem_64 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    reg_t tmp_reg = alloc_tmp_integer_reg();

    emit(COMPOSE_LDL(tmp_reg, 0, addr_reg));
    emit(COMPOSE_SLL_IMM(tmp_reg, 32, tmp_reg));
    emit(COMPOSE_LDL(value_reg, 4, addr_reg));
    emit(COMPOSE_ZAPNOT_IMM(value_reg, 15, value_reg));
    emit(COMPOSE_BIS(value_reg, tmp_reg, value_reg));
    free_tmp_integer_reg(tmp_reg);
#else
    emit(COMPOSE_LDQ(value_reg, 0, addr_reg));
#endif
}
#endif

#if defined(EMU_PPC)
#ifndef USE_HAND_TRANSLATOR
#include "ppc_compiler.c"
#else
#include "ppc_to_alpha_compiler.c"
#endif
#elif defined(EMU_I386)
#include "i386.h"
#ifndef USE_HAND_TRANSLATOR
#include "i386_compiler.c"
#else
#include "i386_to_ppc_compiler.c"
#endif
#endif

void
add_const_32 (word_32 val)
{
    constant_area[num_constants++] = val;
}

void
add_const_64 (word_64 val)
{
    constant_area[num_constants++] = val & 0xffffffff;
    constant_area[num_constants++] = val >> 32;
}

#if defined(ARCH_ALPHA)
#define add_addr_const     add_const_64
#elif defined(ARCH_PPC)
#define add_addr_const     add_const_32
#else
#error unsupported target architecture
#endif

word_64
get_const_64 (int index)
{
    /* FIXME: this is endian dependent */
    return (word_64)constant_area[index] + (((word_64)constant_area[index + 1]) << 32);
}

addr_t
lookup_fragment (word_32 addr)
{
    fragment_hash_supplement_t *supplement;
    fragment_hash_entry_t *entry = fragment_hash_get(addr, &supplement);

    if (entry == 0)
	return 0;
    return entry->native_addr;
}

#ifndef COMPILER_THRESHOLD
void
enter_fragment (word_32 foreign_addr, addr_t native_addr)
{
#ifdef PROFILE_LOOPS
    int i;
#endif

#ifdef PERIODIC_STAT_DUMP
    if (num_translated_blocks % 100 == 0)
	print_compiler_stats();
#endif

    fragment_hash_put(foreign_addr, &fragment_entry, &fragment_entry_supplement);
}
#endif

word_64
div_unsigned_64 (word_64 a, word_64 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

sword_64
div_signed_64 (sword_64 a, sword_64 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

word_64
mod_unsigned_64 (word_64 a, word_64 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

sword_64
mod_signed_64 (sword_64 a, sword_64 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

word_32
div_unsigned_32 (word_32 a, word_32 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

sword_32
div_signed_32 (sword_32 a, sword_32 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

word_32
mod_unsigned_32 (word_32 a, word_32 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

sword_32
mod_signed_32 (sword_32 a, sword_32 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

word_16
div_unsigned_16 (word_16 a, word_16 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

sword_16
div_signed_16 (sword_16 a, sword_16 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

word_16
mod_unsigned_16 (word_16 a, word_16 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

sword_16
mod_signed_16 (sword_16 a, sword_16 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

word_8
div_unsigned_8 (word_8 a, word_8 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

sword_8
div_signed_8 (sword_8 a, sword_8 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

word_8
mod_unsigned_8 (word_8 a, word_8 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

sword_8
mod_signed_8 (sword_8 a, sword_8 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

word_32
count_leading_zeros (word_32 w)
{
    word_32 m = 0x80000000;
    word_32 i;

#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif

    for (i = 0; i < 32; ++i)
    {
	if (w & m)
	    break;
	m >>= 1;
    }

    return i;
}

void
init_compiler (interpreter_t *intp, interpreter_t *dbg_intp)
{
    int i, j, range;
    int native;

    compiler_intp = intp;
#ifdef CROSSDEBUGGER
    debugger_intp = dbg_intp;
#endif

#ifdef NEED_REGISTER_ALLOCATOR
    i = 0;
    for (range = 0; integer_reg_range[range].end > 0; ++range)
    {
	for (native = integer_reg_range[range].begin; native < integer_reg_range[range].end; ++native)
	    native_integer_regs[i++] = native;
    }

    i = 0;
    for (native = FIRST_FLOAT_REG; native < FIRST_FLOAT_REG + NUM_FREE_FLOAT_REGS; ++native)
	native_float_regs[i++] = native;
#endif

    for (i = 0; i < NUM_TMP_INTEGER_REGS; ++i)
    {
	tmp_integer_regs[i].native_reg = FIRST_TMP_INTEGER_REG + i;
	tmp_integer_regs[i].free = 1;
    }

    for (i = 0; i < NUM_TMP_FLOAT_REGS; ++i)
    {
	tmp_float_regs[i].native_reg = FIRST_TMP_FLOAT_REG + i;
	tmp_float_regs[i].free = 1;
    }

#ifdef NEED_REGISTER_ALLOCATOR
    j = 0;
    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
	if (i >= 5 + 32 && i < 5 + 32 + 32)
	{
	    emu_regs[i].type = REG_TYPE_FLOAT;
	    emu_regs[i].preferred_native_reg = NO_REG;
	}
	else
	{
	    emu_regs[i].type = REG_TYPE_INTEGER;
	    emu_regs[i].preferred_native_reg = native_integer_regs[j++ % NUM_FREE_INTEGER_REGS];
	}
#endif

#ifdef SYNC_BLOCKS
    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	unresolved_jumps[i].addr = 0;
#endif

    emit_loc = code_area;
#ifdef DYNAMO_TRACES
    alt_emit_loc = code_area + MAX_CODE_INSNS;
#endif

    add_addr_const((addr_t)direct_dispatcher);
    add_addr_const((addr_t)indirect_dispatcher);
    add_addr_const((addr_t)system_call_entry);
    add_addr_const((addr_t)isync_entry);
    add_addr_const((addr_t)c_stub);

#if defined(ARCH_ALPHA)
    add_const_64((word_64)count_leading_zeros);
    add_const_64((word_64)div_unsigned_64);
    add_const_64((word_64)div_signed_64);
    add_const_64((word_64)mod_unsigned_64);
    add_const_64((word_64)mod_signed_64);
    add_const_64((word_64)div_unsigned_32);
    add_const_64((word_64)div_signed_32);
    add_const_64((word_64)mod_unsigned_32);
    add_const_64((word_64)mod_signed_32);
    add_const_64((word_64)div_unsigned_16);
    add_const_64((word_64)div_signed_16);
    add_const_64((word_64)mod_unsigned_16);
    add_const_64((word_64)mod_signed_16);
    add_const_64((word_64)div_unsigned_8);
    add_const_64((word_64)div_signed_8);
    add_const_64((word_64)mod_unsigned_8);
    add_const_64((word_64)mod_signed_8);

#ifdef COUNT_INSNS
    add_const_64(0);		/* emulated insns executed */
    add_const_64(0);		/* native insns executed */
    add_const_64(0);		/* fragments executed */
    add_const_64(0);		/* premature exits */
    add_const_64(0);		/* end exits */
    add_const_64(0);		/* emulated trace insns executed */
#ifdef SYNC_BLOCKS
    add_const_64(0);		/* empty sync blocks */
    add_const_64(0);		/* standard sync blocks */
    add_const_64(0);		/* custom sync blocks */
#endif
#endif

    for (i = 0; i < NUM_INSNS; ++i)
    {
	insn_infos[i].num_translated = 0;
	insn_infos[i].num_generated_insns = 0;
	insn_infos[i].num_generated_consts = 0;
    }
#elif defined(ARCH_PPC)
    add_addr_const((addr_t)repne_scasb_entry);
    add_addr_const((addr_t)rep_movsd_entry);
#else
#error unsupported target architecture
#endif

    num_constants_init = num_constants;
}

#if 0
addr_t
compile_until_jump (word_32 *addr, int optimize_taken_jump, label_t taken_jump_label, word_32 *target_addr)
{
    addr_t start = (addr_t)emit_loc;
    word_32 insnp = *addr;
#ifdef COLLECT_STATS
    addr_t x = start;
#endif
#ifdef EMU_I386
    int i;
#endif

#ifdef EMU_I386
    compute_liveness(compiler_intp, insnp);

    i = 0;

    compiler_intp->pc = insnp;
#endif

    have_jumped = 0;

    while (!have_jumped)
    {
	start_emu_insn(EMU_INSN_INSN, insnp);

#if defined(EMU_PPC)
#ifndef USE_HAND_TRANSLATOR
	compile_ppc_insn(mem_get_32(compiler_intp, insnp), insnp, optimize_taken_jump, taken_jump_label);
#else
	compile_to_alpha_ppc_insn(mem_get_32(compiler_intp, insnp), insnp, optimize_taken_jump, taken_jump_label,
				  NO_FOREIGN_ADDR, 0xffffffff, 0xffffffff, 0xffffffff);
#endif
#elif defined(EMU_I386)
#ifdef USE_HAND_TRANSLATOR
	compile_to_ppc_i386_insn(compiler_intp);
#else
	compile_i386_insn(compiler_intp, block_insns[i++].flags_killed);
#endif
#endif

#ifdef NO_REGISTER_CACHING
	store_and_free_all_foreign_regs(); /* FIXME */
#endif

#ifdef COLLECT_STATS
	assert(generated_insn_index < NUM_INSNS);
	++insn_infos[generated_insn_index].num_translated;
	insn_infos[generated_insn_index].num_generated_insns += ((addr_t)emit_loc - x) / 4;
#endif

#if defined(COLLECT_STATS)
	x = (addr_t)emit_loc;
#endif

#if defined(EMU_PPC)
	insnp += 4;
#elif defined(EMU_I386)
	insnp = compiler_intp->pc;
#endif
    }

#ifdef EMU_PPC
    if (optimize_taken_jump)
    {
	int num_targets, can_jump_indirectly, can_fall_through;
	word_32 target;

	jump_analyze_ppc_insn(mem_get_32(compiler_intp, insnp - 4), insnp - 4, &num_targets, &target, &can_fall_through, &can_jump_indirectly);

	assert(!can_jump_indirectly);
	assert(num_targets == 1);

	*target_addr = target;
    }
#endif

    *addr = insnp;

    return start;
}
#endif

#ifdef ARCH_ALPHA
void
emit_const_add (int const_index, int amount, word_32 **lda_loc)
{
    assert(amount >= -32768 && amount < 32768);

    emit(COMPOSE_LDQ(16, const_index * 4, CONSTANT_AREA_REG));
    if (lda_loc != 0)
	*lda_loc = &fragment_insns[num_fragment_insns];
    emit(COMPOSE_LDA(16, amount, 16));
    emit(COMPOSE_STQ(16, const_index * 4, CONSTANT_AREA_REG));

#ifdef COLLECT_STATS
    ++num_const_adds;
#endif
}

void
direct_emit_const_add (int const_index, int amount, word_32 **lda_loc)
{
    assert(amount >= -32768 && amount < 32768);

    direct_emit(COMPOSE_LDQ(16, const_index * 4, CONSTANT_AREA_REG));
    if (lda_loc != 0)
	*lda_loc = emit_loc;
    direct_emit(COMPOSE_LDA(16, amount, 16));
    direct_emit(COMPOSE_STQ(16, const_index * 4, CONSTANT_AREA_REG));

#ifdef COLLECT_STATS
    ++num_const_adds;
#endif
}

word_32
patch_mem_disp (word_32 insn, word_16 val)
{
    return (insn & 0xffff0000) | val;
}
#endif

#if defined(COLLECT_STATS) || defined(COUNT_INSNS)
unsigned long old_num_translated_insns;
#endif

addr_t
compile_basic_block (word_32 foreign_addr, int as_trace, unsigned char *preferred_alloced_integer_regs)
{
    static word_32 addrs[MAX_TRACE_INSNS];

    int length;
    addr_t native_addr;
    word_32 fallthrough_addr;
#ifdef COLLECT_LIVENESS
    word_32 live_cr, live_xer, live_gpr;
#endif

    /*
#ifdef COUNT_INSNS
    word_32 *emulated_lda_insn, *native_lda_insn, *old_emit_loc;

    old_num_translated_insns = num_translated_insns;
#endif
    */

#if defined(EMU_PPC)
#ifdef COLLECT_LIVENESS
    length = compute_iterative_liveness(compiler_intp, foreign_addr, addrs, &live_cr, &live_xer, &live_gpr);
#else
    length = compute_iterative_liveness(compiler_intp, foreign_addr, addrs, 0, 0, 0);
#endif
    fallthrough_addr = addrs[length - 1] + 4;
#elif defined(EMU_I386)
    length = compute_liveness(compiler_intp, foreign_addr, addrs, &fallthrough_addr);
#else
#error unsupported emulation
#endif

    native_addr = compile_trace(addrs, length, preferred_alloced_integer_regs,
				fallthrough_addr);

    /*
#ifdef COUNT_INSNS
    emit_const_add(FRAGMENTS_CONST, 1, 0);
    if (as_trace)
    {
	emit_const_add(END_EXITS_CONST, 1, 0);
	emit_const_add(EMULATED_TRACE_INSNS_CONST, 0, &emulated_lda_insn);
    }
    else
	emit_const_add(EMULATED_INSNS_CONST, 0, &emulated_lda_insn);
    emit_const_add(NATIVE_INSNS_CONST, 0, &native_lda_insn);

    old_emit_loc = emit_loc;
#endif
    */

#ifdef COLLECT_STATS
    if (!as_trace)
	++num_translated_blocks;

    /*
    {
	int i, used = 0;

	for (i = 0; i < NUM_EMU_REGISTERS; ++i)
	    if (regs_used_in_fragment[i])
		++used;

	printf("regs used in block: %d\n", used);
    }
    */
#endif

    /*
#ifdef COUNT_INSNS
    *emulated_lda_insn = patch_mem_disp(*emulated_lda_insn, num_translated_insns - old_num_translated_insns);
    *native_lda_insn = patch_mem_disp(*native_lda_insn, emit_loc - old_emit_loc);
#endif
    */

#ifdef COLLECT_LIVENESS
#if LIVENESS_DEPTH > 0
    fragment_entry_supplement.depth = LIVENESS_DEPTH;
#endif
    fragment_entry_supplement.live_cr = live_cr;
    fragment_entry_supplement.live_xer = live_xer;
    fragment_entry_supplement.live_gpr = live_gpr;
#endif

    return native_addr;
}

void
branch_profile_within_trace (void)
{
#ifdef COUNT_INSNS
    emit_const_add(PREMATURE_EXITS_CONST, 1, 0);
    emit_const_add(EMULATED_TRACE_INSNS_CONST, num_translated_insns - old_num_translated_insns, 0);
#endif
}

void
branch_profile_end_trace (void)
{
#ifdef COUNT_INSNS
    emit_const_add(END_EXITS_CONST, 1, 0);
    emit_const_add(EMULATED_TRACE_INSNS_CONST, num_translated_insns - old_num_translated_insns, 0);
#endif
}

#if 0
addr_t
compile_loop_trace (word_32 addr, int length, int bits)
{
    addr_t start;
    int bit;

#ifdef COLLECT_STATS
    old_num_translated_insns = num_translated_insns;
#endif

    start_timer();

    setjmp(compiler_restart);

    start = (addr_t)emit_loc;

    start_fragment();

    branch_profile_func = branch_profile_within_trace;

    for (bit = 1 << (length - 1); bit != 0; bit >>= 1)
    {
	if (bits & bit)
	{
	    label_t taken_jump_label = alloc_label();
	    word_32 target_addr;

	    compile_until_jump(&addr, 1, taken_jump_label, &target_addr);

	    emit_start_direct_jump(1);

#ifdef NEED_REGISTER_ALLOCATOR
	    emit_store_regs(EMU_INSN_INTERLUDE, addr);
#endif

	    emit_direct_jump(addr);

	    emit_label(taken_jump_label);
	    free_label(taken_jump_label);

	    addr = target_addr;
	}
	else
	    compile_until_jump(&addr, 0, 0, 0);
    }

    branch_profile_func = branch_profile_end_trace;

    compile_until_jump(&addr, 0, 0, 0);

    emit_start_direct_jump(1);

#ifdef NEED_REGISTER_ALLOCATOR
    emit_store_regs(EMU_INSN_EPILOGUE, addr);
#endif

    emit_direct_jump(addr);	/* this is not necessary if the jump at the end of the basic block was unconditional */

    branch_profile_func = 0;

    finish_fragment(0, 0);

    flush_icache();

    stop_timer();

#ifdef COLLECT_STATS
    ++num_translated_traces;

    num_translated_trace_insns += num_translated_insns - old_num_translated_insns;
#endif

    return start;
}
#endif

int
count_bits (word_32 x)
{
    int c = 0;

    while (x != 0)
    {
	c += x & 1;
	x >>= 1;
    }

    return c;
}

#ifdef COLLECT_STATS
unsigned long real_killed_bits = 0, killed_bits = 0;
#endif

addr_t
compile_trace (word_32 *addrs, int length, unsigned char *preferred_alloced_integer_regs,
	       word_32 fallthrough_addr)
{
    addr_t native_addr;
    int i;
#ifdef COUNT_INSNS
    word_32 *emulated_lda_insn, *native_lda_insn, *old_emit_loc;

    old_num_translated_insns = num_translated_insns;
#endif

    start_timer();

    setjmp(compiler_restart);

    start_fragment();

    /*
    while (((emit_loc - code_area) & 3) != 0)
	emit(0);
    */

    native_addr = (addr_t)emit_loc;

    for (i = 0; i < length; ++i)
    {
	start_emu_insn(EMU_INSN_INSN, i);

#if defined(EMU_PPC)
	compile_to_alpha_ppc_insn(mem_get_32(compiler_intp, addrs[i]), addrs[i], 0, 0,
				  i + 1 < length ? addrs[i + 1] : NO_FOREIGN_ADDR,
#ifdef COLLECT_LIVENESS
				  block_insns[i].killed_cr, block_insns[i].killed_xer, 0xffffffff
#else
				  0xffffffff, 0xffffffff, 0xffffffff
#endif
				  );
#elif defined(EMU_I386)
	{
	    word_32 old_pc = compiler_intp->pc;

	    compiler_intp->pc = addrs[i];
	    compile_to_ppc_i386_insn(compiler_intp);
	    compiler_intp->pc = old_pc;
	}
#else
#error unsupported emulation
#endif
    }

    emit_start_direct_jump(1);

#ifdef NEED_REGISTER_ALLOCATOR
    emit_store_regs(EMU_INSN_EPILOGUE, addrs[length - 1] + 4);
#endif

    if (fallthrough_addr != NO_FOREIGN_ADDR)
	emit_direct_jump(fallthrough_addr);

    finish_fragment(addrs, preferred_alloced_integer_regs);

    flush_icache(native_addr, emit_loc);

    stop_timer();

#ifdef COLLECT_STATS
    ++num_translated_traces;

    /*
    {
	int i, used = 0;

	for (i = 0; i < NUM_EMU_REGISTERS; ++i)
	    if (regs_used_in_fragment[i])
		++used;

	printf("regs used in block: %d\n", used);
    }
    */
#endif

#ifdef CHECK_TMP_REGS
    for (i = 0; i < NUM_TMP_INTEGER_REGS; ++i)
	assert(tmp_integer_regs[i].free);
#endif

    return native_addr;
}

#ifdef CROSSDEBUGGER
void
compare_register_sets (word_32 addr)
{
    int diff = 0;
    int i;

#if defined(EMU_PPC)
    word_32 live_cr, live_xer, live_gpr;

#ifdef COLLECT_LIVENESS
    fragment_hash_supplement_t *supplement = 0;
    fragment_hash_entry_t *entry = fragment_hash_get(addr, &supplement);

    if (entry != 0)
    {
	live_cr = supplement->live_cr;
	live_xer = supplement->live_xer;
	live_gpr = supplement->live_gpr;
    }
    else
#endif
	live_cr = live_xer = live_gpr = 0xffffffff;

    for (i = 0; i < 5; ++i)
    {
	word_32 mask;

	if (i == 1)
	    mask = live_cr;
	else if (i == 2)
	    mask = live_xer;
	else
	    mask = 0xffffffff;

	if ((compiler_intp->regs_SPR[i] & mask) != (debugger_intp->regs_SPR[i] & mask))
	{
	    diff = 1;
	    printf("diff for spr%d\n", i);
	}
    }
    for (i = 0; i < 32; ++i)
    {
	if ((live_gpr & (1 << i))
	    && compiler_intp->regs_GPR[i] != debugger_intp->regs_GPR[i])
	{
	    diff = 1;
	    printf("diff for gpr%d\n", i);
	}
	if (*(addr_t*)&compiler_intp->regs_FPR[i] != *(addr_t*)&debugger_intp->regs_FPR[i])
	{
	    diff = 1;
	    printf("diff for fpr%d\n", i);
	}
    }

    if (diff)
	printf("liveness      cr: %08x  xer: %08x  gpr: %08x\n", live_cr, live_xer, live_gpr);
#elif defined(EMU_I386)
    /*
    if (compiler_intp->regs_SPR[0] != debugger_intp->regs_SPR[0])
	diff = 1;
    */
    for (i = 0; i < 8; ++i)
    {
	if (compiler_intp->regs_GPR[i] != debugger_intp->regs_GPR[i])
	    diff = 1;
	if (compiler_intp->regs_FPST[i] != debugger_intp->regs_FPST[i])
	    diff = 1;
    }
    if (compiler_intp->regs_FSPR[0] != debugger_intp->regs_FSPR[0])
	diff = 1;
#endif

    if (diff)
    {
	printf("*** compiler\n");
	dump_registers(compiler_intp);
	printf("*** interpreter\n");
	dump_registers(debugger_intp);
	assert(0);
    }
}
#endif

#ifdef COMPILER_THRESHOLD
addr_t
interpret_until_threshold (word_32 addr)
{
    fragment_hash_entry_t *entry;

    move_regs_compiler_to_interpreter(compiler_intp);

    compiler_intp->pc = addr;

    for (;;)
    {
	fragment_hash_supplement_t *supplement;

	entry = fragment_hash_get(compiler_intp->pc, &supplement);
	if (entry == 0)
	{
	    fragment_hash_entry_t new;
	    fragment_hash_supplement_t new_supplement;

	    init_fragment_hash_entry(&new, &new_supplement);
	    new.native_addr = 0;
	    new.times_executed = 1;
	    fragment_hash_put(compiler_intp->pc, &new, &new_supplement);
	}
	else
	{
	    if (entry->native_addr != 0)
	    {
		move_regs_interpreter_to_compiler(compiler_intp);
		return entry->native_addr;
	    }

	    ++entry->times_executed;
	    if (entry->times_executed > COMPILER_THRESHOLD)
	    {
		entry->native_addr = compile_basic_block(compiler_intp->pc, 0, 0);
		move_regs_interpreter_to_compiler(compiler_intp);
		return entry->native_addr;
	    }
	}

	/* printf("interpreting from 0x%08x\n", compiler_intp->pc); */

	compiler_intp->have_jumped = 0;
	while (!compiler_intp->have_jumped)
	    interpret_insn(compiler_intp);

	/* printf("jumping to 0x%08x\n", compiler_intp->pc); */
    }
}
#endif

addr_t
compile_fragment_if_needed (word_32 addr, unsigned char *preferred_alloced_integer_regs)
{
    addr_t native_addr = lookup_fragment(addr);

    /*
    move_regs_compiler_to_interpreter(compiler_intp);
    dump_registers(compiler_intp);
    */

#ifdef COLLECT_STATS
    ++num_direct_and_indirect_jumps;
#endif

#if defined(CROSSDEBUGGER) && !defined(DYNAMO_TRACES)
    printf("*** jumping to %08x\n", addr);
    reset_mem_trace();
    trace_mem = 1;
    debugger_intp->have_jumped = 0;
    while (!debugger_intp->have_jumped)
	interpret_insn(debugger_intp);
    trace_mem = 0;
    move_regs_compiler_to_interpreter(compiler_intp);
    compare_register_sets(addr);
    compare_mem_writes(debugger_intp, compiler_intp);
    if (debugger_intp->pc != addr)
    {
	printf("different target: should be %08x, is %08x\n",
	       debugger_intp->pc, addr);
	assert(0);
    }
#endif

#ifdef DYNAMO_TRACES
    return dynamo_runner(addr);
#else
    if (native_addr != 0)
	return native_addr;

#ifdef COMPILER_THRESHOLD
#ifdef PROFILE_LOOPS
    return loop_profiler(compiler_intp, addr);
#else
    return interpret_until_threshold(addr);
#endif
#else
    native_addr = compile_basic_block(addr, 0, preferred_alloced_integer_regs);
    enter_fragment(addr, native_addr);

    return native_addr;
#endif
#endif
}

addr_t
provide_fragment (word_32 addr)
{
    return compile_fragment_if_needed(addr, 0);
}

#ifdef SYNC_BLOCKS
void
shuffle_regs (unsigned char *src, unsigned char *dst, int i)
{
    int j;
    unsigned char reg;

    if (src[i] == ALLOCED_REG_NONE)
	return;

    for (j = 0; j < NUM_FREE_INTEGER_REGS; ++j)
	if ((src[i] & ALLOCED_REG_REG_MASK) == (dst[j] & ALLOCED_REG_REG_MASK))
	    break;

    assert(j < NUM_FREE_INTEGER_REGS);

    reg = src[i];

    shuffle_regs(src, dst, j);

    assert(src[i] == reg);

    assert(src[j] == ALLOCED_REG_NONE);

    direct_emit(COMPOSE_MOV(native_integer_regs[i], native_integer_regs[j]));

    src[i] = ALLOCED_REG_NONE;
    src[j] = reg;
}

#define SYNC_BLOCK_NONE          1
#define SYNC_BLOCK_STANDARD      2
#define SYNC_BLOCK_CUSTOM        3

int
generate_sync_block (unsigned char *src, unsigned char *dst, word_32 foreign_addr, addr_t native_addr)
{
    int i;

    /* first check whether we have to generate a sync block at all */
    for (i = 0; i < NUM_FREE_INTEGER_REGS; ++i)
	if (!(src[i] == dst[i]
	      || ((src[i] & ALLOCED_REG_REG_MASK) == (dst[i] & ALLOCED_REG_REG_MASK)
		  && (!(src[i] & ALLOCED_REG_DIRTY) || (dst[i] & ALLOCED_REG_SAVED)))
	      || (src[i] != ALLOCED_REG_NONE && !(src[i] & ALLOCED_REG_DIRTY) && dst[i] == ALLOCED_REG_NONE)))
	    break;

    if (i == NUM_FREE_INTEGER_REGS)
    {
	/* printf("no sync block\n\n"); */
	return SYNC_BLOCK_NONE;
    }

    /* now check whether it is equally (or more) efficient to use the
       standard epilogue/prologue */
    for (i = 0; i < NUM_FREE_INTEGER_REGS; ++i)
	if (dst[i] != ALLOCED_REG_NONE)
	{
	    int j;

	    for (j = 0; j < NUM_FREE_INTEGER_REGS; ++j)
		if (src[j] != ALLOCED_REG_NONE && (src[j] & ALLOCED_REG_REG_MASK) == (dst[i] & ALLOCED_REG_REG_MASK))
		    break;

	    if (j < NUM_FREE_INTEGER_REGS)
		break;
	}
    

    if (i == NUM_FREE_INTEGER_REGS)
    {
	/* printf("standard prologue/epilogue\n\n"); */
	return SYNC_BLOCK_STANDARD;
    }

    for (i = 0; i < NUM_FREE_INTEGER_REGS; ++i)
	if (src[i] != ALLOCED_REG_NONE)
	{
	    int j;

	    if (src[i] & ALLOCED_REG_DIRTY)
	    {
		for (j = 0; j < NUM_FREE_INTEGER_REGS; ++j)
		    if ((src[i] & ALLOCED_REG_REG_MASK) == (dst[j] & ALLOCED_REG_REG_MASK))
			break;

		if ((j < NUM_FREE_INTEGER_REGS && !(dst[j] & ALLOCED_REG_SAVED))
		    || j == NUM_FREE_INTEGER_REGS)
		{
		    store_reg(native_integer_regs[i], src[i] & ALLOCED_REG_REG_MASK);
		    if (j < NUM_FREE_INTEGER_REGS)
			src[i] &= ~ALLOCED_REG_DIRTY;
		    else
			src[i] = ALLOCED_REG_NONE;
		}
	    }
	    else
	    {
		for (j = 0; j < NUM_FREE_INTEGER_REGS; ++j)
		    if ((src[i] & ALLOCED_REG_REG_MASK) == (dst[j] & ALLOCED_REG_REG_MASK))
			break;

		if (j == NUM_FREE_INTEGER_REGS)
		    src[i] = ALLOCED_REG_NONE;
	    }
	}

    for (i = 0; i < NUM_FREE_INTEGER_REGS; ++i)
    {
	if (src[i] != ALLOCED_REG_NONE && (src[i] & ALLOCED_REG_REG_MASK) != (dst[i] & ALLOCED_REG_REG_MASK))
	{
	    int j;

	    for (j = 0; j < NUM_FREE_INTEGER_REGS; ++j)
		if ((src[i] & ALLOCED_REG_REG_MASK) == (dst[j] & ALLOCED_REG_REG_MASK))
		    break;

	    assert(j < NUM_FREE_INTEGER_REGS);

	    if (src[j] == ALLOCED_REG_NONE)
	    {
		direct_emit(COMPOSE_MOV(native_integer_regs[i], native_integer_regs[j]));
		src[j] = src[i];
		src[i] = ALLOCED_REG_NONE;
	    }
	    else
	    {
		reg_t reg = src[i] & ALLOCED_REG_REG_MASK;

		direct_emit(COMPOSE_MOV(native_integer_regs[i], 0));
		src[i] = ALLOCED_REG_NONE;

		shuffle_regs(src, dst, j);

		assert(src[j] == ALLOCED_REG_NONE);

		direct_emit(COMPOSE_MOV(0, native_integer_regs[j]));
		src[j] = reg;
	    }
	}
    }

    for (i = 0; i < NUM_FREE_INTEGER_REGS; ++i)
	if (src[i] == ALLOCED_REG_NONE && dst[i] != ALLOCED_REG_NONE)
	{
	    load_reg(native_integer_regs[i], dst[i] & ALLOCED_REG_REG_MASK);
	    src[i] = dst[i] & ALLOCED_REG_REG_MASK;
	}


    for (i = 0; i < NUM_FREE_INTEGER_REGS; ++i)
	assert((src[i] == ALLOCED_REG_NONE && dst[i] == ALLOCED_REG_NONE) || (src[i] & ALLOCED_REG_REG_MASK) == (dst[i] & ALLOCED_REG_REG_MASK));

    return SYNC_BLOCK_CUSTOM;
}
#endif

#if defined(ARCH_ALPHA)
word_32
compose_branch (addr_t branch_addr, addr_t target_addr)
{
    sword_64 disp = target_addr - branch_addr - 4;
    word_32 field;

    assert(disp >= -(1 << 22) && disp < (1 << 22));
    field = (disp >> 2) & 0x1fffff;

    return COMPOSE_BR(31, field);
}
#elif defined(ARCH_PPC)
word_32
compose_branch (addr_t branch_addr, addr_t target_addr)
{
    sword_32 disp = target_addr - branch_addr;
    word_32 field;

    /* printf("composing from %08x to %08x\n", branch_addr, target_addr); */

    assert(disp >= -(1 << 25) && disp < (1 << 25));
    field = (disp >> 2) & 0xffffff;

    return COMPOSE_B(field);
}
#else
#error unsupported target architecture
#endif

addr_t
provide_fragment_and_patch (addr_t jump_addr)
{
    int i;
    addr_t native_addr;
    word_32 foreign_addr;

#if defined(COLLECT_STATS) && !defined(COMPILER_THRESHOLD)
    ++num_direct_jumps;
#endif

#ifdef SYNC_BLOCKS
    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	if (unresolved_jumps[i].addr == jump_addr)
	    break;

    assert(i < MAX_UNRESOLVED_JUMPS);

    foreign_addr = unresolved_jumps[i].target;
#else
    foreign_addr = *(word_32*)jump_addr;
    /* printf("jumping to %08x\n", foreign_addr); */
#endif

#if defined(COMPILER_THRESHOLD) || defined(DYNAMO_TRACES)
    native_addr = lookup_fragment(foreign_addr);

    if (native_addr == 0)
#if defined(DYNAMO_TRACES)
	return dynamo_runner(foreign_addr);
#elif defined(PROFILE_LOOPS)
	return loop_profiler(compiler_intp, foreign_addr);
#else
	return interpret_until_threshold(foreign_addr);
#endif
    else
#endif
    {
#if defined(COLLECT_STATS) && defined(COMPILER_THRESHOLD)
	++num_patched_direct_jumps;
#endif

#if defined(DYNAMO_TRACES) && defined(CROSSDEBUGGER)
	assert(native_addr != 0);
	/* native_addr = dynamo_runner(foreign_addr); */
#elif !defined(COMPILER_THRESHOLD) && !defined(DYNAMO_TRACES)
#ifdef SYNC_BLOCKS
	native_addr = compile_fragment_if_needed(foreign_addr, unresolved_jumps[i].alloced_integer_regs);
#else
	native_addr = provide_fragment(foreign_addr);
#endif
#endif

#ifdef SYNC_BLOCKS
	{
	    fragment_hash_supplement_t *supplement;
	    fragment_hash_entry_t *entry = fragment_hash_get(foreign_addr, &supplement);
	    int j;
	    int sync;
	    word_32 *sync_block_start = emit_loc;
	    int num_epilogue_insns = (word_32*)jump_addr - (word_32*)unresolved_jumps[i].start;

	    start_timer();

	    if (setjmp(compiler_restart))
		return provide_fragment(foreign_addr);

	    assert(entry != 0);

	    /*
	    printf("src: ");
	    for (j = 0; j < NUM_FREE_INTEGER_REGS; ++j)
		if (unresolved_jumps[i].alloced_integer_regs[j] != ALLOCED_REG_NONE)
		    printf("$%d -> %d%s ", native_integer_regs[j], unresolved_jumps[i].alloced_integer_regs[j] & ALLOCED_REG_REG_MASK,
			   (unresolved_jumps[i].alloced_integer_regs[j] & ALLOCED_REG_DIRTY) ? " (d)" : "");
	    printf("\ndst (%08x): ", foreign_addr);
	    for (j = 0; j < NUM_FREE_INTEGER_REGS; ++j)
		if (supplement->alloced_integer_regs[j] != ALLOCED_REG_NONE)
		    printf("$%d -> %d%s ", native_integer_regs[j], supplement->alloced_integer_regs[j] & ALLOCED_REG_REG_MASK,
			   (supplement->alloced_integer_regs[j] & ALLOCED_REG_SAVED) ? " (s)" : "");
	    printf("\n\n");
	    */

	    for (j = 0; j < NUM_FREE_FLOAT_REGS; ++j)
		if (supplement->alloced_float_regs[j] != ALLOCED_REG_NONE || unresolved_jumps[i].alloced_float_regs[j] != ALLOCED_REG_NONE)
		    break;

	    if (j == NUM_FREE_FLOAT_REGS)
		sync = generate_sync_block(unresolved_jumps[i].alloced_integer_regs, supplement->alloced_integer_regs, foreign_addr, native_addr);
	    else
	    {
		/* printf("float regs involved\n"); */
		sync = SYNC_BLOCK_STANDARD;
	    }

	    /*
	    if (sync != SYNC_BLOCK_NONE)
		sync = SYNC_BLOCK_STANDARD;
	    */

#ifdef COLLECT_STATS
	    if (sync == SYNC_BLOCK_CUSTOM)
		++num_sync_blocks;
#endif

	    /*
	    if (sync == SYNC_BLOCK_CUSTOM)
	    {
		printf("\n\nreplacing\n");
		disassemble_alpha_code(unresolved_jumps[i].start, jump_addr);
		printf("and\n");
		disassemble_alpha_code(native_addr, supplement->synced_native_addr);
		printf("with\n");
		disassemble_alpha_code(sync_block_start, emit_loc);
		printf("\n\n");
	    }
	    */

	    if (sync == SYNC_BLOCK_CUSTOM && emit_loc - sync_block_start + 1 <= num_epilogue_insns + NUM_AFTER_JUMP_INSN_SLOTS)
	    {
		word_32 *branch_loc = (word_32*)unresolved_jumps[i].start + (emit_loc - sync_block_start);

		memcpy((void*)unresolved_jumps[i].start, sync_block_start, (emit_loc - sync_block_start) * 4);
		*branch_loc = compose_branch((addr_t)branch_loc, supplement->synced_native_addr);

		emit_loc = sync_block_start; /* revert emitting */

#ifdef COLLECT_STATS
		++num_sync_blocks_in_situ;
#endif
	    }
	    else
	    {
		if (sync != SYNC_BLOCK_NONE)
		    sync = SYNC_BLOCK_STANDARD;

		if (sync == SYNC_BLOCK_NONE)
		{
#ifdef COLLECT_STATS
		    ++num_empty_sync_blocks;
#endif

#ifdef COUNT_INSNS
		    direct_emit_const_add(EMPTY_SYNC_CONST, 1, 0);

		    *(word_32*)unresolved_jumps[i].start = compose_branch(unresolved_jumps[i].start, sync_block_start);
		    direct_emit(compose_branch((addr_t)emit_loc, supplement->synced_native_addr));
#else
		    *(word_32*)unresolved_jumps[i].start = compose_branch(unresolved_jumps[i].start, supplement->synced_native_addr);
#endif
		}
		else if (sync == SYNC_BLOCK_STANDARD)
		{
		    jump_addr -= 8;

#ifdef COUNT_INSNS
		    direct_emit_const_add(STANDARD_SYNC_CONST, 1, 0);

		    *(word_32*)jump_addr = compose_branch(jump_addr, sync_block_start);
		    direct_emit(compose_branch((addr_t)emit_loc, supplement->native_addr));
#else
		    *(word_32*)jump_addr = compose_branch(jump_addr, native_addr);
#endif
		}
		else
		{
		    assert(sync == SYNC_BLOCK_CUSTOM);

#ifdef COUNT_INSNS
		    direct_emit_const_add(CUSTOM_SYNC_CONST, 1, 0);
#endif

		    *(word_32*)unresolved_jumps[i].start = compose_branch(unresolved_jumps[i].start, sync_block_start);
		    direct_emit(compose_branch((addr_t)emit_loc, supplement->synced_native_addr));
		}
	    }

	    unresolved_jumps[i].addr = 0;

	    stop_timer();

	    flush_icache();
	}
#else
#if !defined(CROSSDEBUGGER) && !defined(NO_PATCHING)
	/* printf("patching at %lx\n", jump_addr); */

#if defined(ARCH_ALPHA)
	jump_addr -= 8;
#elif defined(ARCH_PPC)
	jump_addr -= 12;
#else
#error unsupported target architecture
#endif

	*(word_32*)jump_addr = compose_branch(jump_addr, native_addr);

#ifdef SYNC_BLOCKS
	unresolved_jumps[i].addr = 0;
#endif

	flush_icache(jump_addr, jump_addr + 4);
#endif
#endif

	return native_addr;
    }
}

addr_t
compile_and_return_first_native_addr (word_32 addr, int regs_in_compiler)
{
    addr_t native_addr;

    if (!regs_in_compiler)
	move_regs_interpreter_to_compiler(compiler_intp);

#ifdef DYNAMO_TRACES
    native_addr = dynamo_runner(addr);
#else
#ifdef COMPILER_THRESHOLD
#ifdef PROFILE_LOOPS
    native_addr = loop_profiler(compiler_intp, addr);
#else
    native_addr = interpret_until_threshold(addr);
#endif
#else
    native_addr = compile_basic_block(addr, 0, 0);
#endif
#endif

    return native_addr;
}

void
start_compiler (word_32 addr)
{
    start_execution(compile_and_return_first_native_addr(addr, 0)); /* this call never returns */
}

addr_t
isync_handler (word_32 addr)
{
    int i;

#ifdef COLLECT_STATS
    ++num_isyncs;
#endif

    reinit_compiler();

    return compile_and_return_first_native_addr(addr, 1);
}

void
print_compiler_stats (void)
{
#ifdef COLLECT_STATS
    int i;

#ifdef COMPILER_THRESHOLD
    printf("patched direct jumps:          %lu\n", num_patched_direct_jumps);
    printf("indirect jumps:                %lu\n", num_direct_and_indirect_jumps);
#else
    printf("patched direct jumps:          %lu\n", num_direct_jumps);
    printf("indirect jumps:                %lu\n", num_direct_and_indirect_jumps - num_direct_jumps);
#endif
    printf("fragment hash misses:          %lu\n", num_fragment_hash_misses);
    printf("translated blocks:             %lu\n", num_translated_blocks);
    printf("translated traces:             %lu\n", num_translated_traces);
    printf("translated insns:              %lu\n", num_translated_insns);
    printf("translated insns in traces:    %lu\n", num_translated_trace_insns);
    printf("generated insns:               %lu\n", emit_loc - code_area - 3 * num_const_adds);
    printf("load/store reg insns:          %lu\n", num_load_store_reg_insns);
    printf("constants:                     %d\n", num_constants - (NUM_EMU_REGISTERS * 2 + 2));
    printf("sync blocks:                   %ld\n", num_sync_blocks);
    printf("sync blocks in situ:           %ld\n", num_sync_blocks_in_situ);
    printf("empty sync blocks:             %ld\n", num_empty_sync_blocks);
    printf("stub calls:                    %lu\n", num_stub_calls);
    printf("loop profiler calls:           %lu\n", num_loop_profiler_calls);
    printf("isyncs:                        %lu\n", num_isyncs);

    printf("real killed cr bits:           %lu\n", real_killed_bits);
    printf("killed cr bits:                %lu\n", killed_bits);

#ifdef COLLECT_PPC_FPR_STATS
    for (i = 0; i < 32; ++i)
	printf("f%-2d: %u\n", i, fpr_uses[i]);
#endif
#endif

#ifdef COUNT_INSNS
    printf("blocks executed:               %lu\n", get_const_64(FRAGMENTS_CONST));
    printf("premature trace exits:         %lu\n", get_const_64(PREMATURE_EXITS_CONST));
    printf("end trace exits:               %lu\n", get_const_64(END_EXITS_CONST));
    printf("emulated block insns executed: %lu\n", get_const_64(EMULATED_INSNS_CONST));
    printf("emulated trace insns executed: %lu\n", get_const_64(EMULATED_TRACE_INSNS_CONST));
    printf("native block insns executed:   %lu\n", get_const_64(NATIVE_INSNS_CONST));
#ifdef SYNC_BLOCKS
    printf("empty sync blocks:             %lu\n", get_const_64(EMPTY_SYNC_CONST));
    printf("standard sync blocks:          %lu\n", get_const_64(STANDARD_SYNC_CONST));
    printf("custom sync blocks:            %lu\n", get_const_64(CUSTOM_SYNC_CONST));
#endif
#endif

#ifdef COLLECT_STATS
#ifndef USE_HAND_TRANSLATOR
    for (i = 0; i < NUM_INSNS; ++i)
	if (insn_infos[i].num_translated > 0)
	    printf("  %-20s       %10d  %10d\n", insn_names[i], insn_infos[i].num_translated, insn_infos[i].num_generated_insns);
#endif
#endif

#ifdef MEASURE_TIME
    printf("time spent in compiler: %lu\n", compiler_time);
#endif

#ifdef COLLECT_LIVENESS
    save_liveness_info();
#endif
}

void
handle_compiler_system_call (void)
{
    move_regs_compiler_to_interpreter(compiler_intp);
    /*
    printf("*** system call\n");
    dump_ppc_registers(compiler_intp);
    */

    handle_system_call(compiler_intp);

    move_regs_interpreter_to_compiler(compiler_intp);
}
#endif
