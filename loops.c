/*
 * loops.c
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

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "bintrans.h"
#if defined(PROFILE_LOOPS) || defined(DYNAMO_TRACES)
#include "fragment_hash.h"
#include "compiler.h"

#define COUNT_POOL_SIZE             32768

#ifdef DYNAMO_TRACES
int current_dynamo_trace_length;
word_32 current_dynamo_trace[MAX_TRACE_INSNS];
extern fragment_hash_supplement_t fragment_entry_supplement;
#endif

#ifdef PROFILE_LOOPS
trace_count_t count_pool[COUNT_POOL_SIZE];
int pool_mark = 0;

fragment_hash_entry_t *fragment_history[MAX_TRACE_BLOCKS];
int branch_history[MAX_TRACE_JUMPS];

int
pool_alloc (int size)
{
    int mark;

    assert(pool_mark + size <= COUNT_POOL_SIZE);

    mark = pool_mark;
    pool_mark += size;

    return mark;
}

void
inc_trace (int num_jumps)
{
    int index = 0;
    int i;
    fragment_hash_entry_t *entry = fragment_history[num_jumps];
    int pool_index;

    if (num_jumps == 0)
	++entry->trace0_count;
    else
    {
	for (i = num_jumps - 1; i >= 0; --i)
	    index = (index << 1) | branch_history[i];

	if (entry->trace_pool_indexes[num_jumps - 1] == -1)
	{
	    entry->trace_pool_indexes[num_jumps - 1] = pool_alloc(1 << num_jumps);
	    /* printf("new loop with %d jumps\n", num_jumps); */
	}
	assert(index >= 0 && index < (1 << num_jumps));
	pool_index = entry->trace_pool_indexes[num_jumps - 1];
	++count_pool[pool_index + index];
    }
}

fragment_hash_entry_t*
make_entry_for_addr (word_32 addr)
{
    fragment_hash_entry_t new;

    init_fragment_hash_entry(&new);
    return fragment_hash_put(addr, &new);
}

void
clear_history (void)
{
    int i;

    for (i = 0; i < MAX_TRACE_JUMPS; ++i)
    {
	branch_history[i] = -1;
	fragment_history[i] = 0;
    }
    fragment_history[i] = 0;
}

#ifdef COMPILER_THRESHOLD
word_64
compile_block_or_trace (fragment_hash_entry_t *entry)
{
    trace_count_t best_count = -1;
    int best_length = -1;
    int best_bits = 0;
    int i;

    if (entry->trace0_count > 0)
    {
	best_length = 0;
	best_count = entry->trace0_count;
    }

    for (i = 0; i < MAX_TRACE_JUMPS; ++i)
	if (entry->trace_pool_indexes[i] != -1)
	{
	    int j;

	    for (j = 0; j < (2 << i); ++j)
	    {
		trace_count_t count = count_pool[entry->trace_pool_indexes[i] + j];

		if (count > 0 && (best_length < 0 || count > best_count))
		{
		    best_length = i + 1;
		    best_count = count;
		    best_bits = j;
		}
	    }
	}

    if (best_length < 0)
    {
	printf("compiling basic block at %08x\n", entry->foreign_addr);
	return entry->native_addr = compile_basic_block(entry->foreign_addr, 0);
    }
    else if (best_length == 0)
    {
	printf("compiling trace at %08x with length 0\n", entry->foreign_addr);
	return entry->native_addr = compile_basic_block(entry->foreign_addr, 1);
    }
    else
    {
	printf("compiling trace at %08x with length %d, bits %x\n", entry->foreign_addr, best_length, best_bits);
	return entry->native_addr = compile_loop_trace(entry->foreign_addr, best_length, best_bits);
    }
}
#endif

#ifdef COLLECT_STATS
extern long num_loop_profiler_calls;
#endif

#ifdef COMPILER_THRESHOLD
word_64
loop_profiler (interpreter_t *intp, word_32 addr)
#else
void
loop_profiler (interpreter_t *intp)
#endif
{
    fragment_hash_entry_t *entry = 0;
    int i;
    int num_targets;
    word_32 target;
    int can_jump_indirectly, can_fall_through;

#if defined(COLLECT_STATS) && defined(NEED_COMPILER)
    ++num_loop_profiler_calls;
#endif

    /* printf("entering loop profiler at %08x\n", addr); */

    clear_history();

#ifdef COMPILER_THRESHOLD
    move_regs_compiler_to_interpreter(compiler_intp);

    compiler_intp->pc = addr;
#endif

    for (;;)
    {
	word_32 prev_pc;
	int branch;

	if (entry == 0)
	{
	    entry = fragment_hash_get(intp->pc);
	    if (entry == 0)
		entry = make_entry_for_addr(intp->pc);
	}

#ifdef COMPILER_THRESHOLD
	if (entry->native_addr != 0)
	{
	    move_regs_interpreter_to_compiler(intp);

	    return entry->native_addr;
	}
#endif

	++entry->times_executed;

#ifdef COMPILER_THRESHOLD
	if (entry->times_executed > COMPILER_THRESHOLD)
	{
	    word_64 native_addr = compile_block_or_trace(entry);

	    move_regs_interpreter_to_compiler(intp);

	    return native_addr;
	}
#endif

	intp->have_jumped = 0;

	do
	{
	    prev_pc = intp->pc;

	    interpret_insn(intp);
	} while (!intp->have_jumped);

	entry = fragment_hash_get(intp->pc);

	if (entry == 0)
	    entry = make_entry_for_addr(intp->pc);

	jump_analyze_ppc_insn(mem_get_32(intp, prev_pc), prev_pc, &num_targets, &target, &can_fall_through, &can_jump_indirectly);
 
	if (can_jump_indirectly)
	    clear_history();
	else
	{
	    branch = (intp->pc == prev_pc + 4) ? 0 : 1;

	    assert(entry != 0);
	    for (i = 0; i < MAX_TRACE_BLOCKS; ++i)
		if (fragment_history[i] == entry)
		    break;
	    if (i < MAX_TRACE_BLOCKS && prev_pc > intp->pc)
	    {
		/*
		  int j;

		  printf("loop:\n");
		  for (j = i; j >= 0; --j)
	          printf("  0x%08x\n", fragment_history[j]->foreign_addr);
	    */

		inc_trace(i);
	    }

	    fragment_history[MAX_TRACE_BLOCKS - 1] = fragment_history[MAX_TRACE_BLOCKS - 2];
	    for (i = MAX_TRACE_JUMPS - 1; i > 0; --i)
	    {
		branch_history[i] = branch_history[i - 1];
		fragment_history[i] = fragment_history[i - 1];
	    }

	    branch_history[0] = branch;
	    fragment_history[0] = entry;
	}
    }
}

void
print_loop_stats (void)
{
#ifdef COLLECT_STATS
    int i;

#ifdef PROFILE_FRAGMENTS
    printf("\nfragment execution counts:\n");
    for (i = 0; i < FRAGMENT_HASH_ENTRIES; ++i)
	if (fragment_hash_table[i].foreign_addr != (word_32)-1)
	    printf("  %08x: %10ld\n", fragment_hash_table[i].foreign_addr, fragment_hash_table[i].times_executed);
#endif

    printf("\ntrace execution counts:\n");
    for (i = 0; i < FRAGMENT_HASH_ENTRIES; ++i)
	if (fragment_hash_table[i].foreign_addr != (word_32)-1)
	{
	    int have_traces = 0;
	    int j;

	    if (fragment_hash_table[i].trace0_count > 0)
		have_traces = 1;
	    else
	    {
		for (j = 0; j < MAX_TRACE_JUMPS; ++j)
		    if (fragment_hash_table[i].trace_pool_indexes[j] != -1)
			have_traces = 1;
	    }

	    if (have_traces)
	    {
		printf("  %08x\n", fragment_hash_table[i].foreign_addr);
		if (fragment_hash_table[i].trace0_count > 0)
		    printf("    - : %d\n", fragment_hash_table[i].trace0_count);
		for (j = 0; j < MAX_TRACE_JUMPS; ++j)
		    if (fragment_hash_table[i].trace_pool_indexes[j] != -1)
		    {
			int k;

			for (k = 0; k < (2 << j); ++k)
			{
			    trace_count_t count = count_pool[fragment_hash_table[i].trace_pool_indexes[j] + k];

			    if (count > 0)
			    {
				trace_count_t m;
				printf("    ");

				for (m = 1 << j; m != 0; m >>= 1)
				    printf("%c ", (k & m) == 0 ? '0' : '1');
				printf(": %d\n", count);
			    }
			}
		    }
	    }
	}

    printf("\npool: %d/%d\n", pool_mark, COUNT_POOL_SIZE);
#endif
}

void
init_loops (void)
{
    int i;

    for (i = 0; i < COUNT_POOL_SIZE; ++i)
	count_pool[i] = 0;
    clear_history();
}
#endif

#ifdef DYNAMO_TRACES
void
run_until_fragment_start (interpreter_t *intp)
{
    for (;;)
    {
	word_32 prev_pc = intp->pc;

	intp->have_jumped = 0;
	intp->have_syscalled = 0;
	interpret_insn(intp);

	if (intp->have_syscalled)
	    break;
	if (intp->have_jumped)
	{
	    int num_targets, can_fall_through, can_jump_indirectly;
	    word_32 target;

	    jump_analyze_ppc_insn(mem_get_32(intp, prev_pc), prev_pc, &num_targets, &target, &can_fall_through, &can_jump_indirectly);

	    if (!can_jump_indirectly && prev_pc > intp->pc)
		break;
	}
    }
}

void
collect_dynamo_trace (interpreter_t *intp)
{
    current_dynamo_trace_length = 0;

    for (;;)
    {
	word_32 prev_pc = intp->pc;
	int i;

	current_dynamo_trace[current_dynamo_trace_length++] = intp->pc;

	intp->have_jumped = 0;
	intp->have_syscalled = 0;
	interpret_insn(intp);

	if (intp->have_syscalled)
	    break;
	if (intp->have_jumped)
	{
	    int num_targets, can_fall_through, can_jump_indirectly;
	    word_32 target;

	    jump_analyze_ppc_insn(mem_get_32(intp, prev_pc), prev_pc, &num_targets, &target, &can_fall_through, &can_jump_indirectly);

	    if (!can_jump_indirectly && prev_pc > intp->pc)
		break;
	}
	if (current_dynamo_trace_length == MAX_DYNAMO_TRACE)
	    break;
	for (i = 0; i < current_dynamo_trace_length; ++i) /* FIXME: this is quadratic! */
	    if (intp->pc == current_dynamo_trace[i])
		return;
    }
}

#ifdef CROSSDEBUGGER
word_32 last_native = 0;
#endif

word_64
dynamo_runner (word_32 addr)
{
    move_regs_compiler_to_interpreter(compiler_intp);

    compiler_intp->pc = addr;

#ifdef CROSSDEBUGGER
    /* printf("*** jumping to %08x\n", addr); */
    compare_mem_writes(debugger_intp, compiler_intp);
#endif

    for (;;)
    {
	fragment_hash_entry_t *entry;
	fragment_hash_supplement_t *supp;

#ifdef CROSSDEBUGGER
	assert(compiler_intp->pc == debugger_intp->pc);
	compare_register_sets();
#endif

	entry = fragment_hash_get(compiler_intp->pc, &supp);

	if (entry != 0)
	{
	    if (entry->native_addr != 0)
	    {
#ifdef CROSSDEBUGGER
		int trace_index = 0;

		assert(supp->insn_addrs != 0 && supp->insn_addrs[0] == debugger_intp->pc);

		reset_mem_trace();
		trace_mem = 1;

		debugger_intp->have_jumped = 0;

		for (;;)
		{
		    if (trace_index == supp->num_insns || debugger_intp->pc != supp->insn_addrs[trace_index])
			break;

		    interpret_insn(debugger_intp);
		    ++trace_index;
		}

		trace_mem = 0;
#endif

		move_regs_interpreter_to_compiler(compiler_intp);

#ifdef CROSSDEBUGGER
		/* printf("*** native from %08x\n", compiler_intp->pc); */
		last_native = compiler_intp->pc;
#endif

		return entry->native_addr;
	    }
	    else
	    {
		if (++supp->times_executed >= DYNAMO_THRESHOLD)
		{
		    word_32 foreign_addr = compiler_intp->pc;
		    word_64 native_addr;
		    fragment_hash_supplement_t *dummy;

		    collect_dynamo_trace(compiler_intp);

		    native_addr = compile_trace(current_dynamo_trace, current_dynamo_trace_length);

		    if (fragment_hash_get(foreign_addr, &dummy) == 0) /* cache flush */
		    {
#ifdef CROSSDEBUGGER
			assert(0);
#else
			fragment_hash_entry_t new_entry;
			fragment_hash_supplement_t supplement;

			init_fragment_hash_entry(&new_entry, &supplement);

			new_entry.native_addr = native_addr;
			supplement.times_executed = 1;

			fragment_hash_put(foreign_addr, &new_entry, &supplement);

			entry = fragment_hash_get(foreign_addr, &supp);
			assert(entry != 0);
#endif
		    }

		    entry->native_addr = native_addr;

#ifdef SYNC_BLOCKS
		    supp->synced_native_addr = fragment_entry_supplement.synced_native_addr;
		    memcpy(supp->alloced_integer_regs, fragment_entry_supplement.alloced_integer_regs,
			   sizeof(fragment_entry_supplement.alloced_integer_regs));
		    memcpy(supp->alloced_float_regs, fragment_entry_supplement.alloced_float_regs,
			   sizeof(fragment_entry_supplement.alloced_float_regs));
#endif

#ifdef CROSSDEBUGGER
		    supp->num_insns = current_dynamo_trace_length;
		    supp->insn_addrs = (word_32*)malloc(sizeof(word_32) * supp->num_insns);
		    memcpy(supp->insn_addrs, current_dynamo_trace, sizeof(word_32) * supp->num_insns);

		    collect_dynamo_trace(debugger_intp);

		    assert(compiler_intp->pc == debugger_intp->pc);
		    compare_register_sets();
#endif

		    continue;

		    /*
		    move_regs_interpreter_to_compiler(compiler_intp);

		    return entry->native_addr;
		    */
		}
		else
		{
#ifdef CROSSDEBUGGER
		    run_until_fragment_start(debugger_intp);
#endif

		    run_until_fragment_start(compiler_intp);
		}
	    }
	}
	else
	{
	    fragment_hash_entry_t entry;
	    fragment_hash_supplement_t supplement;

	    init_fragment_hash_entry(&entry, &supplement);

	    supplement.times_executed = 1;

	    fragment_hash_put(compiler_intp->pc, &entry, &supplement);

	    run_until_fragment_start(compiler_intp);

#ifdef CROSSDEBUGGER
	    run_until_fragment_start(debugger_intp);
#endif
	}
    }
}

#if 0
void
dynamo_profiler (interpreter_t *intp)
{
    for (;;)
    {
	fragment_hash_supplement_t *supp;
	int trace_index;

	if (fragment_hash_get(intp->pc, &supp) != 0)
	    ++supp->times_executed;
	else
	    supp = 0;

	if (supp != 0)
	{
	    if (supp->insn_addrs != 0)
	    {
		assert(supp->insn_addrs[0] == intp->pc);

		trace_index = 0;

		intp->have_jumped = 0;

		for (;;)
		{
		    if (trace_index == supp->num_insns)
		    {
			++supp->times_complete;
			break;
		    }
		    else if (intp->pc != supp->insn_addrs[trace_index])
		    {
			++supp->times_incomplete;
			supp->insns_incomplete += trace_index;
			break;
		    }

		    interpret_insn(intp);

		    ++trace_index;
		}
	    }
	    else
	    {
		if (supp->times_executed >= DYNAMO_THRESHOLD)
		{
		    collect_dynamo_trace(intp);

		    supp->num_insns = current_dynamo_trace_length;
		    supp->insn_addrs = (word_32*)malloc(sizeof(word_32) * supp->num_insns);
		    memcpy(supp->insn_addrs, current_dynamo_trace, sizeof(word_32) * supp->num_insns);

		    printf("new trace with %d insns, start %08x:\n", supp->num_insns, current_dynamo_trace[0]);
		    /*
		    for (i = 0; i < supp->num_insns; ++i)
			printf("%08x ", supp->insn_addrs[i]);
		    printf("\n");
		    */
		}
		else
		    run_until_fragment_start(intp);
	    }
	}
	else
	{
	    fragment_hash_entry_t entry;
	    fragment_hash_supplement_t supplement;

	    init_fragment_hash_entry(&entry, &supplement);

	    supplement.times_executed = 1;

	    fragment_hash_put(intp->pc, &entry, &supplement);

	    run_until_fragment_start(intp);
	}
    }
}
void
print_trace_stats (void)
{
#ifdef COLLECT_STATS
    int i;

    for (i = 0; i < FRAGMENT_HASH_ENTRIES; ++i)
	if (fragment_hash_table[i].foreign_addr != (word_32)-1
	    && fragment_hash_supplement[i].insn_addrs != 0)
	    printf("  %08x length %-4d: e %8ld  c %8ld  i %8ld %8ld\n",
		   fragment_hash_table[i].foreign_addr, fragment_hash_supplement[i].num_insns,
		   fragment_hash_supplement[i].times_executed, fragment_hash_supplement[i].times_complete,
		   fragment_hash_supplement[i].times_incomplete, fragment_hash_supplement[i].insns_incomplete);
#endif
}
#endif
#endif

#endif
