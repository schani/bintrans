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

#include "bintrans.h"
#ifdef PROFILE_LOOPS
#include "fragment_hash.h"
#include "compiler.h"

#define COUNT_POOL_SIZE             32768

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
	return entry->native_addr = compile_trace(entry->foreign_addr, best_length, best_bits);
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
