#include <stdio.h>
#include <assert.h>

#include "bintrans.h"
#ifdef PROFILE_LOOPS
#include "fragment_hash.h"

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

void
loop_profiler (interpreter_t *intp)
{
    fragment_hash_entry_t entry;
    int i;

#ifdef PROFILE_FRAGMENTS
    entry.times_executed = 1;
#endif
    entry.trace0_count = 0;
    for (i = 0; i < MAX_TRACE_JUMPS; ++i)
	entry.trace_pool_indexes[i] = -1;
    fragment_hash_put(intp->pc, &entry);

    for (;;)
    {
	word_32 prev_pc = intp->pc;
	int branch;

	intp->have_jumped = 0;

	interpret_insn(intp);

	if (intp->have_jumped)
	{
	    fragment_hash_entry_t *old = fragment_hash_get(intp->pc);

	    if (old != 0)
	    {
#ifdef PROFILE_FRAGMENTS
		++old->times_executed;
#endif
	    }
	    else
	    {
#ifdef PROFILE_FRAGMENTS
		entry.times_executed = 1;
#endif
		entry.trace0_count = 0;
		for (i = 0; i < MAX_TRACE_JUMPS; ++i)
		    entry.trace_pool_indexes[i] = -1;
		old = fragment_hash_put(intp->pc, &entry);
	    }

	    branch = (intp->pc == prev_pc + 4) ? 0 : 1;

	    assert(old != 0);
	    for (i = 0; i < MAX_TRACE_BLOCKS; ++i)
		if (fragment_history[i] == old)
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
	    fragment_history[0] = old;
	    branch_history[0] = branch;
	}
    }
}

void
print_loop_stats (void)
{
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
}

void
init_loops (void)
{
    int i;

    for (i = 0; i < COUNT_POOL_SIZE; ++i)
	count_pool[i] = 0;
    for (i = 0; i < MAX_TRACE_JUMPS; ++i)
    {
	branch_history[i] = -1;
	fragment_history[i] = 0;
    }
    fragment_history[i] = 0;
}
#endif
