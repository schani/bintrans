/*
 * fragment_hash.c
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

#include "bintrans.h"
#include "fragment_hash.h"

fragment_hash_entry_t fragment_hash_table[FRAGMENT_HASH_TABLE_SIZE + FRAGMENT_HASH_OVERFLOW];
fragment_hash_supplement_t fragment_hash_supplement[FRAGMENT_HASH_TABLE_SIZE + FRAGMENT_HASH_OVERFLOW];
int first_free_overflow;

#ifdef COLLECT_STATS
unsigned long num_fragment_hash_misses = 0;
#endif

fragment_hash_entry_t*
fragment_hash_get (word_32 addr, fragment_hash_supplement_t **supplement)
{
    int index = HASH_ADDR(addr);

    assert(addr != (word_32)-1);

    if (fragment_hash_table[index].foreign_addr == (word_32)-1)
	return 0;

    if (fragment_hash_table[index].foreign_addr == addr)
    {
	*supplement = &fragment_hash_supplement[index];
	return &fragment_hash_table[index];
    }

#ifdef COLLECT_STATS
    ++num_fragment_hash_misses;
#endif

    index = fragment_hash_table[index].next;
    while (index != -1)
    {
	if (fragment_hash_table[index].foreign_addr == addr)
	{
	    *supplement = &fragment_hash_supplement[index];
	    return &fragment_hash_table[index];
	}
	index = fragment_hash_table[index].next;
    }

#ifdef COLLECT_STATS
    --num_fragment_hash_misses;
#endif

    return 0;
}

fragment_hash_entry_t*
fragment_hash_put (word_32 foreign_addr, fragment_hash_entry_t *entry, fragment_hash_supplement_t *supplement)
{
    fragment_hash_supplement_t *table_supplement;
    fragment_hash_entry_t *table_entry = fragment_hash_get(foreign_addr, &table_supplement);

    if (table_entry != 0)
    {
	assert(table_entry->foreign_addr == foreign_addr);

	/* printf("entry already in table\n"); */

#ifdef NEED_COMPILER
	table_entry->native_addr = entry->native_addr;
#endif
	*table_supplement = *supplement;

	return table_entry;
    }
    else
    {
	int index = HASH_ADDR(foreign_addr);

	if (fragment_hash_table[index].foreign_addr == (word_32)-1)
	{
	    fragment_hash_table[index] = *entry;
	    fragment_hash_table[index].foreign_addr = foreign_addr;
	    fragment_hash_table[index].next = -1;

	    fragment_hash_supplement[index] = *supplement;

	    return &fragment_hash_table[index];
	}
	else
	{
	    int new = first_free_overflow;

	    assert(new != -1);
	    first_free_overflow = fragment_hash_table[new].next;

	    fragment_hash_table[new] = *entry;
	    fragment_hash_table[new].foreign_addr = foreign_addr;
	    fragment_hash_table[new].next = -1;

	    fragment_hash_supplement[new] = *supplement;

	    while (fragment_hash_table[index].next != -1)
		index = fragment_hash_table[index].next;

	    fragment_hash_table[index].next = new;

	    return &fragment_hash_table[new];
	}
    }
}

void
count_fragment_hash_entries (int *used, int *compiled)
{
    int i;

    *used = 0;
    *compiled = 0;
    for (i = 0; i < FRAGMENT_HASH_ENTRIES; ++i)
	if (fragment_hash_table[i].foreign_addr != (word_32)-1)
	{
	    ++*used;
#ifdef NEED_COMPILER
	    if (fragment_hash_table[i].native_addr != 0)
		++*compiled;
#endif
	}
}

void
init_fragment_hash_entry (fragment_hash_entry_t *entry, fragment_hash_supplement_t *supplement)
{
#ifdef PROFILE_LOOPS
    int i;
#endif

    entry->foreign_addr = (word_32)-1;
    entry->next = -1;
#ifdef NEED_COMPILER
    entry->native_addr = 0;
#endif
#if defined(PROFILE_FRAGMENTS) || defined(DYNAMO_TRACES)
    supplement->times_executed = 0;
#endif
#ifdef PROFILE_LOOPS
    supplement->trace0_count = 0;
    for (i = 0; i < MAX_TRACE_JUMPS; ++i)
	supplement->trace_pool_indexes[i] = -1;
#endif
#ifdef DYNAMO_TRACES
    /*
    supplement->times_complete = 0;
    supplement->times_incomplete = 0;
    supplement->insns_incomplete = 0;
    */
#ifdef CROSSDEBUGGER
    supplement->num_insns = 0;
    supplement->insn_addrs = 0;
#endif
#endif
#ifdef COLLECT_LIVENESS
#if LIVENESS_DEPTH > 0
    supplement->depth = -1;
#endif
    supplement->live_cr = 0xffffffff;
    supplement->live_xer = 0xffffffff;
    supplement->live_gpr = 0xffffffff;
#endif
}

void
init_fragment_hash (void)
{
    int i;

    for (i = 0; i < FRAGMENT_HASH_ENTRIES; ++i)
	fragment_hash_table[i].foreign_addr = (word_32)-1;
    for (i = FRAGMENT_HASH_TABLE_SIZE; i < FRAGMENT_HASH_TABLE_SIZE + FRAGMENT_HASH_OVERFLOW - 1; ++i)
	fragment_hash_table[i].next = i + 1;
    fragment_hash_table[i].next = -1;
    first_free_overflow = FRAGMENT_HASH_TABLE_SIZE;
}
