/*
 * fragment_hash.h
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

#define FRAGMENT_HASH_TABLE_SIZE  16384
#define FRAGMENT_HASH_OVERFLOW     8192

#define FRAGMENT_HASH_ENTRIES     (FRAGMENT_HASH_TABLE_SIZE + FRAGMENT_HASH_OVERFLOW)

#define HASH_ADDR(addr)           (((addr) >> 2) & (FRAGMENT_HASH_TABLE_SIZE - 1))

typedef struct
{
    word_32 foreign_addr;
    int next;
#ifdef NEED_COMPILER
    word_64 native_addr;
#endif
} fragment_hash_entry_t;

typedef struct
{
#if defined(PROFILE_FRAGMENTS) || defined(DYNAMO_TRACES)
    trace_count_t times_executed;
#endif
#ifdef PROFILE_LOOPS
    trace_count_t trace0_count;
    int trace_pool_indexes[MAX_TRACE_JUMPS];
#endif
#ifdef SYNC_BLOCKS
    word_64 synced_native_addr;
    unsigned char alloced_integer_regs[NUM_FREE_INTEGER_REGS];
    unsigned char alloced_float_regs[NUM_FREE_FLOAT_REGS];
#endif
#ifdef DYNAMO_TRACES
    /*
    trace_count_t times_complete;
    trace_count_t times_incomplete;
    trace_count_t insns_incomplete;
    */
#ifdef CROSSDEBUGGER
    int num_insns;
    word_32 *insn_addrs;
#endif
#endif
#ifdef COLLECT_LIVENESS
#if LIVENESS_DEPTH > 0
    int depth;
#endif
    word_32 live_cr;
    word_32 live_xer;
    word_32 live_gpr;
#endif
} fragment_hash_supplement_t;

#ifdef COLLECT_STATS
extern unsigned long num_fragment_hash_misses;
#endif

fragment_hash_entry_t* fragment_hash_get (word_32 addr, fragment_hash_supplement_t **supplement);
fragment_hash_entry_t* fragment_hash_put (word_32 foreign_addr, fragment_hash_entry_t *entry, fragment_hash_supplement_t *supplement);
void init_fragment_hash_entry (fragment_hash_entry_t *entry, fragment_hash_supplement_t *supplement);

void init_fragment_hash (void);

void count_fragment_hash_entries (int *used, int *compiled);

extern fragment_hash_entry_t fragment_hash_table[FRAGMENT_HASH_ENTRIES];
extern fragment_hash_supplement_t fragment_hash_supplement[FRAGMENT_HASH_ENTRIES];
