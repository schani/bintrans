#include <assert.h>

#include "bintrans.h"
#include "fragment_hash.h"

fragment_hash_entry_t fragment_hash_table[FRAGMENT_HASH_TABLE_SIZE + FRAGMENT_HASH_OVERFLOW];
int first_free_overflow;

#ifdef COLLECT_STATS
unsigned long num_fragment_hash_misses = 0;
#endif

fragment_hash_entry_t*
fragment_hash_get (word_32 addr)
{
    int index = HASH_ADDR(addr);

    assert(addr != (word_32)-1);

    if (fragment_hash_table[index].foreign_addr == (word_32)-1)
	return 0;

    if (fragment_hash_table[index].foreign_addr == addr)
	return &fragment_hash_table[index];

#ifdef COLLECT_STATS
    ++num_fragment_hash_misses;
#endif

    index = fragment_hash_table[index].next;
    while (index != -1)
    {
	if (fragment_hash_table[index].foreign_addr == addr)
	    return &fragment_hash_table[index];
	index = fragment_hash_table[index].next;
    }

#ifdef COLLECT_STATS
    --num_fragment_hash_misses;
#endif

    return 0;
}

fragment_hash_entry_t*
fragment_hash_put (word_32 foreign_addr, fragment_hash_entry_t *entry)
{
    int index = HASH_ADDR(foreign_addr);

    if (fragment_hash_table[index].foreign_addr == (word_32)-1)
    {
	fragment_hash_table[index] = *entry;
	fragment_hash_table[index].foreign_addr = foreign_addr;
	fragment_hash_table[index].next = -1;

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

	while (fragment_hash_table[index].next != -1)
	    index = fragment_hash_table[index].next;

	fragment_hash_table[index].next = new;

	return &fragment_hash_table[new];
    }
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
