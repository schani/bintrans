#define FRAGMENT_HASH_TABLE_SIZE  16384
#define FRAGMENT_HASH_OVERFLOW     8192

#define FRAGMENT_HASH_ENTRIES     (FRAGMENT_HASH_TABLE_SIZE + FRAGMENT_HASH_OVERFLOW)

#define HASH_ADDR(addr)         (((addr) >> 2) & (FRAGMENT_HASH_TABLE_SIZE - 1))

typedef struct
{
    word_32 foreign_addr;
    int next;
#ifdef NEED_COMPILER
    word_64 native_addr;
#endif
#ifdef PROFILE_FRAGMENTS
    unsigned long times_executed;
    unsigned long pad;
#endif
#ifdef PROFILE_LOOPS
    trace_count_t trace0_count;
    int trace_pool_indexes[MAX_TRACE_JUMPS];
#endif
} fragment_hash_entry_t;

#ifdef COLLECT_STATS
extern unsigned long num_fragment_hash_misses;
#endif

fragment_hash_entry_t* fragment_hash_get (word_32 addr);
fragment_hash_entry_t* fragment_hash_put (word_32 foreign_addr, fragment_hash_entry_t *entry);
void init_fragment_hash (void);

extern fragment_hash_entry_t fragment_hash_table[FRAGMENT_HASH_ENTRIES];
