#include "alpha_types.h"
#include "ppc_defines.h"

#if defined(INTERPRETER) || defined(DEBUGGER) || defined(CROSSDEBUGGER)
#define NEED_INTERPRETER
#endif
#if defined(COMPILER) || defined(CROSSDEBUGGER)
#define NEED_COMPILER
#endif

word_32 leading_zeros (word_32);
word_32 mask (word_32 begin, word_32 end);
word_32 maskmask (word_32 width, word_32 num, word_32 mask);

/* asm routines */
void flush_icache (void);
void start_execution (word_64);

/* these asm routines are only called from generated code */
void direct_dispatcher (void);
void indirect_dispatcher (void);
void system_call_entry (void);
void c_stub (void);

#define SEGMENT_READABLE   1
#define SEGMENT_WRITEABLE  2
#define SEGMENT_EXECUTABLE 4
#define SEGMENT_MMAPPED    8

#define MAX_SEGMENTS 8

typedef struct
{
    word_32 addr;
    word_32 len;
    int flags;
    byte *mem;
    word_32 real_addr;
    word_32 real_len;
    byte *real_mem;
} segment_t;

typedef struct _breakpoint_t
{
    word_32 addr;
    struct _breakpoint_t *next;
} breakpoint_t;

typedef struct
{
    int direct_memory;
    int compiler;
    segment_t segments[MAX_SEGMENTS];
    segment_t *data_segment;
    int num_segments;
    unsigned long insn_count;
    int halt;
    int have_jumped;
    int trace;
    word_32 mmap_addr;
    breakpoint_t *breakpoints;
    PPC_REGISTER_SET;
    word_32 pc;
} interpreter_t;

#define DIRECT_MEM_BASE     0x000000000
#define REAL_ADDR(a)        ((addr_t)(a) + DIRECT_MEM_BASE)

#define direct_mem_set_8(addr,value)   (*(byte*)REAL_ADDR((addr) ^ 3) = (value))
#define direct_mem_set_16(addr,value)  (*(word_16*)REAL_ADDR((addr) ^ 2) = (value))
#define direct_mem_set_32(addr,value)  (*(word_32*)REAL_ADDR(addr) = (value))
#define direct_mem_set_64(addr,value)  ({ word_32 a = (addr); word_64 v = (value); direct_mem_set_32(a, v >> 32); direct_mem_set_32(a + 4, v & 0xffffffff); })
#define direct_mem_get_8(addr)         (*(byte*)REAL_ADDR((addr) ^ 3))
#define direct_mem_get_16(addr)        (*(word_16*)REAL_ADDR((addr) ^ 2))
#define direct_mem_get_32(addr)        (*(word_32*)REAL_ADDR(addr))
#define direct_mem_get_64(addr)        ({ word_32 a = (addr); ((word_64)direct_mem_get_32(a) << 32) | direct_mem_get_32(a + 4); })

void emulated_mem_set_8 (interpreter_t *intp, word_32 addr, word_32 value);
void emulated_mem_set_16 (interpreter_t *intp, word_32 addr, word_16 value);
void emulated_mem_set_32 (interpreter_t *intp, word_32 addr, word_32 value);
void emulated_mem_set_64 (interpreter_t *intp, word_32 addr, word_64 value);
word_8 emulated_mem_get_8 (interpreter_t *intp, word_32 addr);
word_16 emulated_mem_get_16 (interpreter_t *intp, word_32 addr);
word_32 emulated_mem_get_32 (interpreter_t *intp, word_32 addr);
word_64 emulated_mem_get_64 (interpreter_t *intp, word_32 addr);

#define mem_set_8(intp,addr,value)     ((intp)->direct_memory ? direct_mem_set_8((addr),(value)) : emulated_mem_set_8((intp),(addr),(value)))
#define mem_set_16(intp,addr,value)    ((intp)->direct_memory ? direct_mem_set_16((addr),(value)) : emulated_mem_set_16((intp),(addr),(value)))
#define mem_set_32(intp,addr,value)    ((intp)->direct_memory ? direct_mem_set_32((addr),(value)) : emulated_mem_set_32((intp),(addr),(value)))
#define mem_set_64(intp,addr,value)    ((intp)->direct_memory ? direct_mem_set_64((addr),(value)) : emulated_mem_set_64((intp),(addr),(value)))
#define mem_get_8(intp,addr)           ((intp)->direct_memory ? direct_mem_get_8((addr)) : emulated_mem_get_8((intp),addr))
#define mem_get_16(intp,addr)          ((intp)->direct_memory ? direct_mem_get_16((addr)) : emulated_mem_get_16((intp),(addr)))
#define mem_get_32(intp,addr)          ((intp)->direct_memory ? direct_mem_get_32((addr)) : emulated_mem_get_32((intp),(addr)))
#define mem_get_64(intp,addr)          ((intp)->direct_memory ? direct_mem_get_64((addr)) : emulated_mem_get_64((intp),(addr)))

void handle_system_call (interpreter_t *intp);

void init_compiler (interpreter_t *intp, interpreter_t *dbg_intp);
void start_compiler (word_32 addr);
void print_compiler_stats (void);

/* these come from ppc_compiler.c */
void move_ppc_regs_interpreter_to_compiler (interpreter_t *intp);
void move_ppc_regs_compiler_to_interpreter (interpreter_t *intp);

/* theser are from ppc_interpreter.c */
void interpret_ppc_insn (interpreter_t *intp);
void dump_ppc_registers (interpreter_t *intp);
