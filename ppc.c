#include <elf.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <termios.h>
#include <sys/types.h>
#include <ctype.h>

/* Symbolic values for the entries in the auxiliary table
   put on the initial stack */
#define AT_NULL   0	/* end of vector */
#define AT_IGNORE 1	/* entry should be ignored */
#define AT_EXECFD 2	/* file descriptor of program */
#define AT_PHDR   3	/* program headers for program */
#define AT_PHENT  4	/* size of program header entry */
#define AT_PHNUM  5	/* number of program headers */
#define AT_PAGESZ 6	/* system page size */
#define AT_BASE   7	/* base address of interpreter */
#define AT_FLAGS  8	/* flags */
#define AT_ENTRY  9	/* entry point of program */
#define AT_NOTELF 10	/* program is not ELF */
#define AT_UID    11	/* real uid */
#define AT_EUID   12	/* effective uid */
#define AT_GID    13	/* real gid */
#define AT_EGID   14	/* effective gid */
#define AT_PLATFORM 15  /* string identifying CPU for optimizations */
#define AT_HWCAP  16    /* arch dependent hints at CPU capabilities */

#define MAX_SEGMENTS 8

#define SEGMENT_READABLE   1
#define SEGMENT_WRITEABLE  2
#define SEGMENT_EXECUTABLE 4
#define SEGMENT_MMAPPED    8

#define STACK_TOP  0x80000000
#define PAGE_SIZE        4096
#define STACK_SIZE        128
#define MMAP_START 0x30000000

#define PPC_MAP_PRIVATE   0x02
#define PPC_MAP_ANONYMOUS 0x20

#define PPC_TCGETS  0x402c7413

typedef unsigned int word;
typedef unsigned short word_16;
typedef unsigned long word_64;
typedef unsigned char byte;
typedef signed int sword_32;
typedef signed long sword_64;

typedef struct
{
    word addr;
    word len;
    int flags;
    byte *mem;
} segment_t;

typedef struct _breakpoint_t
{
    word addr;
    struct _breakpoint_t *next;
} breakpoint_t;

segment_t segments[MAX_SEGMENTS];
segment_t *data_segment = 0;
int num_segments = 0;
int debug = 0;
long insn_count = 0;
int halt = 0;
int trace = 1;
word mmap_addr = MMAP_START;

breakpoint_t *breakpoints = 0;

segment_t*
find_segment (word addr)
{
    int i;

    for (i = 0; i < num_segments; ++i)
	if (addr >= segments[i].addr && addr < segments[i].addr + segments[i].len)
	    return &segments[i];
    return 0;
}

void
segfault (word addr)
{
    printf("segmentation fault for addr 0x%08x\n", addr);
    halt = 1;
}

void
mem_set (word addr, word value)
{
    segment_t *seg = find_segment(addr);

    if (seg == 0
	|| !(addr >= seg->addr && addr + 4 <= seg->addr + seg->len)
	|| !(seg->flags & SEGMENT_WRITEABLE))
	segfault(addr);
    else
    {
	if (debug)
	    printf("mem[%x] = %x\n", addr, value);

	*(word*)(seg->mem + (addr - seg->addr)) = value;
    }
}

void
mem_set_8 (word addr, word value)
{
    segment_t *seg;

    addr ^= 3;

    seg = find_segment(addr);

    if (seg == 0
	|| !(addr >= seg->addr && addr < seg->addr + seg->len)
	|| !(seg->flags & SEGMENT_WRITEABLE))
	segfault(addr);
    else
    {
	if (debug)
	    printf("mem8[%x] = %x\n", addr, value);

	seg->mem[addr - seg->addr] = value;
    }
}

void
mem_set_16 (word addr, word_16 value)
{
    mem_set_8(addr, value >> 8);
    mem_set_8(addr + 1, value & 0xff);
}

void
mem_set_64 (word addr, word_64 value)
{
    mem_set(addr, value >> 32);
    mem_set(addr + 4, value & 0xffffffff);
}

void
mem_copy (word addr, byte *buf, word len)
{
    word w;

    assert((addr & 3) == 0);
    assert((len & 3) == 0);

    for (w = 0; w < len; w += 4)
	mem_set(addr + w, *(word*)(buf + w));
}

word
mem_get (word addr)
{
    segment_t *seg = find_segment(addr);

    if (seg == 0
	|| !(addr >= seg->addr && addr + 4 <= seg->addr + seg->len)
	|| !(seg->flags & SEGMENT_READABLE))
    {
	segfault(addr);
	return 0;
    }
    else
    {
	/*
	if (debug)
	    printf("mem[%x]\n", addr);
	*/

	return *(word*)(seg->mem + (addr - seg->addr));
    }
}

word
mem_get_8 (word addr)
{
    segment_t *seg;

    addr ^= 3;
    seg = find_segment(addr);

    if (seg == 0
	|| !(addr >= seg->addr && addr < seg->addr + seg->len)
	|| !(seg->flags & SEGMENT_READABLE))
    {
	segfault(addr);
	return 0;
    }
    else
    {
	/*
	if (debug)
	    printf("mem8[%x]\n", addr);
	*/

	return (word)seg->mem[addr - seg->addr];
    }
}

word_16
mem_get_16 (word addr)
{
    return ((word_16)mem_get_8(addr) << 8) | mem_get_8(addr + 1);
}

word_64
mem_get_64 (word addr)
{
    return ((word_64)mem_get(addr) << 32) | mem_get(addr + 4);
}

word
rotl (word x, word i)
{
    assert(i <= 32);

    return (x << i) | (x >> (32 - i));
}

word
mask (word begin, word end)
{
    word x = 0;
    word b;
    int i;

    if (end < begin)
    {
	b = 1;
	for (i = 0; i <= end; ++i)
	{
	    x = x | b;
	    b <<= 1;
	}

	b = 1 << begin;
	for (i = begin; i < 32; ++i)
	{
	    x = x | b;
	    b <<= 1;
	}
    }
    else
    {
	b = 1 << begin;
	for (i = 0; i <= end - begin; ++i)
	{
	    x = x | b;
	    b <<= 1;
	}
    }

    return x;
}

word
maskmask (word width, word num, word mask)
{
    word x;
    word mb = 1, xb = 1;
    word i, j;

    for (i = 0; i < num; ++i)
    {
	word b = mask & mb;

	for (j = 0; j < width; ++j)
	{
	    if (b)
		x |= xb;
	    xb <<= 1;
	}
	mb <<= 1;
    }

    return x;
}

word
leading_zeros (word w)
{
    word m = 0x80000000;
    word i;

    for (i = 0; i < 32; ++i)
    {
	if (w & m)
	    break;
	m >>= 1;
    }

    return i;
}

word
subcarry (word op1, word op2)
{
    if ((sword_64)((sword_32)op1 - (sword_32)op2) != (sword_64)(sword_32)op1 - (sword_64)(sword_32)op2)
	return 1;
    return 0;
}

/*
word
addcarry (word op1, word op2)
{
    if ((sword_64)((sword_32)op1 + (sword_32)op2) != (sword_64)(sword_32)op1 + (sword_64)(sword_32)op2)
	return 1;
    return 0;
}
*/

word
addcarry (word op1, word op2)
{
    if ((word_64)(op1 + op2) != (word_64)op1 + (word_64)op2)
	return 1;
    return 0;
}

void handle_system_call (void);

#include "interpreter.c"

segment_t*
mmap_anonymous_segment (word len, int prot)
{
    byte *mem = (byte*)malloc(len);
    segment_t *segment;

    if (mem == 0)
	return 0;

    assert(num_segments < MAX_SEGMENTS);

    segment = &segments[num_segments++];

    segment->addr = mmap_addr;
    mmap_addr += len;

    segment->len = len;

    segment->flags = SEGMENT_MMAPPED;
    if (prot & PROT_READ)
	segment->flags |= SEGMENT_READABLE;
    if (prot & PROT_WRITE)
	segment->flags |= SEGMENT_WRITEABLE;
    if (prot & PROT_EXEC)
	segment->flags |= SEGMENT_EXECUTABLE;

    segment->mem = mem;

    return segment;
}

void
handle_system_call (void)
{
    switch (regs_GPR[0])
    {
	case 1 :
	    printf("exit (%d)\n", regs_GPR[3]);
	    printf("%ld insn executed\n", insn_count);
	    exit(regs_GPR[3]);
	    break;

	case 4 :
	    /* printf("write\n"); */
	    {
		byte *mem = (byte*)malloc(regs_GPR[5]);
		word i;
		int result;

		assert(mem != 0);

		for (i = 0; i < regs_GPR[5]; ++i)
		    mem[i] = mem_get_8(regs_GPR[4] + i);
		result = write(regs_GPR[3], mem, regs_GPR[5]);

		free(mem);

		regs_GPR[3] = (word)result;
		regs_SPR[1] &= ~0x1000000;
	    }
	    break;

	case 20 :
	    printf("getpid\n");
	    regs_GPR[3] = getpid();
	    regs_SPR[1] &= ~0x1000000;
	    break;

	case 24 :
	    printf("getuid\n");
	    regs_GPR[3] = getuid();
	    regs_SPR[1] &= ~0x1000000;
	    break;

	case 45 :
	    printf("brk\n");
	    if (regs_GPR[3] == 0)
		regs_GPR[3] = data_segment->addr + data_segment->len;
	    else
	    {
		byte *new_mem;
		word new_len;

		assert(regs_GPR[3] > data_segment->addr);

		new_len = regs_GPR[3] - data_segment->addr;
		new_mem = (byte*)realloc(data_segment->mem, new_len);
		assert(new_mem != 0);

		if (new_len > data_segment->len)
		    memset(new_mem + data_segment->len, 0, new_len - data_segment->len);

		data_segment->mem = new_mem;
		data_segment->len = new_len;

		/* gpr3 untouched */
	    }
	    regs_SPR[1] &= ~0x1000000;
	    break;

	case 47 :
	    printf("getgid\n");
	    regs_GPR[3] = getgid();
	    regs_SPR[1] &= ~0x1000000;
	    break;

	case 49 :
	    printf("geteuid\n");
	    regs_GPR[3] = geteuid();
	    regs_SPR[1] &= ~0x1000000;
	    break;

	case 50 :
	    printf("getegid\n");
	    regs_GPR[3] = getegid();
	    regs_SPR[1] &= ~0x1000000;
	    break;

	case 54 :
	    printf("ioctl\n");
	    {
		struct termios arg;
		int result;

		assert(regs_GPR[4] == PPC_TCGETS);
		result = ioctl(regs_GPR[3], TCGETS, &arg);
		if (result == 0)
		    mem_copy(regs_GPR[5], (byte*)&arg, sizeof(struct termios));
		else
		    assert(0);
		regs_GPR[3] = result;
	    }
	    regs_SPR[1] &= ~0x1000000;
	    break;

	case 90 :
	    printf("mmap\n");
	    {
		segment_t *segment;

		assert(regs_GPR[3] == 0);
		assert((regs_GPR[4] & (PAGE_SIZE - 1)) == 0);
		assert(regs_GPR[6] == (PPC_MAP_PRIVATE | PPC_MAP_ANONYMOUS));
		assert(regs_GPR[7] == -1);
		assert(regs_GPR[8] == 0);

		segment = mmap_anonymous_segment(regs_GPR[4], regs_GPR[5]);
		if (segment == 0)
		{
		    regs_GPR[3] = 0xffffffff;
		    assert(0);
		}
		else
		    regs_GPR[3] = segment->addr;
	    }
	    regs_SPR[1] &= ~0x1000000;
	    break;

	case 91 :
	    printf("munmap\n");
	    {
		int i;

		for (i = 0; i < num_segments; ++i)
		    if (segments[i].addr == regs_GPR[3]
			&& segments[i].len == regs_GPR[4]
			&& (segments[i].flags & SEGMENT_MMAPPED))
			break;

		if (i < num_segments)
		{
		    segments[i].addr = 0;
		    segments[i].len = 0;
		    segments[i].flags = 0;
		    free(segments[i].mem);
		    regs_GPR[3] = 0;
		}
		else
		{
		    regs_GPR[3] = 0xffffffff;
		    assert(0);
		}
	    }
	    regs_SPR[1] &= ~0x1000000;
	    break;

	case 108 :
	    printf("fstat\n");
	    {
		struct stat buf;
		int result;

		result = fstat(regs_GPR[3], &buf);
		if (result == 0)
		{
		    mem_set(regs_GPR[4] + 0, buf.st_dev);
		    mem_set(regs_GPR[4] + 4, buf.st_ino);
		    mem_set(regs_GPR[4] + 8, buf.st_mode);
		    mem_set_16(regs_GPR[4] + 12, buf.st_nlink);
		    mem_set(regs_GPR[4] + 16, buf.st_uid);
		    mem_set(regs_GPR[4] + 20, buf.st_gid);
		    mem_set(regs_GPR[4] + 24, buf.st_rdev);
		    mem_set(regs_GPR[4] + 28, buf.st_size);
		    mem_set(regs_GPR[4] + 32, buf.st_blksize);
		    mem_set(regs_GPR[4] + 36, buf.st_blocks);
		    mem_set(regs_GPR[4] + 40, buf.st_atime);
		    mem_set(regs_GPR[4] + 48, buf.st_mtime);
		    mem_set(regs_GPR[4] + 56, buf.st_ctime);
		}
		else
		    assert(0);
		regs_GPR[3] = result;
	    }
	    regs_SPR[1] &= ~0x1000000;
	    break;

	case 136 :
	    printf("personality\n");
	    assert(regs_GPR[3] == 0);
	    regs_GPR[3] = 0;
	    regs_SPR[1] &= ~0x1000000;
	    break;

	default :
	    printf("unhandled system call %d\n", regs_GPR[0]);
	    halt = 1;
    }
}

void
lsbify_elf32_ehdr (Elf32_Ehdr *hdr)
{
    hdr->e_type = ntohs(hdr->e_type);
    hdr->e_machine = ntohs(hdr->e_machine);
    hdr->e_version = ntohl(hdr->e_version);
    hdr->e_entry = ntohl(hdr->e_entry);
    hdr->e_phoff = ntohl(hdr->e_phoff);
    hdr->e_shoff = ntohl(hdr->e_shoff);
    hdr->e_flags = ntohl(hdr->e_flags);
    hdr->e_ehsize = ntohs(hdr->e_ehsize);
    hdr->e_phentsize = ntohs(hdr->e_phentsize);
    hdr->e_phnum = ntohs(hdr->e_phnum);
    hdr->e_shentsize = ntohs(hdr->e_shentsize);
    hdr->e_shnum = ntohs(hdr->e_shnum);
    hdr->e_shstrndx = ntohs(hdr->e_shstrndx);
}

void
lsbify_elf32_phdr (Elf32_Phdr *hdr)
{
    hdr->p_type = ntohl(hdr->p_type);
    hdr->p_offset = ntohl(hdr->p_offset);
    hdr->p_vaddr = ntohl(hdr->p_vaddr);
    hdr->p_paddr = ntohl(hdr->p_paddr);
    hdr->p_filesz = ntohl(hdr->p_filesz);
    hdr->p_memsz = ntohl(hdr->p_memsz);
    hdr->p_flags = ntohl(hdr->p_flags);
    hdr->p_align = ntohl(hdr->p_align);
}

word
copy_string (char *str, word p)
{
    word len = strlen(str) + 1;
    word i;

    p -= len;

    for (i = 0; i < len; ++i)
	mem_set_8(p + i, str[i]);

    return p;
}

word
copy_strings (int num, char **strs, word p)
{
    int i;

    for (i = num - 1; i >= 0; --i)
	p = copy_string(strs[i], p);

    return p;
}

word
strlen_user (word p)
{
    word e = p;

    while (mem_get_8(e++) != 0)
	;

    return e - p;
}

word
setup_stack (word p, char *argv[])
{
    static char *env[] = { "PWD=/bigben/home/schani",
			   "HOSTNAME=samhain.ifs.tuwien.ac.at",
			   "HISTFILESIZE=1000",
			   "USER=schani",
			   "MACHTYPE=powerpc-redhat-linux-gnu",
			   "MAIL=/var/spool/mail/schani",
			   "BASH_ENV=/home/schani/.bashrc",
			   "DISPLAY=samhain.ifs.tuwien.ac.at:13.0",
			   "LOGNAME=schani",
			   "SHLVL=1",
			   "SHELL=/bin/bash",
			   "USERNAME=",
			   "HOSTTYPE=powerpc",
			   "OSTYPE=linux-gnu",
			   "HISTSIZE=1000",
			   "HOME=/home/schani",
			   "TERM=xterm",
			   "PATH=/usr/bin:/bin:/usr/bin:/usr/X11R6/bin:/home/schani/bin",
			   "SSH_TTY=/dev/pts/5",
			   0 };

    int argc;
    int envc = 19;

    word exec;
    word sp;
    word csp;
    word argvp, envp;

    for (argc = 0; argv[argc] != 0; ++argc)
	;

    p -= 4;
    p = copy_string(argv[0], p);
    exec = p;
    p = copy_strings(envc, env, p);
    p = copy_strings(argc, argv, p);

    sp = (~15 & p) - 16;

    csp = sp;
    csp -= 4 * 4;		/* no interpreter, no ELF_PLATFORM */
    csp -= (envc + 1) * 4;
    csp -= (argc + 1) * 4;
    csp -= 1 * 4;		/* ibcs */
    if (csp & 15)
	sp -= csp & 15;

#define NEW_AUX_ENT(nr, id, val) \
    mem_set(sp + (nr) * 2 * 4, id); \
    mem_set(sp + ((nr) * 2 + 1) * 4, val);

    sp -= 2 * 4;
    NEW_AUX_ENT(0, AT_NULL, 0);

    sp -= 2 * 4;
    NEW_AUX_ENT(0, AT_HWCAP, 0x10);

    sp -= (envc + 1) * 4;
    envp = sp;
    sp -= (argc + 1) * 4;
    argvp = sp;

    /*
    sp -= 4;
    mem_set(sp, envp);

    sp -= 4;
    mem_set(sp, argvp);
    */

    sp -= 4;
    mem_set(sp, argc);

    while (argc-- > 0) {
	mem_set(argvp, p);
	argvp += 4;
	p += strlen_user(p);
    }
    mem_set(argvp, 0);

    while (envc-- > 0) {
	mem_set(envp, p);
	envp += 4;
	p += strlen_user(p);
    }
    mem_set(envp, 0);

    return sp;
}

void
run_debugged (void)
{
    breakpoint_t *breakpoint;

    for (;;)
    {
	if (trace)
	{
	    printf("%08x:  ", pc);
	    disassemble_insn(mem_get(pc), pc);
	    printf("   %08x", regs_SPR[2]);
	    printf("\n");
	}
	interpret_insn();
	if (halt)
	    return;
	for (breakpoint = breakpoints; breakpoint != 0; breakpoint = breakpoint->next)
	    if (breakpoint->addr == pc)
		return;
    }
}

#define CMDLINE_LENGTH      256

char*
get_token (char *str, char *tok)
{
    while (*str != 0 && isspace(*str))
	++str;

    if (*str == 0)
	return 0;

    while (*str != 0 && !isspace(*str))
	*tok++ = *str++;

    *tok = 0;

    return str;
}

void
show_breakpoints (void)
{
    int i;
    breakpoint_t *breakpoint;

    for (breakpoint = breakpoints, i = 0; breakpoint != 0; breakpoint = breakpoint->next, ++i)
	printf("%3d  %08x\n", i, breakpoint->addr);
}

void
add_breakpoint (word addr)
{
    breakpoint_t *breakpoint = (breakpoint_t*)malloc(sizeof(breakpoint_t));

    breakpoint->addr = addr;
    breakpoint->next = breakpoints;
    breakpoints = breakpoint;
}

void
delete_breakpoint (int num)
{
    breakpoint_t *breakpoint;

    if (num < 0)
	return;
    if (num == 0)
    {
	if (breakpoints == 0)
	    return;
	breakpoint = breakpoints;
	breakpoints = breakpoint->next;
	free(breakpoint);
    }
    else
    {
	breakpoint_t *tmp;

	for (breakpoint = breakpoints; breakpoint != 0 && num > 1; breakpoint = breakpoint->next, --num)
	    ;
	if (breakpoint == 0 || breakpoint->next == 0)
	    return;
	tmp = breakpoint->next;
	breakpoint->next = tmp->next;
	free(tmp);
    }
}

void
dump_memory (word addr, word len)
{
    word w;

    for (w = addr; w < addr + len; w += 8)
    {
	word o = 0;
	word n = 8;

	if (addr + len - w < n)
	    n = addr + len - w;

	printf("%08x   ", w);
	for (o = 0; o < n; ++o)
	{
	    byte b = mem_get_8(w + o);

	    printf("%02x %c  ", b, isprint(b) ? b : '.');
	}
	printf("\n");
    }
}

void
disassemble (word addr, word len)
{
    word i;

    for (i = 0; i < len; ++i)
    {
	printf("%08x:  ", addr);
	disassemble_insn(mem_get(addr), addr);
	printf("\n");

	addr += 4;
    }
}

void
debugger (void)
{
    char cmdline[CMDLINE_LENGTH];
    char token[CMDLINE_LENGTH];
    char *p;

    for (;;)
    {
	halt = 0;

	printf("%08x > ", pc);
	if (fgets(cmdline, CMDLINE_LENGTH, stdin) == NULL)
	    return;

	p = get_token(cmdline, token);
	if (p == 0)
	    continue;

	if (strcmp(token, "n") == 0)
	    interpret_insn();
	else if (strcmp(token, "regs") == 0)
	    dump_registers();
	else if (strcmp(token, "cont") == 0)
	    run_debugged();
	else if (strcmp(token, "show") == 0)
	    show_breakpoints();
	else if (strcmp(token, "help") == 0)
	    printf("rotfl!\n");
	else if (strcmp(token, "trace") == 0)
	{
	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }

	    if (strcmp(token, "on") == 0)
		trace = 1;
	    else
		trace = 0;
	}
	else if (strcmp(token, "x") == 0)
	{
	    word addr, len;

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }
	    addr = strtol(token, 0, 16);

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }
	    len = strtol(token, 0, 16);

	    dump_memory(addr, len);
	}
	else if (strcmp(token, "dis") == 0)
	{
	    word addr, len;

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }
	    addr = strtol(token, 0, 16);

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }
	    len = strtol(token, 0, 16);

	    disassemble(addr, len);
	}
	else if (strcmp(token, "break") == 0)
	{
	    word addr;

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }
	    addr = strtol(token, 0, 16);

	    add_breakpoint(addr);
	}
	else if (strcmp(token, "del") == 0)
	{
	    int num;

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }
	    num = atoi(token);

	    delete_breakpoint(num);
	}
	else
	    printf("error\n");
    }
}

int
main (int argc, char *argv[])
{
    FILE *file;
    Elf32_Ehdr ehdr;
    Elf32_Phdr *phdrs = 0;
    word stack_bottom;
    size_t num_read;
    int i;
    char **ppc_argv;

    assert(argc >= 2);

    ppc_argv = (char**)malloc(sizeof(char*) * argc);
    ppc_argv[0] = "/bigben/home/schani/./a.out";
    for (i = 2; i < argc; ++i)
	ppc_argv[i - 1] = argv[i];
    ppc_argv[argc - 1] = 0;

    file = fopen(argv[1], "r");
    assert(file != 0);

    num_read = fread(&ehdr, sizeof(Elf32_Ehdr), 1, file);
    assert(num_read == 1);

    assert(ehdr.e_ident[EI_MAG0] == ELFMAG0);
    assert(ehdr.e_ident[EI_MAG1] == ELFMAG1);
    assert(ehdr.e_ident[EI_MAG2] == ELFMAG2);
    assert(ehdr.e_ident[EI_MAG3] == ELFMAG3);

    assert(ehdr.e_ident[EI_CLASS] == ELFCLASS32);
    assert(ehdr.e_ident[EI_DATA] == ELFDATA2MSB);
    assert(ehdr.e_ident[EI_VERSION] == EV_CURRENT);
    assert(ehdr.e_ident[EI_OSABI] == ELFOSABI_SYSV);
    assert(ehdr.e_ident[EI_ABIVERSION] == 0);

    lsbify_elf32_ehdr(&ehdr);

    assert(ehdr.e_type == ET_EXEC);
    assert(ehdr.e_machine == EM_PPC);
    assert(ehdr.e_version == EV_CURRENT);

    phdrs = (Elf32_Phdr*)malloc(sizeof(Elf32_Phdr) * ehdr.e_phnum);

    fseek(file, ehdr.e_phoff, SEEK_SET);
    for (i = 0; i < ehdr.e_phnum; ++i)
    {
	num_read = fread(&phdrs[i], sizeof(Elf32_Phdr), 1, file);
	assert(num_read == 1);
	lsbify_elf32_phdr(&phdrs[i]);
    }

    for (i = 0; i < ehdr.e_phnum; ++i)
    {
	word w;

	segments[i].addr = phdrs[i].p_vaddr;
	segments[i].len = phdrs[i].p_memsz;
	segments[i].flags = 0;
	if (phdrs[i].p_flags & PF_R)
	    segments[i].flags |= SEGMENT_READABLE;
	if (phdrs[i].p_flags & PF_W)
	    segments[i].flags |= SEGMENT_WRITEABLE;
	if (phdrs[i].p_flags & PF_X)
	    segments[i].flags |= SEGMENT_EXECUTABLE;
	segments[i].mem = (byte*)malloc(segments[i].len);
	memset(segments[i].mem, 0, segments[i].len);

	fseek(file, phdrs[i].p_offset, SEEK_SET);
	num_read = fread(segments[i].mem, 1, phdrs[i].p_filesz, file);
	assert(num_read = phdrs[i].p_filesz);

	for (w = 0; w < segments[i].len; w += 4)
	    *(word*)(segments[i].mem + w) = ntohl(*(word*)(segments[i].mem + w));

	if (phdrs[i].p_type == PT_LOAD && phdrs[i].p_flags == (PF_W | PF_R))
	    data_segment = &segments[i];
    }

    segments[ehdr.e_phnum].addr = STACK_TOP - STACK_SIZE * PAGE_SIZE;
    segments[ehdr.e_phnum].len = STACK_SIZE * PAGE_SIZE;
    segments[ehdr.e_phnum].flags = SEGMENT_READABLE | SEGMENT_WRITEABLE;
    segments[ehdr.e_phnum].mem = (byte*)malloc(STACK_SIZE * PAGE_SIZE);
    memset(segments[ehdr.e_phnum].mem, 0, STACK_SIZE * PAGE_SIZE);

    num_segments = ehdr.e_phnum + 1;

    assert(data_segment != 0);

    stack_bottom = setup_stack(STACK_TOP, ppc_argv);
    assert((stack_bottom & 15) == 0);

    for (i = 0; i < 32; ++i)
	regs_GPR[i] = 0; /* 0xdeadbe00 + i; */
    regs_GPR[1] = stack_bottom;
    pc = ehdr.e_entry;

    /*
    for (w = stack_bottom; w < STACK_TOP; w += 8)
    {
	word o = 0;

	printf("%08x   ", w);
	for (o = 0; o < 8; ++o)
	{
	    byte b = mem_get_8(w + o);

	    printf("%02x %c  ", b, isprint(b) ? b : '.');
	}
	printf("\n");
    }
    */

    /* debug = 1; */

    debugger();

    /*
    for (;;)
    {
	if (debug)
	    dump_registers();
	printf("%08x:  ", pc);
	disassemble_insn(mem_get(pc), pc);
	printf("\n");
	interpret_insn();
    }
    */

    return 0;
}
