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
#include <asm/param.h>
#include <fcntl.h>
#include <time.h>
#include <sys/socket.h>
#include <linux/net.h>
#include <sys/time.h>
#include <errno.h>
#include <sys/un.h>

#include "bintrans.h"

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

#define STACK_TOP  0x80000000
#define PAGE_SIZE        4096
#define STACK_SIZE        128
#define MMAP_START 0x30000000

#define PPC_MAP_SHARED    0x01
#define PPC_MAP_PRIVATE   0x02
#define PPC_MAP_FIXED     0x10
#define PPC_MAP_ANONYMOUS 0x20

#define PPC_O_ACCMODE	  0003
#define PPC_O_RDONLY	    00
#define PPC_O_WRONLY	    01
#define PPC_O_RDWR	    02
#define PPC_O_CREAT	  0100	/* not fcntl */
#define PPC_O_EXCL	  0200	/* not fcntl */
#define PPC_O_NOCTTY	  0400	/* not fcntl */
#define PPC_O_TRUNC	 01000	/* not fcntl */
#define PPC_O_APPEND	 02000
#define PPC_O_NONBLOCK	 04000
#define PPC_O_NDELAY	PPC_O_NONBLOCK
#define PPC_O_SYNC	010000
#define PPC_FASYNC	020000	/* fcntl, for BSD compatibility */
#define PPC_O_DIRECTORY	040000	/* must be a directory */
#define PPC_O_NOFOLLOW	0100000	/* don't follow links */

#define PPC_TCGETS  0x402c7413

#define PPC_SOCK_STREAM		1
#define PPC_SOCK_DGRAM		2
#define PPC_SOCK_RAW		3
#define PPC_SOCK_RDM		4
#define PPC_SOCK_SEQPACKET	5
#define PPC_SOCK_PACKET		10

#define PPC_ROOT                "/mnt/itch/schani/ppc-root"

int ppc_errnos[] = { 0,
		     EPERM, ENOENT, ESRCH, EINTR, EIO, ENXIO, E2BIG, ENOEXEC, EBADF,
		     ECHILD, EAGAIN, ENOMEM, EACCES, EFAULT, ENOTBLK, EBUSY, EEXIST,
		     EXDEV, ENODEV, ENOTDIR, EISDIR, EINVAL, ENFILE, EMFILE, ENOTTY,
		     ETXTBSY, EFBIG, ENOSPC, ESPIPE, EROFS, EMLINK, EPIPE, EDOM, ERANGE,
		     EDEADLK, ENAMETOOLONG, ENOLCK, ENOSYS, ENOTEMPTY, ELOOP,
		     0 /* EWOULDBLOCK */, ENOMSG, EIDRM, ECHRNG, EL2NSYNC, EL3HLT, EL3RST,
		     ELNRNG, EUNATCH, ENOCSI, EL2HLT, EBADE, EBADR, EXFULL, ENOANO,
		     EBADRQC, EBADSLT, EDEADLOCK, EBFONT, ENOSTR, ENODATA, ETIME, ENOSR,
		     ENONET, ENOPKG, EREMOTE, ENOLINK, EADV, ESRMNT, ECOMM, EPROTO,
		     EMULTIHOP, EDOTDOT, EBADMSG, EOVERFLOW, ENOTUNIQ, EBADFD, EREMCHG,
		     ELIBACC, ELIBBAD, ELIBSCN, ELIBMAX, ELIBEXEC, EILSEQ, ERESTART,
		     ESTRPIPE, EUSERS, ENOTSOCK, EDESTADDRREQ, EMSGSIZE, EPROTOTYPE,
		     ENOPROTOOPT, EPROTONOSUPPORT, ESOCKTNOSUPPORT, EOPNOTSUPP,
		     EPFNOSUPPORT, EAFNOSUPPORT, EADDRINUSE, EADDRNOTAVAIL, ENETDOWN,
		     ENETUNREACH, ENETRESET, ECONNABORTED, ECONNRESET, ENOBUFS, EISCONN,
		     ENOTCONN, ESHUTDOWN, ETOOMANYREFS, ETIMEDOUT, ECONNREFUSED,
		     EHOSTDOWN, EHOSTUNREACH, EALREADY, EINPROGRESS, ESTALE, EUCLEAN,
		     ENOTNAM, ENAVAIL, EISNAM, EREMOTEIO, EDQUOT, ENOMEDIUM,
		     EMEDIUMTYPE };

#define LAST_PPC_ERRNO 124

#define PPC_PAGE_ALIGN(a)      (((a)+PAGE_SIZE-1)&~(PAGE_SIZE-1))

int debug = 0;

void
align_segment (word_32 addr, word_32 len, word_32 *real_addr, word_32 *real_len)
{
    word_32 aligned_addr = addr & ~(EXEC_PAGESIZE - 1);
    word_32 min_len = len + (addr - aligned_addr);
    word_32 aligned_len;

    if ((min_len & (EXEC_PAGESIZE - 1)) == 0)
	aligned_len = min_len;
    else
	aligned_len = (min_len | (EXEC_PAGESIZE - 1)) + 1;

    *real_addr = aligned_addr;
    *real_len = aligned_len;

    assert((aligned_addr & (EXEC_PAGESIZE - 1)) == 0);
    assert((aligned_len & (EXEC_PAGESIZE - 1)) == 0);
}

segment_t*
setup_segment (interpreter_t *intp, word_32 addr, word_32 len, int flags)
{
    segment_t *segment = &intp->segments[intp->num_segments];

    assert(intp->num_segments < MAX_SEGMENTS);

    segment->addr = addr;
    segment->len = len;
    segment->flags = flags;
    if (intp->direct_memory)
    {
	align_segment(addr, len, &segment->real_addr, &segment->real_len);
	segment->real_mem = (byte*)mmap((void*)REAL_ADDR(segment->real_addr),
					segment->real_len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	assert((addr_t)segment->real_mem == REAL_ADDR(segment->real_addr));
	segment->mem = segment->real_mem + (addr - segment->real_addr);
    }
    else
    {
	segment->mem = (byte*)malloc(len);
	assert(segment->mem != 0);
    }

    ++intp->num_segments;

    return segment;
}

void
protect_segment (interpreter_t *intp, segment_t *segment)
{
    int prot = 0;
    int result;

    assert(intp->direct_memory);

    if (segment->flags & SEGMENT_READABLE)
	prot |= PROT_READ;
    if (segment->flags & SEGMENT_WRITEABLE)
	prot |= PROT_WRITE;

    result = mprotect((void*)REAL_ADDR(segment->real_addr), segment->real_len, prot);
    assert(result == 0);
}

segment_t*
find_segment (interpreter_t *intp, word_32 addr)
{
    int i;

    for (i = 0; i < intp->num_segments; ++i)
	if (addr >= intp->segments[i].addr && addr < intp->segments[i].addr + intp->segments[i].len)
	    return &intp->segments[i];
    return 0;
}

void
segfault (interpreter_t *intp, word_32 addr)
{
    printf("segmentation fault for addr 0x%08x\n", addr);
    intp->halt = 1;
}

void
emulated_mem_set_32 (interpreter_t *intp, word_32 addr, word_32 value)
{
    segment_t *seg = find_segment(intp, addr);

    if (seg == 0
	|| !(addr >= seg->addr && addr + 4 <= seg->addr + seg->len)
	|| !(seg->flags & SEGMENT_WRITEABLE))
	segfault(intp, addr);
    else
    {
	if (debug)
	    printf("mem[%x] = %x\n", addr, value);

	*(word_32*)(seg->mem + (addr - seg->addr)) = value;
    }
}

void
emulated_mem_set_8 (interpreter_t *intp, word_32 addr, word_32 value)
{
    segment_t *seg;

    addr ^= 3;

    seg = find_segment(intp, addr);

    if (seg == 0
	|| !(addr >= seg->addr && addr < seg->addr + seg->len)
	|| !(seg->flags & SEGMENT_WRITEABLE))
	segfault(intp, addr);
    else
    {
	if (debug)
	    printf("mem8[%x] = %x\n", addr, value);

	seg->mem[addr - seg->addr] = value;
    }
}

void
emulated_mem_set_16 (interpreter_t *intp, word_32 addr, word_16 value)
{
    emulated_mem_set_8(intp, addr, value >> 8);
    emulated_mem_set_8(intp, addr + 1, value & 0xff);
}

void
emulated_mem_set_64 (interpreter_t *intp, word_32 addr, word_64 value)
{
    emulated_mem_set_32(intp, addr, value >> 32);
    emulated_mem_set_32(intp, addr + 4, value & 0xffffffff);
}

void
mem_copy_from_user_8 (interpreter_t *intp, byte *buf, word_32 addr, word_32 len)
{
    word_32 w;

    for (w = 0; w < len; ++w)
	buf[w] = mem_get_8(intp, addr + w);
}

void
mem_copy_to_user_32 (interpreter_t *intp, word_32 addr, byte *buf, word_32 len)
{
    word_32 w;

    assert((addr & 3) == 0);
    assert((len & 3) == 0);

    for (w = 0; w < len; w += 4)
	mem_set_32(intp, addr + w, *(word_32*)(buf + w));
}

void
mem_copy_from_user_32 (interpreter_t *intp, byte *buf, word_32 addr, word_32 len)
{
    word_32 w;

    assert((addr & 3) == 0);
    assert((len & 3) == 0);

    for (w = 0; w < len; w += 4)
	*(word_32*)(buf + w) = mem_get_32(intp, addr + w);
}

word_32
emulated_mem_get_32 (interpreter_t *intp, word_32 addr)
{
    segment_t *seg = find_segment(intp, addr);

    if (seg == 0
	|| !(addr >= seg->addr && addr + 4 <= seg->addr + seg->len)
	|| !(seg->flags & SEGMENT_READABLE))
    {
	segfault(intp, addr);
	return 0;
    }
    else
    {
	/*
	if (debug)
	    printf("mem[%x]\n", addr);
	*/

	return *(word_32*)(seg->mem + (addr - seg->addr));
    }
}

word_8
emulated_mem_get_8 (interpreter_t *intp, word_32 addr)
{
    segment_t *seg;

    addr ^= 3;
    seg = find_segment(intp, addr);

    if (seg == 0
	|| !(addr >= seg->addr && addr < seg->addr + seg->len)
	|| !(seg->flags & SEGMENT_READABLE))
    {
	segfault(intp, addr);
	return 0;
    }
    else
    {
	/*
	if (debug)
	    printf("mem8[%x]\n", addr);
	*/

	return (word_8)seg->mem[addr - seg->addr];
    }
}

word_16
emulated_mem_get_16 (interpreter_t *intp, word_32 addr)
{
    return ((word_16)emulated_mem_get_8(intp, addr) << 8) | emulated_mem_get_8(intp, addr + 1);
}

word_64
emulated_mem_get_64 (interpreter_t *intp, word_32 addr)
{
    return ((word_64)emulated_mem_get_32(intp, addr) << 32) | emulated_mem_get_32(intp, addr + 4);
}

word_32
rotl (word_32 x, word_32 i)
{
    assert(i <= 32);

    return (x << i) | (x >> (32 - i));
}

word_32
mask (word_32 begin, word_32 end)
{
    word_32 x = 0;
    word_32 b;
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

word_32
maskmask (word_32 width, word_32 num, word_32 mask)
{
    word_32 x = 0;
    word_32 mb = 1, xb = 1;
    word_32 i, j;

    for (i = 0; i < num; ++i)
    {
	word_32 b = mask & mb;

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

word_32
leading_zeros (word_32 w)
{
    word_32 m = 0x80000000;
    word_32 i;

    for (i = 0; i < 32; ++i)
    {
	if (w & m)
	    break;
	m >>= 1;
    }

    return i;
}

word_32
addcarry (word_32 op1, word_32 op2)
{
    if ((word_64)(op1 + op2) != (word_64)op1 + (word_64)op2)
	return 1;
    return 0;
}

#include "ppc_interpreter.c"
#include "ppc_disassembler.c"

int
can_mmap (interpreter_t *intp, word_32 addr, word_32 len)
{
    int i;

    if (intp->direct_memory)
	assert(0);
    else
	for (i = 0; i < intp->num_segments; ++i)
	    if (intp->segments[i].len > 0)
	    {
		if ((addr <= intp->segments[i].addr && addr + len >= intp->segments[i].addr)
		    || (addr >= intp->segments[i].addr && addr < intp->segments[i].addr + intp->segments[i].len))
		    return 0;
	    }

    return 1;
}

segment_t*
mmap_segment (interpreter_t *intp, word_32 len, int prot, int fixed, word_32 addr)
{
    byte *mem;
    segment_t *segment;
    int map_to_addr;

    if (addr != 0)
	map_to_addr = can_mmap(intp, addr, len);
    else
    {
	map_to_addr = 0;
	assert(can_mmap(intp, intp->mmap_addr, len));
    }

    if (fixed)
	assert(map_to_addr);

    assert(intp->num_segments < MAX_SEGMENTS);

    segment = &intp->segments[intp->num_segments++];

    if (map_to_addr)
	segment->addr = addr;
    else
	segment->addr = intp->mmap_addr;

    if (intp->direct_memory)
    {
	word_32 real_addr, real_len;

	align_segment(segment->addr, len, &real_addr, &real_len);
	assert(segment->addr == real_addr);
	mem = (byte*)mmap((void*)REAL_ADDR(real_addr), real_len, prot, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	assert((addr_t)mem == REAL_ADDR(real_addr));

	segment->real_addr = real_addr;
	segment->real_len = real_len;
	segment->real_mem = mem;

	if (!map_to_addr)
	    intp->mmap_addr += real_len;
    }
    else
    {
	mem = (byte*)malloc(len);
	if (mem == 0)
	    return 0;

	if (!map_to_addr)
	    intp->mmap_addr += len;
    }

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

segment_t*
mmap_anonymous_segment (interpreter_t *intp, word_32 len, int prot, int fixed, word_32 addr)
{
    return mmap_segment(intp, len, prot, fixed, addr);
}

void
unbigendify_mem (word_32 *p, word_32 len)
{
    word_32 i;

    assert((len & 3) == 0);
    len >>= 2;

    for (i = 0; i < len; ++i)
	p[i] = ntohl(p[i]);
}

ssize_t
read_all (int fd, byte *buf, size_t count)
{
    size_t num_read = 0;

    while (num_read < count)
    {
	ssize_t result = read(fd, buf + num_read, count - num_read);

	if (result == 0)
	    return num_read;
	if (result > 0)
	    num_read += result;
	else if (result != EINTR && result != EAGAIN)
	    return -1;
    }

    return -1;
}

segment_t*
mmap_file (interpreter_t *intp, word_32 len, int prot, int fixed, word_32 addr, int fd, word_32 offset)
{
    segment_t *segment = mmap_segment(intp, len, prot, fixed, addr);
    off_t curr_offset;
    off_t seek_result;
    word_32 num_read;

    curr_offset = lseek(fd, 0, SEEK_CUR);
    assert(curr_offset != (off_t)-1);

    seek_result = lseek(fd, offset, SEEK_SET);
    assert(seek_result != (off_t)-1);

    num_read = read_all(fd, segment->mem, len);
    assert(num_read != -1);

    unbigendify_mem((word_32*)segment->mem, len);

    seek_result = lseek(fd, curr_offset, SEEK_SET);
    assert(seek_result != (off_t)-1);

    return segment;
}

word_32
copy_string (interpreter_t *intp, char *str, word_32 p)
{
    word_32 len = strlen(str) + 1;
    word_32 i;

    p -= len;

    for (i = 0; i < len; ++i)
	mem_set_8(intp, p + i, str[i]);

    return p;
}

word_32
copy_strings (interpreter_t *intp, int num, char **strs, word_32 p)
{
    int i;

    for (i = num - 1; i >= 0; --i)
	p = copy_string(intp, strs[i], p);

    return p;
}

word_32
strlen_user (interpreter_t *intp, word_32 p)
{
    word_32 e = p;

    while (mem_get_8(intp, e++) != 0)
	;

    return e - p;
}

char*
strdup_from_user (interpreter_t *intp, word_32 p)
{
    word_32 len = strlen_user(intp, p);
    char *mem = (char*)malloc(len + 1);
    word_32 i;

    assert(mem != 0);
    for (i = 0; i <= len; ++i)
	mem[i] = mem_get_8(intp, p + i);

    return mem;
}

#define MAX_FILENAME_LEN      1023

char*
translate_filename (char *file)
{
    static char mangled[MAX_FILENAME_LEN + 1];

    if (file[0] != '/')
	return file;

    assert(strlen(file) + strlen(PPC_ROOT) <= MAX_FILENAME_LEN);

    strcpy(mangled, PPC_ROOT);
    strcat(mangled, file);

    return mangled;
}

void
handle_system_call (interpreter_t *intp)
{
    int result;

    switch (intp->regs_GPR[0])
    {
	case 1 :
	    printf("exit (%d)\n", intp->regs_GPR[3]);
	    printf("%ld insn executed\n", intp->insn_count);
#ifdef COMPILER
	    print_compiler_stats();
#endif
	    exit(intp->regs_GPR[3]);
	    break;

	case 3 :
	    printf("read\n");
	    {
		byte *mem = (byte*)malloc(intp->regs_GPR[5]);

		assert(mem != 0);

		result = read(intp->regs_GPR[3], mem, intp->regs_GPR[5]);

		if (result > 0)
		{
		    word_32 i;

		    for (i = 0; i < result; ++i)
			mem_set_8(intp, intp->regs_GPR[4] + i, mem[i]);
		}

		free(mem);
	    }
	    break;

	case 4 :
	    /* printf("write\n"); */
	    {
		byte *mem = (byte*)malloc(intp->regs_GPR[5]);
		word_32 i;

		assert(mem != 0);

		for (i = 0; i < intp->regs_GPR[5]; ++i)
		    mem[i] = mem_get_8(intp, intp->regs_GPR[4] + i);
		result = write(intp->regs_GPR[3], mem, intp->regs_GPR[5]);

		free(mem);
	    }
	    break;

	case 5 :
	    printf("open\n");
	    {
		char *real_name = strdup_from_user(intp, intp->regs_GPR[3]);
		char *name = translate_filename(real_name);
		word_32 ppc_flags = intp->regs_GPR[4];
		int flags;

		if ((ppc_flags & PPC_O_ACCMODE) == PPC_O_RDONLY)
		    flags = O_RDONLY;
		else if ((ppc_flags & PPC_O_ACCMODE) == PPC_O_WRONLY)
		    flags = O_WRONLY;
		else if ((ppc_flags & PPC_O_ACCMODE) == PPC_O_RDWR)
		    flags = O_RDWR;
		else
		    assert(0);
		if (ppc_flags & PPC_O_CREAT)
		    flags |= O_CREAT;
		if (ppc_flags & PPC_O_EXCL)
		    flags |= O_EXCL;
		if (ppc_flags & PPC_O_NOCTTY)
		    flags |= O_NOCTTY;
		if (ppc_flags & PPC_O_TRUNC)
		    flags |= O_TRUNC;
		if (ppc_flags & PPC_O_APPEND)
		    flags |= O_APPEND;
		if (ppc_flags & PPC_O_NONBLOCK)
		    flags |= O_NONBLOCK;
		if (ppc_flags & PPC_O_SYNC)
		    flags |= O_SYNC;
		if (ppc_flags & PPC_FASYNC)
		    flags |= FASYNC;
		/*
		if (ppc_flags & PPC_O_DIRECTORY)
		    flags |= O_DIRECTORY;
		if (ppc_flags & PPC_O_NOFOLLOW)
		    flags |= O_NOFOLLOW;
		*/

		result = open(name, flags);

		free(real_name);
	    }
	    break;

	case 6 :
	    printf("close\n");
	    result = close(intp->regs_GPR[3]);
	    break;

	case 13 :
	    printf("time\n");
	    assert(intp->regs_GPR[3] == 0);
	    result = (int)time(0);
	    break;

	case 20 :
	    printf("getpid\n");
	    result = getpid();
	    break;

	case 24 :
	    printf("getuid\n");
	    result = getuid();
	    break;

	case 45 :
	    printf("brk\n");
	    if (intp->regs_GPR[3] == 0)
		result = (int)(intp->data_segment->addr + intp->data_segment->len);
	    else
	    {
		if (intp->direct_memory)
		{
		    assert(intp->regs_GPR[3] > intp->data_segment->addr + intp->data_segment->len);

		    if (intp->data_segment->real_addr + intp->data_segment->real_len < intp->regs_GPR[3])
		    {
			word_32 real_addr, real_len;
			int prot = 0;
			void *p;

			align_segment(intp->data_segment->real_addr, intp->regs_GPR[3] - intp->data_segment->real_addr, &real_addr, &real_len);
			assert(real_addr == intp->data_segment->real_addr);

			if (intp->data_segment->flags & SEGMENT_READABLE)
			    prot |= PROT_READ;
			if (intp->data_segment->flags & SEGMENT_WRITEABLE)
			    prot |= PROT_WRITE;

			p = mmap((void*)REAL_ADDR(intp->data_segment->real_addr + intp->data_segment->real_len),
				 real_len - intp->data_segment->real_len,
				 prot, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
			assert((addr_t)p == REAL_ADDR(intp->data_segment->real_addr + intp->data_segment->real_len));

			intp->data_segment->real_len = real_len;
		    }
		
		    intp->data_segment->len = intp->regs_GPR[3] - intp->data_segment->addr;
		}
		else
		{
		    byte *new_mem;
		    word_32 new_len;

		    assert(intp->regs_GPR[3] > intp->data_segment->addr);

		    new_len = intp->regs_GPR[3] - intp->data_segment->addr;
		    new_mem = (byte*)realloc(intp->data_segment->mem, new_len);
		    assert(new_mem != 0);

		    if (new_len > intp->data_segment->len)
			memset(new_mem + intp->data_segment->len, 0, new_len - intp->data_segment->len);

		    intp->data_segment->mem = new_mem;
		    intp->data_segment->len = new_len;
		}

		result = (int)intp->regs_GPR[3];
	    }
	    break;

	case 47 :
	    printf("getgid\n");
	    result = getgid();
	    break;

	case 49 :
	    printf("geteuid\n");
	    result = geteuid();
	    break;

	case 50 :
	    printf("getegid\n");
	    result = getegid();
	    break;

	case 54 :
	    printf("ioctl\n");
	    {
		struct termios arg;

		assert(intp->regs_GPR[4] == PPC_TCGETS);
		result = ioctl(intp->regs_GPR[3], TCGETS, &arg);
		if (result == 0)
		    mem_copy_to_user_32(intp, intp->regs_GPR[5], (byte*)&arg, sizeof(struct termios));
		else
		    assert(0);
	    }
	    break;

	case 78 :
	    printf("gettimeofday\n");
	    {
		struct timeval tv;

		assert(intp->regs_GPR[4] == 0);	/* timezone */

		result = gettimeofday(&tv, 0);

		if (result == 0)
		{
		    mem_set_32(intp, intp->regs_GPR[3] + 0, tv.tv_sec);
		    mem_set_32(intp, intp->regs_GPR[3] + 4, tv.tv_usec);
		}
	    }
	    break;

	case 90 :
	    printf("mmap\n");
	    {
		segment_t *segment;
		word_32 len = PPC_PAGE_ALIGN(intp->regs_GPR[4]);

		assert(!(intp->regs_GPR[6] & PPC_MAP_SHARED));
		assert(intp->regs_GPR[6] & PPC_MAP_PRIVATE);

		if (intp->regs_GPR[6] & PPC_MAP_ANONYMOUS)
		{
		    assert(intp->regs_GPR[7] == -1);
		    assert(intp->regs_GPR[8] == 0);

		    segment = mmap_anonymous_segment(intp, len, intp->regs_GPR[5], intp->regs_GPR[6] & PPC_MAP_FIXED, intp->regs_GPR[3]);
		}
		else
		    segment = mmap_file(intp, len, intp->regs_GPR[5], intp->regs_GPR[6] & PPC_MAP_FIXED, intp->regs_GPR[3],
					intp->regs_GPR[7], intp->regs_GPR[8]);

		if (segment == 0)
		{
		    result = -1;
		    assert(0);
		}
		else
		    result = (int)segment->addr;
	    }
	    break;

	case 91 :
	    printf("munmap\n");
	    {
		int i;

		for (i = 0; i < intp->num_segments; ++i)
		    if (intp->segments[i].addr == intp->regs_GPR[3]
			&& intp->segments[i].len == intp->regs_GPR[4]
			&& (intp->segments[i].flags & SEGMENT_MMAPPED))
			break;

		if (i < intp->num_segments)
		{
		    intp->segments[i].addr = 0;
		    intp->segments[i].len = 0;
		    intp->segments[i].flags = 0;
		    if (intp->direct_memory)
		    {
			int result;

			result = munmap((void*)REAL_ADDR(intp->segments[i].real_addr), intp->segments[i].real_len);
			assert(result == 0);
		    }
		    else
			free(intp->segments[i].mem);
		    result = 0;
		}
		else
		{
		    result = -1;
		    assert(0);
		}
	    }
	    break;

	case 102 :
#define ARG(n)             (mem_get_32(intp, intp->regs_GPR[4] + (n) * 4))
	    switch (intp->regs_GPR[3])
	    {
		case SYS_SOCKET :
		    {
			int type;

			printf("socket\n");

			switch (ARG(1))
			{
			    case PPC_SOCK_STREAM :
				type = SOCK_STREAM;
				break;
			    case PPC_SOCK_DGRAM :
				type = SOCK_DGRAM;
				break;
			    case PPC_SOCK_RAW :
				type = SOCK_RAW;
				break;
			    case PPC_SOCK_RDM :
				type = SOCK_RDM;
				break;
			    case PPC_SOCK_SEQPACKET :
				type = SOCK_SEQPACKET;
				break;
			    case PPC_SOCK_PACKET :
				type = SOCK_PACKET;
				break;
			    default :
				assert(0);
			}

			assert(ARG(2) == 0);

			result = socket(ARG(0), type, 0);
		    }
		    break;

		case SYS_CONNECT :
		    {
			sa_family_t family = mem_get_16(intp, ARG(1));

			printf("connect\n");

			switch (family)
			{
			    case AF_UNIX :
				{
				    struct sockaddr_un su;
				    word_32 len = ARG(2) - 2;
				    char *real_name = malloc(len + 1);
				    char *name;

				    assert(len <= sizeof(su.sun_path));

				    mem_copy_from_user_8(intp, real_name, ARG(1) + 2, len);
				    real_name[len] = '\0';
				    name = translate_filename(real_name);
				    free(real_name);

				    su.sun_family = AF_UNIX;
				    assert(strlen(name) < sizeof(su.sun_path));
				    strcpy(su.sun_path, name);

				    result = connect(ARG(0), &su, sizeof(su) - sizeof(su.sun_path) + len);
				}
				break;

			    default :
				assert(0);
			}
		    }
		    break;

		case SYS_SETSOCKOPT :
		    {
			byte *optval;

			printf("setsockopt\n");

			optval = (byte*)malloc(ARG(4));
			assert(optval != 0);
			mem_copy_from_user_32(intp, optval, ARG(3), ARG(4));

			result = setsockopt(ARG(0), ARG(1), ARG(2), optval, ARG(4));

			free(optval);
		    }
		    break;

		default :
		    printf("unhandled socket call %d\n", intp->regs_GPR[3]);
		    intp->halt = 1;
	    }
#undef ARG
	    break;

	case 108 :
	    printf("fstat\n");
	    {
		struct stat buf;

		result = fstat(intp->regs_GPR[3], &buf);
		if (result == 0)
		{
		    mem_set_32(intp, intp->regs_GPR[4] + 0, buf.st_dev);
		    mem_set_32(intp, intp->regs_GPR[4] + 4, buf.st_ino);
		    mem_set_32(intp, intp->regs_GPR[4] + 8, buf.st_mode);
		    mem_set_16(intp, intp->regs_GPR[4] + 12, buf.st_nlink);
		    mem_set_32(intp, intp->regs_GPR[4] + 16, buf.st_uid);
		    mem_set_32(intp, intp->regs_GPR[4] + 20, buf.st_gid);
		    mem_set_32(intp, intp->regs_GPR[4] + 24, buf.st_rdev);
		    mem_set_32(intp, intp->regs_GPR[4] + 28, buf.st_size);
		    mem_set_32(intp, intp->regs_GPR[4] + 32, buf.st_blksize);
		    mem_set_32(intp, intp->regs_GPR[4] + 36, buf.st_blocks);
		    mem_set_32(intp, intp->regs_GPR[4] + 40, buf.st_atime);
		    mem_set_32(intp, intp->regs_GPR[4] + 48, buf.st_mtime);
		    mem_set_32(intp, intp->regs_GPR[4] + 56, buf.st_ctime);
		}
	    }
	    break;

	case 125 :
	    printf("mprotect\n");
	    result = 0;
	    break;

	case 136 :
	    printf("personality\n");
	    assert(intp->regs_GPR[3] == 0);
	    result = 0;
	    break;

	default :
	    printf("unhandled system call %d\n", intp->regs_GPR[0]);
	    intp->halt = 1;
    }

    if (result == -1)
    {
	int i;

	assert(errno > 0);

	for (i = 1; i <= LAST_PPC_ERRNO; ++i)
	    if (ppc_errnos[i] == errno)
		break;

	assert(i <= LAST_PPC_ERRNO);

	intp->regs_GPR[3] = (word_32)i;
	intp->regs_SPR[1] |= 0x10000000;
    }
    else
    {
	intp->regs_GPR[3] = (word_32)result;
	intp->regs_SPR[1] &= ~0x10000000;
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

word_32
setup_stack (interpreter_t *intp, word_32 p, char *argv[])
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

    word_32 exec;
    word_32 sp;
    word_32 csp;
    word_32 argvp, envp;

    for (argc = 0; argv[argc] != 0; ++argc)
	;

    p -= 4;
    p = copy_string(intp, argv[0], p);
    exec = p;
    p = copy_strings(intp, envc, env, p);
    p = copy_strings(intp, argc, argv, p);

    sp = (~15 & p) - 16;

    csp = sp;
    csp -= 4 * 4;		/* no interpreter, no ELF_PLATFORM */
    csp -= (envc + 1) * 4;
    csp -= (argc + 1) * 4;
    csp -= 1 * 4;		/* ibcs */
    if (csp & 15)
	sp -= csp & 15;

#define NEW_AUX_ENT(nr, id, val) \
    mem_set_32(intp, sp + (nr) * 2 * 4, id); \
    mem_set_32(intp, sp + ((nr) * 2 + 1) * 4, val);

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
    mem_set_32(intp, sp, argc);

    while (argc-- > 0) {
	mem_set_32(intp, argvp, p);
	argvp += 4;
	p += strlen_user(intp, p);
    }
    mem_set_32(intp, argvp, 0);

    while (envc-- > 0) {
	mem_set_32(intp, envp, p);
	envp += 4;
	p += strlen_user(intp, p);
    }
    mem_set_32(intp, envp, 0);

    return sp;
}

void
run_debugged (interpreter_t *intp)
{
    breakpoint_t *breakpoint;

    for (;;)
    {
	if (intp->trace)
	{
	    printf("%08x:  ", intp->pc);
	    disassemble_ppc_insn(mem_get_32(intp, intp->pc), intp->pc);
	    printf("   %08x", intp->regs_SPR[2]);
	    printf("\n");
	}
	interpret_ppc_insn(intp);
	if (intp->halt)
	    return;
	for (breakpoint = intp->breakpoints; breakpoint != 0; breakpoint = breakpoint->next)
	    if (breakpoint->addr == intp->pc)
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
show_breakpoints (interpreter_t *intp)
{
    int i;
    breakpoint_t *breakpoint;

    for (breakpoint = intp->breakpoints, i = 0; breakpoint != 0; breakpoint = breakpoint->next, ++i)
	printf("%3d  %08x\n", i, breakpoint->addr);
}

void
add_breakpoint (interpreter_t *intp, word_32 addr)
{
    breakpoint_t *breakpoint = (breakpoint_t*)malloc(sizeof(breakpoint_t));

    breakpoint->addr = addr;
    breakpoint->next = intp->breakpoints;
    intp->breakpoints = breakpoint;
}

void
delete_breakpoint (interpreter_t *intp, int num)
{
    breakpoint_t *breakpoint;

    if (num < 0)
	return;
    if (num == 0)
    {
	if (intp->breakpoints == 0)
	    return;
	breakpoint = intp->breakpoints;
	intp->breakpoints = breakpoint->next;
	free(breakpoint);
    }
    else
    {
	breakpoint_t *tmp;

	for (breakpoint = intp->breakpoints; breakpoint != 0 && num > 1; breakpoint = breakpoint->next, --num)
	    ;
	if (breakpoint == 0 || breakpoint->next == 0)
	    return;
	tmp = breakpoint->next;
	breakpoint->next = tmp->next;
	free(tmp);
    }
}

void
dump_memory (interpreter_t *intp, word_32 addr, word_32 len)
{
    word_32 w;

    for (w = addr; w < addr + len; w += 8)
    {
	word_32 o = 0;
	word_32 n = 8;

	if (addr + len - w < n)
	    n = addr + len - w;

	printf("%08x   ", w);
	for (o = 0; o < n; ++o)
	{
	    byte b = mem_get_8(intp, w + o);

	    printf("%02x %c  ", b, isprint(b) ? b : '.');
	}
	printf("\n");
    }
}

void
disassemble (interpreter_t *intp, word_32 addr, word_32 len)
{
    word_32 i;

    for (i = 0; i < len; ++i)
    {
	printf("%08x:  ", addr);
	disassemble_ppc_insn(mem_get_32(intp, addr), addr);
	printf("\n");

	addr += 4;
    }
}

void
show_segments (interpreter_t *intp)
{
    int i;

    printf("start     end       len       flags\n");
    printf("-----------------------------------\n");
    for (i = 0; i < intp->num_segments; ++i)
	if (intp->segments[i].len > 0)
	{
	    char flags[5] = "    ";

	    if (intp->segments[i].flags & SEGMENT_READABLE)
		flags[0] = 'r';
	    if (intp->segments[i].flags & SEGMENT_WRITEABLE)
		flags[1] = 'w';
	    if (intp->segments[i].flags & SEGMENT_EXECUTABLE)
		flags[2] = 'x';
	    if (intp->segments[i].flags & SEGMENT_MMAPPED)
		flags[3] = 'm';

	    printf("%08x  %08x  %8u  %s\n", intp->segments[i].addr,
		   intp->segments[i].addr + intp->segments[i].len, intp->segments[i].len, flags);
	}
}

void
debugger (interpreter_t *intp)
{
    char cmdline[CMDLINE_LENGTH];
    char token[CMDLINE_LENGTH];
    char *p;

    for (;;)
    {
	intp->halt = 0;

	printf("%08x > ", intp->pc);
	if (fgets(cmdline, CMDLINE_LENGTH, stdin) == NULL)
	    return;

	p = get_token(cmdline, token);
	if (p == 0)
	    continue;

	if (strcmp(token, "n") == 0)
	    interpret_ppc_insn(intp);
	else if (strcmp(token, "regs") == 0)
	    dump_ppc_registers(intp);
	else if (strcmp(token, "cont") == 0)
	    run_debugged(intp);
	else if (strcmp(token, "show") == 0)
	    show_breakpoints(intp);
	else if (strcmp(token, "help") == 0)
	    printf("rotfl!\n");
	else if (strcmp(token, "segs") == 0)
	    show_segments(intp);
	else if (strcmp(token, "trace") == 0)
	{
	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }

	    if (strcmp(token, "on") == 0)
		intp->trace = 1;
	    else
		intp->trace = 0;
	}
	else if (strcmp(token, "x") == 0)
	{
	    word_32 addr, len;

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

	    dump_memory(intp, addr, len);
	}
	else if (strcmp(token, "dis") == 0)
	{
	    word_32 addr, len;

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

	    disassemble(intp, addr, len);
	}
	else if (strcmp(token, "break") == 0)
	{
	    word_32 addr;

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }
	    addr = strtol(token, 0, 16);

	    add_breakpoint(intp, addr);
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

	    delete_breakpoint(intp, num);
	}
	else
	    printf("error\n");
    }
}

void
init_interpreter_struct (interpreter_t *intp, int direct_memory, int compiler)
{
    int i;

    intp->direct_memory = direct_memory;
    intp->compiler = compiler;
    intp->data_segment = 0;
    intp->num_segments = 0;
    intp->insn_count = 0;
    intp->halt = 0;
    intp->trace = !compiler;
    intp->mmap_addr = MMAP_START;
    intp->breakpoints = 0;

    for (i = 0; i < 5; ++i)
	intp->regs_SPR[i] = 0;
    for (i = 0; i < 32; ++i)
    {
	intp->regs_GPR[i] = 0; /* 0xdeadbe00 + i; */
	intp->regs_FPR[i] = 0.0;
    }
}

int
main (int argc, char *argv[])
{
    FILE *file;
    Elf32_Ehdr ehdr;
    Elf32_Phdr *phdrs = 0;
    word_32 stack_bottom;
    size_t num_read;
    int i;
    char **ppc_argv;
    segment_t *stack_segment;
#ifdef NEED_INTERPRETER
    interpreter_t interpreter;
#endif
#ifdef NEED_COMPILER
    interpreter_t compiler;
#endif

#ifdef NEED_INTERPRETER
    init_interpreter_struct(&interpreter, 0, 0);
#endif
#ifdef NEED_COMPILER
    init_interpreter_struct(&compiler, 1, 1);
#endif

    assert(argc >= 2);

    ppc_argv = (char**)malloc(sizeof(char*) * argc);
    ppc_argv[0] = "/bigben/home/schani/./a.out";
    for (i = 2; i < argc; ++i)
	ppc_argv[i - 1] = argv[i];
    ppc_argv[argc - 1] = 0;

    file = fopen(translate_filename(argv[1]), "r");
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
	int flags = 0;
	segment_t *segment;

	if (phdrs[i].p_type != PT_LOAD)
	    continue;

	if (phdrs[i].p_flags & PF_R)
	    flags |= SEGMENT_READABLE;
	if (phdrs[i].p_flags & PF_W)
	    flags |= SEGMENT_WRITEABLE;
	if (phdrs[i].p_flags & PF_X)
	    flags |= SEGMENT_EXECUTABLE;

#ifdef NEED_INTERPRETER
	segment = setup_segment(&interpreter, phdrs[i].p_vaddr, phdrs[i].p_memsz, flags);
	assert(segment != 0);

	memset(segment->mem, 0, segment->len);

	fseek(file, phdrs[i].p_offset, SEEK_SET);
	num_read = fread(segment->mem, 1, phdrs[i].p_filesz, file);
	assert(num_read = phdrs[i].p_filesz);

	unbigendify_mem((word_32*)segment->mem, segment->len);

	if (phdrs[i].p_type == PT_LOAD && phdrs[i].p_flags == (PF_W | PF_R))
	    interpreter.data_segment = segment;
#endif
#ifdef NEED_COMPILER
	segment = setup_segment(&compiler, phdrs[i].p_vaddr, phdrs[i].p_memsz, flags);
	assert(segment != 0);

	memset(segment->mem, 0, segment->len);

	fseek(file, phdrs[i].p_offset, SEEK_SET);
	num_read = fread(segment->mem, 1, phdrs[i].p_filesz, file);
	assert(num_read = phdrs[i].p_filesz);

	unbigendify_mem((word_32*)segment->mem, segment->len);

	protect_segment(&compiler, segment);

	if (phdrs[i].p_type == PT_LOAD && phdrs[i].p_flags == (PF_W | PF_R))
	    compiler.data_segment = segment;
#endif
    }

    fclose(file);

#ifdef NEED_INTERPRETER
    stack_segment = setup_segment(&interpreter, STACK_TOP - STACK_SIZE * PAGE_SIZE, STACK_SIZE * PAGE_SIZE, SEGMENT_READABLE | SEGMENT_WRITEABLE);

    memset(stack_segment->mem, 0, STACK_SIZE * PAGE_SIZE);

    assert(interpreter.data_segment != 0);

    stack_bottom = setup_stack(&interpreter, STACK_TOP, ppc_argv);
    assert((stack_bottom & 15) == 0);

    interpreter.regs_GPR[1] = stack_bottom;
    interpreter.pc = ehdr.e_entry;
#endif
#ifdef NEED_COMPILER
    stack_segment = setup_segment(&compiler, STACK_TOP - STACK_SIZE * PAGE_SIZE, STACK_SIZE * PAGE_SIZE, SEGMENT_READABLE | SEGMENT_WRITEABLE);

    memset(stack_segment->mem, 0, STACK_SIZE * PAGE_SIZE);

    assert(compiler.data_segment != 0);

    stack_bottom = setup_stack(&compiler, STACK_TOP, ppc_argv);
    assert((stack_bottom & 15) == 0);

    compiler.regs_GPR[1] = stack_bottom;
    compiler.pc = ehdr.e_entry;
#endif

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

#if defined(DEBUGGER)
    debugger(&interpreter);
#elif defined(COMPILER) || defined(CROSSDEBUGGER)
#ifdef CROSSDEBUGGER
    init_compiler(&compiler, &interpreter);
#else
    init_compiler(&compiler, 0);
#endif
    start_compiler(compiler.pc);
#elif defined(INTERPRETER)
    for (;;)
	interpret_ppc_insn(&interpreter);
#endif

    return 0;
}
