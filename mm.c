#include <unistd.h>
#include <sys/mman.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>
#include <errno.h>

#include "bintrans.h"

extern int debug;

#ifndef MIN
#define MIN(a,b)    ((a)<(b)?(a):(b))
#endif

#define NATIVE_PAGE_SIZE      8192
#define NATIVE_PAGE_SHIFT       13
#define NATIVE_PAGE_MASK    0x1fff

int
prot_to_flags (int prot)
{
    int flags = 0;

    if (prot & PROT_READ)
	flags |= PAGE_READABLE;
    if (prot & PROT_WRITE)
	flags |= PAGE_WRITEABLE;
    if (prot & PROT_EXEC)
	flags |= PAGE_EXECUTABLE;

    return flags;
}

int
flags_to_prot (int flags)
{
    int prot = 0;

    if (flags & PAGE_READABLE)
	prot |= PROT_READ;
    if (flags & PAGE_WRITEABLE)
	prot |= PROT_WRITE;
    if (flags & PAGE_EXECUTABLE)
	prot |= PROT_EXEC;

    return prot;
}

page_t*
get_page (interpreter_t *intp, word_32 addr)
{
    page_t *l2 = intp->pagetable[LEVEL1_INDEX(addr)];

    if (l2 == 0)
	return 0;
    return &l2[LEVEL2_INDEX(addr)];
}

int
get_page_flags (interpreter_t *intp, word_32 addr)
{
    page_t *page = get_page(intp, addr);

    if (page == 0)
	return 0;
    return page->flags;
}

void
mprotect_pages (interpreter_t *intp, word_32 addr, word_32 len, int flags)
{
    word_32 num_pages;
    word_32 l1, l2;
    word_32 i;
    page_t *level1;

    assert((addr & PPC_PAGE_MASK) == 0);
    assert((len & PPC_PAGE_MASK) == 0);

    num_pages = len >> PPC_PAGE_SHIFT;

    assert(num_pages > 0);

    l1 = LEVEL1_INDEX(addr);
    l2 = LEVEL2_INDEX(addr);

    level1 = intp->pagetable[l1];

    i = 0;
    while (i < num_pages)
    {
	if (level1 == 0)
	{
	    if (flags == 0)
	    {
		i += LEVEL2_SIZE - (l2 & LEVEL2_MASK);
		++l1;
		assert((l1 & LEVEL1_MASK) != 0);
		l2 = 0;

		continue;
	    }
	    else
	    {
		level1 = intp->pagetable[l1] = (page_t*)malloc(sizeof(page_t) * LEVEL2_SIZE);
		memset(level1, 0, sizeof(page_t) * LEVEL2_SIZE);
	    }
	}

	level1[l2].flags = flags;
	if (intp->direct_memory)
	    level1[l2].mem = (byte*)REAL_ADDR((l1 << LEVEL1_SHIFT) | (l2 << LEVEL2_SHIFT));
	else
	{
	    if (flags == 0 && level1[l2].mem != 0)
	    {
		free(level1[l2].mem);
		level1[l2].mem = 0;
	    }
	    else if (flags != 0 && level1[l2].mem == 0)
	    {
		assert(flags & PAGE_MMAPPED);

		level1[l2].mem = (byte*)malloc(PPC_PAGE_SIZE);
	    }

	    if (flags & PAGE_MMAPPED)
	    {
		if (intp->direct_memory)
		{
		    if (get_page_flags(intp, ((l1 << LEVEL1_SHIFT) | (l2 << LEVEL2_SHIFT)) & ~NATIVE_PAGE_MASK) & PAGE_NATIVE_MMAPPED)
			memset(level1[l2].mem, 0, PPC_PAGE_SIZE);
		}
		else
		    memset(level1[l2].mem, 0, PPC_PAGE_SIZE);
	    }
	}

	++i;
	l2 = (l2 + 1) & LEVEL2_MASK;
	if (l2 == 0)
	{
	    ++l1;
	    assert((l1 & LEVEL1_MASK) != 0);
	    level1 = intp->pagetable[l1];
	}
    }
}

void
natively_mprotect_pages (interpreter_t *intp, word_32 addr, word_32 len, int flags)
{
    if (intp->direct_memory)
    {
	word_32 first_page_addr, last_page_addr;
	word_32 start, end;

	first_page_addr = addr & ~NATIVE_PAGE_MASK;
	last_page_addr = (addr + len - 1) & ~NATIVE_PAGE_MASK;

	start = first_page_addr;
	for (;;)
	{
	    do
	    {
		if ((flags != 0 && !(get_page_flags(intp, start) & PAGE_NATIVE_MMAPPED))
		    || (flags == 0 && (get_page_flags(intp, start) & PAGE_NATIVE_MMAPPED)))
		    break;
		start += NATIVE_PAGE_SIZE;
	    } while (start <= last_page_addr);

	    if (start > last_page_addr)
		break;

	    end = start;
	    while (end <= last_page_addr)
	    {
		if (flags != 0 && (get_page_flags(intp, end) & PAGE_NATIVE_MMAPPED))
		    break;
		else
		    get_page(intp, end)->flags &= ~PAGE_NATIVE_MMAPPED;

		if (flags == 0 && !(get_page_flags(intp, end) & PAGE_NATIVE_MMAPPED))
		    break;
		else
		    get_page(intp, end)->flags |= PAGE_NATIVE_MMAPPED;

		end += NATIVE_PAGE_SIZE;
	    }

	    if (flags == 0)
	    {
		int result;

		result = munmap((void*)REAL_ADDR(start), end - start);
		assert(result == 0);
	    }
	    else
	    {
		void *result;

		result = mmap((void*)REAL_ADDR(start), end - start, flags_to_prot(flags), MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		assert(result != 0);
	    }

	    start = end + NATIVE_PAGE_SIZE;
	}

	if (NATIVE_PAGE_SIZE <= PPC_PAGE_SIZE)
	{
	    int result;

	    result = mprotect((void*)REAL_ADDR(addr), len, flags_to_prot(flags));
	    assert(result == 0);
	}
	else
	{
	    int first_page_flags = flags, last_page_flags = flags;
	    word_32 a;
	    int result;

	    for (a = first_page_addr; a < first_page_addr + NATIVE_PAGE_SIZE; a += PPC_PAGE_SIZE)
		first_page_flags |= get_page_flags(intp, a);

	    if (first_page_addr != last_page_addr)
		for (a = last_page_addr; a < last_page_addr + NATIVE_PAGE_SIZE; a += PPC_PAGE_SIZE)
		    last_page_flags |= get_page_flags(intp, a);

	    /* first page */
	    result = mprotect((void*)REAL_ADDR(first_page_addr), NATIVE_PAGE_SIZE, flags_to_prot(first_page_flags));
	    assert(result == 0);

	    /* the bulk */
	    if (first_page_addr + NATIVE_PAGE_SIZE < last_page_addr)
	    {
		result = mprotect((void*)REAL_ADDR(first_page_addr + NATIVE_PAGE_SIZE), last_page_addr - first_page_addr - NATIVE_PAGE_SIZE,
				  flags_to_prot(flags));
		assert(result == 0);
	    }

	    /* last page */
	    if (first_page_addr != last_page_addr)
	    {
		result = mprotect((void*)REAL_ADDR(last_page_addr), NATIVE_PAGE_SIZE, flags_to_prot(last_page_flags));
		assert(result == 0);
	    }
	}
    }
}

void
segfault (interpreter_t *intp, word_32 addr)
{
    printf("segmentation fault for addr 0x%08x (pc=0x%08x)\n", addr, intp->pc);
    intp->halt = 1;
}

void
emulated_mem_set_32 (interpreter_t *intp, word_32 addr, word_32 value)
{
    if ((addr & 3) == 0)
    {
	page_t *page = get_page(intp, addr);

	assert((addr & PPC_PAGE_MASK) + 4 <= PPC_PAGE_SIZE);

	if (page == 0 || !(page->flags & PAGE_WRITEABLE))
	    segfault(intp, addr);
	else
	{
	    if (debug)
		printf("mem[%x] = %x\n", addr, value);

	    *(word_32*)(page->mem + (addr & PPC_PAGE_MASK)) = value;
	}
    }
    else
    {
	printf("unaligned write 32 access at 0x%08x (pc=0x%08x)\n", addr, intp->pc);

#ifdef EMU_BIG_ENDIAN
	emulated_mem_set_8(intp, addr, value >> 24);
	emulated_mem_set_8(intp, addr + 1, (value >> 16) & 0xff);
	emulated_mem_set_8(intp, addr + 2, (value >> 8) & 0xff);
	emulated_mem_set_8(intp, addr + 3, value & 0xff);
#else
	emulated_mem_set_8(intp, addr + 3, value >> 24);
	emulated_mem_set_8(intp, addr + 2, (value >> 16) & 0xff);
	emulated_mem_set_8(intp, addr + 1, (value >> 8) & 0xff);
	emulated_mem_set_8(intp, addr, value & 0xff);
#endif
    }
}

void
emulated_mem_set_8 (interpreter_t *intp, word_32 addr, word_32 value)
{
    page_t *page;

#ifdef DIFFERENT_BYTEORDER
    addr ^= 3;
#endif

    page = get_page(intp, addr);

    if (page == 0 || !(page->flags & PAGE_WRITEABLE))
	segfault(intp, addr);
    else
    {
	if (debug)
	    printf("mem8[%x] = %x\n", addr, value);

	page->mem[addr & PPC_PAGE_MASK] = value;
    }
}

void
emulated_mem_set_16 (interpreter_t *intp, word_32 addr, word_16 value)
{
    assert((addr & PPC_PAGE_MASK) + 2 <= PPC_PAGE_SIZE);

    if ((addr & 1) != 0)
	printf("unaligned write 16 access at 0x%08x (pc=0x%08x)\n", addr, intp->pc);

#ifdef EMU_BIG_ENDIAN
    emulated_mem_set_8(intp, addr, value >> 8);
    emulated_mem_set_8(intp, addr + 1, value & 0xff);
#else
    emulated_mem_set_8(intp, addr + 1, value >> 8);
    emulated_mem_set_8(intp, addr, value & 0xff);
#endif
}

void
emulated_mem_set_64 (interpreter_t *intp, word_32 addr, word_64 value)
{
    assert((addr & PPC_PAGE_MASK) + 8 <= PPC_PAGE_SIZE);

#ifdef EMU_BIG_ENDIAN
    emulated_mem_set_32(intp, addr, value >> 32);
    emulated_mem_set_32(intp, addr + 4, value & 0xffffffff);
#else
    emulated_mem_set_32(intp, addr + 4, value >> 32);
    emulated_mem_set_32(intp, addr, value & 0xffffffff);
#endif
}

word_16
mem_get_16_unaligned (interpreter_t *intp, word_32 addr)
{
#ifdef EMU_BIG_ENDIAN
    return (word_16)mem_get_8(intp, addr) << 8
	| (word_16)mem_get_8(intp, addr + 1);
#else
    return (word_16)mem_get_8(intp, addr + 1) << 8
	| (word_16)mem_get_8(intp, addr);
#endif
}

word_32
mem_get_32_unaligned (interpreter_t *intp, word_32 addr)
{
#ifdef EMU_BIG_ENDIAN
    return (word_32)mem_get_8(intp, addr) << 24
	| (word_32)mem_get_8(intp, addr + 1) << 16
	| (word_32)mem_get_8(intp, addr + 2) << 8
	| (word_32)mem_get_8(intp, addr + 3);
#else
    return (word_32)mem_get_8(intp, addr + 3) << 24
	| (word_32)mem_get_8(intp, addr + 2) << 16
	| (word_32)mem_get_8(intp, addr + 1) << 8
	| (word_32)mem_get_8(intp, addr);
#endif
}

void
mem_copy_to_user_8 (interpreter_t *intp, word_32 addr, byte *buf, word_32 len)
{
    word_32 w;

    for (w = 0; w < len; ++w)
	mem_set_8(intp, addr + w, buf[w]);
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
    if ((addr & 3) == 0)
    {
	page_t *page = get_page(intp, addr);

	assert((addr & PPC_PAGE_MASK) + 4 <= PPC_PAGE_SIZE);

	if (page == 0)
	{
	    segfault(intp, addr);
	    return 0;
	}
	else
	    return *(word_32*)(page->mem + (addr & PPC_PAGE_MASK));
    }
    else
    {
	printf("unaligned read 32 access at 0x%08x (pc=0x%08x)\n", addr, intp->pc);

	return ((word_32)emulated_mem_get_8(intp, addr) << 24)
	    | ((word_32)emulated_mem_get_8(intp, addr + 1) << 16)
	    | ((word_32)emulated_mem_get_8(intp, addr + 2) << 8)
	    | (word_32)emulated_mem_get_8(intp, addr + 3);
    }
}

word_8
emulated_mem_get_8 (interpreter_t *intp, word_32 addr)
{
    page_t *page;

#ifdef DIFFERENT_BYTEORDER
    addr ^= 3;
#endif
    page = get_page(intp, addr);

    if (page == 0)
    {
	segfault(intp, addr
#ifdef DIFFERENT_BYTEORDER
		 ^ 3
#endif
		 );
	return 0;
    }
    else
	return (word_8)page->mem[addr & PPC_PAGE_MASK];
}

word_16
emulated_mem_get_16 (interpreter_t *intp, word_32 addr)
{
    if ((addr & 1) != 0)
	printf("unaligned read 16 access at 0x%08x (pc=0x%08x)\n", addr, intp->pc);

#ifdef EMU_BIG_ENDIAN
    return ((word_16)emulated_mem_get_8(intp, addr) << 8) | emulated_mem_get_8(intp, addr + 1);
#else
    return ((word_16)emulated_mem_get_8(intp, addr + 1) << 8) | emulated_mem_get_8(intp, addr);
#endif
}

word_64
emulated_mem_get_64 (interpreter_t *intp, word_32 addr)
{
    assert((addr & PPC_PAGE_MASK) + 8 <= PPC_PAGE_SIZE);

#ifdef EMU_BIG_ENDIAN
    return ((word_64)emulated_mem_get_32(intp, addr) << 32) | emulated_mem_get_32(intp, addr + 4);
#else
    return ((word_64)emulated_mem_get_32(intp, addr + 4) << 32) | emulated_mem_get_32(intp, addr);
#endif
}

word_32
first_fit_addr (interpreter_t *intp, word_32 start, word_32 len)
{
    word_32 num_pages;
    word_32 l1, l2;
    word_32 i;
    page_t *level1;

    assert((start & PPC_PAGE_MASK) == 0);
    assert((len & PPC_PAGE_MASK) == 0);

    num_pages = len >> PPC_PAGE_SHIFT;

    for (;;)
    {
	word_32 end;

	l1 = LEVEL1_INDEX(start);
	l2 = LEVEL2_INDEX(start);

	level1 = intp->pagetable[l1];

	/* find a free page */
	for (;;)
	{
	    if (level1 == 0)
		break;
	    if (level1[l2].flags == 0)
		break;

	    start += PPC_PAGE_SIZE;
	    l2 = (l2 + 1) & LEVEL2_MASK;
	    if (l2 == 0)
	    {
		++l1;
		assert((l1 & LEVEL1_MASK) != 0);
		level1 = intp->pagetable[l1];
	    }
	}

	end = start;

	/* see whether there are enough free pages */
	i = 0;
	while (i < num_pages)
	{
	    if (level1 != 0 && level1[l2].flags != 0)
		break;

	    ++i;

	    end += PPC_PAGE_SIZE;
	    l2 = (l2 + 1) & LEVEL2_MASK;
	    if (l2 == 0)
	    {
		++l1;
		assert((l1 & LEVEL1_MASK) != 0);
		level1 = intp->pagetable[l1];
	    }
	}

	if (i >= num_pages)
	    return start;

	start = end;
    }

    return 0;
}

word_32
mmap_segment (interpreter_t *intp, word_32 len, int flags, int fixed, word_32 addr)
{
    word_32 used_addr;

    used_addr = first_fit_addr(intp, addr == 0 ? MMAP_START : addr, len);

    assert(used_addr != 0);

    if (fixed)
	assert(used_addr == addr);

    mprotect_pages(intp, used_addr, len, flags | PAGE_MMAPPED);

    return used_addr;
}

word_32
mmap_anonymous (interpreter_t *intp, word_32 len, int flags, int fixed, word_32 addr)
{
    addr = mmap_segment(intp, len, flags | PAGE_MMAPPED, fixed, addr);

    if (addr != 0)
	natively_mprotect_pages(intp, addr, len, flags);
    return addr;
}

#ifdef DIFFERENT_BYTEORDER
void
unbigendify_mem (word_32 *p, word_32 len)
{
    word_32 i;

    len = (len + 3) >> 2;

    for (i = 0; i < len; ++i)
	p[i] = ntohl(p[i]);
}
#endif

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
	else if (errno != EINTR && errno != EAGAIN)
	    return -1;
    }

    return num_read;
}

word_32
copy_file_to_mem (interpreter_t *intp, int fd, word_32 addr, word_32 len, word_32 offset, int reset)
{
    off_t curr_offset = 0;
    off_t seek_result;
    word_32 num_read = 0;

    if (reset)
    {
	curr_offset = lseek(fd, 0, SEEK_CUR);
	assert(curr_offset != (off_t)-1);
    }

    seek_result = lseek(fd, offset, SEEK_SET);
    assert(seek_result != (off_t)-1);

    while (num_read < len)
    {
	page_t *page = get_page(intp, addr + num_read);
	int result;

	result = read_all(fd, page->mem + ((addr + num_read) & PPC_PAGE_MASK),
			  MIN(MIN(PPC_PAGE_SIZE, len - num_read), PPC_PAGE_SIZE - ((addr + num_read) & PPC_PAGE_MASK)));
	assert(result != -1);

#ifdef DIFFERENT_BYTEORDER
	unbigendify_mem((word_32*)(page->mem + ((addr + num_read) & PPC_PAGE_MASK)), result);
#endif

	if (result == 0)
	    break;

	num_read += result;
    }

    if (reset)
    {
	seek_result = lseek(fd, curr_offset, SEEK_SET);
	assert(seek_result != (off_t)-1);
    }

    return num_read;
}

word_32
mmap_file (interpreter_t *intp, word_32 len, int flags, int fixed, word_32 addr, int fd, word_32 offset)
{
    addr = mmap_segment(intp, len, flags, fixed, addr);

    if (addr == 0)
	return 0;

    natively_mprotect_pages(intp, addr, len, PAGE_WRITEABLE);

    copy_file_to_mem(intp, fd, addr, len, offset, 1);

    natively_mprotect_pages(intp, addr, len, flags);

    return addr;
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
