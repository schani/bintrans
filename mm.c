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

/*
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
*/

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
	    assert(0);
	else
	{
	    if (flags == 0 && level1[l2].mem != 0)
	    {
		free(level1[l2].mem);
		level1[l2].mem = 0;
	    }
	    else if (flags != 0 && level1[l2].mem == 0)
	    {
		level1[l2].mem = (byte*)malloc(PPC_PAGE_SIZE);
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

page_t*
get_page (interpreter_t *intp, word_32 addr)
{
    page_t *l2 = intp->pagetable[LEVEL1_INDEX(addr)];

    if (l2 == 0)
	return 0;
    return &l2[LEVEL2_INDEX(addr)];
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

void
emulated_mem_set_8 (interpreter_t *intp, word_32 addr, word_32 value)
{
    page_t *page;

    addr ^= 3;

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

    emulated_mem_set_8(intp, addr, value >> 8);
    emulated_mem_set_8(intp, addr + 1, value & 0xff);
}

void
emulated_mem_set_64 (interpreter_t *intp, word_32 addr, word_64 value)
{
    assert((addr & PPC_PAGE_MASK) + 8 <= PPC_PAGE_SIZE);

    emulated_mem_set_32(intp, addr, value >> 32);
    emulated_mem_set_32(intp, addr + 4, value & 0xffffffff);
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

word_8
emulated_mem_get_8 (interpreter_t *intp, word_32 addr)
{
    page_t *page;

    addr ^= 3;
    page = get_page(intp, addr);

    if (page == 0)
    {
	segfault(intp, addr ^ 3);
	return 0;
    }
    else
	return (word_8)page->mem[addr & PPC_PAGE_MASK];
}

word_16
emulated_mem_get_16 (interpreter_t *intp, word_32 addr)
{
    assert((addr & 1) == 0);

    return ((word_16)emulated_mem_get_8(intp, addr) << 8) | emulated_mem_get_8(intp, addr + 1);
}

word_64
emulated_mem_get_64 (interpreter_t *intp, word_32 addr)
{
    assert((addr & PPC_PAGE_MASK) + 8 <= PPC_PAGE_SIZE);

    return ((word_64)emulated_mem_get_32(intp, addr) << 32) | emulated_mem_get_32(intp, addr + 4);
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
mmap_anonymous (interpreter_t *intp, word_32 len, int prot, int fixed, word_32 addr)
{
    return mmap_segment(intp, len, prot, fixed, addr);
}

void
unbigendify_mem (word_32 *p, word_32 len)
{
    word_32 i;

    len = (len + 3) >> 2;

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
	else if (errno != EINTR && errno != EAGAIN)
	    return -1;
    }

    return num_read;
}

word_32
copy_file_to_mem (interpreter_t *intp, int fd, word_32 addr, word_32 len, word_32 offset, int reset)
{
    off_t curr_offset;
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

	assert(!intp->direct_memory);

	result = read_all(fd, page->mem + ((addr + num_read) & PPC_PAGE_MASK),
			  MIN(MIN(PPC_PAGE_SIZE, len - num_read), PPC_PAGE_SIZE - ((addr + num_read) & PPC_PAGE_MASK)));
	assert(result != -1);

	unbigendify_mem((word_32*)(page->mem + ((addr + num_read) & PPC_PAGE_MASK)), result);

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

    copy_file_to_mem(intp, fd, addr, len, offset, 1);

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
