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
#define _LINUX_SOCKET_H
#include <linux/net.h>
#include <sys/time.h>
#include <errno.h>
#include <sys/un.h>
#include <sys/utsname.h>

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

#if defined(EMU_I386)
#define EMU_HWCAPS 0x0080f9ff
#elif defined(EMU_PPC)
#define EMU_HWCAPS 0x10
#endif

#if defined(EMU_PPC)
#define STACK_TOP  0x80000000
#elif defined(EMU_I386)
#define STACK_TOP  0xc0000000
#endif
#define STACK_SIZE        128

#define PPC_MAP_SHARED    0x01
#define PPC_MAP_PRIVATE   0x02
#define PPC_MAP_FIXED     0x10
#define PPC_MAP_ANONYMOUS 0x20

#if defined(EMU_PPC)
#define EMU_FIONREAD 0x4004667f
#define EMU_TCGETS   0x402c7413
#elif defined(EMU_I386)
#define EMU_FIONREAD     0x5401
#define EMU_TCGETS       0x541b
#endif

#define PPC_SOCK_STREAM		1
#define PPC_SOCK_DGRAM		2
#define PPC_SOCK_RAW		3
#define PPC_SOCK_RDM		4
#define PPC_SOCK_SEQPACKET	5
#define PPC_SOCK_PACKET		10

#if defined(EMU_PPC)
#define EMU_O_ACCMODE	  0003
#define EMU_O_RDONLY	    00
#define EMU_O_WRONLY	    01
#define EMU_O_RDWR	    02
#define EMU_O_CREAT	  0100	/* not fcntl */
#define EMU_O_EXCL	  0200	/* not fcntl */
#define EMU_O_NOCTTY	  0400	/* not fcntl */
#define EMU_O_TRUNC	 01000	/* not fcntl */
#define EMU_O_APPEND	 02000
#define EMU_O_NONBLOCK	 04000
#define EMU_O_NDELAY	EMU_O_NONBLOCK
#define EMU_O_SYNC	010000
#define EMU_O_ASYNC	020000	/* fcntl, for BSD compatibility */
#define EMU_O_DIRECTORY	040000	/* must be a directory */
#define EMU_O_NOFOLLOW	0100000	/* don't follow links */

#define EMU_F_GETFD             1
#define EMU_F_SETFD             2
#define EMU_F_GETFL             3
#define EMU_F_SETFL             4

#define EMU_FD_CLOEXEC          1
#elif defined(EMU_I386)
#define EMU_O_ACCMODE	   0003
#define EMU_O_RDONLY	     00
#define EMU_O_WRONLY	     01
#define EMU_O_RDWR	     02
#define EMU_O_CREAT	   0100	/* not fcntl */
#define EMU_O_EXCL	   0200	/* not fcntl */
#define EMU_O_NOCTTY	   0400	/* not fcntl */
#define EMU_O_TRUNC	  01000	/* not fcntl */
#define EMU_O_APPEND	  02000
#define EMU_O_NONBLOCK	  04000
#define EMU_O_NDELAY	EMU_O_NONBLOCK
#define EMU_O_SYNC	 010000
#define EMU_O_ASYNC	 020000	/* fcntl, for BSD compatibility */
#define EMU_O_DIRECT     040000 /* direct disk access hint - currently ignored */
#define EMU_O_LARGEFILE 0100000
#define EMU_O_DIRECTORY 0200000 /* must be a directory */
#define EMU_O_NOFOLLOW  0400000 /* don't follow links */

#define EMU_F_GETFD             1
#define EMU_F_SETFD             2
#define EMU_F_GETFL             3
#define EMU_F_SETFL             4

#define EMU_FD_CLOEXEC          1
#endif


#if COMPLANG
#if defined(EMU_PPC)
#define EMU_ROOT                "/a5/schani/ppc-root"
#elif defined(EMU_I386)
#define EMU_ROOT                "/a5/schani/i386-root"
#endif
#else
#if defined(EMU_PPC)
#define EMU_ROOT                "/nethome/hansolo/schani/Work/unix/bintrans/ppc-root"
#elif defined(EMU_I386)
#define EMU_ROOT                "/nethome/hansolo/schani/Work/unix/bintrans/i386-root"
#endif
#endif

#if defined(EMU_PPC)
int emu_errnos[] = { 0,
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
#define LAST_EMU_ERRNO 124

#define SYSCALL_EXIT               1
#define SYSCALL_READ               3
#define SYSCALL_WRITE              4
#define SYSCALL_OPEN               5
#define SYSCALL_CLOSE              6
#define SYSCALL_TIME              13
#define SYSCALL_GETPID            20
#define SYSCALL_GETUID            24
#define SYSCALL_ACCESS            33
#define SYSCALL_BRK               45
#define SYSCALL_GETGID            47
#define SYSCALL_GETEUID           49
#define SYSCALL_GETEGID           50
#define SYSCALL_IOCTL             54
#define SYSCALL_FCNTL             55
#define SYSCALL_GETTIMEOFDAY      78
#define SYSCALL_MMAP              90
#define SYSCALL_MUNMAP            91
#define SYSCALL_SOCKETCALL       102
#define SYSCALL_FSTAT            108
#define SYSCALL_UNAME            122
#define SYSCALL_MPROTECT         125
#define SYSCALL_PERSONALITY      136
#define SYSCALL_LLSEEK           140
#define SYSCALL_SELECT           142
#define SYSCALL_READV            145
#define SYSCALL_WRITEV           146
#define SYSCALL_RT_SIGACTION     173
#elif defined(EMU_I386)
int emu_errnos[] = { 0,
		     EPERM, ENOENT, ESRCH, EINTR, EIO, ENXIO, E2BIG, ENOEXEC, EBADF,
		     ECHILD, EAGAIN, ENOMEM, EACCES, EFAULT, ENOTBLK, EBUSY, EEXIST,
		     EXDEV, ENODEV, ENOTDIR, EISDIR, EINVAL, ENFILE, EMFILE, ENOTTY,
		     ETXTBSY, EFBIG, ENOSPC, ESPIPE, EROFS, EMLINK, EPIPE, EDOM, ERANGE,
		     EDEADLK, ENAMETOOLONG, ENOLCK, ENOSYS, ENOTEMPTY, ELOOP,
		     0 /* EWOULDBLOCK */, ENOMSG, EIDRM, ECHRNG, EL2NSYNC, EL3HLT,
		     EL3RST, ELNRNG, EUNATCH, ENOCSI, EL2HLT, EBADE, EBADR, EXFULL,
		     ENOANO, EBADRQC, EBADSLT, 0 /* EDEADLOCK */, EBFONT, ENOSTR,
		     ENODATA, ETIME, ENOSR, ENONET, ENOPKG, EREMOTE, ENOLINK, EADV,
		     ESRMNT, ECOMM, EPROTO, EMULTIHOP, EDOTDOT, EBADMSG, EOVERFLOW,
		     ENOTUNIQ, EBADFD, EREMCHG, ELIBACC, ELIBBAD, ELIBSCN, ELIBMAX,
		     ELIBEXEC, EILSEQ, ERESTART, ESTRPIPE, EUSERS, ENOTSOCK,
		     EDESTADDRREQ, EMSGSIZE, EPROTOTYPE, ENOPROTOOPT, EPROTONOSUPPORT,
		     ESOCKTNOSUPPORT, EOPNOTSUPP, EPFNOSUPPORT, EAFNOSUPPORT, EADDRINUSE,
		     EADDRNOTAVAIL, ENETDOWN, ENETUNREACH, ENETRESET, ECONNABORTED,
		     ECONNRESET, ENOBUFS, EISCONN, ENOTCONN, ESHUTDOWN, ETOOMANYREFS,
		     ETIMEDOUT, ECONNREFUSED, EHOSTDOWN, EHOSTUNREACH, EALREADY,
		     EINPROGRESS, ESTALE, EUCLEAN, ENOTNAM, ENAVAIL, EISNAM, EREMOTEIO,
		     EDQUOT, ENOMEDIUM, EMEDIUMTYPE };
#define LAST_EMU_ERRNO 124

#define SYSCALL_EXIT               1
#define SYSCALL_READ               3
#define SYSCALL_WRITE              4
#define SYSCALL_OPEN               5
#define SYSCALL_CLOSE              6
#define SYSCALL_TIME              13
#define SYSCALL_GETPID            20
#define SYSCALL_GETUID            24
#define SYSCALL_ACCESS            33
#define SYSCALL_BRK               45
#define SYSCALL_GETGID            47
#define SYSCALL_GETEUID           49
#define SYSCALL_GETEGID           50
#define SYSCALL_IOCTL             54
#define SYSCALL_FCNTL             55
#define SYSCALL_GETTIMEOFDAY      78
#define SYSCALL_MMAP              90
#define SYSCALL_MUNMAP            91
#define SYSCALL_SOCKETCALL       102
#define SYSCALL_FSTAT            108
#define SYSCALL_UNAME            122
#define SYSCALL_MPROTECT         125
#define SYSCALL_PERSONALITY      136
#define SYSCALL_LLSEEK           140
#define SYSCALL_SELECT           142
#define SYSCALL_READV            145
#define SYSCALL_WRITEV           146
#define SYSCALL_RT_SIGACTION     173
#endif

#define SYSCALL_OUTPUT
#ifdef SYSCALL_OUTPUT
#define ANNOUNCE_SYSCALL(n)         printf("%s\n", (n))
#else
#define ANNOUNCE_SYSCALL(n)
#endif

/*
#define FAKE_PID            0x4851

#define FAKE_FSTAT_DEV      0
#define FAKE_FSTAT_INO      0x3242bf
#define FAKE_FSTAT_NLINK    1
#define FAKE_FSTAT_MODE     0x1180
#define FAKE_FSTAT_RDEV     0x3db
#define FAKE_FSTAT_SIZE     0
#define FAKE_FSTAT_BLOCKS   4
#define FAKE_FSTAT_BLKSIZE  0x1000
#define FAKE_FSTAT_ATIME    0x3b4cdcac
#define FAKE_FSTAT_MTIME    0x3b4cdcac
#define FAKE_FSTAT_CTIME    0x3b4cdcac
*/

int debug = 0;

#define MAX_FDS      1024

fd_mapping_t fd_map[MAX_FDS];

word_32
rotl (word_32 x, word_32 i)
{
    assert(i <= 32);

    return (x << i) | (x >> (32 - i));
}

#define rotl_32       rotl

word_32
mask_32 (word_32 begin, word_32 end)
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

word_64
mask_64 (word_32 begin, word_32 end)
{
    word_64 x = 0;
    word_64 b;
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
	for (i = begin; i < 64; ++i)
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

int
can_inv_maskmask (int width, word_64 value)
{
    word_64 bitmask = ((1 << width) - 1);

    while (value != 0)
    {
	if ((value & bitmask) != 0 && (value & bitmask) != bitmask)
	    return 0;
	value >>= width;
    }

    return 1;
}

word_64
inv_maskmask (int width, word_64 value)
{
    word_64 bit = 1;
    word_64 result = 0;

    while (value != 0)
    {
	if (value & 1)
	    result |= bit;
	bit <<= 1;
	value >>= width;
    }

    return result;
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
addcarry_32 (word_32 op1, word_32 op2)
{
    if (((word_64)op1 + (word_64)op2) >> 32 != 0)
	return 1;
    return 0;
}

word_32
addcarry_16 (word_16 op1, word_16 op2)
{
    if (((word_32)op1 + (word_32)op2) >> 16 != 0)
	return 1;
    return 0;
}

word_32
addcarry_8 (word_8 op1, word_8 op2)
{
    if (((word_32)op1 + (word_32)op2) >> 8 != 0)
	return 1;
    return 0;
}

word_32
subcarry_32 (word_32 op1, word_32 op2)
{
    if (((word_64)op1 - (word_64)op2) >> 32 != 0)
	return 1;
    return 0;
}

word_32
subcarry_16 (word_16 op1, word_16 op2)
{
    if (((word_32)op1 - (word_32)op2) >> 16 != 0)
	return 1;
    return 0;
}

word_32
subcarry_8 (word_8 op1, word_8 op2)
{
    if (((word_32)op1 - (word_32)op2) >> 8 != 0)
	return 1;
    return 0;
}

word_32
addoverflow_32 (word_32 op1, word_32 op2)
{
#define MASK  0x80000000
    if ((op1 & MASK) == (op2 & MASK))
    {
	word_32 result = op1 + op2;

	if ((result & MASK) != (op1 & MASK))
	    return 1;
    }

    return 0;
#undef MASK
}

word_32
addoverflow_16 (word_16 op1, word_16 op2)
{
#define MASK  0x8000
    if ((op1 & MASK) == (op2 & MASK))
    {
	word_16 result = op1 + op2;

	if ((result & MASK) != (op1 & MASK))
	    return 1;
    }

    return 0;
#undef MASK
}

word_32
addoverflow_8 (word_8 op1, word_8 op2)
{
#define MASK  0x80
    if ((op1 & MASK) == (op2 & MASK))
    {
	word_8 result = op1 + op2;

	if ((result & MASK) != (op1 & MASK))
	    return 1;
    }

    return 0;
#undef MASK
}

#ifdef EMU_PPC
#include "ppc_interpreter.c"
#include "ppc_disassembler.c"
#endif

#define MAX_FILENAME_LEN      1023

char*
translate_filename (char *file)
{
    static char *unmangled[] = { "/tmp/.X11-unix/X0",
				 0 };

    static char mangled[MAX_FILENAME_LEN + 1];

    int i;

    if (file[0] != '/')
	return file;

    for (i = 0; unmangled[i] != 0; ++i)
	if (strcmp(unmangled[i], file) == 0)
	    return file;

    assert(strlen(file) + strlen(EMU_ROOT) <= MAX_FILENAME_LEN);

    strcpy(mangled, EMU_ROOT);
    strcat(mangled, file);

    return mangled;
}

int
open_fd (int fd)
{
    int i;

    for (i = 0; i < MAX_FDS; ++i)
	if (fd_map[i].free)
	    break;

    assert(i < MAX_FDS);

    fd_map[i].free = 0;
    fd_map[i].native_fd = fd;

    return i;
}

int
lookup_fd (word_32 fd)
{
    assert(fd < MAX_FDS);
    if (fd_map[fd].free)
	return -1;
    return fd_map[fd].native_fd;
}

int
reverse_lookup_fd (int fd)
{
    int i;

    for (i = 0; i < MAX_FDS; ++i)
	if (!fd_map[i].free && fd_map[i].native_fd == fd)
	    return i;

    return -1;
}

void
close_fd (word_32 fd)
{
    assert(fd < MAX_FDS);
    assert(!fd_map[fd].free);
    fd_map[fd].free = 1;
}

void
convert_native_sockaddr_to_ppc (interpreter_t *intp, struct sockaddr *sa, word_32 ppc_addr, word_32 ppc_len, word_32 *used_len)
{
    switch (sa->sa_family)
    {
	case AF_INET :
	    {
		struct sockaddr_in *si = (struct sockaddr_in*)sa;

		assert(ppc_len >= 16);

		*used_len = 16;

		mem_set_16(intp, ppc_addr + 0, AF_INET);
		mem_set_16(intp, ppc_addr + 2, ntohs(si->sin_port));
		mem_set_32(intp, ppc_addr + 4, ntohl(si->sin_addr.s_addr));
		mem_set_64(intp, ppc_addr + 8, 0);
	    }
	    break;

	default :
	    assert(0);
    }
}

int
convert_ppc_fdset_to_native (interpreter_t *intp, int maxfd, fd_set *fds, word_32 addr)
{
    word_32 bits = 0;
    int i;

    FD_ZERO(fds);

    for (i = 0; i < maxfd; ++i)
    {
	if ((i & 31) == 0)
	    bits = mem_get_32(intp, addr + (i >> 3));
	if (bits & 1)
	{
	    int fd = lookup_fd(i);

	    if (fd == -1)
		return -1;

	    FD_SET(fd, fds);
	    bits >>= 1;
	}
    }

    return 0;
}

void
convert_native_fdset_to_ppc (interpreter_t *intp, int maxfd, word_32 addr, fd_set *fds)
{
    word_32 bits = 0;
    int i;

    for (i = 0; i < maxfd; ++i)
    {
	int fd;

	if ((i & 31) == 0)
	    bits = 0;

	fd = reverse_lookup_fd(i);
	if (fd != -1 && FD_ISSET(fd, fds))
	    bits |= 1 << (i & 31);

	if ((i & 31) == 31 || i == maxfd - 1)
	    mem_set_32(intp, addr + ((i & ~31) >> 3), bits);
    }
}

void
convert_ppc_timeval_to_native (interpreter_t *intp, struct timeval *tv, word_32 addr)
{
    tv->tv_sec = mem_get_32(intp, addr + 0);
    tv->tv_usec = mem_get_32(intp, addr + 4);
}

void
convert_native_timeval_to_ppc (interpreter_t *intp, word_32 addr, struct timeval *tv)
{
    mem_set_32(intp, addr + 0, tv->tv_sec);
    mem_set_32(intp, addr + 4, tv->tv_usec);
}

#if defined(EMU_PPC)
void
convert_native_termios_to_emu (interpreter_t *intp, word_32 addr, struct termios *tio)
{
    mem_set_32(intp, addr + 0, tio->c_iflag);
    mem_set_32(intp, addr + 4, tio->c_oflag);
    mem_set_32(intp, addr + 8, tio->c_cflag);
    mem_set_32(intp, addr + 12, tio->c_lflag);
    mem_copy_to_user_8(intp, addr + 16, tio->c_cc, 19);
    mem_set_8(intp, addr + 35, tio->c_line);
    mem_set_32(intp, addr + 36, tio->c_ispeed);
    mem_set_32(intp, addr + 40, tio->c_ospeed);
}
#elif defined(EMU_I386)
void
convert_native_termios_to_emu (interpreter_t *intp, word_32 addr, struct termios *tio)
{
    mem_set_32(intp, addr + 0, tio->c_iflag);
    mem_set_32(intp, addr + 4, tio->c_oflag);
    mem_set_32(intp, addr + 8, tio->c_cflag);
    mem_set_32(intp, addr + 12, tio->c_lflag);
    mem_set_8(intp, addr + 16, tio->c_line);
    mem_copy_to_user_8(intp, addr + 17, tio->c_cc, 19);
}
#endif

int
process_system_call (interpreter_t *intp, word_32 number,
		     word_32 arg1, word_32 arg2, word_32 arg3, word_32 arg4, word_32 arg5, word_32 arg6)
{
    int result;
    int fd;

    switch (number)
    {
	case SYSCALL_EXIT :
	    printf("exit (%d)\n", arg1);
	    printf("%ld insn executed\n", intp->insn_count);
#ifdef COMPILER
	    print_compiler_stats();
#endif
	    exit(arg1);
	    break;

	case SYSCALL_READ :
	    ANNOUNCE_SYSCALL("read");
	    fd = lookup_fd(arg1);
	    if (fd == -1)
	    {
		result = -1;
		errno = EBADF;
	    }
	    else
	    {
		byte *mem = (byte*)malloc(arg3);

		assert(mem != 0);

		result = read(fd, mem, arg3);

		if (result > 0)
		{
		    word_32 i;

		    for (i = 0; i < result; ++i)
			mem_set_8(intp, arg2 + i, mem[i]);
		}

		free(mem);
	    }
	    break;

	case SYSCALL_WRITE :
	    ANNOUNCE_SYSCALL("write");
	    fd = lookup_fd(arg1);
	    if (fd == -1)
	    {
		result = -1;
		errno = EBADF;
	    }
	    else
	    {
		byte *mem = (byte*)malloc(arg3);
		word_32 i;

		assert(mem != 0);

		for (i = 0; i < arg3; ++i)
		    mem[i] = mem_get_8(intp, arg2 + i);
		result = write(fd, mem, arg3);

		free(mem);
	    }
	    break;

	case SYSCALL_OPEN :
	    ANNOUNCE_SYSCALL("open");
	    {
		char *real_name = strdup_from_user(intp, arg1);
		char *name = translate_filename(real_name);
		word_32 ppc_flags = arg2;
		int flags;

		if ((ppc_flags & EMU_O_ACCMODE) == EMU_O_RDONLY)
		    flags = O_RDONLY;
		else if ((ppc_flags & EMU_O_ACCMODE) == EMU_O_WRONLY)
		    flags = O_WRONLY;
		else if ((ppc_flags & EMU_O_ACCMODE) == EMU_O_RDWR)
		    flags = O_RDWR;
		else
		    assert(0);
		if (ppc_flags & EMU_O_CREAT)
		    flags |= O_CREAT;
		if (ppc_flags & EMU_O_EXCL)
		    flags |= O_EXCL;
		if (ppc_flags & EMU_O_NOCTTY)
		    flags |= O_NOCTTY;
		if (ppc_flags & EMU_O_TRUNC)
		    flags |= O_TRUNC;
		if (ppc_flags & EMU_O_APPEND)
		    flags |= O_APPEND;
		if (ppc_flags & EMU_O_NONBLOCK)
		    flags |= O_NONBLOCK;
		if (ppc_flags & EMU_O_SYNC)
		    flags |= O_SYNC;
		if (ppc_flags & EMU_O_ASYNC)
		    flags |= O_ASYNC;
		/*
		if (ppc_flags & EMU_O_DIRECTORY)
		    flags |= O_DIRECTORY;
		if (ppc_flags & EMU_O_NOFOLLOW)
		    flags |= O_NOFOLLOW;
		*/

		result = open(name, flags);
		if (result != -1)
		    result = open_fd(result);

		free(real_name);
	    }
	    break;

	case SYSCALL_CLOSE :
	    ANNOUNCE_SYSCALL("close");
	    fd = lookup_fd(arg1);
	    if (fd == -1)
	    {
		result = -1;
		errno = EBADF;
	    }
	    else
	    {
		result = close(fd);
		close_fd(arg1);
	    }
	    break;

	case SYSCALL_TIME :
	    ANNOUNCE_SYSCALL("time");
	    assert(arg1 == 0);
	    result = (int)time(0);
	    break;

	case SYSCALL_GETPID :
	    ANNOUNCE_SYSCALL("getpid");
#ifdef FAKE_PID
	    result = FAKE_PID;
#else
	    result = getpid();
#endif
	    break;

	case SYSCALL_GETUID :
	    ANNOUNCE_SYSCALL("getuid");
	    result = getuid();
	    break;

	case SYSCALL_ACCESS :
	    ANNOUNCE_SYSCALL("access");
	    {
		char *real_name = strdup_from_user(intp, arg1);
		char *name = translate_filename(real_name);

		result = access(name, arg2);

		free(real_name);
	    }
	    break;

	case SYSCALL_BRK :
	    ANNOUNCE_SYSCALL("brk");
	    if (arg1 == 0)
		result = (int)intp->data_segment_top;
	    else
	    {
		word_32 old_top_aligned = PPC_PAGE_ALIGN(intp->data_segment_top);
		word_32 new_top = arg1;
		word_32 new_top_aligned = PPC_PAGE_ALIGN(new_top);

		assert(new_top > intp->data_segment_top);

		if (new_top_aligned > old_top_aligned)
		    mmap_anonymous(intp, new_top_aligned - old_top_aligned, PAGE_READABLE | PAGE_WRITEABLE, 1, old_top_aligned);

		intp->data_segment_top = new_top;

		result = (int)new_top;
	    }
	    break;

	case SYSCALL_GETGID :
	    ANNOUNCE_SYSCALL("getgid");
	    result = getgid();
	    break;

	case SYSCALL_GETEUID :
	    ANNOUNCE_SYSCALL("geteuid");
	    result = geteuid();
	    break;

	case SYSCALL_GETEGID :
	    ANNOUNCE_SYSCALL("getegid");
	    result = getegid();
	    break;

	case SYSCALL_IOCTL :
	    ANNOUNCE_SYSCALL("ioctl");
	    fd = lookup_fd(arg1);
	    if (fd == -1)
	    {
		result = -1;
		errno = EBADF;
	    }
	    else
	    {
		switch (arg2)
		{
		    case EMU_TCGETS :
			{
			    struct termios arg;

			    result = ioctl(fd, TCGETS, &arg);
			    if (result == 0)
				convert_native_termios_to_emu(intp, arg3, &arg);
			    else
				assert(0);
			}
			break;

		    case EMU_FIONREAD :
			{
			    int arg;

			    result = ioctl(fd, FIONREAD, &arg);
			    if (result == 0)
				mem_set_32(intp, arg3, (word_32)arg);
			}
			break;

		    default :
			assert(0);
		}
	    }
	    break;

	case SYSCALL_FCNTL :
	    ANNOUNCE_SYSCALL("fcntl");
	    fd = lookup_fd(arg1);
	    if (fd == -1)
	    {
		result = -1;
		errno = EBADF;
	    }
	    else
		switch (arg2)
		{
		    case EMU_F_GETFD :
			result = fcntl(fd, F_GETFD); /* we may have to translate result as well */
			break;

		    case EMU_F_SETFD :
			result = fcntl(fd, F_SETFD, (arg3 & EMU_FD_CLOEXEC) ? FD_CLOEXEC : 0);
			break;

		    case EMU_F_GETFL :
			{
			    int native_result = fcntl(fd, F_GETFL);

			    assert(0); /* all open flags are returned */

			    if (native_result != -1)
			    {
				result = 0;
				if (native_result & O_APPEND)
				    result |= EMU_O_APPEND;
				if (native_result & O_NONBLOCK)
				    result |= EMU_O_NONBLOCK;
				if (native_result & O_ASYNC)
				    result |= EMU_O_ASYNC;
			    }
			    else
				result = -1;
			}
			break;

		    case EMU_F_SETFL :
			{
			    long native_flags = 0;

			    if (arg3 & EMU_O_APPEND)
				native_flags |= O_APPEND;
			    if (arg3 & EMU_O_NONBLOCK)
				native_flags |= O_NONBLOCK;
			    if (arg3 & EMU_O_ASYNC)
				native_flags |= O_ASYNC;

			    result = fcntl(fd, F_SETFD, native_flags);
			}
			break;

		    default :
			printf("unhandled fcntl %d\n", arg2);
			intp->halt = 1;
		}
	    break;

	case SYSCALL_GETTIMEOFDAY :
	    ANNOUNCE_SYSCALL("gettimeofday");
	    {
		struct timeval tv;
		struct timezone tz;
		struct timezone *tzp;

		if (arg2 == 0)
		    tzp = 0;
		else
		    tzp = &tz;

		result = gettimeofday(&tv, tzp);

		if (result == 0)
		{
		    convert_native_timeval_to_ppc(intp, arg1, &tv);

		    if (tzp != 0)
		    {
			mem_set_32(intp, arg2 + 0, tz.tz_minuteswest);
			mem_set_32(intp, arg2 + 4, tz.tz_dsttime);
		    }
		}
	    }
	    break;

	case SYSCALL_MMAP :
	    ANNOUNCE_SYSCALL("mmap");
	    {
		word_32 len = PPC_PAGE_ALIGN(arg2);
		word_32 addr;

		assert(!(arg4 & PPC_MAP_SHARED));
		assert(arg4 & PPC_MAP_PRIVATE);

		if (arg4 & PPC_MAP_ANONYMOUS)
		{
		    assert(arg5 == -1);
		    assert(arg6 == 0);

		    addr = mmap_anonymous(intp, len, prot_to_flags(arg3), arg4 & PPC_MAP_FIXED, arg1);
		}
		else
		{
		    fd = lookup_fd(arg5);
		    if (fd == -1)
		    {
			addr = 0;
			result = EBADF;
		    }
		    else
			addr = mmap_file(intp, len, prot_to_flags(arg3), arg4 & PPC_MAP_FIXED, arg1, fd, arg6);
		}

		if (addr == 0)
		{
		    result = -1;
		    assert(0);
		}
		else
		    result = (int)addr;
	    }
	    break;

	case SYSCALL_MUNMAP :
	    ANNOUNCE_SYSCALL("munmap");
	    {
		word_32 mem_len;

		assert((arg1 & PPC_PAGE_MASK) == 0);

		mem_len = PPC_PAGE_ALIGN(arg2);

		mprotect_pages(intp, arg1, mem_len, 0);

		result = 0;
	    }
	    break;

	case SYSCALL_SOCKETCALL :
#define ARG(n)             (mem_get_32(intp, arg2 + (n) * 4))
	    switch (arg1)
	    {
		case SYS_SOCKET :
		    {
			int type;

			ANNOUNCE_SYSCALL("socket");

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
			if (result != -1)
			    result = open_fd(result);
		    }
		    break;

		case SYS_CONNECT :
		    {
			sa_family_t family = mem_get_16(intp, ARG(1));

			ANNOUNCE_SYSCALL("connect");

			fd = lookup_fd(ARG(0));
			if (fd == -1)
			{
			    result = -1;
			    errno = EBADF;
			}
			else
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

					result = connect(fd, &su, sizeof(su) - sizeof(su.sun_path) + len);
				    }
				    break;

				case AF_INET :
				    {
					struct sockaddr_in si;

					assert(ARG(2) == 16);

					si.sin_family = AF_INET;
					si.sin_port = htons(mem_get_16(intp, ARG(1) + 2));
					si.sin_addr.s_addr = htonl(mem_get_32(intp, ARG(1) + 4));
					memset(si.sin_zero, 0, sizeof(si.sin_zero));

					result = connect(fd, &si, sizeof(si));
				    }
				    break;

				default :
				    assert(0);
			    }
		    }
		    break;

		case SYS_GETSOCKNAME :
		    {
			byte buffer[1024];
			socklen_t len = 1024;

			ANNOUNCE_SYSCALL("getsockname");

			fd = lookup_fd(ARG(0));
			if (fd == -1)
			{
			    result = -1;
			    errno = EBADF;
			}
			else
			{
			    result = getsockname(fd, (struct sockaddr*)buffer, &len);

			    if (result == 0)
			    {
				word_32 used_len;

				convert_native_sockaddr_to_ppc(intp, (struct sockaddr*)buffer, ARG(1), mem_get_32(intp, ARG(2)), &used_len);

				mem_set_32(intp, ARG(2), used_len);
			    }
			}
		    }
		    break;

		case SYS_GETPEERNAME :
		    {
			byte buffer[1024];
			socklen_t len = 1024;

			ANNOUNCE_SYSCALL("getpeername");

			fd = lookup_fd(ARG(0));
			if (fd == -1)
			{
			    result = -1;
			    errno = EBADF;
			}
			else
			{
			    result = getpeername(fd, (struct sockaddr*)buffer, &len);

			    if (result == 0)
			    {
				word_32 used_len;

				convert_native_sockaddr_to_ppc(intp, (struct sockaddr*)buffer, ARG(1), mem_get_32(intp, ARG(2)), &used_len);

				mem_set_32(intp, ARG(2), used_len);
			    }
			}
		    }
		    break;

		case SYS_SHUTDOWN :
		    ANNOUNCE_SYSCALL("shutdown");
		    fd = lookup_fd(ARG(0));
		    if (fd == -1)
		    {
			result = -1;
			errno = EBADF;
		    }
		    else
			result = shutdown(fd, ARG(1));
		    break;

		case SYS_SETSOCKOPT :
		    {
			byte *optval;

			ANNOUNCE_SYSCALL("setsockopt");

			fd = lookup_fd(ARG(0));
			if (fd == -1)
			{
			    result = -1;
			    errno = EBADF;
			}
			else
			{
			    optval = (byte*)malloc(ARG(4));
			    assert(optval != 0);
			    mem_copy_from_user_32(intp, optval, ARG(3), ARG(4));

			    result = setsockopt(fd, ARG(1), ARG(2), optval, ARG(4));

			    free(optval);
			}
		    }
		    break;

		default :
		    printf("unhandled socket call %d\n", arg1);
		    intp->halt = 1;
	    }
#undef ARG
	    break;

	case SYSCALL_FSTAT :
	    ANNOUNCE_SYSCALL("fstat");
	    fd = lookup_fd(arg1);
	    if (fd == -1)
	    {
		result = -1;
		errno = EBADF;
	    }
	    else
	    {
		struct stat buf;

		result = fstat(fd, &buf);
		if (result == 0)
		{
#if defined(EMU_PPC)
		    mem_set_32(intp, arg2 + 0, buf.st_dev);
		    mem_set_32(intp, arg2 + 4, buf.st_ino);
		    mem_set_32(intp, arg2 + 8, buf.st_mode);
		    mem_set_16(intp, arg2 + 12, buf.st_nlink);
		    mem_set_32(intp, arg2 + 16, buf.st_uid);
		    mem_set_32(intp, arg2 + 20, buf.st_gid);
		    mem_set_32(intp, arg2 + 24, buf.st_rdev);
		    mem_set_32(intp, arg2 + 28, buf.st_size);
		    mem_set_32(intp, arg2 + 32, buf.st_blksize);
		    mem_set_32(intp, arg2 + 36, buf.st_blocks);
		    mem_set_32(intp, arg2 + 40, buf.st_atime);
		    mem_set_32(intp, arg2 + 48, buf.st_mtime);
		    mem_set_32(intp, arg2 + 56, buf.st_ctime);
#elif defined(EMU_I386)
#ifdef FAKE_FSTAT_DEV
		    mem_set_16(intp, arg2 + 0, FAKE_FSTAT_DEV);
#else
		    mem_set_16(intp, arg2 + 0, buf.st_dev);
#endif
#ifdef FAKE_FSTAT_INO
		    mem_set_32(intp, arg2 + 4, FAKE_FSTAT_INO);
#else
		    mem_set_32(intp, arg2 + 4, buf.st_ino);
#endif
#ifdef FAKE_FSTAT_MODE
		    mem_set_16(intp, arg2 + 8, FAKE_FSTAT_MODE);
#else
		    mem_set_16(intp, arg2 + 8, buf.st_mode);
#endif
#ifdef FAKE_FSTAT_NLINK
		    mem_set_16(intp, arg2 + 10, FAKE_FSTAT_NLINK);
#else
		    mem_set_16(intp, arg2 + 10, buf.st_nlink);
#endif
		    mem_set_16(intp, arg2 + 12, buf.st_uid);
		    mem_set_16(intp, arg2 + 14, buf.st_gid);
#ifdef FAKE_FSTAT_RDEV
		    mem_set_16(intp, arg2 + 16, FAKE_FSTAT_RDEV);
#else
		    mem_set_16(intp, arg2 + 16, buf.st_rdev);
#endif
#ifdef FAKE_FSTAT_SIZE
		    mem_set_32(intp, arg2 + 20, FAKE_FSTAT_SIZE);
#else
		    mem_set_32(intp, arg2 + 20, buf.st_size);
#endif
#ifdef FAKE_FSTAT_BLKSIZE
		    mem_set_32(intp, arg2 + 24, FAKE_FSTAT_BLKSIZE);
#else
		    mem_set_32(intp, arg2 + 24, buf.st_blksize);
#endif
#ifdef FAKE_FSTAT_BLOCKS
		    mem_set_32(intp, arg2 + 28, FAKE_FSTAT_BLOCKS);
#else
		    mem_set_32(intp, arg2 + 28, buf.st_blocks);
#endif
#ifdef FAKE_FSTAT_ATIME
		    mem_set_32(intp, arg2 + 32, FAKE_FSTAT_ATIME);
#else
		    mem_set_32(intp, arg2 + 32, buf.st_atime);
#endif
#ifdef FAKE_FSTAT_MTIME
		    mem_set_32(intp, arg2 + 40, FAKE_FSTAT_MTIME);
#else
		    mem_set_32(intp, arg2 + 40, buf.st_mtime);
#endif
#ifdef FAKE_FSTAT_CTIME
		    mem_set_32(intp, arg2 + 48, FAKE_FSTAT_CTIME);
#else
		    mem_set_32(intp, arg2 + 48, buf.st_ctime);
#endif
#endif
		}
	    }
	    break;

	case SYSCALL_UNAME :
	    ANNOUNCE_SYSCALL("uname");
	    assert(SYS_NMLN == 65);
	    {
		struct utsname un;

		result = uname(&un);

		if (result == 0)
		{
		    copy_string(intp, "Linux", arg1 + 0 * 65);
		    copy_string(intp, un.nodename, arg1 + 1 * 65);
		    copy_string(intp, "2.2.9", arg1 + 2 * 65);
		    copy_string(intp, "#5 Wed Jun 9 14:10:26 MEST 1999", arg1 + 3 * 65);
		    copy_string(intp, "ppc", arg1 + 4 * 65);
		    /* copy_string(intp, "", arg1 + 5 * 65); */
		}
	    }
	    break;

	case SYSCALL_MPROTECT :
	    ANNOUNCE_SYSCALL("mprotect");
	    {
		word_32 mem_len;

		assert(0);

		assert((arg1 & PPC_PAGE_MASK) == 0);

		mem_len = PPC_PAGE_ALIGN(arg2);

		mprotect_pages(intp, arg1, mem_len, prot_to_flags(arg3));

		result = 0;
	    }
	    break;

	case SYSCALL_PERSONALITY :
	    ANNOUNCE_SYSCALL("personality");
	    assert(arg1 == 0);
	    result = 0;
	    break;

	case SYSCALL_LLSEEK :
	    {
		off_t offset = ((off_t)arg2 << 32) | (off_t)arg3;
		off_t pos;

		fd = lookup_fd(arg1);

		pos = lseek(fd, offset, arg5);

		if (pos == (off_t)-1)
		    result = -1;
		else
		{
		    mem_set_32(intp, arg4 + 0, pos >> 32);
		    mem_set_32(intp, arg4 + 4, pos & 0xffffffff);

		    result = 0;
		}
	    }
	    break;

	case SYSCALL_SELECT :
	    ANNOUNCE_SYSCALL("select");
	    {
		struct timeval tv;
		struct timeval *tvp;
		fd_set read_set, write_set, exc_set;
		fd_set *rsp, *wsp, *esp;
		int maxfd;

		assert(arg1 < 4096);

		maxfd = arg1;

		if (arg2 != 0)
		{
		    convert_ppc_fdset_to_native(intp, maxfd, &read_set, arg2);
		    rsp = &read_set;
		}
		else
		    rsp = 0;

		if (arg3 != 0)
		{
		    convert_ppc_fdset_to_native(intp, maxfd, &write_set, arg3);
		    wsp = &write_set;
		}
		else
		    wsp = 0;


		if (arg4 != 0)
		{
		    convert_ppc_fdset_to_native(intp, maxfd, &exc_set, arg4);
		    esp = &exc_set;
		}
		else
		    esp = 0;

		if (arg5 != 0)
		{
		    convert_ppc_timeval_to_native(intp, &tv, arg5);
		    tvp = &tv;
		}
		else
		    tvp = 0;

		result = select(maxfd, rsp, wsp, esp, tvp);

		if (result >= 0)
		{
		    if (rsp != 0)
			convert_native_fdset_to_ppc(intp, maxfd, arg2, &read_set);
		    if (wsp != 0)
			convert_native_fdset_to_ppc(intp, maxfd, arg3, &write_set);
		    if (esp != 0)
			convert_native_fdset_to_ppc(intp, maxfd, arg4, &exc_set);
		    if (tvp != 0)
			convert_native_timeval_to_ppc(intp, arg5, &tv);
		}
	    }
	    break;

	case SYSCALL_READV :
	    ANNOUNCE_SYSCALL("readv");
	    fd = lookup_fd(arg1);
	    if (fd == -1)
	    {
		result = -1;
		errno = EBADF;
	    }
	    else
	    {
		word_32 i;
		word_32 out_len = 0;
		word_32 num_copied = 0;
		byte *buf;

		for (i = 0; i < arg3; ++i)
		    out_len += mem_get_32(intp, arg2 + i * 8 + 4);

		buf = (byte*)malloc(out_len);
		assert(buf != 0);

		result = read(fd, buf, out_len);

		if (result > 0)
		{
		    for (i = 0; i < arg3; ++i)
		    {
			word_32 addr = mem_get_32(intp, arg2 + i * 8 + 0);
			word_32 len = mem_get_32(intp, arg2 + i * 8 + 4);

			mem_copy_to_user_8(intp, addr, buf + num_copied, len);
			num_copied += len;
		    }

		    assert(num_copied == out_len);
		}

		free(buf);
	    }
	    break;

	case SYSCALL_WRITEV :
	    ANNOUNCE_SYSCALL("writev");
	    fd = lookup_fd(arg1);
	    if (fd == -1)
	    {
		result = -1;
		errno = EBADF;
	    }
	    else
	    {
		word_32 i;
		word_32 in_len = 0;
		word_32 num_copied = 0;
		byte *buf;

		for (i = 0; i < arg3; ++i)
		    in_len += mem_get_32(intp, arg2 + i * 8 + 4);

		buf = (byte*)malloc(in_len);
		assert(buf != 0);

		for (i = 0; i < arg3; ++i)
		{
		    word_32 addr = mem_get_32(intp, arg2 + i * 8 + 0);
		    word_32 len = mem_get_32(intp, arg2 + i * 8 + 4);

		    mem_copy_from_user_8(intp, buf + num_copied, addr, len);
		    num_copied += len;
		}

		assert(num_copied == in_len);

		result = write(fd, buf, in_len);

		free(buf);
	    }
	    break;

	case SYSCALL_RT_SIGACTION :
	    ANNOUNCE_SYSCALL("rt_sigaction");
	    result = 0;		/* FIXME: this is certainly a bit naive. */
	    break;

	default :
	    printf("unhandled system call %d\n", number);
	    intp->halt = 1;
    }

#ifdef SYSCALL_OUTPUT
    printf("  %08x\n", (word_32)result);
#endif

    return result;
}

int
lookup_errno (int host_errno)
{
    int i;

    assert(host_errno > 0);

    for (i = 1; i <= LAST_EMU_ERRNO; ++i)
	if (emu_errnos[i] == host_errno)
	    break;

    assert(i <= LAST_EMU_ERRNO);

    return i;
}

void
handle_system_call (interpreter_t *intp)
{
    static struct { word_32 num; int num_args; } syscall_args[] = {
	{ SYSCALL_EXIT, 1 },
	{ SYSCALL_READ, 3 },
	{ SYSCALL_WRITE, 3 },
	{ SYSCALL_OPEN, 2 },
	{ SYSCALL_CLOSE, 1 },
	{ SYSCALL_TIME, 1 },
	{ SYSCALL_GETPID, 0 },
	{ SYSCALL_GETUID, 0 },
	{ SYSCALL_ACCESS, 2 },
	{ SYSCALL_BRK, 1 },
	{ SYSCALL_GETGID, 0 },
	{ SYSCALL_GETEUID, 0 },
	{ SYSCALL_GETEGID, 0 },
	{ SYSCALL_IOCTL, 3 },
	{ SYSCALL_FCNTL, 3 },
	{ SYSCALL_GETTIMEOFDAY, 2 },
	{ SYSCALL_MMAP, 6 },
	{ SYSCALL_MUNMAP, 2 },
	{ SYSCALL_SOCKETCALL, 2 },
	{ SYSCALL_FSTAT, 2 },
	{ SYSCALL_UNAME, 1 },
	{ SYSCALL_MPROTECT, 3 },
	{ SYSCALL_PERSONALITY, 1 },
	{ SYSCALL_LLSEEK, 5 },
	{ SYSCALL_SELECT, 5 },
	{ SYSCALL_READV, 3 },
	{ SYSCALL_WRITEV, 3 },
	{ SYSCALL_RT_SIGACTION, 3 },
	{ (word_32)-1, 0 }
    };

    int result;
    int i;
    word_32 num;
    int num_args;

#if defined(EMU_PPC)
    num = intp->regs_GPR[0];
#elif defined(EMU_I386)
    num = intp->regs_GPR[0];
#endif

    for (i = 0; syscall_args[i].num != (word_32)-1; ++i)
	if (syscall_args[i].num == num)
	    break;

    if (syscall_args[i].num == (word_32)-1)
    {
	printf("unhandled system call %d\n", num);
	intp->halt = 1;
	return;
    }

    assert(syscall_args[i].num != (word_32)-1);
    num_args = syscall_args[i].num_args;

#if defined(EMU_PPC)
    result = process_system_call(intp, intp->regs_GPR[0],
				 intp->regs_GPR[3], intp->regs_GPR[4], intp->regs_GPR[5],
				 intp->regs_GPR[6], intp->regs_GPR[7], intp->regs_GPR[8]);

    if (result == -1)
    {
	intp->regs_GPR[3] = (word_32)lookup_errno(errno);
	intp->regs_SPR[1] |= 0x10000000;
    }
    else
    {
	intp->regs_GPR[3] = (word_32)result;
	intp->regs_SPR[1] &= ~0x10000000;
    }
#elif defined(EMU_I386)
    if (num_args <= 3)
	result = process_system_call(intp, num,
				     intp->regs_GPR[3], intp->regs_GPR[1], intp->regs_GPR[2],
				     0, 0, 0);
    else
    {
	word_32 arg1, arg2, arg3, arg4, arg5 = 0, arg6 = 0;

	arg1 = mem_get_32(intp, intp->regs_GPR[3] + 0);
	arg2 = mem_get_32(intp, intp->regs_GPR[3] + 4);
	arg3 = mem_get_32(intp, intp->regs_GPR[3] + 8);
	arg4 = mem_get_32(intp, intp->regs_GPR[3] + 12);
	if (num_args >= 5)
	{
	    arg5 = mem_get_32(intp, intp->regs_GPR[3] + 16);
	    if (num_args >= 6)
		arg6 = mem_get_32(intp, intp->regs_GPR[3] + 20);
	}

	result = process_system_call(intp, num, arg1, arg2, arg3, arg4, arg5, arg6);
    }

    if (result == -1)
	intp->regs_GPR[0] = (word_32)-lookup_errno(errno);
    else
	intp->regs_GPR[0] = (word_32)result;
#endif
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
    /*
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
    */

#if defined(EMU_PPC)
    static char *env[] = { "PWD=/",
			   "HOSTNAME=quinta.schani.net",
			   "HISTFILESIZE=1000",
			   "USER=schani",
			   "MACHTYPE=powerpc-redhat-linux-gnu",
			   "MAIL=/var/spool/mail/schani",
			   "DISPLAY=:0.0",
			   "LOGNAME=schani",
			   "SHLVL=1",
			   "SHELL=/bin/bash",
			   "HOSTTYPE=powerpc",
			   "OSTYPE=linux-gnu",
			   "HISTSIZE=1000",
			   "HOME=/home/schani",
			   "TERM=xterm",
			   "PATH=/bin",
			   0 };
#elif defined(EMU_I386)
    static char *env[] = { "PWD=/mnt/homes/nethome/hansolo/schani/Work/unix/bintrans/i386-root/bin",
			   "HOSTNAME=vader",
			   "MACHTYPE=i686-pc-linux-gnu",
			   "SHLVL=0",
			   "SHELL=/bin/bash",
			   "HOSTTYPE=i686",
			   "OSTYPE=linux-gnu",
			   "TERM=dumb",
			   "PATH=/usr/local/bin:/bin:/usr/bin:/usr/X11R6/bin:.",
			   0 };
#endif

    int argc;
    int envc;

    word_32 exec;
    word_32 sp;
    word_32 csp;
    word_32 argvp, envp;
    word_32 platform;
    word_32 argtop;

    for (argc = 0; argv[argc] != 0; ++argc)
	;

    for (envc = 0; env[envc] != 0; ++envc)
	;

    p -= 4;
    p = copy_string(intp, argv[0], p);
    exec = p;
    p = copy_strings(intp, envc, env, p);
    argtop = p = copy_strings(intp, argc, argv, p);

#ifdef EMU_I386
    p = copy_string(intp, "i686", p);
    platform = p;
#else
    platform = 0;
#endif

    sp = (~15 & p) - 16;

    csp = sp;
    csp -= 4 * 4 + (platform != 0 ? 2 * 4 : 0);	/* no interpreter */
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

#ifdef EMU_I386
    sp -= 2 * 4;
    NEW_AUX_ENT(0, AT_PLATFORM, platform);
#endif

    sp -= 2 * 4;
    NEW_AUX_ENT(0, AT_HWCAP, EMU_HWCAPS);

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

    p = argtop;

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
setup_ppc_registers (interpreter_t *intp, word_32 stack_bottom)
{
    intp->regs_GPR[1] = stack_bottom;
}

void
run_debugged (interpreter_t *intp)
{
    breakpoint_t *breakpoint;

    for (;;)
    {
	if (intp->trace)
	{
#if defined(EMU_PPC)
	    printf("%08x:  ", intp->pc);
	    disassemble_ppc_insn(mem_get_32(intp, intp->pc), intp->pc);
	    printf("   %08x", intp->regs_SPR[2]);
	    printf("\n");
#elif defined(EMU_I386)
	    word_32 pc = intp->pc;

	    printf("%08x:  ", pc);
	    disassemble_i386_insn(intp);
	    intp->pc = pc;
	    printf("\n");

	    /*
	    printf("%08x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x\n", intp->pc,
		   intp->regs_GPR[0], intp->regs_GPR[1], intp->regs_GPR[2], intp->regs_GPR[3],
		   intp->regs_GPR[4], intp->regs_GPR[5], intp->regs_GPR[6], intp->regs_GPR[7]);
	    */
#endif
	}
	interpret_insn(intp);
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

#ifdef EMU_I386
    word_32 old_pc = intp->pc;

    intp->pc = addr;
#endif
    
    for (i = 0; i < len; ++i)
    {
#if defined(EMU_PPC)
	printf("%08x:  ", addr);
	disassemble_ppc_insn(mem_get_32(intp, addr), addr);
	addr += 4;
#elif defined(EMU_I386)
	printf("%08x:  ", intp->pc);
	disassemble_i386_insn(intp);
#endif
	printf("\n");
    }

#ifdef EMU_I386
    intp->pc = old_pc;
#endif
}

void
show_segments (interpreter_t *intp)
{
    word_32 start = 0;
    int flags = 0;
    word_32 l1, l2;

    printf("start     end       len       flags\n");
    printf("-----------------------------------\n");

    /* you don't want to traverse the page table this way on a 64 bit
       architecture! */

    for (l1 = 0; l1 < LEVEL1_SIZE; ++l1)
    {
	for (l2 = 0; l2 < LEVEL2_SIZE; ++l2)
	{
	    word_32 addr = (l1 << LEVEL1_SHIFT) | (l2 << LEVEL2_SHIFT);
	    int page_flags;

	    if (intp->pagetable[l1] == 0)
		page_flags = 0;
	    else
		page_flags = intp->pagetable[l1][l2].flags & PAGE_PROT_MASK;

	    if (page_flags != flags)
	    {
		if (flags != 0)
		{
		    char flags_str[4] = "   ";

		    if (flags & PAGE_READABLE)
			flags_str[0] = 'r';
		    if (flags & PAGE_WRITEABLE)
			flags_str[1] = 'w';
		    if (flags & PAGE_EXECUTABLE)
			flags_str[2] = 'x';

		    printf("%08x  %08x  %8u  %s\n", start, addr, addr - start, flags_str);
		}

		start = addr;
		flags = page_flags;
	    }
	}
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
	    interpret_insn(intp);
	else if (strcmp(token, "regs") == 0)
	    dump_registers(intp);
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
#ifdef EMU_I386
	else if (strcmp(token, "liveness") == 0)
	{
	    word_32 addr;

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }
	    addr = strtol(token, 0, 16);

	    compute_liveness(intp, addr);
	    print_liveness(intp);
	}
#endif
	else if (strcmp(token, "file") == 0)
	{
	    int fd, native_fd;
	    int flags;

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }
	    fd = atoi(token);
	    assert(fd >= 0 && fd < MAX_FDS);

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }
	    if (strcmp(token, "r") == 0)
		flags = O_RDONLY;
	    else if (strcmp(token, "w") == 0)
		flags = O_WRONLY;
	    else if (strcmp(token, "rw") == 0)
		flags = O_RDWR;
	    else
	    {
		printf("error\n");
		continue;
	    }

	    p = get_token(p, token);
	    if (p == 0)
	    {
		printf("error\n");
		continue;
	    }

	    native_fd = open(token, flags);
	    if (native_fd == -1)
	    {
		printf("cannot open file: %s\n", strerror(errno));
		continue;
	    }

	    fd_map[fd].free = 0;
	    fd_map[fd].native_fd = native_fd;
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
    intp->data_segment_top = 0;
    intp->insn_count = 0;
    intp->halt = 0;
#ifdef EMU_PPC
    intp->trace = !compiler;
#else
    intp->trace = 0;
#endif
    intp->breakpoints = 0;

    for (i = 0; i < LEVEL1_SIZE; ++i)
	intp->pagetable[i] = 0;

#if defined(EMU_PPC)
    for (i = 0; i < 5; ++i)
	intp->regs_SPR[i] = 0;
    for (i = 0; i < 32; ++i)
    {
	intp->regs_GPR[i] = 0; /* 0xdeadbe00 + i; */
	intp->regs_FPR[i] = 0.0;
    }
#elif defined(EMU_I386)
    for (i = 0; i < 8; ++i)
    {
	intp->regs_GPR[i] = 0;
	intp->regs_FPST[i] = 0.0;
    }

    intp->regs_SPR[0] = 0;
    intp->regs_FSPR[0] = 0;
#endif
}

int
main (int argc, char *argv[])
{
    int exec_fd;
    Elf32_Ehdr ehdr;
    Elf32_Phdr *phdrs = 0;
    word_32 stack_bottom;
    ssize_t num_read;
    int i;
    char **ppc_argv;
#ifdef NEED_INTERPRETER
    interpreter_t interpreter;
#endif
#ifdef NEED_COMPILER
    interpreter_t compiler;
#endif

#ifdef NEED_INTERPRETER
#if defined(NEED_COMPILER) || defined(EMULATED_MEM)
    init_interpreter_struct(&interpreter, 0, 0);
#else
    init_interpreter_struct(&interpreter, 1, 0);
#endif
#endif
#ifdef NEED_COMPILER
    init_interpreter_struct(&compiler, 1, 1);
#endif

    assert(argc >= 2);

    fd_map[0].free = 0;
    fd_map[0].native_fd = 0;

    fd_map[1].free = 0;
    fd_map[1].native_fd = 1;

    fd_map[2].free = 0;
    fd_map[2].native_fd = 2;

    for (i = 3; i < MAX_FDS; ++i)
	fd_map[i].free = 1;

    ppc_argv = (char**)malloc(sizeof(char*) * argc);
#if defined(EMU_PPC)
    ppc_argv[0] = "/bigben/home/schani/./a.out";
#elif defined(EMU_I386)
    ppc_argv[0] = "/mnt/homes/nethome/hansolo/schani/Work/unix/bintrans/i386-root/bin/go";
#endif
    for (i = 2; i < argc; ++i)
	ppc_argv[i - 1] = argv[i];
    ppc_argv[argc - 1] = 0;

    exec_fd = open(translate_filename(argv[1]), O_RDONLY);
    assert(exec_fd != -1);

    num_read = read_all(exec_fd, (byte*)&ehdr, sizeof(Elf32_Ehdr));
    assert(num_read == sizeof(Elf32_Ehdr));

    assert(ehdr.e_ident[EI_MAG0] == ELFMAG0);
    assert(ehdr.e_ident[EI_MAG1] == ELFMAG1);
    assert(ehdr.e_ident[EI_MAG2] == ELFMAG2);
    assert(ehdr.e_ident[EI_MAG3] == ELFMAG3);

    assert(ehdr.e_ident[EI_CLASS] == ELFCLASS32);

#if defined(EMU_PPC)
    assert(ehdr.e_ident[EI_DATA] == ELFDATA2MSB);
#elif defined(EMU_I386)
    assert(ehdr.e_ident[EI_DATA] == ELFDATA2LSB);
#endif

    assert(ehdr.e_ident[EI_VERSION] == EV_CURRENT);

    /*
    assert(ehdr.e_ident[EI_OSABI] == ELFOSABI_SYSV);
    assert(ehdr.e_ident[EI_ABIVERSION] == 0);
    */

#ifdef DIFFERENT_BYTEORDER
    lsbify_elf32_ehdr(&ehdr);
#endif

    assert(ehdr.e_type == ET_EXEC);
#if defined(EMU_PPC)
    assert(ehdr.e_machine == EM_PPC);
#elif defined(EMU_I386)
    assert(ehdr.e_machine == EM_386);
#endif

    assert(ehdr.e_version == EV_CURRENT);

    phdrs = (Elf32_Phdr*)malloc(sizeof(Elf32_Phdr) * ehdr.e_phnum);

    lseek(exec_fd, ehdr.e_phoff, SEEK_SET);
    for (i = 0; i < ehdr.e_phnum; ++i)
    {
	num_read = read_all(exec_fd, (byte*)&phdrs[i], sizeof(Elf32_Phdr));
	assert(num_read == sizeof(Elf32_Phdr));
#ifdef DIFFERENT_BYTEORDER
	lsbify_elf32_phdr(&phdrs[i]);
#endif
    }

    for (i = 0; i < ehdr.e_phnum; ++i)
    {
	int flags = 0;
	word_32 read_len;
	word_32 mem_start;
	word_32 mem_len;

	if (phdrs[i].p_type != PT_LOAD)
	    continue;

	if (phdrs[i].p_flags & PF_R)
	    flags |= PAGE_READABLE;
	if (phdrs[i].p_flags & PF_W)
	    flags |= PAGE_WRITEABLE;
	if (phdrs[i].p_flags & PF_X)
	    flags |= PAGE_EXECUTABLE;

	mem_start = PPC_PAGE_ALIGN_DOWN(phdrs[i].p_vaddr);
	mem_len = PPC_PAGE_ALIGN(phdrs[i].p_vaddr + phdrs[i].p_memsz) - mem_start;

#ifdef NEED_INTERPRETER
	mprotect_pages(&interpreter, mem_start, mem_len, flags | PAGE_MMAPPED);
	natively_mprotect_pages(&interpreter, mem_start, mem_len, PAGE_WRITEABLE);
	read_len = copy_file_to_mem(&interpreter, exec_fd, phdrs[i].p_vaddr, phdrs[i].p_filesz, phdrs[i].p_offset, 0);
	assert(read_len == phdrs[i].p_filesz);
	natively_mprotect_pages(&interpreter, mem_start, mem_len, flags);

	if (phdrs[i].p_type == PT_LOAD && phdrs[i].p_flags == (PF_W | PF_R))
	    interpreter.data_segment_top = phdrs[i].p_vaddr + phdrs[i].p_memsz;
#endif
#ifdef NEED_COMPILER
	mprotect_pages(&compiler, mem_start, mem_len, flags | PAGE_MMAPPED);
	natively_mprotect_pages(&compiler, mem_start, mem_len, PAGE_WRITEABLE);
	read_len = copy_file_to_mem(&compiler, exec_fd, phdrs[i].p_vaddr, phdrs[i].p_filesz, phdrs[i].p_offset, 0);
	assert(read_len == phdrs[i].p_filesz);
	natively_mprotect_pages(&compiler, mem_start, mem_len, flags);

	if (phdrs[i].p_type == PT_LOAD && phdrs[i].p_flags == (PF_W | PF_R))
	    compiler.data_segment_top = phdrs[i].p_vaddr + phdrs[i].p_memsz;
#endif
    }

    close(exec_fd);

#ifdef NEED_INTERPRETER
    mprotect_pages(&interpreter, STACK_TOP - STACK_SIZE * PPC_PAGE_SIZE, STACK_SIZE * PPC_PAGE_SIZE, PAGE_READABLE | PAGE_WRITEABLE | PAGE_MMAPPED);
    natively_mprotect_pages(&interpreter, STACK_TOP - STACK_SIZE * PPC_PAGE_SIZE, STACK_SIZE * PPC_PAGE_SIZE, PAGE_READABLE | PAGE_WRITEABLE);

    assert(interpreter.data_segment_top != 0);

    stack_bottom = setup_stack(&interpreter, STACK_TOP, ppc_argv);
    assert((stack_bottom & 15) == 0);

    setup_registers(&interpreter, stack_bottom);
    interpreter.pc = ehdr.e_entry;
#endif
#ifdef NEED_COMPILER
    mprotect_pages(&compiler, STACK_TOP - STACK_SIZE * PPC_PAGE_SIZE, STACK_SIZE * PPC_PAGE_SIZE, PAGE_READABLE | PAGE_WRITEABLE | PAGE_MMAPPED);
    natively_mprotect_pages(&compiler, STACK_TOP - STACK_SIZE * PPC_PAGE_SIZE, STACK_SIZE * PPC_PAGE_SIZE, PAGE_READABLE | PAGE_WRITEABLE);

    assert(compiler.data_segment_top != 0);

    stack_bottom = setup_stack(&compiler, STACK_TOP, ppc_argv);
    assert((stack_bottom & 15) == 0);

    setup_registers(&compiler, stack_bottom);
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

#ifdef DIFFERENT_BYTEORDER
    init_unaligned();
#endif

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
	interpret_insn(&interpreter);
#endif

    return 0;
}
