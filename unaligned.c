#define __LIBRARY__
#include <asm/unistd.h>
#undef __LIBRARY__

#include <asm/sysinfo.h>
#include <sys/syscall.h>
#include <errno.h>
#include <assert.h>
#include <signal.h>
#include <asm/sigcontext.h>
typedef unsigned long old_sigset_t;
#include <asm/ucontext.h>
#include <stdio.h>

#include "bintrans.h"

_syscall5(int,osf_setsysinfo,unsigned long,op,void*,buffer,unsigned long,nbytes,void*,arg,unsigned long,flag)

/*
int
load_unaligned_32 (unsigned long addr)
{
    unsigned long addr0 = addr & ~3;
    unsigned long addr1 = addr0 + 4;
    unsigned int val0 = *(unsigned int*)addr0;
    unsigned int val1 = *(unsigned int*)addr1;
    int disp = addr & 3;

    return (val0 >> (disp << 3)) | (val1 << ((4 - disp) << 3));
}
*/

int
load_unaligned_32_bigendian (unsigned long addr)
{
    unsigned long addr0 = addr & ~3;
    unsigned long addr1 = addr0 + 4;
    unsigned int val0 = *(unsigned int*)addr0;
    unsigned int val1 = *(unsigned int*)addr1;
    int disp = addr & 3;

    return (val0 << (disp << 3)) | (val1 >> ((4 - disp) << 3));
}

short
load_unaligned_16_bigendian (unsigned long addr)
{
    return load_unaligned_32_bigendian(addr) >> 16;
}

void
sigbus_handler (int signo, siginfo_t *si, struct ucontext *uc)
{
    unsigned int insn;
    unsigned int target_reg;

/*
    printf("0x%016lx\n", uc->uc_mcontext.sc_pc);
    printf("0x%016lx\n", uc->uc_mcontext.sc_traparg_a0);
*/

    insn = *(unsigned int*)uc->uc_mcontext.sc_pc;

    target_reg = (insn >> 21) & 0x1f;

/*
    printf("target %d\n", target_reg);
*/

    switch (insn >> 26)
    {
	case 0x0c :		/* LDWU */
	    /* printf("ldwu\n"); */
	    uc->uc_mcontext.sc_regs[target_reg] = load_unaligned_16_bigendian(uc->uc_mcontext.sc_traparg_a0 ^ 2);
	    break;

	case 0x28 :		/* LDL */
	    /* printf("ldl\n"); */
	    uc->uc_mcontext.sc_regs[target_reg] = load_unaligned_32_bigendian(uc->uc_mcontext.sc_traparg_a0);
	    break;

	case 0x22 :		/* LDS */
	    {
		double f64;
		word_32 val32;

		val32 = load_unaligned_32_bigendian(uc->uc_mcontext.sc_traparg_a0);
		f64 = (double)*(float*)&val32;
		uc->uc_mcontext.sc_fpregs[target_reg] = *(long*)&f64;
	    }
	    break;

	default :
	    assert(0);
    }

    uc->uc_mcontext.sc_pc += 4;

    sigreturn((void*)uc);
}

void
init_unaligned (void)
{
    int buf[2];
    int error;
    struct sigaction act;

    buf[0] = SSIN_UACPROC;
    buf[1] = UAC_SIGBUS;
    error = osf_setsysinfo(SSI_NVPAIRS, buf, 1, 0, 0);
    assert(error == 0);

    act.sa_handler = 0;
    act.sa_sigaction = sigbus_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = SA_NOMASK | SA_SIGINFO;
    error = sigaction(SIGBUS, &act, 0);
    assert(error == 0);
}
