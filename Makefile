ARCH = -DARCH_ALPHA
ASM_OBJS = alpha_asm.o
COMPILER_OBJS = compiler.o
#ARCH = -DARCH_I386
#DEFINES = -DINTERPRETER -DEMULATED_MEM
#DEFINES = -DCROSSDEBUGGER -DCOLLECT_STATS -DDUMP_CODE
#DEFINES = -DCOMPILER -DCOLLECT_STATS -DDUMP_CODE
#DEFINES = -DCOMPILER -DCOLLECT_STATS -DPERIODIC_STAT_DUMP
#DEFINES = -DDEBUGGER -DEMULATED_MEM
#DEFINES = -DCOMPILER -DDUMP_CODE
DEFINES = -O -DCOMPILER -DCOLLECT_STATS
#DEFINES = -O -DCOMPILER
CFLAGS = $(DEFINES) $(ARCH)

bintrans : ppc.o mm.o $(COMPILER_OBJS) $(ASM_OBJS) 
	gcc -o bintrans ppc.o mm.o $(COMPILER_OBJS) $(ASM_OBJS)

ppc.o : ppc.c ppc_interpreter.c ppc_disassembler.c alpha_types.h bintrans.h
	gcc $(CFLAGS) -Wall -g -c ppc.c

compiler.o : compiler.c alpha_composer.h ppc_compiler.c alpha_disassembler.c alpha_types.h bintrans.h
	gcc $(CFLAGS) -Wall -g -c compiler.c

mm.o : mm.c bintrans.h alpha_types.h ppc_defines.h
	gcc $(CFLAGS) -Wall -g -c mm.c

alpha_asm.o : alpha_asm.S
	gcc $(DEFINES) -g -c alpha_asm.S

elfer : elfer.c
	gcc -o elfer elfer.c

clean :
	rm -f *~ core ppc.o compiler.o alpha_asm.o mm.o bintrans
