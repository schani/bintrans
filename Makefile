#DEFINES = -DCROSSDEBUGGER -DCOLLECT_STATS -DDUMP_CODE
#DEFINES = -DCOMPILER -DCOLLECT_STATS -DDUMP_CODE
#DEFINES = -DCOMPILER
DEFINES = -DDEBUGGER
CFLAGS = $(DEFINES)

bintrans : ppc.o compiler.o alpha_asm.o mm.o
	gcc -o bintrans ppc.o compiler.o mm.o alpha_asm.o

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
	rm -f *~ core ppc.o compiler.o alpha_asm.o
