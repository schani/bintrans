#OC = ocamlc
#OEXT = cmo
OC = ocamlopt
OEXT = cmx
AEXT = cma

ENGINE_OBJS = bitmath.$(OEXT) expr.$(OEXT) instruction.$(OEXT) matcher.$(OEXT) simplifiers.$(OEXT) simplify.$(OEXT) \
	    machine.$(OEXT) pruner.$(OEXT) irmacros.$(OEXT) target_alpha.$(OEXT)

TEST_OBJS = test.$(OEXT)

all : test
# engine

test : $(ENGINE_OBJS) $(TEST_OBJS)
	$(OC) -o test $(ENGINE_OBJS) $(TEST_OBJS)

engine : $(ENGINE_OBJS)
	ocamlmktop -o engine $(ENGINE_OBJS)

%.$(OEXT) : %.ml
	$(OC) -c $<

simplifiers.ml irmacros.ml : simplify.lisp irmacros.lisp lisp2ml.lisp
	clisp build.lisp

clean :
	rm -f *.cmo *.cmx *.o *.cmi simplifiers.ml irmacros.ml test engine *~