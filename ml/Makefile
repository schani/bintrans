WARN_OPTS = -w A -w e

#OC = ocamlc -g $(WARN_OPTS)
#OEXT = cmo
#ENGINE = engine
OC = ocamlopt $(WARN_OPTS)
OEXT = cmx
AEXT = cma

ENGINE_OBJS = monad.$(OEXT) memoize.$(OEXT) bitmath.$(OEXT) \
	      expr.$(OEXT) cond_monad.$(OEXT) instruction.$(OEXT) matcher.$(OEXT) \
	      simplifiers.$(OEXT) simplify.$(OEXT) \
	      machine.$(OEXT) pruner.$(OEXT) \
	      irmacros.$(OEXT) \
	      explorer.$(OEXT) \
	      cgen.$(OEXT) switcher.$(OEXT) \
	      target_alpha.$(OEXT)

TEST_OBJS = test.$(OEXT)

all : test $(ENGINE)

test : $(ENGINE_OBJS) $(TEST_OBJS)
	$(OC) -o test $(ENGINE_OBJS) $(TEST_OBJS)

engine : $(ENGINE_OBJS)
	ocamlmktop -o engine $(ENGINE_OBJS)

%.$(OEXT) : %.ml
	$(OC) -c $<

simplifiers.ml irmacros.ml target_alpha.ml : simplify.lisp irmacros.lisp target_alpha.lisp lisp2ml.lisp
	clisp build.lisp

clean :
	rm -f *.cmo *.cmx *.o *.cmi simplifiers.ml irmacros.ml target_alpha.ml test engine *~
