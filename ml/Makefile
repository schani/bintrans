WARN_OPTS = -w A -w e

OC = ocamlc -g $(WARN_OPTS)
OEXT = cmo
ENGINE = engine
#OC = ocamlopt $(WARN_OPTS)
#OEXT = cmx
AEXT = cma

ENGINE_OBJS = monad.$(OEXT) memoize.$(OEXT) bitmath.$(OEXT) utils.$(OEXT) \
	      expr.$(OEXT) cond_monad.$(OEXT) uncertainty.$(OEXT) \
	      machine.$(OEXT) mapping.$(OEXT) \
	      matcher.$(OEXT) \
	      normal_form.$(OEXT) simplifiers.$(OEXT) simplify.$(OEXT) \
	      irmacros.$(OEXT) \
	      pruner.$(OEXT) simple_opts.$(OEXT) \
	      explorer.$(OEXT) \
	      cgen.$(OEXT) switcher.$(OEXT) \
	      machine_ppc.$(OEXT) \
	      target_alpha.$(OEXT)

TEST_OBJS = test.$(OEXT)

all : test $(ENGINE)

test : $(ENGINE_OBJS) $(TEST_OBJS)
	$(OC) -o test $(ENGINE_OBJS) $(TEST_OBJS)

engine : $(ENGINE_OBJS)
	ocamlmktop -o engine $(ENGINE_OBJS)

%.$(OEXT) : %.ml
	$(OC) -c $<

simplifiers.ml irmacros.ml target_alpha.ml machine_ppc.ml : simplify.lisp irmacros.lisp target_alpha.lisp machine_ppc.lisp lisp2ml.lisp
	clisp build.lisp

clean :
	rm -f *.cmo *.cmx *.o *.cmi simplifiers.ml irmacros.ml target_alpha.ml machine_ppc.ml test engine *~
