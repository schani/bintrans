(load "lisp2ml.lisp")

(load "irmacros.lisp")
(make-ir-macros)

(load "simplify.lisp")
(make-simplifies)

(load "target_alpha.lisp")
(make-matchers "target_alpha.ml" "alpha_matchers" "alpha_printers" "alpha_gen_gens")

(load "machine_ppc.lisp")
(make-insns "machine_ppc.ml" "ppc")
