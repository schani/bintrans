(load "generator")
(load "ui.lisp")

(load "transformations.lisp")
(load "ppc.lisp")
(load "alpha.lisp")

(defparameter *source-machine* *ppc*)
(defparameter *target-machine* *alpha*)
(defparameter *register-mapping* *ppc-to-alpha-register-mapping*)

(generate-all-generators *alpha*)

(defun generate-all-files ()
  (generate-defines-file *ppc*)
  (generate-composer-file *alpha*)
  (generate-disassembler-file *alpha*)
  (generate-disassembler-file *ppc*)
  (generate-interpreter-file *ppc*)
  (generate-compiler-file *ppc*))
