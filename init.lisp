(load "generator.lisp")
(load "ppc.lisp")
(load "alpha.lisp")

(defparameter *ppc-and* (find 'and (machine-insns *ppc*) :key #'insn-name))
(defparameter *ppc-add* (find 'add (machine-insns *ppc*) :key #'insn-name))
(defparameter *ppc-64-add* (64bitify (first (insn-effect *ppc-add*)) *ppc* *alpha* *ppc-to-alpha-register-mapping*))

(defparameter *alpha-and* (find 'and (machine-insns *alpha*) :key #'insn-name))
(defparameter *alpha-and-generators* (generate-generators *alpha-and*))
(defparameter *alpha-generic-and-generator* (car (last *alpha-and-generators*)))

(defparameter *alpha-addl* (find 'addl (machine-insns *alpha*) :key #'insn-name))
(defparameter *alpha-addl-generators* (generate-generators *alpha-addl*))
(defparameter *alpha-generic-addl-generator* (car (last *alpha-addl-generators*)))

(defun generate-all-files ()
  (generate-defines-file *ppc*)
  (generate-composer-file *alpha*)
  (generate-disassembler-file *alpha*)
  (generate-disassembler-file *ppc*)
  (generate-interpreter-file *ppc*)
  (generate-compiler-file *ppc*))
