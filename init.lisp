;; init.lisp

;; bintrans

;; Copyright (C) 2001,2002 Mark Probst

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

(load "generator")
(load "ui.lisp")

(load "transformations.lisp")

(defvar *ppc* nil)
(defvar *alpha* nil)

(load "ppc.lisp")
;(load "alpha.lisp")

;(defparameter *source-machine* *ppc*)
;(defparameter *target-machine* *alpha*)
;(defparameter *register-mapping* *ppc-to-alpha-register-mapping*)

;(generate-all-generators *alpha*)

(defun generate-all-files ()
  (generate-defines-file *ppc*)
  (generate-composer-file *alpha*)
  (generate-disassembler-file *alpha*)
  (generate-disassembler-file *ppc*)
  (generate-interpreter-file *ppc*)
  (generate-compiler-file *ppc*))

(defun generate-ppc-insn-analyzers ()
  (generate-jump-analyzer *ppc*)
  (generate-livenesser *ppc* (list (lookup-register 'cr) (lookup-register 'xer)) (list (lookup-register-class 'gpr)))
  (generate-killer *ppc* (list (lookup-register 'cr) (lookup-register 'xer)) (list (lookup-register-class 'gpr)))
  (generate-consumer *ppc* (list (lookup-register 'cr) (lookup-register 'xer)) (list (lookup-register-class 'gpr))))

(defun generate-all-ppc-files ()
  (generate-defines-file *ppc*)
  (generate-composer-file *ppc*)
  (generate-disassembler-file *ppc*)
  (generate-interpreter-file *ppc*)
  (generate-jump-analyzer *ppc*)
  (generate-skeleton-file *ppc*)
  (generate-livenesser *ppc* (list (lookup-register 'cr) (lookup-register 'xer)) (list (lookup-register-class 'gpr)))
  (generate-killer *ppc* (list (lookup-register 'cr) (lookup-register 'xer)) (list (lookup-register-class 'gpr)))
  (generate-consumer *ppc* (list (lookup-register 'cr) (lookup-register 'xer)) (list (lookup-register-class 'gpr))))
