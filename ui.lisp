;; ui.lisp

;; bintrans

;; Copyright (C) 2001 Mark Probst

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

(defstruct state
  effect
  source)

(defvar *states* nil)

(defun show-states-from (num)
  (do ((states (nthcdr num *states*) (cdr states))
       (num num (1+ num)))
      ((null states))
    (let ((state (car states)))
      (format t "~A ~A = ~A~%" num (state-source state) (expr-to-sexp (first (state-effect state)))))))

(defun add-states (states)
  (let ((old-length (length *states*)))
    (setf *states* (nconc *states* states))
    (show-states-from old-length)))

(defun add-state (state)
  (add-states (list state)))

(defun the-state-expr (num)
  (first (state-effect (nth num *states*))))

(defun show ()
  (show-states-from 0)
  (values))

(defun clear ()
  (setf *states* nil)
  (values))

(defun insn (name)
  (add-states (mapcar #'(lambda (effect)
			  (make-state :effect (list effect) :source `(insn ,name)))
		      (insn-effect (find name (machine-insns *source-machine*) :key #'insn-name))))
  (values))

(defun intel-insn (name mode)
  (add-states (mapcar #'(lambda (effect)
			  (make-state :effect (list effect) :source `(intel-insn ,name ,mode)))
		      (intel-insn-effect (find-if #'(lambda (x) (and (eq (intel-insn-name x) name) (eq (intel-insn-mode x) mode)))
						  (machine-insns *source-machine*)))))
  (values))

(defun decompose (num)
  (let* ((state (nth num *states*))
	 (decomposed (decompose-effect (state-effect state) nil)))
    (add-states (mapcar #'(lambda (x) (make-state :effect (first x) :source `(decompose ,num))) decomposed)))
  (values))

(defun transform (num)
  (let* ((state (nth num *states*))
	 (expr (first (state-effect state))))
    (add-states (mapcar #'(lambda (x) (make-state :effect (list x)
						  :source `(transform ,num)))
			(remove expr (expr-alternatives expr)))))
  (values))

(defun matches (num)
  (let ((state (nth num *states*)))
    (mapcar #'(lambda (x)
		(list (insn-name (first x))
		      (position (second x) (insn-generators (first x)))))
	    (all-generator-matches (64bitify (first (state-effect state)))))))

(defun match (num insn-name generator-num)
  (let* ((state (nth num *states*))
	 (insn (find insn-name (machine-insns *target-machine*) :key #'insn-name))
	 (generator (nth generator-num (insn-generators insn))))
    (assert (= (length (car generator)) 1))
    (dolist (match (match-pattern (64bitify (first (state-effect state))) (caar generator)))
      (destructuring-bind (read-bindings write-bindings)
	  match
	(multiple-value-bind (text can-fail num-insns new-effects)
	    (generate-match-code insn generator read-bindings write-bindings nil :shallow t)
	  (add-states (mapcar #'(lambda (x) (make-state :effect x :source `(match ,num ,insn-name ,generator-num))) new-effects))
	  (format t "can ~:[not ~;~]fail~%" can-fail)))))
  (values))

(defun generator (name num)
  (let* ((insn (find name (machine-insns *target-machine*) :key #'insn-name))
	 (generator (nth num (insn-generators insn))))
    (expr-to-sexp (caar generator))))

(defun subinsn (num &rest indexes)
  (let* ((state (nth num *states*))
	 (expr (first (state-effect state))))
    (add-state (make-state :effect (list (do ((expr expr (nth (1- (car indexes)) (expr-operands expr)))
					      (indexes indexes (cdr indexes)))
					     ((null indexes) expr)))
			   :source `(subinsn ,num . ,indexes))))
  (values))
