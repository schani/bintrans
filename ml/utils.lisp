;; utils.lisp

;; MathMap

;; Copyright (C) 2002-2004 Mark Probst

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

(defun map-times (n f)
  (labels ((map (i)
	     (if (>= i n)
		 '()
		 (cons (funcall f i) (map (1+ i))))))
    (map 0)))

(defun integers-upto (n)
  (map-times n #'(lambda (i) i)))

(defun mappend (func &rest lists)
  (reduce #'append (apply #'mapcar func lists)))

;; down-case-symbol
(defun dcs (x)
  (substitute #\d #\. (substitute #\p #\+ (substitute #\_ #\- (string-downcase (symbol-name x))))))

;; up-case-symbol
(defun ucs (x)
  (substitute #\d #\. (substitute #\p #\+ (substitute #\_ #\- (string-upcase (symbol-name x))))))

(defvar *tmp-num* 0)
(defun make-tmp-name ()
  (let ((name (format nil "tmp_~A" *tmp-num*)))
    (incf *tmp-num*)
    name))

(defmacro with-tmp-name-undo (&rest body)
  `(let ((*tmp-save* *tmp-num*)
	 (*result* (progn ,@body)))
     (setq *tmp-num* *tmp-save*)
     *result*))

(defun my-macroexpand (sexp macros)
  (if (and (listp sexp)
	   (symbolp (car sexp)))
      (let ((macro (cdr (assoc (car sexp) macros))))
	(if macro
	    (my-macroexpand (apply macro (cdr sexp)) macros)
	    sexp))
      sexp))

(defun subst-many (new old sexp)
  (if (null new)
      sexp
      (subst-many (cdr new) (cdr old) (subst (car new) (car old) sexp))))
