;; lisp2ml.lisp

;; bintrans

;; Copyright (C) 2004 Mark Probst

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

(load "let-match.lisp")
(load "utils.lisp")

;; an operator kind definition is a list (lisp-name ml-name num-args need-width)
(defvar *operator-kinds*
  '((unary "Unary" 1 nil)
    (unary-width "UnaryWidth" 1 t)
    (binary "Binary" 2 nil)
    (binary-width "BinaryWidth" 2 t)
    (ternary-width "TernaryWidth" 3 t)))

;; an operator definition is a list (lisp-name ml-name kind)
(defvar *operators*
  '((load-byte "LoadByte" unary)
    (int-to-float "IntToFloat" unary)
    (float-to-int "FloatToInt" unary)
    (condition-to-int "ConditionToInt" unary)
    (int-even-p "IntEven" unary)
    (int-neg "IntNeg" unary)
    (bit-neg "BitNeg" unary)
    (condition-neg "ConditionNeg" unary)
    (float-sqrt "FloatSqrt" unary)
    (float-neg "FloatNeg" unary)
    (float-abs "FloatAbs" unary)
    (low-one-bits "LowOneBits" unary)
    (low-mask "LowMask" unary)
    (high-mask "HighMask" unary)

    (int-zero-p "IntZero" unary-width)
    (int-parity-even-p "IntParityEven" unary-width)
    (int-sign-p "IntSign" unary-width)
    (sex "Sex" unary-width)
    (zex "Zex" unary-width)

    (=f "FloatEqual" binary)
    (<f "FloatLess" binary)
    (+i "IntAdd" binary)
    (-i "IntSub" binary)
    (*i "IntMul" binary)
    (bit-and "BitAnd" binary)
    (bit-or "BitOr" binary)
    (bit-xor "BitXor" binary)
    (shiftl "ShiftL" binary)
    (and "ConditionAnd" binary)
    (or "ConditionOr" binary)
    (xor "ConditionXor" binary)
    (+f "FloatAdd" binary)
    (-f "FloatSub" binary)
    (*f "FloatMul" binary)
    (/f "FloatDiv" binary)
    (both-low-one-bits "BothLowOneBits" binary)
    (bit-mask "BitMask" binary)

    (=i "IntEqual" binary-width)
    (<iu "LessU" binary-width)
    (<is "LessS" binary-width)
    (add-carry-p "AddCarry" binary-width)
    (sub-carry-p "SubCarry" binary-width)
    (overflow-p "Overflow" binary-width)
    (lshiftr "LShiftR" binary-width)
    (ashiftr "AShiftR" binary-width)
    (*ihs "IntMulHS" binary-width)
    (*ihu "IntMulHU" binary-width)

    (/is "DivS" ternary-width)
    (/iu "DivU" ternary-width)
    (mod-is "ModS" ternary-width)
    (mod-iu "ModU" ternary-width)))

;; an ir macro is a list (lisp-name args needs-width body)
(defvar *ir-macros* '())

(defmacro defirmacro (name args body)
  (multiple-value-bind (args needs-width)
      (if (eql (car args) 'width)
	  (values (cdr args) t)
	(values args nil))
    `(push (list ',name ',args ',needs-width ',body) *ir-macros*)))

;; a simplify is a list (pattern expr)
(defvar *simplifies* '())

(defmacro defsimplify (pattern expr)
  `(push (list ',pattern ',expr) *simplifies*))

;; a matcher is a list (name pattern cost-expr exec-format gen-format)
(defvar *matchers* '())

(defmacro defmatcher (name pattern cost-expr exec-format gen-format)
  `(push (list ',name ',pattern ',cost-expr ',exec-format ',gen-format) *matchers*))

;; returns ml-name, kind-ml-name, num-args, and need-width
;; kind-ml-name is nil of the operator is a macro
(defun lookup-operator (name)
  (let ((op-entry (cdr (assoc name *operators*))))
    (if op-entry
	(destructuring-bind (ml-name kind)
	    op-entry
	  (destructuring-bind (kind-ml-name num-args need-width)
	      (cdr (assoc kind *operator-kinds*))
	    (values ml-name kind-ml-name num-args need-width)))
      (let ((macro (cdr (assoc name *ir-macros*))))
	(if macro
	    (destructuring-bind (args needs-width body)
		macro
	      (values (dcs name) nil (length args) needs-width))
	  nil)))))

(defun convert-byte-order (byte-order)
  (case byte-order
    (big-endian "BigEndian")
    (little-endian "LittleEndian")
    (t (error "illegal byte order ~A" byte-order))))

(defun lookup-int (bindings name)
  (let ((binding (assoc name bindings)))
    (if binding
	(destructuring-bind (name ml-name type)
	    binding
	  (if (eql type 'int)
	      ml-name
	      (error "variable ~A is of wrong type (should be int, but is ~A)" name type)))
	(error "unbound symbol ~A" name))))

;; a binding is a list of the form (lisp-name ml-name type) where type can be
;; int or expr
(defun convert-expr (expr bindings)
  (labels ((lookup-expr (name)
	     (let ((binding (assoc name bindings)))
	       (if binding
		   (destructuring-bind (name ml-name type)
		       binding
		     (case type
		       (expr
			ml-name)
		       (int
			(format nil "int_literal_expr (~A)" ml-name))
		       (int-const
			ml-name)
		       (t
			(error "variable ~A is of wrong type (~A)" name type))))
		 (error "unbound symbol ~A" name))))
	   (convert-int-const (int-const)
	     (case-match expr
	       ((field ?name)
		(format nil "IntField \"~A\"" (dcs name)))
	       (t
		(cond ((integerp int-const)
		       (format nil "IntLiteral (~AL)" int-const))
		      ((symbolp int-const)
		       (format nil "IntLiteral ~A" (lookup-int bindings int-const)))
		      (t
		       (error "illegal int const ~A" int-const))))))
	   (convert-width (width)
			  (cond ((integerp width)
				 (format nil "~A" width))
				((symbolp width)
				 (format nil "(to_int ~A)" (lookup-int bindings width)))
				(t
				 (error "illegal width ~A" width))))
	   (convert (expr)
	     (case-match expr
	       ((const int ?expr)
		(convert expr))
	       ((load ?byte-order ?width ?addr)
		(format nil "LoadBO (~A, ~A, ~A)"
			(convert-byte-order byte-order)
			(convert-width width)
			(convert addr)))
	       ((extract ?arg ?start ?length)
		(format nil "Extract (~A, ~A, ~A)"
			(convert arg)
			(convert-int-const start)
			(convert-int-const length)))
	       ((insert ?arg1 ?arg2 ?start ?length)
		(format nil "Insert (~A, ~A, ~A, ~A)"
			(convert arg1)
			(convert arg2)
			(convert-int-const start)
			(convert-int-const length)))
	       ((if ?condition ?consequent ?alternative)
		(format nil "If (~A, ~A, ~A)"
			(convert condition)
			(convert consequent)
			(convert alternative)))
	       ((user-op ?name . ?args)
		(format nil "UserOp (\"~A\", [~{~A~^ ; ~}])"
			name (mapcar #'convert args)))
	       ((?op . ?args)
		(multiple-value-bind (ml-name kind-ml-name num-args need-width)
		    (lookup-operator op)
		  (if ml-name
		      (let ((width-string (if need-width (convert-width (car args)) nil))
			    (args (if need-width (cdr args) args)))
			(if (and (or (not need-width) width-string)
				 (= num-args (length args)))
			    (if kind-ml-name
				(format nil "~A (~A, ~A~{~A~^, ~})"
					kind-ml-name ml-name (if need-width (format nil "~A, " width-string) "")
					(mapcar #'convert args))
			      (format nil "make_~A ~A~{(~A)~^ ~}"
				      ml-name (if need-width (format nil "~A " width-string) "")
				      (mapcar #'convert args)))
			  (error "illegal expression ~A" expr)))
		    (error "unknown operator ~A" op))))
	       (?expr
		(cond ((integerp expr)
		       (format nil "int_literal_expr (~AL)" expr))
		      ((floatp expr)
		       (format nil "FloatConst ~A" expr))
		      ((eql expr 't)
		       "ConditionConst true")
		      ((eql expr 'nil)
		       "ConditionConst false")
		      ((symbolp expr)
		       (lookup-expr expr))
		      (t
		       (error "illegal expression ~A" expr)))))))
    (convert expr)))

(defmacro let-bindings (bindings-name let-bindings &rest body)
  (let ((let-syms (mapcar #'(lambda (x) (gensym)) let-bindings))
	(binding-syms (mapcar #'(lambda (x) (gensym)) let-bindings)))
    (labels ((generate (rest-let-bindings rest-let-syms rest-binding-syms)
	       (if (null rest-let-bindings)
		   `(let ((,bindings-name (append ,@binding-syms))
			  ,@(mapcar #'list (mapcar #'first let-bindings) let-syms))
		     ,@body)
		   `(multiple-value-bind (,(car rest-let-syms) ,(car rest-binding-syms))
		     ,(second (car rest-let-bindings))
		     ,(generate (cdr rest-let-bindings) (cdr rest-let-syms) (cdr rest-binding-syms))))))
      (generate let-bindings let-syms binding-syms))))

;; returns the ML string and a list of bindings.  the in-bindings have
;; the same format as in convert-expr.
(defun convert-pattern (pattern in-bindings)
  (labels ((lookup-expr-pattern (name)
	     (let ((binding (assoc name in-bindings)))
	       (if binding
		   (destructuring-bind (name ml-name type)
		       binding
		     (case type
		       (expr
			ml-name)
		       (int
			(format nil "IntPattern (TheInt (int_literal_expr (~A)))" ml-name))
		       (int-const
			ml-name)
		       (t
			(error "variable ~A is of wrong type (~A)" name type))))
		 (error "unbound symbol ~A" name))))
	   (convert-int-const (int-const)
	     (cond ((integerp int-const)
		    (format nil "TheInt (int_literal_expr (~AL))" int-const))
		   ((dont-care-symbol-p int-const)
		    "AnyInt \"__dummy__\"")
		   ((var-symbol-p int-const)
		    (let ((name (symbol-for-var-symbol int-const)))
		      (values (format nil "AnyInt \"~A\"" (dcs name))
			      (list (list name 'int-const)))))
		   ((symbolp int-const)
		    (format nil "TheInt (int_literal_expr (~A))" (lookup-int in-bindings int-const)))
		   (t
		    (error "illegal int const pattern ~A" int-const))))
	   (convert-width (width)
	     (cond ((integerp width)
		    (format nil "([~A], \"__dummy__\")" width))
		   ((dont-care-symbol-p width)
		    "([1; 2; 4; 8], \"__dummy__\")")
		   ((var-symbol-p width)
		    (let ((width (symbol-for-var-symbol width)))
		      (values (format nil "([1; 2; 4; 8], \"~A\")" (dcs width))
			      (list (list width 'int)))))
		   ((symbolp width)
		    (format nil "([to_int ~A], \"__dummy__\")" (lookup-int in-bindings width)))
		   (t
		    (let-match (((?name ?widths) width))
		      (progn
			(unless (var-symbol-p name)
			  (error "~A is not a var symbol" name))
			(let ((name (symbol-for-var-symbol name)))
			  (values (format nil "([~{~A~^; ~}], \"~A\")" widths (dcs name))
				  (list (list name 'int)))))
		      (error "illegal width pattern ~A" width)))))
	   (convert (pattern)
	     (case-match pattern
	       ((const int ?expr)
		(values (format nil "IntPattern (TheInt (~A))" (convert-expr expr in-bindings))
			'()))
	       ((any-int ?name)
		(if (var-symbol-p name)
		    (let ((name (symbol-for-var-symbol name)))
		      (values (format nil "IntPattern (AnyInt \"~A\")" (dcs name))
			      (list (list name 'int-const))))
		    (error "~A is not a var symbol" name)))
	       ((register ?name)
		(if (var-symbol-p name)
		    (let ((name (symbol-for-var-symbol name)))
		      (values (format nil "RegisterPattern \"~A\"" (dcs name))
			      (list (list name 'reg))))
		    (error "~A is not a var symbol" name)))
	       ((load ?byte-order ?width ?addr)
		(let-bindings bindings ((byte-order-string (convert-byte-order byte-order))
					(width-string (convert-width width))
					(addr-string (convert addr)))
		  (values (format nil "LoadBOPattern (~A, ~A, ~A)"
				  byte-order-string width-string addr-string)
			  bindings)))
	       ((extract ?arg ?start ?length)
		(let-bindings bindings ((arg-string (convert arg))
					(start-string (convert-int-const start))
					(length-string (convert-int-const length)))
		  (values (format nil "ExtractPattern (~A, ~A, ~A)"
				  arg-string start-string length-string)
			  bindings)))
	       ((insert ?arg1 ?arg2 ?start ?length)
		(let-bindings bindings ((arg1-string (convert arg1))
					(arg2-string (convert arg2))
					(start-string (convert-int-const start))
					(length-string (convert-int-const length)))
		  (values (format nil "InsertPattern (~A, ~A, ~A, ~A)"
				  arg1-string arg2-string start-string length-string)
			  bindings)))
	       ((if ?condition ?consequent ?alternative)
		(let-bindings bindings ((condition-string (convert condition))
					(consequent-string (convert consequent))
					(alternative-string (convert alternative)))
		  (values (format nil "IfPattern (~A, ~A, ~A)"
				  condition-string consequent-string alternative-string)
			  bindings)))
	       ((set ?reg ?value)
		(if (var-symbol-p reg)
		    (let ((reg-name (symbol-for-var-symbol reg)))
		      (let-bindings bindings ((value-string (convert value)))
			(values (format nil "AssignPattern (\"~A\", ~A)"
					(dcs reg-name) value-string)
				(cons (list reg-name 'reg) bindings))))
		    (error "expr ~A is not an lhs" reg)))
	       ((?op . ?args)
		(multiple-value-bind (ml-name kind-ml-name num-args need-width)
		    (lookup-operator op)
		  (if ml-name
		      (multiple-value-bind (width-string width-bindings)
			  (if need-width
			      (let ((width (car args)))
				(if kind-ml-name
				    (convert-width width)
				    (values (cond ((integerp width)
						   (format nil "~A" width))
						  ((symbolp width)
						   (format nil "(to_int ~A)" (lookup-int in-bindings (car args))))
						  (t
						   (error "illegal pattern width argument ~A" width)))
					    nil)))
			      (values nil nil))
			(let ((args (if need-width (cdr args) args)))
			  (if (and (or (not need-width) width-string)
				   (= num-args (length args)))
			      (let ((strings-and-bindings (mapcar #'(lambda (arg)
								      (multiple-value-bind (string bindings)
									  (convert arg)
									(cons string bindings)))
								  args)))
				    (values (if kind-ml-name
						(format nil "~APattern (~A, ~A~{~A~^, ~})"
							kind-ml-name ml-name (if need-width (format nil "~A, " width-string) "")
							(mapcar #'car strings-and-bindings))
						(format nil "make_~A_pattern ~A~{(~A)~^ ~}"
							ml-name (if need-width (format nil "~A " width-string) "")
							(mapcar #'car strings-and-bindings)))
					    (apply #'append width-bindings (mapcar #'cdr strings-and-bindings))))
			      (error "illegal expression ~A" expr))))
		    (error "unknown operator ~A" op))))
	       (?pattern
		(cond ((integerp pattern)
		       (format nil "IntPattern (TheInt (int_literal_expr (~AL)))" pattern))
		      ((floatp pattern)
		       (format nil "FloatPattern (TheFloat (~AL))" pattern))
		      ((dont-care-symbol-p pattern)
		       "ExprPattern \"__dummy__\"")
		      ((var-symbol-p pattern)
		       (let ((name (symbol-for-var-symbol pattern)))
			 (values (format nil "ExprPattern \"~A\"" (dcs name))
				 (list (list name 'expr)))))
		      ((eql pattern 't)
		       "ConditionConstPattern (TheBool true)")
		      ((eql pattern 'nil)
		       "ConditionConstPattern (TheBool false)")
		      ((symbolp pattern)
		       (lookup-expr-pattern pattern))
		      (t
		       (error "illegal pattern ~A" pattern)))))))
    (convert pattern)))

;; returns the ml string of the cost expression and an ml string of
;; the condition which has to be satisfied for a match.
(defun convert-cost-expr (expr bindings)
  (let ((convert-expr-bindings (mapcar #'(lambda (binding)
					   (list (first binding) (dcs (first binding)) (second binding)))
				       bindings)))
    (labels ((convert (expr)
	       (case-match expr
	         ((when ?condition ?consequent)
		  (let ((condition-string (convert-expr condition convert-expr-bindings)))
		    (multiple-value-bind (consequent-string consequent-cond)
			(convert consequent)
		      (values (format nil "uc_when fields (~A) (fun _ -> ~A)"
				      condition-string consequent-string)
			      (format nil "and_expr (~A) (~A)"
				      condition-string consequent-cond)))))
		 (t
		  (cond ((integerp expr)
			 (values (format nil "uc_return ~A" expr)
				 "ConditionConst true"))
			(t
			 (error "illegal cost expression ~A" expr)))))))
      (convert expr))))

(defun convert-format (format args bindings for-gen-gen)
  (labels ((convert-tilde (char args rest-format)
	     (case char
	       (#\~ (format nil "\"~~\" ^ ~A" (convert rest-format args)))
	       (#\% (format nil "\"\\n\" ^ ~A" (convert rest-format args)))
	       (#\A
		(let* ((name (car args))
		       (binding (assoc name bindings))
		       (type (cadr binding)))
		  (if (null binding)
		      (error "unbound variable ~A" name)
		    (case type
		      (int
		       (format nil "(string_of_int ~A) ^ ~A" (dcs name) (convert rest-format (cdr args))))
		      (int-const
		       (format nil "(expr_to_c alloc ~A) ^ ~A" (dcs name) (convert rest-format (cdr args))))
		      (reg
		       (format nil "(register_to_c~A alloc ~A) ^ ~A" (if for-gen-gen "_gen" "") (dcs name) (convert rest-format (cdr args))))
		      (t
		       (error "binding type ~A not allowed in format" type))))))
	       (t
		(error "illegal format directive #\~A" char))))
	   (convert (format args)
	     (let ((tilde-pos (position #\~ format)))
	       (cond ((null tilde-pos)
		      (format nil "\"~A\"" format))
		     ((= tilde-pos 0)
		      (convert-tilde (char format (1+ tilde-pos)) args (subseq format (+ tilde-pos 2))))
		     (t
		      (format nil "\"~A\" ^ ~A"
			      (subseq format 0 tilde-pos)
			      (convert-tilde (char format (1+ tilde-pos)) args (subseq format (+ tilde-pos 2)))))))))
    (convert format args)))

(defun make-ir-macros ()
  (with-open-file (out "irmacros.ml" :direction :output :if-exists :supersede)
    (format out "open Int64~%~%open Expr~%~%")
    (dolist (macro (reverse *ir-macros*))
      (destructuring-bind (name args needs-width body)
	  macro
	(let* ((arg-bindings (mapcar #'(lambda (arg) (list arg (dcs arg) 'expr)) args))
	       (bindings (if needs-width
			     (cons '(width "(of_int width)" int) arg-bindings)
			   arg-bindings)))
	  (format out "let make_~A~A~{ ~A~} = ~%  ~A~%"
		  (dcs name) (if needs-width " width" "") (mapcar #'dcs args)
		  (convert-expr body bindings))
	  (format out "let make_~A_pattern~A~{ ~A~} = ~%  ~A~%~%"
		  (dcs name) (if needs-width " width" "") (mapcar #'dcs args)
		  (convert-pattern body bindings)))))))

(defun make-simplifies ()
  (with-open-file (out "simplifiers.ml" :direction :output :if-exists :supersede)
    (format out (string-concat "open Int64~%~%open Expr~%open Matcher~%~%"
			       "type simplifier =~%    { pattern : pattern ;~%      apply : expr -> binding list -> expr }~%~%"
			       "let simplifiers =~%  [~%"))
    (dolist (simplify (reverse *simplifies*))
      (destructuring-bind (pattern expr)
	  simplify
	(multiple-value-bind (pattern-string bindings)
	    (convert-pattern pattern '())
	  (format out "    { pattern = ~A ;~%      apply = fun e bindings ->~%        ~A } ;~%"
		  pattern-string
		  (if (null bindings)
		      (convert-expr expr '())
		    (let* ((binding-names (mapcar #'first bindings))
			   (binding-ml-names (mapcar #'dcs binding-names))
			   (binding-types (mapcar #'second bindings)))
		      (format nil (string-concat         "match (~{find_binding bindings \"~A\"~^, ~}) with~%"
					         "          (~{~ABinding (_, ~A)~^, ~}) ->~%"
						 "            ~A~%"
						 "        | _ -> raise Wrong_binding")
			      binding-ml-names
			      (mappend #'(lambda (type name) (list (cdr (assoc type '((int . "Int") (expr . "Expr")))) name))
				       binding-types binding-ml-names)
			      (convert-expr expr (mapcar #'list binding-names binding-ml-names binding-types)))))))))
    (format out "  ]~%")))

(defun make-matchers (filename matchers-name printers-name gen-gens-name)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out "open Int64~%~%open Expr~%open Uncertainty~%open Matcher~%open Irmacros~%open Cgen~%~%")
    (dolist (matcher (reverse *matchers*))
      (destructuring-bind (name pattern cost-expr exec-format gen-format)
	  matcher
	(multiple-value-bind (pattern-string bindings)
	    (convert-pattern pattern '())
	  (multiple-value-bind (cost-string cost-cond)
	      (convert-cost-expr cost-expr bindings)
	    (let* ((binding-names (mapcar #'first bindings))
		   (binding-ml-names (mapcar #'dcs binding-names))
		   (binding-types (mapcar #'second bindings))
		   (cgen-bindings-format-list (mappend #'(lambda (type name)
							   (list name
								 (case type
								   (int (format nil "get_width_binding bindings \"~A\"" name))
								   (int-const (format nil "get_const_binding bindings \"~A\"" name))
								   (reg (format nil "get_register_binding bindings alloc \"~A\"" name))
								   (t (error "binding of type ~A is not allowed in matcher pattern" type)))))
						       binding-types binding-ml-names)))
	      (format out (string-concat "let ~A_matcher =~%"
					 "  { name = \"~A\" ;~%"
					 "    pattern = ~A ;~%"
					 "    matcher = (fun fields bindings ->~%"
					 "                 let __dummy__ = []~{ and ~A = ~A~}~%"
					 "                 in ~A) }~%"
					 "and ~A_printer =~%"
					 "  (\"~A\", (fun alloc bindings ->~%"
					 "              let~{ ~A = ~A~^ and~}~%"
					 "              in \"/* ~A */ { assert(\" ^ (expr_to_c [] (~A)) ^ \"); \" ^ ~A ^ \" }\"))~%"
					 "and ~A_gen_gen =~%"
					 "  (\"~A\", (fun alloc bindings ->~%"
					 "              let~{ ~A = ~A~^ and~}~%"
					 "              in \"{ bt_assert(\" ^ (expr_to_c [] (~A)) ^ \"); \" ^ ~A ^ \" }\"))~%~%")
		      (dcs name) (dcs name) ;matcher
		      pattern-string
		      (mappend #'(lambda (type name)
				   (case type
				     (int (list name (format nil "get_width_binding bindings \"~A\"" name)))
				     (int-const (list name (format nil "get_const_binding bindings \"~A\"" name)))
				     (t '())))
			       binding-types binding-ml-names)
		      cost-string
		      
		      (dcs name) (dcs name) ;printer
		      cgen-bindings-format-list
		      (dcs name) cost-cond (convert-format (car exec-format) (cdr exec-format) bindings nil)
		      
		      (dcs name) (dcs name) ;gen-gen
		      cgen-bindings-format-list
		      cost-cond (convert-format (car gen-format) (cdr gen-format) bindings t)))))))
    (format out "let ~A = [ ~{~A_matcher~^ ; ~} ]~%" matchers-name (mapcar #'dcs (mapcar #'first (reverse *matchers*))))
    (format out "let ~A = [ ~{~A_printer~^ ; ~} ]~%" printers-name (mapcar #'dcs (mapcar #'first (reverse *matchers*))))
    (format out "let ~A = [ ~{~A_gen_gen~^ ; ~} ]~%" gen-gens-name (mapcar #'dcs (mapcar #'first (reverse *matchers*))))))
