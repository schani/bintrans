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
    (condition-and "ConditionAnd" binary)
    (condition-or "ConditionOr" binary)
    (condition-xor "ConditionXor" binary)
    (+f "FloatAdd" binary)
    (-f "FloatSub" binary)
    (*f "FloatMul" binary)
    (/f "FloatDiv" binary)

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

;; returns ml-name, kind-ml-name, num-args, and need-width
(defun lookup-operator (name)
  (let ((op-entry (cdr (assoc name *operators*))))
    (if op-entry
	(destructuring-bind (ml-name kind)
	    op-entry
	  (destructuring-bind (kind-ml-name num-args need-width)
	      (cdr (assoc kind *operator-kinds*))
	    (values ml-name kind-ml-name num-args need-width)))
      nil)))

(defun convert-byte-order (byte-order)
  (case byte-order
    (big-endian "BigEndian")
    (little-endian "LittleEndian")
    (t (error "illegal byte order ~A" byte-order))))

;; a binding is a list of the form (lisp-name ml-name type) where type can be
;; int or expr
(defun convert-expr (expr bindings)
  (labels ((lookup-int (name)
	     (let ((binding (assoc name bindings)))
	       (if binding
		   (destructuring-bind (name ml-name type)
		       binding
		     (if (eql type 'int)
			 ml-name
		       (error "variable ~A is of wrong type (should be int, but is ~A)" name type)))
		 (error "unbound symbol ~A" name))))
	   (lookup-expr (name)
	     (let ((binding (assoc name bindings)))
	       (if binding
		   (destructuring-bind (name ml-name type)
		       binding
		     (case type
		       (expr
			ml-name)
		       (int
			(format nil "IntConst (IntLiteral ~A)" ml-name))
		       (t
			(error "variable ~A is of wrong type (should be int, but is ~A)" name type))))
		 (error "unbound symbol ~A" name))))
	   (convert-int-const (int-const)
	     (case-match expr
	       ((field ?name)
		(format nil "IntField \"~A\"" (dcs name)))
	       (t
		(cond ((integerp int-const)
		       (format nil "IntLiteral (~AL)" int-const))
		      ((symbolp int-const)
		       (format nil "IntLiteral ~A" (lookup-int int-const)))
		      (t
		       (error "illegal int const ~A" int-const))))))
	   (convert-width (width)
			  (cond ((integerp width)
				 (format nil "~A" width))
				((symbolp width)
				 (format nil "(to_int ~A)" (lookup-int width)))
				(t
				 (error "illegal width ~A" width))))
	   (convert (expr)
	     (case-match expr
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
	       ((?op . ?args)
		(multiple-value-bind (ml-name kind-ml-name num-args need-width)
		    (lookup-operator op)
		  (if ml-name
		      (let ((width-string (if need-width (convert-width (car args)) nil))
			    (args (if need-width (cdr args) args)))
			(if (and (or (not need-width) width-string)
				 (= num-args (length args)))
			    (format nil "~A (~A, ~A~{~A~^, ~})"
				    kind-ml-name ml-name (if need-width (format nil "~A, " width-string) "")
				    (mapcar #'convert args))
			  (error "illegal expression ~A" expr)))
		    (error "unknown operator ~A" op))))
	       (?expr
		(cond ((integerp expr)
		       (format nil "IntConst (IntLiteral (~AL))" expr))
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

;; returns the ML string and a list of bindings
(defun convert-pattern (pattern)
  (labels ((convert-int-const (int-const)
	     (cond ((integerp int-const)
		    (format nil "TheInt ~AL" int-const))
		   ((dont-care-symbol-p int-const)
		    "AnyInt \"__dummy__\"")
		   ((var-symbol-p int-const)
		    (let ((name (symbol-for-var-symbol int-const)))
		      (values (format nil "AnyInt \"~A\"" (dcs name))
			      (list (list name 'int)))))
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
		   (t
		    (let-match (((?name ?widths) width))
		      (progn
			(unless (var-symbol-p name)
			  (error "~A is not a var symbol" name))
			(let ((name (symbol-for-var-symbol name)))
			  (value (format nil "([~{~A~^; ~}], \"~A\")" widths (dcs name))
				 (list (list name 'int)))))
		      (error "illegal width pattern ~A" width)))))
	   (convert (pattern)
	     (case-match pattern
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
	       ((?op . ?args)
		(multiple-value-bind (ml-name kind-ml-name num-args need-width)
		    (lookup-operator op)
		  (if ml-name
		      (multiple-value-bind (width-string width-bindings)
			  (if need-width
			      (convert-width (car args))
			      (values nil nil))
			(let ((args (if need-width (cdr args) args)))
			  (if (and (or (not need-width) width-string)
				   (= num-args (length args)))
			      (let ((strings-and-bindings (mapcar #'(lambda (arg)
								      (multiple-value-bind (string bindings)
									  (convert arg)
									(cons string bindings)))
								  args)))
				(values (format nil "~APattern (~A, ~A~{~A~^, ~})"
						kind-ml-name ml-name (if need-width (format nil "~A, " width-string) "")
						(mapcar #'car strings-and-bindings))
					(apply #'append width-bindings (mapcar #'cdr strings-and-bindings))))
			      (error "illegal expression ~A" expr))))
		    (error "unknown operator ~A" op))))
	       (?pattern
		(cond ((integerp pattern)
		       (format nil "IntPattern (TheInt ~AL)" pattern))
		      ((floatp pattern)
		       (format nil "FloatPattern (TheFloat ~AL)" pattern))
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
		      (t
		       (error "illegal pattern ~A" pattern)))))))
    (convert pattern)))

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
	  (format out "let make_~A~A~{ ~A~} = ~%  ~A~%~%"
		  (dcs name) (if needs-width " width" "") (mapcar #'dcs args)
		  (convert-expr body bindings)))))))

(defun make-simplifies ()
  (with-open-file (out "simplifiers.ml" :direction :output :if-exists :supersede)
    (format out (string-concat "open Int64~%~%open Expr~%open Matcher~%~%"
			       "type simplifier =~%    { pattern : pattern ;~%      apply : expr -> binding list -> expr }~%~%"
			       "let simplifiers =~%  [~%"))
    (dolist (simplify (reverse *simplifies*))
      (destructuring-bind (pattern expr)
	  simplify
	(multiple-value-bind (pattern-string bindings)
	    (convert-pattern pattern)
	  (format out "    { pattern = ~A ;~%      apply = fun e b ->~%        ~A } ;~%"
		  pattern-string
		  (if (null bindings)
		      (convert-expr expr '())
		    (let* ((binding-names (mapcar #'first bindings))
			   (binding-ml-names (mapcar #'dcs binding-names))
			   (binding-types (mapcar #'second bindings)))
		      (format nil (string-concat         "match (~{find_binding b \"~A\"~^, ~}) with~%"
					         "          (~{~ABinding (_, ~A)~^, ~}) ->~%"
						 "            ~A~%"
						 "        | _ -> raise Wrong_binding")
			      binding-ml-names
			      (mappend #'(lambda (type name) (list (cdr (assoc type '((int . "Int") (expr . "Expr")))) name))
				       binding-types binding-ml-names)
			      (convert-expr expr (mapcar #'list binding-names binding-ml-names binding-types)))))))))
    (format out "  ]~%")))
