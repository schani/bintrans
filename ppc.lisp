(defmacro dofromto (spec &rest body)
  (destructuring-bind (var from to)
      spec
    `(do ((,var ,from (1+ ,var)))
      ((> ,var ,to))
      ,@body)))

(defmacro letmv (spec &rest body)
  (labels ((expand (spec)
	     (if (null spec)
		 `(progn ,@body)
		 `(multiple-value-bind ,(caar spec)
		   ,(cadar spec)
		   ,(expand (cdr spec))))))
    (expand spec)))

(defparameter *fields* nil)

(defparameter *insn-bits* 32)
(defparameter *word-bits* 32)

(defstruct insn
  name
  fields
  effect
  asm
  known-bits
  known-bit-values)

(defstruct register-class
  name
  width)

(defparameter *register-classes* nil)

(defstruct register
  name
  register-class
  number)

(defparameter *registers* nil)

(defstruct subregister
  name
  register
  begin
  end)

(defparameter *subregisters* nil)

(defparameter *insns* nil)

(defvar *tmp-num* 0)

(defun define-register-class (name width registers)
  (let ((class (make-register-class :name name
				    :width width)))
    (push class *register-classes*)
    (do ((registers registers (cdr registers))
	 (i 0 (1+ i)))
	((null registers))
      (push (make-register :name (car registers)
			   :register-class class
			   :number i)
	    *registers*))))

(defun registerp (expr)
  (if (and (symbolp expr)
	   (find expr *registers* :key #'register-name))
      t
      nil))

(defun lookup-register (name)
  (find name *registers* :key #'register-name))

(defun lookup-register-class (name)
  (find name *register-classes* :key #'register-class-name))

(defun define-subregisters (subregisters)
  (dolist (subregister subregisters)
    (destructuring-bind (name register-name begin end)
	subregister
      (let ((register (lookup-register register-name)))
	(when (null register)
	  (error "not a register: ~A~%" register-name))
	(push (make-subregister :name name
				:register register
				:begin begin
				:end end)
	      *subregisters*)))))

(defun subregisterp (expr)
  (if (and (symbolp expr)
	   (find expr *subregisters* :key #'subregister-name))
      t
      nil))

(defun lookup-subregister (name)
  (find name *subregisters* :key #'subregister-name))

(defun define-fields (fields)
  (dolist (field fields)
    (destructuring-bind (name begin end)
	field
      (when (assoc name *fields*)
	(error "field already defined: ~A~%" name))
      (setq *fields* (acons name (cons begin end) *fields*)))))

(defun fieldp (expr)
  (if (and (symbolp expr)
	   (assoc expr *fields*))
      t
      nil))

(defun lookup-field (name)
  (destructuring-bind (begin . end)
      (cdr (assoc name *fields*))
    (values begin end)))

(defun integer-to-bit-vector (x &optional (num-bits *insn-bits*))
  (let ((bits (make-array num-bits :element-type 'bit :initial-element 0)))
    (dofromto (i 0 (1- num-bits))
      (when (logbitp i x)
	(setf (bit bits i) 1)))
    bits))

(defun bit-vector-to-integer (v)
  (let ((x 0))
    (dofromto (i 0 (1- (length v)))
      (when (= (bit v i) 1)
	(incf x (expt 2 i))))
    x))

(defun bit-range (begin end &optional (num-bits *insn-bits*))
  (integer-to-bit-vector (ash (1- (expt 2 (1+ (- end begin)))) begin) num-bits))

(defmacro define-insn (name fields effect asm)
  (when (assoc name *insns*)
    (error "insn already defined: ~A~%" name))
  (let ((bits (make-array *insn-bits* :element-type 'bit :initial-element 0))
	(bit-values (make-array *insn-bits* :element-type 'bit :initial-element 0)))
    (dolist (field fields)
      (destructuring-bind (field-name value)
	  field
	(multiple-value-bind (begin end)
	    (lookup-field field-name)
	  (when (or (< value 0)
		    (>= value (expt 2 (1+ (- end begin)))))
	    (error "value ~A does not fit into field ~A in insn ~A" value field-name name))
	  (dofromto (i begin end)
	    (when (= (bit bits i) 1)
	      (error "insn ~A specifies bit ~A more than once~%" name i))
	    (setf (bit bits i) 1))
	  (bit-ior bit-values (integer-to-bit-vector (ash value begin)) bit-values))))
    `(push (make-insn :name ',name
	              :fields ',(mapcar #'(lambda (field) (cons (car field) (cadr field))) fields)
	              :effect ',effect
	              :asm ',asm
	              :known-bits ,bits
	              :known-bit-values ,bit-values)
           *insns*)))

(defun one-sequences (bits &optional (start 0))
  (let ((begin (position 1 bits :start start)))
    (if (null begin)
	nil
	(let ((end (position 0 bits :start (1+ begin))))
	  (if (null end)
	      (list (cons begin (1- (length bits))))
	      (cons (cons begin (1- end)) (one-sequences bits (1+ end))))))))

(defun common-known-bits (insns used-bits)
  (let ((bits (bit-not used-bits)))
    (dolist (insn insns)
      (bit-and bits (insn-known-bits insn) bits))
    (one-sequences bits)))

(defun build-decision-tree (insns &optional (used-bits (make-array *insn-bits* :element-type 'bit :initial-element 0)))
  (labels ((build-tree (insns used-bits)
	     (cond ((null insns)
		    (error "no insns~%"))
		   ((null (cdr insns))
		    (car insns))
		   (t
		    (let* ((common (sort (common-known-bits insns used-bits)
					 #'(lambda (a b)
					     (> (- (cdr a) (car a))
						(- (cdr b) (car b))))))
			   (discriminator (car common)))
		      (when (null discriminator)
			(error "cannot discriminate between insns ~A~%" insns))
		      (cons discriminator (discriminate insns discriminator used-bits))))))
	   (value-at-discriminator (insn discriminator)
	     (let ((begin (car discriminator))
		   (end (cdr discriminator))
		   (value 0))
	       (dofromto (i begin end)
		 (when (= (bit (insn-known-bit-values insn) i) 1)
		   (incf value (expt 2 (- i begin)))))
	       value))
	   (discriminate (insns discriminator used-bits)
	     (if (null insns)
		 nil
		 (let* ((value (value-at-discriminator (car insns) discriminator))
			(matching-insns (remove-if-not #'(lambda (insn)
							   (= (value-at-discriminator insn discriminator) value))
						       insns))
			(rest-insns (remove-if #'(lambda (insn)
						   (= (value-at-discriminator insn discriminator) value))
					       insns)))
		   (cons (cons value (build-tree matching-insns
						 (bit-ior used-bits (bit-range (car discriminator) (cdr discriminator)))))
			 (discriminate rest-insns discriminator used-bits))))))
    (build-tree insns used-bits)))

(defun code-for-field (expr begin end)
  (let ((width (1+ (- end begin))))
    (format nil "((~A >> ~A) & 0x~X)" expr begin (1- (expt 2 width)))))

(defun make-tmp-name ()
  (let ((name (format nil "tmp_~A" *tmp-num*)))
    (incf *tmp-num*)
    name))

(defun type-for-width (width &optional (signed nil))
  (if signed
      (format nil "sword_~A" width)
      (if (= width *word-bits*)
	  "word"
	  (format nil "word_~A" width))))

(defun generate-expr-code (expr &optional (bindings nil) (required-width nil))
  (labels ((match-width (width)
	     (when (and (not (null required-width))
			(/= width required-width))
	       (error "width ~A should be ~A in ~A~%" width required-width expr)))
	   (resolve-register (name-or-field class-name)
	     (if (registerp name-or-field)
		 (let* ((register (lookup-register name-or-field))
			(class (register-register-class register))
			(width (register-class-width class)))
		   (values (format nil "(regs_~A[~A])"
				   (register-class-name class)
				   (register-number register))
			   width))
		 (let* ((class (lookup-register-class class-name))
			(width (register-class-width class)))
		   (when (null class)
		     (error "unknown register class in ~A~%" expr))
		   (values (format nil "(regs_~A[~A])"
				   (register-class-name class)
				   (generate-expr-code name-or-field bindings))
			   width)))))
    (cond ((fieldp expr)
	   (multiple-value-bind (begin end)
	       (lookup-field expr)
	     (let ((width (1+ (- end begin))))
	       (match-width width)
	       (values (code-for-field "insn" begin end) width))))
	  ((registerp expr)
	   (let* ((register (lookup-register expr))
		  (width (register-class-width (register-register-class register))))
	     (match-width width)
	     (values (format nil "~A" (register-number register)) width)))
	  ((subregisterp expr)
	   (let* ((subregister (lookup-subregister expr))
		  (begin (subregister-begin subregister))
		  (end (subregister-end subregister))
		  (width (1+ (- end begin))))
	     (match-width width)
	     (let ((register (subregister-register subregister)))
	       (values (code-for-field (format nil "regs_~A[~A]"
					       (register-class-name (register-register-class register))
					       (register-number register)) begin end)
		       width))))
	  ((integerp expr)
	   (values (format nil "~A" expr) (if (null required-width) *word-bits* required-width)))
	  ((stringp expr)
	   (format nil "\"~A\"" expr))
	  ((eq expr 'pc)
	   (match-width *word-bits*)
	   (values (format nil "pc") *word-bits*))
	  ((eq expr 'addr)
	   (match-width *word-bits*)
	   (values (format nil "addr") *word-bits*))
	  ((symbolp expr)
	   (let ((binding (assoc expr bindings)))
	     (if (null expr)
		 (error "unknown variable ~A~%" expr)
		 (destructuring-bind (width c-name)
		     (cdr binding)
		   (when (and (not (null required-width))
			      (/= required-width width))
		     (error "width ~A does not match width ~A of variable ~A~%" required-width width expr))
		   (values c-name width)))))
	  ((consp expr)
	   (case (first expr)
	     (let
	       (unless (null required-width)
		 (error "let has no return value~%"))
	       (with-output-to-string (out)
		 (format out "{~%")
		 (let ((new-bindings (append (mapcar #'(lambda (binding)
							 (destructuring-bind (name expr)
							     binding
							   (multiple-value-bind (code width)
							       (generate-expr-code expr bindings)
							     (let ((c-name (make-tmp-name)))
							       (format out "~A ~A = ~A;~%" (type-for-width width) c-name code)
							       (list name width c-name)))))
						     (second expr))
					     bindings)))
		   (dolist (expr (cddr expr))
		     (format out "~A;~%" (generate-expr-code expr (append new-bindings bindings)))))
		 (format out "}~%")))
	     (set
	      (unless (null required-width)
		(error "set has no return value~%"))
	      (cond ((subregisterp (second expr))
		     (let* ((subregister (lookup-subregister (second expr)))
			    (register (subregister-register subregister))
			    (register-number (register-number register))
			    (class-name (register-class-name (register-register-class register)))
			    (begin (subregister-begin subregister))
			    (end (subregister-end subregister))
			    (width (1+ (- end begin))))
		       (format nil "(regs_~A[~A] = (regs_~A[~A] & 0x~X) | (~A << ~A))"
			       class-name register-number
			       class-name register-number (bit-vector-to-integer (bit-not (bit-range begin end *word-bits*)))
			       (generate-expr-code (third expr) bindings width) begin)))
		    (t
		     (case (first (second expr))
		       (reg
			(multiple-value-bind (code width)
			    (resolve-register (second (second expr)) (third (second expr)))
			  (format nil "(~A = ~A)" code (generate-expr-code (third expr) bindings width))))
		       (numbered-subreg
			(let ((width (second (second expr))))
			  (multiple-value-bind (reg-code reg-width)
			      (resolve-register (fourth (second expr)) (fifth (second expr)))
			    (let ((index-code (generate-expr-code (third (second expr)) bindings)))
			      (format nil "(~A = (~A & ~~mask(~A * ~A, ~A * ~A + ~A - 1)) | (~A << (~A * ~A)))"
				      reg-code
				      reg-code width index-code width index-code width
				      (generate-expr-code (third expr) bindings width) width index-code)))))
		       (mem
			(if (null (third (second expr)))
			    (format nil "mem_set(~A, ~A)"
				    (generate-expr-code (second (second expr)) bindings *word-bits*)
				    (generate-expr-code (third expr) bindings *word-bits*))
			    (let ((width (third (second expr))))
			      (format nil "mem_set_~A(~A, ~A)"
				      width
				      (generate-expr-code (second (second expr)) bindings *word-bits*)
				      (generate-expr-code (third expr) bindings width)))))
		       (t
			(error "not an lvalue: ~A~%" (second expr)))))))
	     (width
	      (let ((width (second expr)))
		(when (and (not (null required-width))
			   (/= required-width width))
		  (error "width should be ~A in ~A~%" required-width expr))
		(generate-expr-code (third expr) bindings width)))
	     (promote
	      (let ((width (second expr)))
		(when (and (not (null required-width))
			   (/= required-width width))
		  (error "width should be ~A in ~A~%" required-width expr))
		(values (format nil "((~A)~A)" (type-for-width width) (generate-expr-code (third expr) bindings)) width)))
	     (jump-relative
	      (unless (null required-width)
		(error "jump-relative has no return value~%"))
	      (format nil "(next_pc = pc + ~A)"
		      (generate-expr-code (second expr) bindings *word-bits*)))
	     (jump-absolute
	      (unless (null required-width)
		(error "jump-absolute has no return value~%"))
	      (format nil "(next_pc = ~A)"
		      (generate-expr-code (second expr) bindings *word-bits*)))
	     (if
	      (letmv (((val1 width1) (generate-expr-code (third expr) bindings required-width))
		      ((val2 width2) (generate-expr-code (fourth expr) bindings required-width)))
		(when (not (eql width1 width2))
		  (error "widths do not match in ~A~%" expr))
		(when (and (not (null required-width))
			   (null width1))
		  (error "width should be ~A in ~A~%" required-width expr))
		(values (format nil "(~A ? ~A : ~A)"
				(generate-expr-code (second expr) bindings)
				val1 val2)
			width1)))
	     (=
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings))
		      ((op2 width2) (generate-expr-code (third expr) bindings)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A~%" expr))
		(format nil "(~A == ~A)" op1 op2)))
	     (<
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings))
		      ((op2 width2) (generate-expr-code (third expr) bindings)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A~%" expr))
		(format nil "(~A < ~A)" op1 op2)))
	     (<s
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings))
		      ((op2 width2) (generate-expr-code (third expr) bindings)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A~%" expr))
		(format nil "((sword_~A)~A < (sword_~A)~A)" width1 op1 width2 op2)))
	     (>
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings))
		      ((op2 width2) (generate-expr-code (third expr) bindings)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A~%" expr))
		(format nil "(~A > ~A)" op1 op2)))
	     (>s
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings))
		      ((op2 width2) (generate-expr-code (third expr) bindings)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A~%" expr))
		(format nil "((sword_~A)~A > (sword_~A)~A)" width1 op1 width2 op2)))
	     (bit-set-p
	      (format nil "(~A & (1 << ~A))" (generate-expr-code (second expr) bindings) (generate-expr-code (third expr) bindings)))
	     (reg
	      (multiple-value-bind (code width)
		  (resolve-register (second expr) (third expr))
		(match-width width)
		(values code width)))
	     (subreg
	      (let* ((begin (second expr))
		     (end (third expr))
		     (width (1+ (- end begin))))
		(match-width width)
		(multiple-value-bind (code reg-width)
		    (resolve-register (fourth expr) (fifth expr))
		  (values (format nil "((~A >> ~A) & 0x~X)" code begin (1- (expt 2 width))) width))))
	     (numbered-subreg
	      (let ((width (second expr)))
		(multiple-value-bind (code reg-width)
		    (resolve-register (fourth expr) (fifth expr))
		  (values (format nil "((~A >> (~A * ~A)) & 0x~X)"
				  code width (generate-expr-code (third expr) bindings)
				  (1- (expt 2 width)))
			  width))))
	     (mem
	      (when (null required-width)
		(error "no required width in ~A~%" expr))
	      (if (= required-width *word-bits*)
		  (values (format nil "mem_get(~A)" (generate-expr-code (second expr) bindings *word-bits*)) *word-bits*)
		  (values (format nil "mem_get_~A(~A)" required-width (generate-expr-code (second expr) bindings *word-bits*)) required-width)))
	     (zex
	      (when (null required-width)
		(error "no required width in ~A~%" expr))
	      (values (generate-expr-code (second expr) bindings) required-width))
	     (sex
	      (when (null required-width)
		(error "no required width in ~A~%" expr))
	      (multiple-value-bind (value width)
		  (generate-expr-code (second expr) bindings)
		(values (format nil "((~A & 0x~X) ? (~A | 0x~X) : ~A)"
				value (expt 2 (1- width))
				value (- (1- (expt 2 required-width)) (1- (expt 2 width)))
				value)
			required-width)))
	     (+
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings required-width))
		      ((op2 width2) (generate-expr-code (third expr) bindings required-width)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A" expr))
		(values (format nil "(~A + ~A)" op1 op2) width1)))
	     (-
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings required-width))
		      ((op2 width2) (generate-expr-code (third expr) bindings required-width)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A" expr))
		(values (format nil "(~A - ~A)" op1 op2) width1)))
	     (+carry
	      (when (and (not (null required-width))
			 (/= required-width 1))
		(error "width should be 1 in ~A~%" expr))
	      (let ((op1 (generate-expr-code (second expr) bindings *word-bits*))
		    (op2 (generate-expr-code (third expr) bindings *word-bits*)))
		(values (format nil "addcarry(~A, ~A)" op1 op2) 1)))
	     (-carry
	      (when (and (not (null required-width))
			 (/= required-width 1))
		(error "width should be 1 in ~A~%" expr))
	      (let ((op1 (generate-expr-code (second expr) bindings *word-bits*))
		    (op2 (generate-expr-code (third expr) bindings *word-bits*)))
		(values (format nil "subcarry(~A, ~A)" op1 op2) 1)))
	     (*
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings required-width))
		      ((op2 width2) (generate-expr-code (third expr) bindings required-width)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A" expr))
		(values (format nil "(~A * ~A)" op1 op2) width1)))
	     (*s
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings required-width))
		      ((op2 width2) (generate-expr-code (third expr) bindings required-width)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A" expr))
		(let ((type (type-for-width width1 t)))
		  (values (format nil "((~A)((~A)~A * (~A)~A))" (type-for-width width1) type op1 type op2) width1))))
	     (/
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings required-width))
		      ((op2 width2) (generate-expr-code (third expr) bindings required-width)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A" expr))
		(values (format nil "(~A / ~A)" op1 op2) width1)))
	     (/s
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings required-width))
		      ((op2 width2) (generate-expr-code (third expr) bindings required-width)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A" expr))
		(let ((type (type-for-width width1 t)))
		  (values (format nil "((~A)((~A)~A / (~A)~A))" (type-for-width width1) type op1 type op2) width1))))
	     (neg
	      (when (and (not (null required-width))
			 (/= required-width *word-bits*))
		(error "width should be ~A in ~A~%" *word-bits* expr))
	      (values (format nil "(-~A)" (generate-expr-code (second expr) bindings *word-bits*)) *word-bits*))
	     (bitneg
	      (when (and (not (null required-width))
			 (/= required-width *word-bits*))
		(error "width should be ~A in ~A~%" *word-bits* expr))
	      (values (format nil "(~~~A)" (generate-expr-code (second expr) bindings *word-bits*)) *word-bits*))
	     (logor
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings required-width))
		      ((op2 width2) (generate-expr-code (third expr) bindings required-width)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A" expr))
		(values (format nil "(~A | ~A)" op1 op2) width1)))
	     (logand
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings required-width))
		      ((op2 width2) (generate-expr-code (third expr) bindings required-width)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A" expr))
		(values (format nil "(~A & ~A)" op1 op2) width1)))
	     (logxor
	      (letmv (((op1 width1) (generate-expr-code (second expr) bindings required-width))
		      ((op2 width2) (generate-expr-code (third expr) bindings required-width)))
		(when (/= width1 width2)
		  (error "widths do not match in ~A" expr))
		(values (format nil "(~A ^ ~A)" op1 op2) width1)))
	     (shiftl
	      (multiple-value-bind (op1 width)
		  (generate-expr-code (second expr) bindings required-width)
		(values (format nil "(~A << ~A)" op1 (generate-expr-code (third expr) bindings)) width)))
	     (shiftr
	      (multiple-value-bind (op1 width)
		  (generate-expr-code (second expr) bindings required-width)
		(values (format nil "(~A >> ~A)" op1 (generate-expr-code (third expr) bindings)) width)))
	     (ashiftr
	      (when (and (not (null required-width))
			 (/= required-width *word-bits*))
		(error "width should be ~A in ~A~%" *word-bits* expr))
	      (let ((op1 (generate-expr-code (second expr) bindings *word-bits*))
		    (op2 (generate-expr-code (third expr) bindings)))
		(format nil "((~A >> ~A) | ((~A & 0x80000000) ? ~~((1 << (32 - ~A)) - 1) : 0))"
			op1 op2 op1 op2)))
	     (rotl
	      (multiple-value-bind (op1 width)
		  (generate-expr-code (second expr) bindings required-width)
		(values (format nil "rotl(~A, ~A)" op1 (generate-expr-code (third expr) bindings)) width)))
	     (mask
	      (values (format nil "mask(~A, ~A)" (generate-expr-code (second expr) bindings) (generate-expr-code (third expr) bindings))
		      (if (null required-width)
			  *word-bits*
			  required-width)))
	     (maskmask
	      (let ((bit-width (second expr)))
		(multiple-value-bind (mask mask-width)
		    (generate-expr-code (third expr) bindings)
		  (when (and (not (null required-width))
			     (/= required-width (* bit-width mask-width)))
		    (error "widths do not match in ~A~%" expr))
		  (values (format nil "maskmask(~A, ~A, ~A)" bit-width mask-width mask) (* bit-width mask-width)))))
	     (leading-zeros
	      (values (format nil "leading_zeros(~A)" (generate-expr-code (second expr) bindings *word-bits*))
		      (if (null required-width)
			  *word-bits*
			  required-width)))
	     (nop
	      (format nil "0 /* nop */"))
	     (syscall
	      (format nil "handle_system_call()"))
	     (t
	      (error "unknown pattern ~A~%" (first expr)))))
	  (t
	   (error "illegal expr ~A~%" expr)))))

(defun generate-code-for-insn (insn)
  (dolist (expr (insn-effect insn))
    (format t "~A;~%" (generate-expr-code expr))))

(defun disassemble-insn (insn)
  (labels ((find-directives (str &optional (start 0))
	     (let ((pos (position #\% str :start start)))
	       (if (null pos)
		   nil
		   (let* ((c (elt str (1+ pos)))
			  (directive (case c
				       ((#\x #\u) 'unsigned)
				       (#\d 'signed)
				       (#\s 'string)
				       (t (error "unknown directive %~A~%" c)))))
		     (cons directive (find-directives str (+ pos 2))))))))
    (let* ((format-string (car (insn-asm insn)))
	   (args (cdr (insn-asm insn)))
	   (directives (find-directives format-string))
	   (c-exprs (mapcar #'(lambda (directive expr)
				(case directive
				  (unsigned (generate-expr-code `(width ,*word-bits* (zex ,expr))))
				  (signed (generate-expr-code `(width ,*word-bits* (sex ,expr))))
				  (string (generate-expr-code expr))
				  (t (error "unknown directive ~A~%" directive))))
			    directives args)))
      (format t "printf(\"~A\"~{, ~A~});~%" format-string c-exprs))))

(defun generate-insn-recognizer (tree action)
  (if (insn-p tree)
      (progn
	(format t "/* ~A */~%" (insn-name tree) (insn-name tree))
	(format t "assert((insn & 0x~X) == 0x~X);~%" (bit-vector-to-integer (insn-known-bits tree)) (bit-vector-to-integer (insn-known-bit-values tree)))
	(funcall action tree))
      (let ((begin (caar tree))
	    (end (cdar tree))
	    (subtrees (cdr tree)))
	(format t "switch (~A) {~%" (code-for-field "insn" begin end))
	(dolist (subtree subtrees)
	  (format t "case ~A:~%" (car subtree))
	  (generate-insn-recognizer (cdr subtree) action)
	  (format t "break;~%"))
	(format t "default:~%assert(0);~%}~%"))))

(defun generate ()
  (with-open-file (out "interpreter.c" :direction :output :if-exists :supersede)
    (let ((*standard-output* out)
	  (decision-tree (build-decision-tree *insns*)))
      (dolist (class *register-classes*)
	(let ((type (type-for-width (register-class-width class))))
	  (format t "~A regs_~A[~A];~%" type (register-class-name class) (length (remove-if-not #'(lambda (reg)
												    (eq (register-register-class reg) class))
												*registers*)))))
      (format t "word pc;~%~%")
      (format t "void interpret_insn (void) {~%word insn = mem_get(pc);~%word next_pc = pc + ~A;~%" (/ *insn-bits* 8))
      (generate-insn-recognizer decision-tree #'generate-code-for-insn)
      (format t "pc = next_pc;~%++insn_count;~%}~%")
      (format t "void disassemble_insn (word insn, word addr) {~%")
      (generate-insn-recognizer decision-tree #'disassemble-insn)
      (format t "}~%")
      (format t "void dump_registers (void) {~%")
      (dolist (reg (reverse *registers*))
	(format t "printf(\"~A: 0x%x\\n\", regs_~A[~A]);~%"
		(register-name reg)
		(register-class-name (register-register-class reg))
		(register-number reg)))
      (format t "printf(\"PC: 0x%x\\n\", pc);~%}~%"))))
    
(define-register-class 'spr 32
  '(lr cr xer ctr fpscr))

(define-register-class 'gpr 32
  '(gpr0 gpr1 gpr2 gpr3 gpr4 gpr5 gpr6 gpr7
    gpr8 gpr9 gpr10 gpr11 gpr12 gpr13 gpr14 gpr15
    gpr16 gpr17 gpr18 gpr19 gpr20 gpr21 gpr22 gpr23
    gpr24 gpr25 gpr26 gpr27 gpr28 gpr29 gpr30 gpr31))

(define-register-class 'fpr 64
  '(fpr0 fpr1 fpr2 fpr3 fpr4 fpr5 fpr6 fpr7
    fpr8 fpr9 fpr10 fpr11 fpr12 fpr13 fpr14 fpr15
    fpr16 fpr17 fpr18 fpr19 fpr20 fpr21 fpr22 fpr23
    fpr24 fpr25 fpr26 fpr27 fpr28 fpr29 fpr30 fpr31))

(define-subregisters
    '((xer-ca xer 29 29)
      (xer-ov xer 30 30)
      (xer-so xer 31 31)
      (cr-0 cr 28 31)
      (cr-1 cr 24 27)
      (cr-2 cr 20 23)
      (cr-3 cr 16 19)
      (cr-7 cr 0 3)))

(define-fields
    '((OPCD 26 31)
      (LI 2 25)
      (AA 1 1)
      (LK 0 0)
      (BD 2 15)
      (BO 21 25)
      (rS 21 25)
      (frS 21 25)
      (rD 21 25)
      (frD 21 25)
      (crbD 21 25)
      (crfD 23 25)
      (crDz 21 22)
      (L 21 21)
      (Lz 22 22)
      (TO 21 25)
      (BI 16 20)
      (BIcr 18 20)
      (BIcc 16 17)
      (rA 16 20)
      (frA 16 20)
      (crbA 16 20)
      (crfS 18 20)
      (crSz 16 17)
      (rB 11 15)
      (frB 11 15)
      (crbB 11 15)
      (NB 11 15)
      (SH 11 15)
      (C 6 10)
      (frC 6 10)
      (MB 6 10)
      (ME 1 5)
      (d 0 15)
      (SIMM 0 15)
      (UIMM 0 15)
      (ds 2 15)
      (Xo0 0 1)
      (Xo1 1 10)
      (Xo2 2 10)
      (Xo4 2 4)
      (Xo5 1 5)
      (Xo9 1 9)
      (OE 10 10)
      (SR 16 19)
      (SRz 20 20)
      (Rc 0 0)
      (IMM 12 15)
      (IMMz 11 11)
      (spr 11 20)
      (tbrH 11 15)
      (tbrL 16 20)
      (FM 17 24)
      (FMz1 16 16)
      (FMz2 25 25)
      (CRM 12 19)
      (CRz1 11 11)
      (CRz2 20 20)
      (mbe 5 5)))

(define-insn add
    ((opcd 31)
     (xo9 266)
     (oe 0)
     (rc 0))
  ((set (reg rd gpr) (+ (reg ra gpr) (reg rb gpr))))
  ("add r%u,r%u,r%u" rd ra rb))

(define-insn adde
    ((opcd 31)
     (xo9 138)
     (oe 0)
     (rc 0))
  ((let ((old-ca (width 32 (zex xer-ca))))
     (set (reg rd gpr) (+ (+ (reg ra gpr) (reg rb gpr)) (zex xer-ca)))
     (set xer-ca (logor (+carry (reg ra gpr) (reg rb gpr))
			(+carry (+ (reg ra gpr) (reg rb gpr)) (zex old-ca))))))
  ("adde r%u,r%u,r%u" rd ra rb))

(define-insn addi
    ((opcd 14))
  ((set (reg rd gpr) (if (= ra (width 5 0))
			 (sex simm)
			 (+ (reg ra gpr) (sex simm)))))
  ("addi r%u,r%u,%d" rd ra simm))

(define-insn addic
    ((opcd 12))
  ((set xer-ca (+carry (reg ra gpr) (sex simm)))
   (set (reg rd gpr) (+ (reg ra gpr) (sex simm))))
  ("addic r%u,r%u,%d" rd ra simm))

(define-insn addic.
    ((opcd 13))
  ((set xer-ca (+carry (reg ra gpr) (sex simm)))
   (set (reg rd gpr) (+ (reg ra gpr) (sex simm)))
   (set cr-0 (logor (if (<s (reg rd gpr) 0)
			8
			(if (>s (reg rd gpr) 0)
			    4
			    2))
		    (zex xer-so))))
  ("addic. r%u,r%u,%d" rd ra simm))

(define-insn addis
    ((opcd 15))
  ((set (reg rd gpr) (if (= ra (width 5 0))
			 (shiftl (zex simm) 16)
			 (+ (reg ra gpr) (shiftl (zex simm) 16)))))
  ("addis r%u,r%u,%d" rd ra simm))

(define-insn addze
    ((opcd 31)
     (rb 0)
     (oe 0)
     (xo9 202)
     (rc 0))
  ((let ((old-ca (width 32 (zex xer-ca))))
     (set (reg rd gpr) (+ (reg ra gpr) (zex xer-ca)))
     (set xer-ca (+carry (reg ra gpr) (zex old-ca)))))
  ("addze r%u,r%u" rd ra))

(define-insn and
    ((opcd 31)
     (xo1 28))
  ((set (reg ra gpr) (logand (reg rs gpr) (reg rb gpr)))
   (if (= rc (width 1 1))
       (set cr-0 (logor (if (<s (reg ra gpr) 0)
			    8
			    (if (>s (reg ra gpr) 0)
				4
				2))
			(zex xer-so)))
       (nop)))
  ("and%s r%u,r%u,r%u" (if (= rc (width 1 1)) "." "") ra rs rb))

(define-insn andc
    ((opcd 31)
     (xo1 60))
  ((set (reg ra gpr) (logand (reg rs gpr) (bitneg (reg rb gpr))))
   (if (= rc (width 1 1))
       (set cr-0 (logor (if (<s (reg ra gpr) 0)
			    8
			    (if (>s (reg ra gpr) 0)
				4
				2))
			(zex xer-so)))
       (nop)))
  ("andc%s r%u,r%u,r%u" (if (= rc (width 1 1)) "." "") ra rs rb))

(define-insn andi.
    ((opcd 28))
  ((set (reg ra gpr) (logand (reg rs gpr) (zex uimm)))
   (set cr-0 (logor (if (<s (reg ra gpr) 0)
			8
			(if (>s (reg ra gpr) 0)
			    4
			    2))
		    (zex xer-so))))
  ("andi. r%u,r%u,%u" ra rs uimm))

(define-insn b
    ((opcd 18)
     (aa 0)
     (lk 0))
  ((jump-relative (shiftl (sex li) 2)))
  ("b 0x%x" (width 32 (+ addr (shiftl (sex li) 2)))))

(define-insn bctr
    ((opcd 19)
     (bo 20)
     (rb 0)
     (xo1 528)
     (lk 0))
  ((jump-absolute (logand (reg ctr) #xfffffffc)))
  ("bctr"))

(define-insn bdnz
    ((opcd 16)
     (bo 16)
     (aa 0)
     (lk 0))
  ((set (reg ctr) (- (reg ctr) 1))
   (if (= (reg ctr) 0)
       (nop)
       (jump-relative (shiftl (sex bd) 2))))
  ("bdnz 0x%x" (width 32 (+ addr (shiftl (sex li) 2)))))

(define-insn beq
    ((opcd 16)
     (bo 12)
     (aa 0)
     (lk 0))
  ((if (bit-set-p (reg cr) (width 5 (- 31 bi)))
       (jump-relative (shiftl (sex bd) 2))
       (nop)))
  ("bs %u,0x%x" bi (width 32 (+ addr (shiftl (sex li) 2)))))

(define-insn beq+
    ((opcd 16)
     (bo 13)
     (aa 0)
     (lk 0))
  ((if (bit-set-p (reg cr) (width 5 (- 31 bi)))
       (jump-relative (shiftl (sex bd) 2))
       (nop)))
  ("bs+ %u,0x%x" bi (width 32 (+ addr (shiftl (sex li) 2)))))

(define-insn beqlr
    ((opcd 19)
     (bo 12)
     (rb 0)
     (xo1 16)
     (lk 0))
  ((if (bit-set-p (reg cr) (width 5 (- 31 bi)))
       (jump-absolute (logand (reg lr) #xfffffffc))
       (nop)))
  ("bslr %u" bi))

(define-insn bl
    ((opcd 18)
     (aa 0)
     (lk 1))
  ((set (reg lr) (+ pc 4))
   (jump-relative (shiftl (sex li) 2)))
  ("bl 0x%x" (width 32 (+ addr (shiftl (sex li) 2)))))

(define-insn blr
    ((opcd 19)
     (bo 20)
     (rb 0)
     (xo1 16)
     (lk 0))
  ((jump-absolute (logand (reg lr) #xfffffffc)))
  ("blr"))

(define-insn blrl
    ((opcd 19)
     (bo 20)
     (rb 0)
     (xo1 16)
     (lk 1))
  ((jump-absolute (logand (reg lr) #xfffffffc))
   (set (reg lr) (+ pc 4)))
  ("blrl"))

(define-insn bne
    ((opcd 16)
     (bo 4)
     (aa 0)
     (lk 0))
  ((if (bit-set-p (reg cr) (width 5 (- 31 bi)))
       (nop)
       (jump-relative (shiftl (sex bd) 2))))
  ("bns %u,0x%x" bi (width 32 (+ addr (shiftl (sex li) 2)))))

(define-insn bne-
    ((opcd 16)
     (bo 5)
     (aa 0)
     (lk 0))
  ((if (bit-set-p (reg cr) (width 5 (- 31 bi)))
       (nop)
       (jump-relative (shiftl (sex bd) 2))))
  ("bns- %u,0x%x" bi (width 32 (+ addr (shiftl (sex li) 2)))))

(define-insn bnelr+
    ((opcd 19)
     (bo 5)
     (rb 0)
     (xo1 16)
     (lk 0))
  ((if (bit-set-p (reg cr) (width 5 (- 31 bi)))
       (nop)
       (jump-absolute (logand (reg lr) #xfffffffc))))
  ("bnslr+ %u" bi))

(define-insn bnslr
    ((opcd 19)
     (bo 4)
     (rb 0)
     (xo1 16)
     (lk 0))
  ((if (bit-set-p (reg cr) (width 5 (- 31 bi)))
       (nop)
       (jump-absolute (logand (reg lr) #xfffffffc))))
  ("bnslr %u" bi))

(define-insn cmplw
    ((opcd 31)
     (lz 0)
     (l 0)
     (xo1 32)
     (rc 0))
  ((set (numbered-subreg 4 (- (width 3 7) crfd) cr)
	(logor (if (< (reg ra gpr) (reg rb gpr))
		   8
		   (if (> (reg ra gpr) (reg rb gpr))
		       4
		       2))
	       (zex xer-so))))
  ("cmplw cr%u,r%u,r%u" crfd ra rb))

(define-insn cmplwi
    ((opcd 10)
     (l 0)
     (lz 0))
  ((set (numbered-subreg 4 (- (width 3 7) crfd) cr)
	(logor (if (< (reg ra gpr) (width 32 (zex simm)))
		   8
		   (if (> (reg ra gpr) (width 32 (zex simm)))
		       4
		       2))
	       (zex xer-so))))
  ("cmplwi cr%u,r%u,%u" crfd ra uimm))

(define-insn cmpw
    ((opcd 31)
     (lz 0)
     (l 0)
     (xo1 0)
     (rc 0))
  ((set (numbered-subreg 4 (- (width 3 7) crfd) cr)
	(logor (if (<s (reg ra gpr) (reg rb gpr))
		   8
		   (if (>s (reg ra gpr) (reg rb gpr))
		       4
		       2))
	       (zex xer-so))))
  ("cmpw cr%u,r%u,r%u" crfd ra rb))

(define-insn cmpwi
    ((opcd 11)
     (l 0)
     (lz 0))
  ((set (numbered-subreg 4 (- (width 3 7) crfd) cr)
	(logor (if (<s (reg ra gpr) (width 32 (sex simm)))
		   8
		   (if (>s (reg ra gpr) (width 32 (sex simm)))
		       4
		       2))
	       (zex xer-so))))
  ("cmpwi cr%u,r%u,%d" crfd ra simm))

(define-insn cntlzw
    ((opcd 31)
     (rb 0)
     (xo1 26)
     (rc 0))
  ((set (reg ra gpr) (leading-zeros (reg rs gpr))))
  ("cntlzw r%u,r%u" ra rs))

(define-insn crxor
    ((opcd 19)
     (xo1 193)
     (rc 0))
  ((set (numbered-subreg 1 (- (width 5 31) crbd) cr)
	(logxor (numbered-subreg 1 (- (width 5 31) crba) cr)
		(numbered-subreg 1 (- (width 5 31) crbb) cr))))
  ("crxor crb%u,crb%u,crb%u" crbd crba crbb))

(define-insn cror
    ((opcd 19)
     (xo1 449)
     (rc 0))
  ((set (numbered-subreg 1 (- (width 5 31) crbd) cr)
	(logor (numbered-subreg 1 (- (width 5 31) crba) cr)
	       (numbered-subreg 1 (- (width 5 31) crbb) cr))))
  ("cror crb%u,crb%u,crb%u" crbd crba crbb))

(define-insn dcbz
    ((opcd 31)
     (xo1 1014)
     (rc 0)
     (rd 0))
  ((set (mem (+ (if (= ra (width 5 0)) 0 (reg ra gpr)) (reg rb gpr)) 8) 0))
  ("dcbz r%u,r%u" ra rb))

(define-insn divw
    ((opcd 31)
     (xo9 491)
     (rc 0)
     (oe 0))
  ((set (reg rd gpr) (/s (reg ra gpr) (reg rb gpr))))
  ("divw r%u,r%u,r%u" rd ra rb))

(define-insn divwu
    ((opcd 31)
     (xo9 459)
     (rc 0)
     (oe 0))
  ((set (reg rd gpr) (/ (reg ra gpr) (reg rb gpr))))
  ("divwu r%u,r%u,r%u" rd ra rb))

(define-insn extsb
    ((opcd 31)
     (xo1 954)
     (rb 0)
     (rc 0))
  ((set (reg ra gpr) (width 32 (sex (subreg 0 7 rs gpr)))))
  ("extsb r%u,r%u" ra rs))

(define-insn lbz
    ((opcd 34))
  ((set (reg rd gpr) (zex (width 8 (mem (if (= ra (width 5 0))
					    (sex d)
					    (+ (reg ra gpr) (sex d))))))))
  ("lbz r%u,%d(r%u)" rd d ra))

(define-insn lbzu
    ((opcd 35))
  ((set (reg rd gpr) (zex (width 8 (mem (+ (reg ra gpr) (sex d))))))
   (set (reg ra gpr) (+ (reg ra gpr) (sex d))))
  ("lbzu r%u,%d(r%u)" rd d ra))

(define-insn lbzx
    ((opcd 31)
     (xo1 87)
     (rc 0))
  ((set (reg rd gpr) (zex (width 8 (mem (if (= ra (width 5 0))
					    (reg rb gpr)
					    (+ (reg ra gpr) (reg rb gpr))))))))
  ("lbzx r%u,r%u,r%u" rd ra rb))

(define-insn lfd
    ((opcd 50))
  ((set (reg rd fpr) (if (= ra (width 5 0))
			 (mem (sex d))
			 (mem (+ (reg ra gpr) (sex d))))))
  ("lbz fr%u,%d(r%u)" rd d ra))

(define-insn lhz
    ((opcd 40))
  ((set (reg rd gpr) (zex (width 16 (mem (if (= ra (width 5 0))
					     (sex d)
					     (+ (reg ra gpr) (sex d))))))))
  ("lhz r%u,%d(r%u)" rd d ra))

(define-insn lhzx
    ((opcd 31)
     (xo1 279)
     (rc 0))
  ((set (reg rd gpr) (zex (width 16 (mem (if (= ra (width 5 0))
					     (reg rb gpr)
					     (+ (reg ra gpr) (reg rb gpr))))))))
  ("lhzx r%u,r%u,r%u" rd ra rb))

(define-insn lwz
    ((opcd 32))
  ((set (reg rd gpr) (if (= ra (width 5 0))
			 (mem (sex d))
			 (mem (+ (reg ra gpr) (sex d))))))
  ("lwz r%u,%d(r%u)" rd d ra))

(define-insn lwzu
    ((opcd 33))
  ((set (reg rd gpr) (mem (+ (reg ra gpr) (sex d))))
   (set (reg ra gpr) (+ (reg ra gpr) (sex d))))
  ("lwzu r%u,%d(r%u)" rd d ra))

(define-insn lwzx
    ((opcd 31)
     (xo1 23)
     (rc 0))
  ((set (reg rd gpr) (mem (if (= ra (width 5 0))
			      (reg rb gpr)
			      (+ (reg ra gpr) (reg rb gpr))))))
  ("lwzx r%u,r%u,r%u" rd ra rb))

(define-insn mcrf
    ((opcd 19)
     (crdz 0)
     (crsz 0)
     (rb 0)
     (xo1 0)
     (rc 0))
  ((set (numbered-subreg 4 (- (width 3 7) crfd) cr)
	(numbered-subreg 4 (- (width 3 7) crfs) cr)))
  ("mcrf cr%u,cr%u" crfd crfs))

(define-insn mfcr
    ((opcd 31)
     (xo1 19)
     (rc 0)
     (ra 0)
     (rb 0))
  ((set (reg rd gpr) (reg cr)))
  ("mfcr r%u" rd))

(define-insn mflr
    ((opcd 31)
     (xo1 339)
     (rc 0)
     (spr 256))
  ((set (reg rd gpr) (reg lr)))
  ("mflr r%u" rd))

(define-insn mffs
    ((opcd 63)
     (ra 0)
     (rb 0)
     (xo1 583)
     (rc 0))
  ((set (reg rd fpr) (zex (reg fpscr))))
  ("mffs fr%u" rd))

(define-insn mtcrf
    ((opcd 31)
     (crz2 0)
     (crz1 0)
     (xo1 144)
     (rc 0))
  ((set (reg cr) (logor (logand (reg cr) (bitneg (maskmask 4 crm)))
			(logand (reg rs gpr) (maskmask 4 crm)))))
  ("mtcrf %u,r%u" crm rs))

(define-insn mtctr
    ((opcd 31)
     (xo1 467)
     (rc 0)
     (spr 288))
  ((set (reg ctr) (reg rs gpr)))
  ("mtctr r%u" rs))

(define-insn mtfsf
    ((opcd 63)
     (fmz2 0)
     (fm 255)
     (fmz1 0)
     (xo1 711)
     (rc 0))
  ((set (reg fpscr) (subreg 0 31 rb fpr)))
  ("mtfsf 0xff,fr%u" rb))

(define-insn mtlr
    ((opcd 31)
     (xo1 467)
     (rc 0)
     (spr 256))
  ((set (reg lr) (reg rs gpr)))
  ("mtlr r%u" rs))

(define-insn mulhw
    ((opcd 31)
     (oe 0)
     (xo9 75)
     (rc 0))
  ((set (reg rd gpr) (promote 32 (shiftr (*s (width 64 (sex (reg ra gpr)))
					     (width 64 (sex (reg rb gpr))))
					 32))))
  ("mulhw r%u,r%u,r%u" rd ra rb))

(define-insn mulhwu
    ((opcd 31)
     (oe 0)
     (xo9 11)
     (rc 0))
  ((set (reg rd gpr) (promote 32 (shiftr (* (promote 64 (reg ra gpr))
					    (promote 64 (reg rb gpr)))
					 32))))
  ("mulhwu r%u,r%u,r%u" rd ra rb))

(define-insn mulli
    ((opcd 7))
  ((set (reg rd gpr) (* (reg ra gpr) (sex simm))))
  ("mulli r%u,r%u,%d" rd ra simm))

(define-insn mullw
    ((opcd 31)
     (oe 0)
     (xo9 235)
     (rc 0))
  ((set (reg rd gpr) (* (reg ra gpr) (reg rb gpr))))
  ("mullw r%u,r%u,r%u" rd ra rb))

(define-insn nand
    ((opcd 31)
     (xo1 476))
  ((set (reg ra gpr) (bitneg (logand (reg rs gpr) (reg rb gpr))))
   (if (= rc (width 1 1))
       (set cr-0 (logor (if (<s (reg ra gpr) 0)
			    8
			    (if (>s (reg ra gpr) 0)
				4
				2))
			(zex xer-so)))
       (nop)))
  ("nand%s r%u,r%u,r%u" (if (= rc (width 1 1)) "." "") ra rs rb))

(define-insn neg
    ((opcd 31)
     (rb 0)
     (oe 0)
     (xo9 104)
     (rc 0))
  ((set (reg rd gpr) (neg (reg ra gpr))))
  ("neg r%u,r%u" rd ra))

(define-insn nor
    ((opcd 31)
     (xo1 124))
  ((set (reg ra gpr) (bitneg (logor (reg rs gpr) (reg rb gpr))))
   (if (= rc (width 1 1))
       (set cr-0 (logor (if (<s (reg ra gpr) 0)
			    8
			    (if (>s (reg ra gpr) 0)
				4
				2))
			(zex xer-so)))
       (nop)))
  ("nor%s r%u,r%u,r%u" (if (= rc (width 1 1)) "." "") ra rs rb))

(define-insn or
    ((opcd 31)
     (xo1 444))
  ((set (reg ra gpr) (logor (reg rs gpr) (reg rb gpr)))
   (if (= rc (width 1 1))
       (set cr-0 (logor (if (<s (reg ra gpr) 0)
			    8
			    (if (>s (reg ra gpr) 0)
				4
				2))
			(zex xer-so)))
       (nop)))
  ("or%s r%u,r%u,r%u" (if (= rc (width 1 1)) "." "") ra rs rb))

(define-insn orc
    ((opcd 31)
     (xo1 412))
  ((set (reg ra gpr) (logor (reg rs gpr) (bitneg (reg rb gpr))))
   (if (= rc (width 1 1))
       (set cr-0 (logor (if (<s (reg ra gpr) 0)
			    8
			    (if (>s (reg ra gpr) 0)
				4
				2))
			(zex xer-so)))
       (nop)))
  ("orc%s r%u,r%u,r%u" (if (= rc (width 1 1)) "." "") ra rs rb))

(define-insn ori
    ((opcd 24))
  ((set (reg ra gpr) (logor (reg rs gpr) (zex uimm))))
  ("ori r%u,r%u,%u" ra rs uimm))

(define-insn oris
    ((opcd 25))
  ((set (reg ra gpr) (logor (reg rs gpr) (shiftl (zex uimm) 16))))
  ("oris r%u,r%u,%u" ra rs uimm))

(define-insn rlwimi			;rotate left word immediate then mask insert
    ((opcd 20)
     (rc 0))
  ((set (reg ra gpr) (logor (logand (rotl (reg rs gpr) sh) (mask (width 5 (- 31 me)) (width 5 (- 31 mb))))
			    (logand (reg ra gpr) (bitneg (mask (width 5 (- 31 me)) (width 5 (- 31 mb))))))))
  ("rlwimi r%u,r%u,%u,%u,%u" ra rs sh mb me))
  

(define-insn rlwinm			;rotate left word immediate then AND with mask
    ((opcd 21))
  ((set (reg ra gpr) (logand (rotl (reg rs gpr) sh) (mask (width 5 (- 31 me)) (width 5 (- 31 mb)))))
   (if (= rc (width 1 1))
       (set cr-0 (logor (if (<s (reg ra gpr) 0)
			    8
			    (if (>s (reg ra gpr) 0)
				4
				2))
			(zex xer-so)))
       (nop)))
  ("rlwinm%s r%u,r%u,%u,%u,%u" (if (= rc (width 1 1)) "." "") ra rs sh mb me))

(define-insn sc
    ((opcd 17)
     (rs 0)
     (ra 0)
     (bd 0)
     (aa 1)
     (lk 0))
  ((syscall))
  ("sc"))

(define-insn slw
    ((opcd 31)
     (xo1 24)
     (rc 0))
  ((set (reg ra gpr) (if (bit-set-p (reg rb gpr) 5)
			 0
			 (shiftl (reg rs gpr) (subreg 0 4 rb gpr)))))
  ("slw r%u,r%u,r%u" ra rs rb))

(define-insn srawi
    ((opcd 31)
     (xo1 824)
     (rc 0))
  ((set xer-ca (if (<s (reg rs gpr) 0)
		   (if (= (bitneg (logand (- (shiftl 1 sh) 1) (reg rs gpr))) 0)
		       0
		       1)
		   0))
   (set (reg ra gpr) (ashiftr (reg rs gpr) sh)))
  ("srawi r%u,r%u,%u" ra rs sh))

(define-insn srw
    ((opcd 31)
     (xo1 536)
     (rc 0))
  ((set (reg ra gpr) (shiftr (reg rs gpr) (logand (reg rb gpr) #x1f))))
  ("slw r%u,r%u,r%u" ra rs rb))

(define-insn stb
    ((opcd 38))
  ((set (mem (if (= ra (width 5 0))
		 (sex d)
		 (+ (reg ra gpr) (sex d))) 8)
	(subreg 0 7 rs gpr)))
  ("stb r%u,%d(r%u)" rs d ra))

(define-insn stbu
    ((opcd 39))
  ((set (mem (+ (reg ra gpr) (sex d)) 8) (subreg 0 7 rs gpr))
   (set (reg ra gpr) (+ (reg ra gpr) (sex d))))
  ("stbu r%u,%d(r%u)" rs d ra))

(define-insn stbx
    ((opcd 31)
     (xo1 215)
     (rc 0))
  ((set (mem (+ (if (= ra (width 5 0)) 0 (reg ra gpr)) (reg rb gpr)) 8) (subreg 0 7 rs gpr)))
  ("stbx r%u,r%u,r%u" rs ra rb))

(define-insn stfd
    ((opcd 54))
  ((set (mem (+ (if (= ra (width 5 0)) 0 (reg ra gpr)) (sex d)) 64) (reg rs fpr)))
  ("stfd fr%u,%d(r%u)" rs d ra))

(define-insn sth
    ((opcd 44))
  ((set (mem (if (= ra (width 5 0))
		 (sex d)
		 (+ (reg ra gpr) (sex d))) 16)
	(subreg 0 15 rs gpr)))
  ("sth r%u,%d(r%u)" rs d ra))

(define-insn stw
    ((opcd 36))
  ((set (mem (+ (if (= ra (width 5 0)) 0 (reg ra gpr)) (sex d))) (reg rs gpr)))
  ("stw r%u,%d(r%u)" rs d ra))

(define-insn stwu
    ((opcd 37))
  ((set (mem (+ (reg ra gpr) (sex d))) (reg rs gpr))
   (set (reg ra gpr) (+ (reg ra gpr) (sex d))))
  ("stwu r%u,%d(r%u)" rs d ra))

(define-insn stwx
    ((opcd 31)
     (xo1 151)
     (rc 0))
  ((set (mem (+ (if (= ra (width 5 0)) 0 (reg ra gpr)) (reg rb gpr))) (reg rs gpr)))
  ("stwx r%u,r%u,r%u" rs ra rb))

(define-insn stwux
    ((opcd 31)
     (xo1 183)
     (rc 0))
  ((set (mem (+ (reg ra gpr) (reg rb gpr))) (reg rs gpr))
   (set (reg ra gpr) (+ (reg ra gpr) (reg rb gpr))))
  ("stwux r%u,r%u,r%u" rs ra rb))

(define-insn subf
    ((opcd 31)
     (xo9 40)
     (oe 0))
  ((set (reg rd gpr) (- (reg rb gpr) (reg ra gpr)))
   (if (= rc (width 1 1))
       (set cr-0 (logor (if (<s (reg rd gpr) 0)
			    8
			    (if (>s (reg rd gpr) 0)
				4
				2))
			(zex xer-so)))
       (nop)))
  ("subf%s r%u,r%u,r%u" (if (= rc (width 1 1)) "." "") rd ra rb))

(define-insn subfc
    ((opcd 31)
     (xo9 8)
     (oe 0))
  ((set xer-ca (logor (+carry (bitneg (reg ra gpr)) (reg rb gpr))
		      (+carry (+ (bitneg (reg ra gpr)) (reg rb gpr)) 1)))
   (set (reg rd gpr) (- (reg rb gpr) (reg ra gpr)))
   (if (= rc (width 1 1))
       (set cr-0 (logor (if (<s (reg rd gpr) 0)
			    8
			    (if (>s (reg rd gpr) 0)
				4
				2))
			(zex xer-so)))
       (nop)))
  ("subfc%s r%u,r%u,r%u" (if (= rc (width 1 1)) "." "") rd ra rb))

(define-insn subfe
    ((opcd 31)
     (xo9 136)
     (oe 0)
     (rc 0))
  ((let ((old-ca (width 32 (zex xer-ca))))
     (set (reg rd gpr) (- (reg rb gpr) (+ (reg ra gpr) (- 1 (zex xer-ca)))))
     (set xer-ca (logor (+carry (bitneg (reg ra gpr)) (reg rb gpr))
			(+carry (+ (bitneg (reg ra gpr)) (reg rb gpr)) old-ca)))))
  ("subfe r%u,r%u,r%u" rd ra rb))

(define-insn subfic
    ((opcd 8))
  ((set xer-ca (logor (+carry (bitneg (reg ra gpr)) (sex simm))
		      (+carry (+ (bitneg (reg ra gpr)) (sex simm)) 1)))
   (set (reg rd gpr) (- (sex simm) (reg ra gpr))))
  ("subfic r%u,r%u,%d" rd ra simm))

(define-insn xor
    ((opcd 31)
     (xo1 316)
     (rc 0))
  ((set (reg ra gpr) (logxor (reg rs gpr) (reg rb gpr))))
  ("xor r%u,r%u,r%u" ra rs rb))

(define-insn xori
    ((opcd 26))
  ((set (reg ra gpr) (logxor (reg rs gpr) (zex uimm))))
  ("xori r%u,r%u,%u" ra rs uimm))
