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

(defun my-macroexpand (sexp macros)
  (labels ((expand (sexp)
	     (if (consp sexp)
		 (if (symbolp (car sexp))
		     (let ((macro (cdr (assoc (car sexp) macros))))
		       (if macro
			   (expand (apply macro (cdr sexp)))
			   (cons (car sexp) (mapcar #'expand (cdr sexp)))))
		     (mapcar #'expand sexp))
		 sexp)))
    (expand sexp)))

(defun sort-by-fixed-order (lst order)
  (sort lst #'(lambda (a b) (< (position a order) (position b order)))))

(defun dcs (x)
  (string-downcase (symbol-name x)))

(defparameter *machine-name* nil)

(defparameter *fields* nil)
(defparameter *operand-order* nil)

(defstruct insn
  name
  fields
  field-equivalences
  effect
  asm
  operands
  known-bits
  known-bit-values
  operand-bits
  dont-care-bits)

(defstruct register-class
  name
  type
  width
  start-index)

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

(defstruct expr
  kind
  constp
  (type nil)
  (width nil)
  (operands nil))

(defparameter *insn-macros* nil)
(defparameter *insns* nil)

(defstruct mnemonic
  name
  args
  substitution)

(defparameter *mnemonics* nil)

(defvar *tmp-num* 0)

(defun new-machine (name)
  (setf *machine-name* name)
  (setf *fields* nil)
  (setf *operand-order* nil)
  (setf *register-classes* nil)
  (setf *registers* nil)
  (setf *subregisters* nil)
  (setf *insn-macros* nil)
  (setf *insns* nil)
  (setf *mnemonics* nil)
  (setf *tmp-num* 0))

(defun define-register-class (name type width registers)
  (let ((class (make-register-class :name name
				    :type type
				    :width width
				    :start-index (length *registers*))))
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
	(unless (eq (register-class-type (register-register-class register)) 'integer)
	  (error "only subregisters of integer registers are allowed: ~A~%" name))
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

(defun define-operand-order (order)
  (setf *operand-order* order))

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

(defun subst-many (new old sexp)
  (if (null new)
      sexp
      (subst-many (cdr new) (cdr old) (subst (car new) (car old) sexp))))

(defmacro define-insn-macro (name arg-names subst)
  `(push (cons ',name #'(lambda (&rest args) (subst-many args ',arg-names ',subst)))
    *insn-macros*))

(defmacro define-insn (name fields-spec effect asm)
  (when (assoc name *insns*)
    (error "insn already defined: ~A~%" name))
  (let* ((known-bits (make-array *insn-bits* :element-type 'bit :initial-element 0))
	 (known-bit-values (make-array *insn-bits* :element-type 'bit :initial-element 0))
	 (dont-care-bits (make-array *insn-bits* :element-type 'bit :initial-element 0))
	 (effect (mapcar #'(lambda (expr) (generate-expr expr nil nil nil)) (my-macroexpand effect *insn-macros*)))
	 (operands  (sort-by-fixed-order (fields-in-expr effect) *operand-order*))
	 (operand-bits (make-array *insn-bits* :element-type 'bit :initial-element 0))
	 (fields nil)
	 (field-equivalences nil)
	 (field-equivalences-bits (make-array *insn-bits* :element-type 'bit :initial-element 0)))
    (dolist (operand-field operands)
      (multiple-value-bind (begin end)
	  (lookup-field operand-field)
	(dofromto (i begin end)
	  (when (= (bit operand-bits i) 1)
	    (error "overlapping operands in insn ~A~%" name))
	  (setf (bit operand-bits i) 1))))
    (dolist (field fields-spec)
      (destructuring-bind (field-name value)
	  field
	(multiple-value-bind (begin end)
	    (lookup-field field-name)
	  (when (and (integerp value)
		     (or (< value 0)
			 (>= value (expt 2 (1+ (- end begin))))))
	    (error "value ~A does not fit into field ~A in insn ~A" value field-name name))
	  (dofromto (i begin end)
	    (when (or (= (bit known-bits i) 1)
		      (= (bit dont-care-bits i) 1))
	      (error "insn ~A specifies bit ~A more than once~%" name i))
	    (when (= (bit operand-bits i) 1)
	      (error "non-operand bits are used in operands in insn ~A~%" name))
	    (cond ((eq value 'dont-care)
		   (setf (bit dont-care-bits i) 1))
		  ((symbolp value)
		   (setf (bit field-equivalences-bits i) 1))
		  (t
		   (setf (bit known-bits i) 1))))
	  (cond ((and (symbolp value) (not (eq value 'dont-care)))
		 (unless (member value operands)
		   (error "illegal field equivalence in insn ~A~%" name))
		 (push (cons field-name value) field-equivalences))
		((integerp value)
		 (push (cons field-name value) fields)
		 (bit-ior known-bit-values (integer-to-bit-vector (ash value begin)) known-bit-values))))))
    (when (find 0 (reduce #'bit-ior (list known-bits dont-care-bits operand-bits field-equivalences-bits)))
      (error "not all bits are specified in insn ~A~%" name))
    `(push (make-insn :name ',name
	              :fields ',fields
	              :field-equivalences ',field-equivalences
	              :effect ',effect
	              :asm ',asm
	              :operands ',operands
	              :known-bits ,known-bits
	              :known-bit-values ,known-bit-values
	              :operand-bits ,operand-bits
	              :dont-care-bits ,dont-care-bits)
           *insns*)))

(defmacro define-mnemonic (name args substitution)
  `(push (make-mnemonic :name ',name
	                :args ',args
	                :substitution ',substitution)
         *mnemonics*))

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

(defun c-type (width type &optional (signed nil))
  (case type
    (integer
     (if signed
	 (format nil "sword_~A" width)
	 (format nil "word_~A" width)))
    (float
     (case width
       (32 "float")
       (64 "double")
       (t (error "no float type with width ~A~%" width))))
    (t
     (error "no type ~A~%" type))))

(defun fields-in-expr (expr)
  (cond ((consp expr)
	 (union (fields-in-expr (car expr)) (fields-in-expr (cdr expr))))
	((expr-p expr)
	 (if (eq (expr-kind expr) 'field)
	     (list (first (expr-operands expr)))
	     (fields-in-expr (expr-operands expr))))
	(t
	 nil)))

(defun generate-expr (expr &optional (bindings nil) (required-width nil) (required-type 'integer))
  (labels ((match-width (width)
	     (when (and (not (null required-width))
			(/= width required-width))
	       (error "width ~A should be ~A in ~A~%" width required-width expr)))
	   (match-type (type)
	     (when (and (not (null required-type))
			(not (eq required-type type)))
	       (error "type ~A should be ~A in ~A~%" type required-type expr)))
	   (resolve-register (name-or-field class-name)
	     (if (registerp name-or-field)
		 (let ((register (lookup-register name-or-field)))
		   (values register (register-register-class register) nil))
		 (let ((class (lookup-register-class class-name)))
		   (when (null class)
		     (error "unknown register class in ~A~%" expr))
		   (values nil class (generate-expr name-or-field bindings)))))
	   (generate ()
	     (cond ((fieldp expr)
		    (multiple-value-bind (begin end)
			(lookup-field expr)
		      (let ((width (1+ (- end begin))))
			(make-expr :kind 'field :type 'integer :width width :operands (list expr begin end) :constp t))))
		   ((registerp expr)
		    (let* ((register (lookup-register expr))
			   (register-class (register-register-class register))
			   (width (register-class-width register-class))
			   (type (register-class-type register-class)))
		      (make-expr :kind 'register :type type :width width :operands (list register nil nil) :constp nil)))
		   ((subregisterp expr)
		    (let* ((subregister (lookup-subregister expr))
			   (register (subregister-register subregister))
			   (begin (subregister-begin subregister))
			   (end (subregister-end subregister))
			   (width (1+ (- end begin)))
			   (class (register-register-class register))
			   (type (register-class-type class)))
		      (make-expr :kind 'subregister :type type :width width :operands (list register class nil begin end t) :constp nil)))
		   ((integerp expr)
		    (make-expr :kind 'integer :type 'integer :width (or required-width *word-bits*) :operands (list expr) :constp t))
		   ((floatp expr)
		    (make-expr :kind 'float :type 'float :width (or required-width *double-bits*) :operands (list expr) :constp t))
		   ((stringp expr)
		    (make-expr :kind 'string :type 'string :width nil :operands (list expr) :constp t))
		   ((eq expr 'pc)
		    (make-expr :kind 'pc :type 'integer :width *word-bits* :constp t))
		   ((eq expr 'addr)
		    (make-expr :kind 'addr :type 'integer :width *word-bits* :constp t))
		   ((symbolp expr)
		    (let ((binding (cadr (assoc expr bindings))))
		      (if (null binding)
			  (error "unknown variable ~A~%" expr)
			  (let ((width (expr-width binding))
				(type (expr-type binding)))
			    (make-expr :kind 'symbol :type type :width width :operands (list expr) :constp (expr-constp binding))))))
		   ((consp expr)
		    (case (first expr)
		      (ignore
		       (unless (and (null required-width)
				    (null required-type))
			 (error "ignore has no return value~%"))
		       (make-expr :kind 'ignore :operands (list (generate-expr (second expr) bindings)) :constp nil))
		      (let
			  (unless (and (null required-width)
				       (null required-type))
			    (error "let has no return value~%"))
			(let ((new-bindings (mapcar #'(lambda (binding)
							(destructuring-bind (name value)
							    binding
							  (let ((expr (generate-expr value bindings nil nil)))
							    (list name expr))))
						    (second expr))))
			  (make-expr :kind 'let :type nil :width nil
				     :operands (list new-bindings (mapcar #'(lambda (expr)
									      (generate-expr expr (append new-bindings bindings)
											     nil nil))
									  (cddr expr)))
				     :constp nil)))
		      (set
		       (unless (and (null required-width)
				    (null required-type))
			 (error "set has no return value~%"))
		       (cond ((subregisterp (second expr))
			      (let* ((subregister (lookup-subregister (second expr)))
				     (begin (subregister-begin subregister))
				     (end (subregister-end subregister))
				     (width (1+ (- end begin))))
				(make-expr :kind 'set-subregister :type nil :width nil
					   :operands (list subregister (generate-expr (third expr) bindings width 'integer))
					   :constp nil)))
			     (t
			      (case (first (second expr))
				(reg
				 (multiple-value-bind (register class number)
				     (resolve-register (second (second expr)) (third (second expr)))
				   (let ((type (register-class-type class))
					 (width (register-class-width class)))
				     (make-expr :kind 'set-register :type nil :width nil
						:operands (list register class number (generate-expr (third expr) bindings width type))
						:constp nil))))
				(numbered-subreg
				 (let ((width (second (second expr))))
				   (multiple-value-bind (register class number)
				       (resolve-register (fourth (second expr)) (fifth (second expr)))
				     (unless (eq (register-class-type class) 'integer)
				       (error "numbered subregisters are only allowed of integer registers: ~A~%" expr))
				     (let ((index (generate-expr (third (second expr)) bindings)))
				       (make-expr :kind 'set-numbered-subregister :type nil :width nil
						  :operands (list register class number width index
								  (generate-expr (third expr) bindings width))
						  :constp nil)))))
				(mem
				 (let ((width (or (third (second expr)) *word-bits*)))
				   (make-expr :kind 'set-mem :type nil :width nil
					      :operands (list width (generate-expr (second (second expr)) bindings *word-bits*)
							      (generate-expr (third expr) bindings width))
					      :constp nil)))
				(t
				 (error "not an lvalue: ~A~%" (second expr)))))))
		      (width
		       (let ((width (second expr)))
			 (generate-expr (third expr) bindings width (or required-type 'integer))))
		      (promote
		       (let ((width (second expr))
			     (value (generate-expr (third expr) bindings)))
			 (make-expr :kind 'promote :type 'integer :width width :operands (list value) :constp (expr-constp value))))
		      (jump-relative
		       (unless (and (null required-width)
				    (null required-type))
			 (error "jump-relative has no return value~%"))
		       (let ((offset (generate-expr (second expr) bindings *word-bits*)))
			 (make-expr :kind 'jump
				    :operands (list (make-expr :kind '+ :type 'integer :width *word-bits*
							       :operands (list (make-expr :kind 'pc :constp t) offset)
							       :constp (expr-constp offset)))
				    :constp nil)))
		      (jump-absolute
		       (unless (and (null required-width)
				    (null required-type))
			 (error "jump-absolute has no return value~%"))
		       (make-expr :kind 'jump
				  :operands (list (generate-expr (second expr) bindings *word-bits*))
				  :constp nil))
		      (if
		       (let ((cond (generate-expr (second expr) bindings))
			     (cons (generate-expr (third expr) bindings required-width required-type))
			     (alt (generate-expr (fourth expr) bindings required-width required-type)))
			 (unless (and (eql (expr-width cons) (expr-width alt))
				      (eq (expr-type cons) (expr-type alt)))
			   (error "widths and/or types do not match in ~A~%" expr))
			 (when (and (not (null required-width))
				    (null (expr-width cons)))
			   (error "width should be ~A in ~A~%" required-width expr))
			 (when (and (not (null required-type))
				    (null (expr-type cons)))
			   (error "type should be ~A in ~A~%" required-type expr))
			 (make-expr :kind 'if :type (expr-type cons) :width (expr-width cons)
				    :operands (list cond cons alt)
				    :constp (and (expr-constp cond) (expr-constp cons) (expr-constp alt)))))
		      ((= =f < <s <f <= <=s <=f > >s >f >= >=s >=f)
		       (let* ((op-type (if (member (first expr) '(=f <f <=f >f >=f)) 'float 'integer))
			      (op1 (generate-expr (second expr) bindings nil op-type))
			      (op2 (generate-expr (third expr) bindings nil op-type)))
			 (when (/= (expr-width op1) (expr-width op2))
			   (error "widths do not match in ~A~%" expr))
			 (make-expr :kind (first expr) :type 'integer :width *word-bits* :operands (list op1 op2)
				    :constp (and (expr-constp op1) (expr-constp op2)))))
		      (bit-set-p
		       (let ((value (generate-expr (second expr) bindings))
			     (bit (generate-expr (third expr) bindings)))
			 (make-expr :kind 'bit-set-p :type 'integer :width *word-bits*
				    :operands (list value bit)
				    :constp (and (expr-constp value) (expr-constp bit)))))
		      (reg
		       (multiple-value-bind (register class number)
			   (resolve-register (second expr) (third expr))
			 (let ((type (register-class-type class))
			       (width (register-class-width class)))
			   (make-expr :kind 'register :type type :width width :operands (list register class number) :constp nil))))
		      (subreg
		       (let* ((begin (second expr))
			      (end (third expr))
			      (width (1+ (- end begin))))
			 (multiple-value-bind (register class number)
			     (resolve-register (fourth expr) (fifth expr))
			   (unless (eq (register-class-type class) 'integer)
			     (error "only subregisters of integer registers are allowed: ~A~%" expr))
			   (make-expr :kind 'subregister :type 'integer :width width
				      :operands (list register class number begin end nil) :constp nil))))
		      (numbered-subreg
		       (let ((width (second expr)))
			 (multiple-value-bind (register class number)
			     (resolve-register (fourth expr) (fifth expr))
			   (unless (eq (register-class-type class) 'integer)
			     (error "only numbered subregisters of integer registers are allowed: ~A~%" expr))
			   (make-expr :kind 'numbered-subregister :type 'integer :width width
				      :operands (list register class number width (generate-expr (third expr) bindings)) :constp nil))))
		      (mem
		       (when (null required-width)
			 (error "no required width in ~A~%" expr))
		       (make-expr :kind 'mem :type 'integer :width (or required-width *word-bits*)
				  :operands (list (generate-expr (second expr) bindings *word-bits*)) :constp nil))
		      ((zex sex)
		       (when (null required-width)
			 (error "no required width in ~A~%" expr))
		       (let ((op (generate-expr (second expr) bindings)))
			 (make-expr :kind (first expr) :type 'integer :width required-width :operands (list op) :constp (expr-constp op))))
		      ((+ - * *s / /s logor logand logxor)
		       (let ((op1 (generate-expr (second expr) bindings required-width))
			     (op2 (generate-expr (third expr) bindings required-width)))
			 (when (/= (expr-width op1) (expr-width op2))
			   (error "widths do not match in ~A" expr))
			 (make-expr :kind (first expr) :type 'integer :width (expr-width op1)
				    :operands (list op1 op2) :constp (and (expr-constp op1) (expr-constp op2)))))
		      ((+f -f *f /f)
		       (let ((op1 (generate-expr (second expr) bindings required-width 'float))
			     (op2 (generate-expr (third expr) bindings required-width 'float)))
			 (when (/= (expr-width op1) (expr-width op2))
			   (error "widths do not match in ~A" expr))
			 (make-expr :kind (first expr) :type 'float :width (expr-width op1)
				    :operands (list op1 op2) :constp (and (expr-constp op1) (expr-constp op2)))))
		      (+carry
		       (let ((op1 (generate-expr (second expr) bindings *word-bits*))
			     (op2 (generate-expr (third expr) bindings *word-bits*)))
			 (make-expr :kind '+carry :type 'integer :width 1
				    :operands (list op1 op2) :constp (and (expr-constp op1) (expr-constp op2)))))
		      ((neg bitneg not)
		       (let ((op (generate-expr (second expr) bindings *word-bits*)))
			 (make-expr :kind (first expr) :type 'integer :width *word-bits*
				    :operands (list op) :constp (expr-constp op))))
		      ((fneg sqrt)
		       (let* ((op (generate-expr (second expr) bindings required-width 'float))
			      (width (expr-width op)))
			 (make-expr :kind (first expr) :type 'float :width width :operands (list op) :constp (expr-constp op))))
		      ((shiftl shiftr rotl)
		       (let* ((op1 (generate-expr (second expr) bindings required-width))
			      (op2 (generate-expr (third expr) bindings))
			      (width (expr-width op1)))
			 (make-expr :kind (first expr) :type 'integer :width width
				    :operands (list op1 op2) :constp (and (expr-constp op1) (expr-constp op2)))))
		      (ashiftr
		       (let ((op1 (generate-expr (second expr) bindings *word-bits*))
			     (op2 (generate-expr (third expr) bindings)))
			 (make-expr :kind 'ashiftr :type 'integer :width *word-bits*
				    :operands (list op1 op2) :constp (and (expr-constp op1) (expr-constp op2)))))
		      (mask
		       (let ((op1 (generate-expr (second expr) bindings))
			     (op2 (generate-expr (third expr) bindings)))
			 (make-expr :kind 'mask :type 'integer :width (or required-width *word-bits*)
				    :operands (list op1 op2) :constp (and (expr-constp op1) (expr-constp op2)))))
		      (maskmask
		       (let* ((bit-width (second expr))
			      (mask (generate-expr (third expr) bindings))
			      (mask-width (expr-width mask))
			      (width (* bit-width mask-width)))
			 (make-expr :kind 'maskmask :type 'integer :width width
				    :operands (list bit-width mask) :constp (expr-constp mask))))
		      ((leading-zeros trailing-zeros population)
		       (let ((op (generate-expr (second expr) bindings *word-bits*)))
			 (make-expr :kind (first expr) :type 'integer :width (or required-width *word-bits*)
				    :operands (list op) :constp (expr-constp op))))
		      ((single-to-double double-to-single)
		       (multiple-value-bind (old-width new-width)
			   (if (eq (first expr) 'single-to-double)
			       (values *single-bits* *double-bits*)
			       (values *double-bits* *single-bits*))
			 (let ((value (generate-expr (second expr) bindings old-width 'float)))
			   (make-expr :kind 'convert-float :type 'float :width new-width
				      :operands (list value) :constp (expr-constp value)))))
		      ((bits-to-single bits-to-double)
		       (let* ((width (if (eq (first expr) 'bits-to-single) *single-bits* *double-bits*))
			      (value (generate-expr (second expr) bindings width 'integer)))
			 (make-expr :kind 'bits-to-float :type 'float :width width
				    :operands (list value) :constp (expr-constp value))))
		      ((single-to-bits double-to-bits)
		       (let* ((width (if (eq (first expr) 'single-to-bits) *single-bits* *double-bits*))
			      (value (generate-expr (second expr) bindings width 'float)))
			 (make-expr :kind 'float-to-bits :type 'integer :width width
				    :operands (list value) :constp (expr-constp value))))
		      ((single-to-integer double-to-integer)
		       (let* ((float-width (if (eq (first expr) 'single-to-integer) *single-bits* *double-bits*))
			      (integer-width (or required-width *word-bits*))
			      (value (generate-expr (second expr) bindings float-width 'float)))
			 (make-expr :kind 'float-to-integer :type 'integer :width integer-width
				    :operands (list value) :constp (expr-constp value))))
		      ((integer-to-single integer-to-double)
		       (let* ((integer-width (or required-width *word-bits*))
			      (float-width (if (eq (first expr) 'integer-to-single) *single-bits* *double-bits*))
			      (value (generate-expr (second expr) bindings integer-width 'integer)))
			 (make-expr :kind 'integer-to-float :type 'float :width float-width
				    :operands (list value) :constp (expr-constp value))))
		      (nop
		       (make-expr :kind 'nop))
		      (syscall
		       (make-expr :kind 'system-call))
		      (call-pal
		       (let ((value (generate-expr (second expr) bindings *word-bits*)))
			 (make-expr :kind 'call-pal :operands (list value))))
		      (not-implemented
		       (make-expr :kind 'not-implemented))
		      (t
		       (error "unknown pattern ~A~%" (first expr)))))
		   (t
		    (error "illegal expr ~A~%" expr)))))
    (let ((result (generate)))
      (match-type (expr-type result))
      (match-width (expr-width result))
      result)))

(defun generate-interpreter (expr bindings)
  (labels ((register-code (reg &optional class number (with-parens t))
	     (format nil "~:[~;(~]intp->regs_~A[~A]~:[~;)~]"
		     with-parens
		     (register-class-name (if (null class) (register-register-class reg) class))
		     (if (null number) (register-number reg) (generate-interpreter number bindings))
		     with-parens)))
    (let ((s-generators `((field ,#'(lambda (name begin end) (code-for-field "insn" begin end)))
			  (integer ,#'(lambda (value) (format nil "~A" value)))
			  (string ,#'(lambda (value) (format nil "\"~A\"" value)))
			  (pc ,#'(lambda () "pc"))
			  (addr ,#'(lambda () "addr"))
			  (symbol ,#'(lambda (name) (cdr (assoc name bindings))))
			  (ignore ,#'(lambda (value) "0 /* ignore */"))
			  (let ,#'(lambda (let-bindings body)
				    (with-output-to-string (out)
				      (format out "{~%")
				      (let ((new-bindings (append (mapcar #'(lambda (binding)
									      (destructuring-bind (name expr)
										  binding
										(let ((code (generate-interpreter expr bindings))
										      (c-name (make-tmp-name)))
										  (format out "~A ~A = ~A;~%"
											  (c-type (expr-width expr) (expr-type expr)) c-name code)
										  (cons name c-name))))
									  let-bindings)
								  bindings)))
					(dolist (expr body)
					  (format out "~A;~%" (generate-interpreter expr new-bindings))))
				      (format out "}~%"))))
			  (set-register ,#'(lambda (reg class number rhs)
					     (format nil "(~A = ~A)" (register-code reg class number) (generate-interpreter rhs bindings))))
			  (set-subregister ,#'(lambda (subreg rhs)
						(let* ((reg (subregister-register subreg))
						       (reg-code (register-code reg nil nil nil))
						       (class (register-register-class reg))
						       (begin (subregister-begin subreg))
						       (end (subregister-end subreg))
						       (width (1+ (- end begin))))
						  (format nil "(~A = (~A & 0x~X) | (~A << ~A))"
							  reg-code
							  reg-code (bit-vector-to-integer (bit-not (bit-range begin end (register-class-width class))))
							  (generate-interpreter rhs bindings) begin))))
			  (set-numbered-subregister ,#'(lambda (reg class number width index rhs)
							 (let ((reg-code (register-code reg class number))
							       (index-code (generate-interpreter index bindings)))
							   (format nil "(~A = (~A & ~~mask(~A * ~A, ~A * ~A + ~A - 1)) | (~A << (~A * ~A)))"
								   reg-code
								   reg-code width index-code width index-code width
								   (generate-interpreter rhs bindings) width index-code))))
			  (set-mem ,#'(lambda (width addr rhs)
					(format nil "mem_set_~A(intp, ~A, ~A)" width (generate-interpreter addr bindings)
						(generate-interpreter rhs bindings))))
			  (promote ,#'(lambda (value)
					(format nil "((~A)~A)" (c-type (expr-width expr) 'integer) (generate-interpreter value bindings))))
			  (jump ,#'(lambda (target) (format nil "(next_pc = ~A)" (generate-interpreter target bindings))))
			  (if ,#'(lambda (cond cons alt)
				   (format nil "(~A ? ~A : ~A)"
					   (generate-interpreter cond bindings) (generate-interpreter cons bindings)
					   (generate-interpreter alt bindings))))
			  (bit-set-p ,#'(lambda (value index)
					  (format nil "(~A & (1 << ~A))" (generate-interpreter value bindings) (generate-interpreter index bindings))))
			  (register ,#'(lambda (reg class number) (register-code reg class number)))
			  (subregister ,#'(lambda (reg class number begin end named)
					    (let ((width (1+ (- end begin)))
						  (reg-code (register-code reg class number (not named))))
					      (format nil "((~A >> ~A) & 0x~X)" reg-code begin (1- (expt 2 width))))))
			  (numbered-subregister ,#'(lambda (reg class number width index)
						     (let ((reg-code (register-code reg class number)))
						       (format nil "((~A >> (~A * ~A)) & 0x~X)"
							       reg-code width (generate-interpreter index bindings)
							       (1- (expt 2 width))))))
			  (mem ,#'(lambda (addr) (format nil "mem_get_~A(intp, ~A)" (expr-width expr) (generate-interpreter addr bindings))))
			  (zex ,#'(lambda (value) (generate-interpreter value bindings)))
			  (sex ,#'(lambda (value)
				    (let ((old-width (expr-width value))
					  (new-width (expr-width expr))
					  (code (generate-interpreter value bindings)))
				      (format nil "((~A & 0x~X) ? (~A | 0x~X) : ~A)"
					      code (expt 2 (1- old-width))
					      code (- (1- (expt 2 new-width)) (1- (expt 2 old-width)))
					      code))))
			  (ashiftr ,#'(lambda (value amount)
					(let ((value-code (generate-interpreter value bindings))
					      (amount-code (generate-interpreter amount bindings)))
					  (format nil "((~A >> ~A) | ((~A & 0x80000000) ? ~~((1 << (32 - ~A)) - 1) : 0))"
						  value-code amount-code value-code amount-code))))
			  (maskmask ,#'(lambda (bit-width mask)
					 (format nil "maskmask(~A, ~A, ~A)" bit-width (expr-width mask) (generate-interpreter mask bindings))))
			  (leading-zeros ,#'(lambda (value) (format nil "leading_zeros(~A)" (generate-interpreter value bindings))))
			  (convert-float ,#'(lambda (value)
					      (format nil "((~A)~A)" (c-type (expr-width expr) 'float) (generate-interpreter value bindings))))
			  (bits-to-float ,#'(lambda (value)
					      (let ((width (expr-width expr)))
						(format nil "({ ~A tmp = ~A; *(~A*)&tmp; })"
							(c-type width 'integer)
							(generate-interpreter value bindings)
							(c-type width 'float)))))
			  (float-to-bits ,#'(lambda (value)
					      (let ((width (expr-width expr)))
						(format nil "({ ~A tmp = ~A; *(~A*)&tmp; })"
							(c-type width 'float)
							(generate-interpreter value bindings)
							(c-type width 'integer)))))
			  (float-to-integer ,#'(lambda (value)
						 (let ((width (expr-width expr)))
						   (format nil "((~A)(~A)~A)" (c-type width 'integer) (c-type width 'integer t)
							   (generate-interpreter value bindings)))))
			  (nop ,#'(lambda () "0 /* nop */"))
			  (system-call ,#'(lambda () "handle_system_call(intp)"))))
	  (m-generators `((((= "==") (< "<") (<f "<") (> ">") (>f ">") (+ "+") (+f "+") (- "-") (-f "-") (* "*") (*f "*") (/ "/") (/f "/")
			    (logor "|") (logand "&") (logxor "^") (shiftl "<<") (shiftr ">>"))
			   ,#'(lambda (op left right)
				(format nil "(~A ~A ~A)" (generate-interpreter left bindings) op (generate-interpreter right bindings))))
			  (((*s "*") (/s "/"))
			   ,#'(lambda (op left right)
				(let* ((width (expr-width expr))
				       (type (c-type width 'integer t)))
				  (format nil "((~A)((~A)~A ~A (~A)~A))"
					  (c-type width 'integer)
					  type (generate-interpreter left bindings) op type (generate-interpreter right bindings)))))
			  (((neg "-") (fneg "-") (bitneg "~"))
			   ,#'(lambda (op value)
				(format nil "(~A~A)" op (generate-interpreter value bindings))))
			  (((<s "<") (>s ">"))
			   ,#'(lambda (op left right)
				(let ((type (c-type (expr-width left) 'integer t)))
				  (format nil "((~A)~A ~A (~A)~A)"
					  type (generate-interpreter left bindings) op
					  type (generate-interpreter right bindings)))))
			  (((+carry "addcarry") (-carry "subcarry") (rotl "rotl") (mask "mask"))
			   ,#'(lambda (func first second)
				(format nil "~A(~A, ~A)" func (generate-interpreter first bindings) (generate-interpreter second bindings)))))))
      (let ((s-generator (cadr (assoc (expr-kind expr) s-generators))))
	(if (null s-generator)
	    (let ((m-generator (dolist (p m-generators)
				 (let ((op (cadr (assoc (expr-kind expr) (car p)))))
				   (unless (null op)
				     (return (cons op (cadr p))))))))
	      (if (null m-generators)
		  (error "no generator for ~A~%" (expr-kind expr))
		  (apply (cdr m-generator) (car m-generator) (expr-operands expr))))
	    (apply s-generator (expr-operands expr)))))))

(defun register-index (reg class number)
  (if reg
      (+ (register-number reg) (register-class-start-index class))
      (format nil "(~A + ~A)" (register-class-start-index class) (generate-interpreter number nil))))

(defun contains-jump-p (expr)
  (cond ((consp expr)
	 (or (contains-jump-p (car expr)) (contains-jump-p (cdr expr))))
	((expr-p expr)
	 (or (eq (expr-kind expr) 'jump)
	     (contains-jump-p (expr-operands expr))))
	(t
	 nil)))

(defun generate-compiler (target expr bindings &key true-label false-label (foreign-target -1))
  (labels ((load-reg-name (type width)
	     (format nil "load_reg_~A_~A" (dcs type) width))
	   (store-reg-name (type width)
	     (format nil "store_reg_~A_~A" (dcs type) width)))
    (let ((s-generators `((field ,#'(lambda (name begin end)
				      (format nil "~A = ref_integer_reg_for_writing(~A);
emit_load_integer_32(~A, ~A);~%"
					      target foreign-target
					      target (code-for-field "insn" begin end))))
			  (integer ,#'(lambda (value)
					(format nil "~A = ref_integer_reg_for_writing(~A);
emit_load_integer_32(~A, ~A);~%"
						target foreign-target
						target value)))
			  (string ,#'(lambda (value) (error "cannot generate compiler for string~%")))
			  (pc ,#'(lambda () (format nil "~A = ref_integer_reg_for_writing(~A);
emit_load_integer_32(~A, pc);~%"
						    target foreign-target
						    target)))
			  (addr ,#'(lambda () (error "cannot generate compiler for addr~%")))
			  (symbol ,#'(lambda (name)
				       (let ((value (cdr (assoc name bindings))))
					 (format nil "~A = ref_integer_reg_for_writing(~A);~%~A~%"
						 target foreign-target
						 (if (expr-constp expr)
						     (format nil "emit_load_integer_32(~A, ~A);~%" target value)
						     (format nil "emit(COMPOSE_MOV(~A, ~A));~%" value target))))))
			  (let ,#'(lambda (let-bindings body)
				    (with-output-to-string (out)
				      (format out "{~%")
				      (let* ((new-bindings (mapcar #'(lambda (binding)
								       (destructuring-bind (name expr)
									   binding
									 (let ((c-name (make-tmp-name)))
									   (if (expr-constp expr)
									       (let ((code (generate-interpreter expr bindings)))
										 (format out "~A ~A = ~A;~%"
											 (c-type (expr-width expr) (expr-type expr)) c-name code))
									       (format out "reg_t ~A = ref_~A_reg_for_writing(-1);
~A"
										       c-name (dcs (expr-type expr))
										       (generate-compiler c-name expr bindings
													  :foreign-target (format nil
																  "~A | NEED_NATIVE"
																  c-name))))
									   (cons name c-name))))
								   let-bindings))
					     (all-bindings (append new-bindings bindings)))
					(dolist (expr body)
					  (format out "~A;~%" (generate-compiler nil expr all-bindings)))
					(mapc #'(lambda (let-binding binding)
						  (format out "unref_~A_reg(~A);~%"
							  (dcs (expr-type (cadr let-binding)))
							  (cdr binding)))
					      let-bindings new-bindings)
					(format out "}~%")))))
			  (set-register ,#'(lambda (reg class number rhs)
					     (let ((reg-index (register-index reg class number))
						   (type (register-class-type class))
						   (reg-reg (make-tmp-name)))
					       (format nil "{
reg_t ~A;
~Aunref_~A_reg(~A);
}~%"
						       reg-reg
						       (generate-compiler reg-reg rhs bindings :foreign-target reg-index) (dcs type) reg-reg))))
			  (set-subregister ,#'(lambda (subreg rhs)
						(let* ((reg (subregister-register subreg))
						       (class (register-register-class reg))
						       (reg-index (register-index reg class (register-number reg)))
						       (reg-width (register-class-width class))
						       (begin (subregister-begin subreg))
						       (end (subregister-end subreg))
						       (width (1+ (- end begin)))
						       (rhs-reg (make-tmp-name))
						       (mask-reg (make-tmp-name))
						       (reg-reg (make-tmp-name)))
						  (format nil "{
reg_t ~A, ~A, ~A;
~Aemit(COMPOSE_SLL_IMM(~A, ~A, ~A));
emit(COMPOSE_SRL_IMM(~A, ~A, ~A));
~A = ref_integer_reg_for_writing(-1);
emit_load_integer_32(~A, -1);
emit(COMPOSE_SLL_IMM(~A, ~A, ~A));
emit(COMPOSE_SRL_IMM(~A, ~A, ~A));
~A = ref_integer_reg_for_reading_and_writing(~A);
emit(COMPOSE_BIC(~A, ~A, ~A));
unref_integer_reg(~A);
emit(COMPOSE_BIS(~A, ~A, ~A));
unref_integer_reg(~A);
unref_integer_reg(~A);
}~%"
							  rhs-reg mask-reg reg-reg
							  (generate-compiler rhs-reg rhs bindings) rhs-reg (- 64 width) rhs-reg
							  rhs-reg (- 63 end) rhs-reg
							  mask-reg
							  mask-reg
							  mask-reg (- 64 width) mask-reg
							  mask-reg (- 63 end) mask-reg
							  reg-reg reg-index
							  reg-reg mask-reg reg-reg
							  mask-reg
							  reg-reg rhs-reg reg-reg
							  rhs-reg
							  reg-reg))))
			  (set-numbered-subregister ,#'(lambda (reg class number width index rhs)
							 (let ((reg-index (register-index reg class number))
							       (reg-width (register-class-width class))
							       (rhs-reg (make-tmp-name))
							       (mask-reg (make-tmp-name))
							       (reg-reg (make-tmp-name)))
							   (format nil "{
word_64 begin = ~A * ~A;
reg_t ~A, ~A, ~A;
~Aemit(COMPOSE_SLL_IMM(~A, begin, ~A));
~A = ref_integer_reg_for_writing(-1);
emit_load_integer_32(~A, -1);
emit(COMPOSE_SRL_IMM(~A, ~A, ~A));
emit(COMPOSE_SLL_IMM(~A, begin, ~A));
emit(COMPOSE_AND(~A, ~A, ~A));
~A = ref_integer_reg_for_reading_and_writing(~A);
emit(COMPOSE_BIC(~A, ~A, ~A));
unref_integer_reg(~A);
emit(COMPOSE_BIS(~A, ~A, ~A));
unref_integer_reg(~A);
unref_integer_reg(~A);
}~%"
								   (generate-interpreter index nil) width
								   rhs-reg mask-reg reg-reg
								   (generate-compiler rhs-reg rhs bindings) rhs-reg rhs-reg
								   mask-reg
								   mask-reg
								   mask-reg (- 64 width) mask-reg
								   mask-reg mask-reg
								   rhs-reg mask-reg rhs-reg
								   reg-reg reg-index
								   reg-reg mask-reg reg-reg
								   mask-reg
								   reg-reg rhs-reg reg-reg
								   rhs-reg
								   reg-reg))))
			  (set-mem ,#'(lambda (width addr rhs)
					(let ((addr-reg (make-tmp-name))
					      (rhs-reg (make-tmp-name)))
					  (format nil "{
reg_t ~A, ~A;
~Aemit(COMPOSE_ZAPNOT_IMM(~A, 15, ~A));
~Aemit_store_mem_~A(~A, ~A);
unref_integer_reg(~A);
unref_integer_reg(~A);
}~%"
						  addr-reg rhs-reg
						  (generate-compiler addr-reg addr bindings) addr-reg addr-reg
						  (generate-compiler rhs-reg rhs bindings) width rhs-reg addr-reg
						  addr-reg
						  rhs-reg))))
			  (promote ,#'(lambda (value)
					(let ((src-width (expr-width value))
					      (dst-width (expr-width expr))
					      (target-code (generate-compiler target value bindings :foreign-target foreign-target)))
					  (if (and (= src-width 32) (= dst-width 64))
					      (format nil "~Aemit(COMPOSE_ZAPNOT_IMM(~A, 15, ~A));~%"
						      target-code target target)
					      (progn
						(unless (and (= src-width 64) (= dst-width 32))
						  (error "cannot promote from ~A to ~A bits~%" src-width dst-width))
						(format nil "~Aemit(COMPOSE_SLL_IMM(~A, ~A, ~A));
emit(COMPOSE_SRA_IMM(~A, ~A, ~A));~%"
							target-code target (- src-width dst-width) target
							target (- src-width dst-width) target))))))
			  (jump ,#'(lambda (target)
				     (format nil "store_all_foreign_regs();~%~A~%"
					     (if (expr-constp target)
						 (format nil "emit_direct_jump(~A);" (generate-interpreter target nil))
						 (let ((target-reg (make-tmp-name)))
						   (format nil "{
reg_t ~A;
~Aemit_indirect_jump();
}~%"
							   target-reg
							   (generate-compiler target-reg target bindings
									      :foreign-target "JUMP_TARGET_REG | NEED_NATIVE")))))))
			  (if ,#'(lambda (cond cons alt)
				   (if (expr-constp cond)
				       (format nil "if (~A) {~%~A} else {~%~A}~%"
					       (generate-interpreter cond nil)
					       (generate-compiler target cons bindings :foreign-target foreign-target)
					       (generate-compiler target alt bindings :foreign-target foreign-target))
				       (let* ((result-reg (if (null target) nil (make-tmp-name)))
					      (dummy-foreign-target (if (null target) -1 (format nil "~A | NEED_NATIVE" result-reg)))
					      (true (make-tmp-name))
					      (false (make-tmp-name))
					      (end (make-tmp-name))
					      (can-jump (or (contains-jump-p cons) (contains-jump-p alt))))
					 (format nil "{
label_t ~A = alloc_label(), ~A = alloc_label(), ~A = alloc_label();
~A
~A
~A
emit_label(~A);
push_alloc();
~Apop_alloc();
emit_branch(COMPOSE_BR(31, 0), ~A);
emit_label(~A);
push_alloc();
~Apop_alloc();
emit_label(~A);
free_label(~A);
free_label(~A);
free_label(~A);
~A
}~%"
						 true false end
						 (if (null target) "" (format nil "reg_t ~A;" result-reg))
						 (generate-compiler nil cond bindings :true-label true :false-label false)
						 (if (null target)
						     ""
						     (format nil "~A = ref_~A_reg_for_writing(-1);" result-reg (dcs (expr-type expr))))
						 true
						 (generate-compiler result-reg cons bindings :foreign-target dummy-foreign-target)
						 end
						 false
						 (generate-compiler result-reg alt bindings :foreign-target dummy-foreign-target)
						 end
						 true false end
						 (if (null target)
						     ""
						     (if (eql foreign-target -1)
							 (format nil "~A = ~A;" target result-reg)
							 (format nil "~A = ref_~A_reg_for_writing(~A);
emit(COMPOSE_~AMOV(~A, ~A));
unref_~A_reg(~A);"
								 target (dcs (expr-type expr)) foreign-target
								 (if (eq (expr-type expr) 'float) "F" "") result-reg target
								 (dcs (expr-type expr)) result-reg))))))))
			  (bit-set-p ,#'(lambda (value index)
					  (let ((value-reg (make-tmp-name))
						(bit-reg (make-tmp-name)))
					    (format nil "{
reg_t ~A, ~A;
~A~A = ref_integer_reg_for_writing(-1);
emit(COMPOSE_SRL_IMM(~A, ~A, ~A));
unref_integer_reg(~A);
emit_branch(COMPOSE_BLBS(~A, 0), ~A);
unref_integer_reg(~A);
emit_branch(COMPOSE_BR(31, 0), ~A);
}~%"
						    value-reg bit-reg
						    (generate-compiler value-reg value bindings) bit-reg
						    value-reg (generate-interpreter index nil) bit-reg
						    value-reg
						    bit-reg true-label
						    bit-reg
						    false-label))))
			  (register ,#'(lambda (reg class number)
					 (let ((reg-index (register-index reg class number))
					       (type (register-class-type class)))
					   (if (eql foreign-target -1)
					       (format nil "~A = ref_~A_reg_for_reading(~A);~%"
						       target (dcs type) reg-index)
					       (let ((tmp-reg (make-tmp-name)))
						 (format nil "{
reg_t ~A = ref_~A_reg_for_reading(~A);
~A = ref_~A_reg_for_writing(~A);
emit(COMPOSE_~AMOV(~A, ~A));
unref_~A_reg(~A);
}~%"
							 tmp-reg (dcs type) reg-index
							 target (dcs type) foreign-target
							 (if (eq type 'float) "F" "") tmp-reg target
							 (dcs type) tmp-reg))))))
			  (subregister ,#'(lambda (reg class number begin end named)
					    (let ((reg-index (register-index reg class number))
						  (reg-type (register-class-type class))
						  (reg-width (register-class-width class))
						  (width (1+ (- end begin)))
						  (reg-reg (make-tmp-name)))
					      (format nil "{
reg_t ~A = ref_~A_reg_for_reading(~A);
~A = ref_~A_reg_for_writing(~A);
emit(COMPOSE_SLL_IMM(~A, ~A, ~A));
unref_~A_reg(~A);
emit(COMPOSE_SRL_IMM(~A, ~A, ~A));
}~%"
						      reg-reg (dcs reg-type) reg-index
						      target (dcs reg-type) foreign-target
						      reg-reg (- 63 end) target
						      (dcs reg-type) reg-reg
						      target (- 64 width) target))))
			  (numbered-subregister ,#'(lambda (reg class number width index)
						     (let ((reg-index (register-index reg class number))
							   (reg-type (register-class-type class))
							   (reg-reg (make-tmp-name)))
						       (format nil "{
word_32 index = ~A;
reg_t ~A = ref_~A_reg_for_reading(~A);
~A = ref_~A_reg_for_writing(~A);
emit(COMPOSE_SLL_IMM(~A, 64 - (index + 1) * ~A, ~A));
unref_~A_reg(~A);
emit(COMPOSE_SRL_IMM(~A, ~A, ~A));
}~%"
							       (generate-interpreter index nil)
							       reg-reg (dcs reg-type) reg-index
							       target (dcs reg-type) foreign-target
							       reg-reg width target
							       (dcs reg-type) reg-reg
							       target (- 64 width) target))))
			  (mem ,#'(lambda (addr)
				    (let ((addr-reg (make-tmp-name)))
				      (format nil "{
reg_t ~A;
~Aemit(COMPOSE_ZAPNOT_IMM(~A, 15, ~A));
~A = ref_~A_reg_for_writing(~A);
emit_load_mem_~A(~A, ~A);
unref_integer_reg(~A);
}~%"
					      addr-reg
					      (generate-compiler addr-reg addr bindings) addr-reg addr-reg
					      target (dcs (expr-type expr)) foreign-target
					      (expr-width expr) target addr-reg
					      addr-reg))))
			  (zex ,#'(lambda (value)
				    (let ((value-code (generate-compiler target value bindings :foreign-target foreign-target)))
				      (if (and (= (expr-width expr) 64) (= (expr-width value) 32))
					  (format nil "~Aemit(COMPOSE_ZAPNOT_IMM(~A, 15, ~A));~%" value-code target target)
					  (progn
					    (when (> (expr-width expr) 32)
					      (error "cannot zero extend to ~A bits~%" (expr-width expr)))
					    value-code)))))
			  (sex ,#'(lambda (value)
				    (format nil "~Aemit(COMPOSE_SLL_IMM(~A, ~A, ~A));
emit(COMPOSE_SRA_IMM(~A, ~A, ~A));~%"
					    (generate-compiler target value bindings :foreign-target foreign-target)
					    target (- 64 (expr-width value)) target
					    target (- 64 (expr-width value)) target)))
			  (+carry ,#'(lambda (op1 op2)
				       (let ((op1-reg (make-tmp-name))
					     (op2-reg (make-tmp-name))
					     (op1-zapped-reg (make-tmp-name))
					     (op2-zapped-reg (make-tmp-name)))
					 (format nil "{
reg_t ~A, ~A, ~A, ~A;
~A~A = ref_integer_reg_for_writing(-1);
emit(COMPOSE_ZAPNOT_IMM(~A, 15, ~A));
unref_integer_reg(~A);
~A~A = ref_integer_reg_for_writing(-1);
emit(COMPOSE_ZAPNOT_IMM(~A, 15, ~A));
unref_integer_reg(~A);
~A = ref_integer_reg_for_writing(~A);
emit(COMPOSE_ADDQ(~A, ~A, ~A));
unref_integer_reg(~A);
unref_integer_reg(~A);
emit(COMPOSE_SRL_IMM(~A, 32, ~A));
}~%"
						 op1-reg op2-reg op1-zapped-reg op2-zapped-reg
						 (generate-compiler op1-reg op1 bindings) op1-zapped-reg
						 op1-reg op1-zapped-reg
						 op1-reg
						 (generate-compiler op2-reg op2 bindings) op2-zapped-reg
						 op2-reg op2-zapped-reg
						 op2-reg
						 target foreign-target
						 op1-zapped-reg op2-zapped-reg target
						 op1-zapped-reg
						 op2-zapped-reg
						 target target))))
			  (shiftl ,#'(lambda (value amount)
				       (let ((value-reg (make-tmp-name))
					     (amount-reg (make-tmp-name)))
					 (format nil "{
reg_t ~A, ~A;
~A~A~A = ref_integer_reg_for_writing(~A);
emit(COMPOSE_SLL(~A, ~A, ~A));
unref_integer_reg(~A);
unref_integer_reg(~A);
~A}~%"
						 value-reg amount-reg
						 (generate-compiler value-reg value bindings)
						 (generate-compiler amount-reg amount bindings) target foreign-target
						 value-reg amount-reg target
						 value-reg
						 amount-reg
						 (if (= (expr-width expr) 64)
						     ""
						     (progn
						       (unless (= (expr-width expr) 32)
							 (error "cannot shiftl width ~A~%" (expr-width expr)))
						       (format nil
							       "emit(COMPOSE_ADDL(~A, 31, ~A));~%"
							       target target)))))))
			  (shiftr ,#'(lambda (value amount)
				       (let ((value-reg (make-tmp-name))
					     (amount-reg (make-tmp-name)))
					 (if (= (expr-width expr) 64)
					     (format nil "{
reg_t ~A, ~A;
~A~A~A = ref_integer_reg_for_writing(~A);
emit(COMPOSE_SRL(~A, ~A, ~A));
unref_integer_reg(~A);
unref_integer_reg(~A);
}~%"
						     value-reg amount-reg
						     (generate-compiler value-reg value bindings)
						     (generate-compiler amount-reg amount bindings) target foreign-target
						     value-reg amount-reg target
						     value-reg
						     amount-reg)
					     (progn
					       (unless (= (expr-width expr) 32)
						 (error "cannot shiftr width ~A~%" (expr-width expr)))
					       (format nil "{
reg_t ~A, ~A;
~A~Aemit(COMPOSE_ZAPNOT_IMM(~A, 15, ~A));
~A = ref_integer_reg_for_writing(~A);
emit(COMPOSE_SRL(~A, ~A, ~A));
unref_integer_reg(~A);
unref_integer_reg(~A);
emit(COMPOSE_ADDL(~A, 31, ~A));
}~%"
						       value-reg amount-reg
						       (generate-compiler value-reg value bindings)
						       (generate-compiler amount-reg amount bindings) value-reg value-reg
						       target foreign-target
						       value-reg amount-reg target
						       value-reg
						       amount-reg
						       target target))))))
			  (rotl ,#'(lambda (value amount)
				     (let ((tmp-reg (make-tmp-name))
					   (value-reg (make-tmp-name))
					   (zapped-reg (make-tmp-name)))
				       (format nil "{
reg_t ~A, ~A, ~A;
word_64 amount = ~A;
if (amount == 0)
{
~A}
else
{
~A~A = ref_integer_reg_for_writing(-1);
emit(COMPOSE_ZAPNOT_IMM(~A, 15, ~A));
unref_integer_reg(~A);
~A = ref_integer_reg_for_writing(-1);
emit(COMPOSE_SLL_IMM(~A, amount, ~A));
emit(COMPOSE_SLL_IMM(~A, 32 + amount, ~A));
emit(COMPOSE_BIS(~A, ~A, ~A));
unref_integer_reg(~A);
~A = ref_integer_reg_for_writing(~A);
emit(COMPOSE_SRA_IMM(~A, 32, ~A));
unref_integer_reg(~A);
}
}~%"
					       tmp-reg value-reg zapped-reg
					       (generate-interpreter amount nil)
					       (generate-compiler target value bindings :foreign-target foreign-target)
					       (generate-compiler value-reg value bindings) zapped-reg
					       value-reg zapped-reg
					       value-reg
					       tmp-reg
					       zapped-reg tmp-reg
					       zapped-reg zapped-reg
					       zapped-reg tmp-reg zapped-reg
					       tmp-reg
					       target foreign-target
					       zapped-reg target
					       zapped-reg))))
			  (leading-zeros ,#'(lambda (value)
					      (let ((tmp-reg (make-tmp-name)))
						(format nil "{
reg_t ~A;
~Aemit(COMPOSE_LDQ(0, LEADING_ZEROS_CONST * 4, CONSTANT_AREA_REG));
emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, C_STUB_CONST * 4, CONSTANT_AREA_REG));
emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));
~A = ref_integer_reg_for_writing(~A);
emit(COMPOSE_MOV(0, ~A));
}~%"
							tmp-reg
							(generate-compiler tmp-reg value bindings :foreign-target "16 | NEED_NATIVE")
							target foreign-target
							target))))
			  (convert-float ,#'(lambda (value) (generate-compiler target value bindings :foreign-target foreign-target)))
			  (bits-to-float ,#'(lambda (value)
					      (let ((bits-reg (make-tmp-name))
						    (float-char (if (= (expr-width expr) 32) "S" "T"))
						    (int-char (if (= (expr-width expr) 32) "L" "Q")))
						(format nil "{
reg_t ~A;
~Aemit(COMPOSE_ST~A(~A, SCRATCH_OFFSET, CONSTANT_AREA_REG));
unref_integer_reg(~A);
~A = ref_float_reg_for_writing(~A);
emit(COMPOSE_LD~A(~A, SCRATCH_OFFSET, CONSTANT_AREA_REG));
}~%"
							bits-reg
							(generate-compiler bits-reg value bindings) int-char bits-reg
							bits-reg
							target foreign-target
							float-char target))))
			  (float-to-bits ,#'(lambda (value)
					      (let ((float-reg (make-tmp-name))
						    (float-char (if (= (expr-width expr) 32) "S" "T"))
						    (int-char (if (= (expr-width expr) 32) "L" "Q")))
						(format nil "{
reg_t ~A;
~Aemit(COMPOSE_ST~A(~A, SCRATCH_OFFSET, CONSTANT_AREA_REG));
unref_float_reg(~A);
~A = ref_integer_reg_for_writing(~A);
emit(COMPOSE_LD~A(~A, SCRATCH_OFFSET, CONSTANT_AREA_REG));
}~%"
							float-reg
							(generate-compiler float-reg value bindings) float-char float-reg
							float-reg
							target foreign-target
							int-char target))))
			  (float-to-integer ,#'(lambda (value)
						 (let* ((float-reg (make-tmp-name))
							(float-int-reg (make-tmp-name))
							(width (expr-width expr))
							(float-char (if (= width 32) "S" "T"))
							(int-char (if (= width 32) "L" "Q")))
						   (format nil "{
reg_t ~A, ~A;
~A~A = ref_float_reg_for_writing(-1);
emit(COMPOSE_CVTTQC(~A, ~A));
unref_float_reg(~A);
~A
emit(COMPOSE_ST~A(~A, SCRATCH_OFFSET, CONSTANT_AREA_REG));
unref_float_reg(~A);
~A = ref_integer_reg_for_writing(~A);
emit(COMPOSE_LD~A(~A, SCRATCH_OFFSET, CONSTANT_AREA_REG));
}~%"
							   float-reg float-int-reg
							   (generate-compiler float-reg value bindings) float-int-reg
							   float-reg float-int-reg
							   float-reg
							   (if (= width 32) (format nil "emit(COMPOSE_CVTQL(~A, ~A));~%" float-int-reg float-int-reg) "")
							   float-char float-int-reg
							   float-int-reg
							   target foreign-target
							   int-char target))))
			  (nop ,#'(lambda () (format nil "/* nop */~%")))
			  (system-call ,#'(lambda () (format nil "store_and_free_all_foreign_regs();
emit_system_call();~%")))))
	  (m-generators `((((= ("EQ" nil))
			    (< ("ULT" nil)) (<f ("LT" nil)) (<s ("LT" nil))
			    (<= ("ULE" nil))
			    (> ("ULT" t)) (>f ("LT" t)) (>s ("LT" t))
			    (>= ("ULE" t)))
			   ,#'(lambda (spec left right)
				(destructuring-bind (op swap)
				    spec
				  (let* ((left-reg (make-tmp-name))
					 (right-reg (make-tmp-name))
					 (result-reg (make-tmp-name))
					 (type (expr-type left))
					 (type-cmp-str (if (eq type 'integer) "" "T"))
					 (type-branch-str (if (eq type 'integer) "" "F")))
				    (format nil "{
reg_t ~A, ~A, ~A;
~A~A~A = ref_~A_reg_for_writing(-1);
emit(COMPOSE_CMP~A~A(~A, ~A, ~A));
unref_~A_reg(~A);
unref_~A_reg(~A);
emit_branch(COMPOSE_~ABEQ(~A, 0), ~A);
unref_~A_reg(~A);
emit_branch(COMPOSE_BR(31, 0), ~A);
}~%"
					    left-reg right-reg result-reg
					    (generate-compiler left-reg left bindings)
					    (generate-compiler right-reg right bindings)
					    result-reg (dcs type)
					    type-cmp-str op (if swap right-reg left-reg) (if swap left-reg right-reg) result-reg
					    (dcs type) left-reg
					    (dcs type) right-reg
					    type-branch-str result-reg false-label
					    (dcs type) result-reg
					    true-label)))))
			  (((logor "BIS") (logand "AND") (logxor "XOR") (ashiftr "SRA"))
			   ,#'(lambda (op left right)
				(let ((left-reg (make-tmp-name))
				      (right-reg (make-tmp-name)))
				  (format nil "{
reg_t ~A, ~A;
~A~A~A = ref_integer_reg_for_writing(~A);
emit(COMPOSE_~A(~A, ~A, ~A));
unref_integer_reg(~A);
unref_integer_reg(~A);
}~%"
					  left-reg right-reg
					  (generate-compiler left-reg left bindings)
					  (generate-compiler right-reg right bindings) target foreign-target
					  op left-reg right-reg target
					  left-reg
					  right-reg))))
			  (((neg "NEGQ") (fneg "FNEGT") (bitneg "NOT"))
			   ,#'(lambda (op value)
				(let ((value-reg (make-tmp-name))
				      (type (expr-type expr)))
				  (format nil "{
reg_t ~A;
~A~A = ref_~A_reg_for_writing(~A);
emit(COMPOSE_~A(~A, ~A));
unref_~A_reg(~A);
}~%"
					  value-reg
					  (generate-compiler value-reg value bindings) target (dcs type) foreign-target
					  op value-reg target
					  (dcs type) value-reg))))
			  (((+ "ADD~A") (+f "ADD~A") (- "SUB~A") (-f "SUB~A")
			    (* "MUL~A") (*s "MUL~A") (*f "MUL~A") (/f "DIV~A"))
			   ,#'(lambda (op left right)
				(let* ((left-reg (make-tmp-name))
				       (right-reg (make-tmp-name))
				       (type (expr-type expr))
				       (width (expr-width expr))
				       (type-char (if (eq type 'float) "T" (cadr (assoc width '((32 "L") (64 "Q"))))))
				       (op (format nil op type-char)))
				  (format nil "{
reg_t ~A, ~A;
~A~A~A = ref_~A_reg_for_writing(~A);
emit(COMPOSE_~A(~A, ~A, ~A));
unref_~A_reg(~A);
unref_~A_reg(~A);
}~%"
					  left-reg right-reg
					  (generate-compiler left-reg left bindings)
					  (generate-compiler right-reg right bindings) target (dcs type) foreign-target
					  op left-reg right-reg target
					  (dcs type) left-reg
					  (dcs type) right-reg))))
			  (((/ "DIV_UNSIGNED_32_CONST") (/s "DIV_SIGNED_32_CONST"))
			   ,#'(lambda (const left right)
				(let ((left-reg (make-tmp-name))
				      (right-reg (make-tmp-name)))
				  (format nil "{
reg_t ~A, ~A;
~A~Aemit(COMPOSE_MOV(~A, 16));
emit(COMPOSE_MOV(~A, 17));
unref_integer_reg(~A);
unref_integer_reg(~A);
emit(COMPOSE_LDQ(0, ~A * 4, CONSTANT_AREA_REG));
emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, C_STUB_CONST * 4, CONSTANT_AREA_REG));
emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));
~A = ref_integer_reg_for_writing(~A);
emit(COMPOSE_MOV(0, ~A));
}~%"
					  left-reg right-reg
					  (generate-compiler left-reg left bindings)
					  (generate-compiler right-reg right bindings)
					  left-reg right-reg
					  left-reg right-reg
					  const
					  target foreign-target
					  target)))))))
      (if (expr-constp expr)
	  (progn
	    (unless (eq (expr-type expr) 'integer)
	      (error "can only load integers~%"))
	    (format nil "~A = ref_integer_reg_for_writing(~A);
emit_load_integer_32(~A, ~A);~%"
		    target foreign-target
		    target (generate-interpreter expr nil)))
	  (let ((s-generator (cadr (assoc (expr-kind expr) s-generators))))
	    (if (null s-generator)
		(let ((m-generator (dolist (p m-generators)
				     (let ((op (cadr (assoc (expr-kind expr) (car p)))))
				       (unless (null op)
					 (return (cons op (cadr p))))))))
		  (if (null m-generator)
		      (error "no generator for ~A~%" (expr-kind expr))
		      (apply (cdr m-generator) (car m-generator) (expr-operands expr))))
		(apply s-generator (expr-operands expr))))))))

(defun generate-expr-code (expr &optional required-width (required-type 'integer))
  (generate-interpreter (generate-expr expr nil required-width required-type) nil))

(defun generate-code-for-insn (insn)
  (dolist (expr (insn-effect insn))
    (format t "~A;~%" (generate-interpreter expr nil))))

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
				  (string (generate-expr-code expr nil nil))
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

(defun generate-interpreter-file ()
  (with-open-file (out (format nil "~A_interpreter.c" (dcs *machine-name*)) :direction :output :if-exists :supersede)
    (let ((*standard-output* out)
	  (decision-tree (build-decision-tree *insns*)))
      (format t "void interpret_~A_insn (interpreter_t *intp) {~%~A insn = mem_get_32(intp, intp->pc);~%word_32 pc = intp->pc, next_pc = pc + ~A;~%"
	      (dcs *machine-name*) (c-type *insn-bits* 'integer) (/ *insn-bits* 8))
      (generate-insn-recognizer decision-tree #'(lambda (insn)
						  (when (contains-jump-p (insn-effect insn))
						    (format t "intp->have_jumped = 1;~%"))
						  (generate-code-for-insn insn)))
      (format t "intp->pc = next_pc;~%++intp->insn_count;~%}~%")
      (format t "void dump_~A_registers (interpreter_t *intp) {~%" (dcs *machine-name*))
      (dolist (reg (reverse *registers*))
	(let* ((class (register-register-class reg))
	       (name (register-name reg))
	       (class-name (register-class-name class))
	       (number (register-number reg)))
	  (if (eq (register-class-type class) 'float)
	      (format t "printf(\"~A: 0x%lx (%f)\\n\", *(word_64*)&intp->regs_~A[~A], intp->regs_~A[~A]);~%"
		      name class-name number class-name number)
	      (format t "printf(\"~A: 0x%x\\n\", intp->regs_~A[~A]);~%"
		      name class-name number))))
      (format t "printf(\"PC: 0x%x\\n\", intp->pc);~%}~%"))))
    
(defun generate-disassembler-file ()
  (with-open-file (out (format nil "~A_disassembler.c" (dcs *machine-name*)) :direction :output :if-exists :supersede)
    (let ((*standard-output* out)
	  (decision-tree (build-decision-tree *insns*)))
      (format t "void disassemble_~A_insn (~A insn, ~A addr) {~%" (dcs *machine-name*) (c-type *insn-bits* 'integer) (c-type *word-bits* 'integer))
      (generate-insn-recognizer decision-tree #'disassemble-insn)
      (format t "}~%"))))

(defun generate-compiler-file ()
  (with-open-file (out (format nil "~A_compiler.c" (dcs *machine-name*)) :direction :output :if-exists :supersede)
    (let ((*standard-output* out)
	  (decision-tree (build-decision-tree *insns*)))
      (format t "#define SCRATCH_OFFSET   ~A~%" (* 8 (length *registers*)))
      (format t "void move_~A_regs_interpreter_to_compiler (interpreter_t *intp) {~%" (dcs *machine-name*))
      (dolist (reg *registers*)
	(let* ((class (register-register-class reg))
	       (width (register-class-width class))
	       (type (register-class-type class)))
	  (format t "*(~A*)&constant_area[~A * 2] = ~A;~%"
		  (c-type width type) (register-index reg class nil)
		  (generate-interpreter (make-expr :kind 'register :type type :width width :operands (list reg nil nil) :constp nil) nil))))
      (format t "}~%void move_~A_regs_compiler_to_interpreter (interpreter_t *intp) {~%" (dcs *machine-name*))
      (dolist (reg *registers*)
	(let* ((class (register-register-class reg))
	       (width (register-class-width class))
	       (type (register-class-type class)))
	  (format t "~A = *(~A*)&constant_area[~A * 2];~%"
		  (generate-interpreter (make-expr :kind 'register :type type :width width :operands (list reg nil nil) :constp nil) nil)
		  (c-type width type) (register-index reg class nil))))
      (format t "}~%void compile_~A_insn (~A insn, ~A pc) {~%" (dcs *machine-name*) (c-type *insn-bits* 'integer) (c-type *word-bits* 'integer))
      (generate-insn-recognizer decision-tree #'(lambda (insn)
						  (dolist (expr (insn-effect insn))
						    (princ (generate-compiler nil expr nil)))))
      (format t "~%#ifdef COLLECT_STATS~%++num_translated_insns;~%#endif~%}~%"))))

(defun generate-defines-file ()
  (with-open-file (out (format nil "~A_defines.h" (dcs *machine-name*)) :direction :output :if-exists :supersede)
    (let ((*standard-output* out))
      (format t "typedef ~A ~A_word;~%" (c-type *word-bits* 'integer) (dcs *machine-name*))
      (format t "#define NUM_~A_REGISTERS ~A~%" *machine-name* (length *registers*))
      (format t "#define ~A_REGISTER_SET" *machine-name*)
      (dolist (class *register-classes*)
	(let ((type (c-type (register-class-width class) (register-class-type class))))
	  (format t " ~A regs_~A[~A];" type (register-class-name class) (length (remove-if-not #'(lambda (reg)
												    (eq (register-register-class reg) class))
												*registers*)))))
      (format t "~%"))))

(defun generate-composer-file (&key (safe t))
  (with-open-file (out (format nil "~A_composer.h" (dcs *machine-name*)) :direction :output :if-exists :supersede)
    (let ((*standard-output* out))
      (dolist (insn (reverse *insns*))
	(let* ((name (insn-name insn))
	       (known-bits (insn-known-bits insn))
	       (dont-care-bits (insn-dont-care-bits insn))
	       (non-operand-bits (bit-ior known-bits dont-care-bits))
	       (operands (mapcar #'(lambda (o) (cons o (substitute #\_ #\- (symbol-name o)))) (insn-operands insn))))
	  (format t "#define COMPOSE_~A(~{~A~^,~}) " name (mapcar #'cdr operands))
	  (when safe
	    (format t "({ ")
	    (dolist (operand operands)
	      (multiple-value-bind (begin end)
		  (lookup-field (car operand))
		(let ((c-name (cdr operand))
		      (width (1+ (- end begin))))
		  (format t "assert((~A) >= 0 && (~A) < ~A); " c-name c-name (expt 2 width))))))
	  (format t "0x~X" (bit-vector-to-integer (insn-known-bit-values insn)))
	  (dolist (operand operands)
	    (multiple-value-bind (begin end)
		(lookup-field (car operand))
	      (let ((c-name (cdr operand)))
		(format t " | ((~A) << ~A)" c-name begin))))
	  (dolist (equiv (insn-field-equivalences insn))
	    (destructuring-bind (field-name . operand-name)
		equiv
	      (multiple-value-bind (begin end)
		  (lookup-field field-name)
		(let ((c-name (cdr (assoc operand-name operands))))
		  (format t " | ((~A) << ~A)" c-name begin)))))
	  (if safe
	      (format t "; })~%")
	      (format t "~%"))))
      (dolist (mnemonic (reverse *mnemonics*))
	(format t "#define COMPOSE_~A(~{~A~^,~}) COMPOSE_~A(~{~A~^,~})~%"
		(mnemonic-name mnemonic) (mnemonic-args mnemonic)
		(car (mnemonic-substitution mnemonic)) (cdr (mnemonic-substitution mnemonic)))))))
