(new-machine 'i386)

(setf (machine-word-bits *this-machine*) 32)

(setf (machine-single-bits *this-machine*) 32)
(setf (machine-double-bits *this-machine*) 64)

(defstruct intel-insn
  name
  opcode
  extended-opcode
  mode
  effect)

(dolist (field '((mod 2) (rm 3) (reg 3) (scale 2) (index 3) (base 3) (disp8 8) (disp32 32) (imm8 8) (imm16 16) (imm32 32) (opcode-reg 3)))
  (destructuring-bind (name width)
      field
    (push (cons name (make-expr :kind 'field :type 'integer :width width :operands (list name nil nil)))
	  (machine-fields *this-machine*))))

#|
(defmacro define-component (name length)
  nil)

(define-component lock-repeat-prefix 8)
(define-component segment-override-prefix 8)
(define-component operand-size-override-prefix 8)
(define-component address-size-override-prefix 8)
(define-component opcode8 8)
(define-component opcode16 16)
(define-component modrm 8)
(define-component sib 8)
(define-component disp8 8)
(define-component disp16 16)
(define-component disp32 32)
(define-component imm8 8)
(define-component imm16 16)
(define-component imm32 32)

(define-fields
    '((rm 0 2 modrm)
      (reg 3 5 modrm)
      (xo 3 5 modrm)
      (mod 6 7 modrm)
      (base 0 2 sib)
      (index 3 5 sib)
      (scale 6 7 sib)))

(define-instruction-format
    (; (* (or lock-repeat-prefix segment-override-prefix operand-size-override-prefix address-size-override-prefix))
     (or opcode8 opcode16)
     (? modrm)
     (? sib)
     (? (or disp8 disp16 disp32))
     (? (or imm8 imm16 imm32))))
;|#

(define-register-class 'gpr 'integer 32
  '(eax ecx edx ebx esp ebp esi edi))

(define-register-class 'spr 'integer 32
  '(eflags))

(define-register-class 'fspr 'integer 16
  '(fpsw))

(define-register-array 'fpst 'float 64 8)

(define-subregisters '((al eax 0 7)
		       (ah eax 8 15)
		       (ax eax 0 15)
		       (cl ecx 0 7)
		       (cf eflags 0 0)
		       (zf eflags 6 6)
		       (sf eflags 7 7)
		       (df eflags 10 10)
		       (of eflags 11 11)
		       (fc0 fpsw 8 8)
		       (fc1 fpsw 9 9)
		       (fc2 fpsw 10 10)
		       (top fpsw 11 13)
		       (fc3 fpsw 14 14)))

;(defun asm-for-mode (insn mode &optional reg)
;  nil)

(define-insn-macro r8 ()
  (case reg
    ((0 1 2 3) (subreg 0 7 reg gpr))
    ((4 5 6 7) (subreg 8 15 (- reg (width 3 4)) gpr))
    :cse t))

(define-insn-macro r16 ()
  (subreg 0 15 reg gpr))

(define-insn-macro r32 ()
  (reg reg gpr))

(define-insn-macro sib-address ()
  (+ (case base
       ((0 1 2 3 4 6 7) (reg base gpr))
       (5 (if (= mod (width 2 0)) disp32 (reg ebp))))
     (case index
       ((0 1 2 3 5 6 7) (shiftl (reg index gpr) (zex scale)))
       (4 0))
     :cse t))

(define-insn-macro ea ()
  (case mod
    (0 (case rm
	 ((0 1 2 3 6 7) (reg rm gpr))
	 (4 (sib-address))
	 (5 disp32)))
    (1 (case rm
	 ((0 1 2 3 5 6 7) (+ (reg rm gpr) (sex disp8)))
	 (4 (+ (sex disp8) (sib-address)))))
    (2 (case rm
	 ((0 1 2 3 5 6 7) (+ (reg rm gpr) disp32))
	 (4 (+ disp32 (sib-address)))))
    :cse t))

(define-insn-macro rm8 ()
  (case mod
    ((0 1 2) (width 8 (mem (ea))))
    (3 (case rm
	 ((0 1 2 3) (subreg 0 7 rm gpr))
	 ((4 5 6 7) (subreg 8 15 (- rm (width 3 4)) gpr))))
    :cse t))

(define-insn-macro rm16 ()
  (case mod
    ((0 1 2) (width 16 (mem (ea))))
    (3 (subreg 0 15 rm gpr))
    :cse t))

(define-insn-macro rm32 ()
  (case mod
    ((0 1 2) (width 32 (mem (ea))))
    (3 (reg rm gpr))
    :cse t))

(define-insn-macro m64 ()
  (width 64 (mem (ea))))

(define-insn-macro set-sf (val w)
  (set sf (if (bit-set-p (width w val) (- w 1)) 1 0)))

(define-insn-macro set-zf (val w)
  (set zf (if (= (width w val) (width w 0)) 1 0)))

#|
(defmacro define-std-unary-insn (name modes effect)
  (prin1
   `(progn
     ,@(mappend #'(lambda (mode)
		    (destructuring-bind (mode-name opcode &optional xo)
			(if (member mode-name '(r32))
			    (destructuring-bind (reg-expr op-width)
				(cdr (assoc mode-name '((r32 (reg num gpr) 32)
							(r16 (subreg 0 15 num gpr) 16))))
			      (dotimes (num 8)
				`(define-insn ,(intern 
			       
;|#

(defmacro define-intel-insn (name mode opcode xo effect)
  (let ((effect (mapcar #'(lambda (expr) (generate-expr expr nil nil nil)) (my-macroexpand effect (machine-insn-macros *this-machine*)))))
    `(push (make-intel-insn :name ',name :opcode ',opcode :extended-opcode ,xo :mode ',mode :effect ',effect)
      (machine-insns *this-machine*))))

(defmacro define-std-simple-insn (name opcode effect)
  `(define-intel-insn ,name no-args ,opcode nil ,effect))

(defun insn-mode-params (mode)
  (let ((params (cdr (assoc mode '((m16-noprefix (rm16) nil 16 nil t)
				   (m32 (rm32) nil 32 nil t)
				   (m64 (m64) nil 64 nil t)
				   (rm8 (rm8) nil 8 nil nil)
				   (rm16 (rm16) nil 16 t nil)
				   (rm32 (rm32) nil 32 nil nil)
				   (+r16 (subreg 0 15 opcode-reg gpr) nil 16 t nil)
				   (+r32 (reg opcode-reg gpr) nil 32 nil nil)
				   (imm8 imm8 nil 8 nil nil)
				   (simm8 (width 32 (sex imm8)) nil 32 nil nil)
				   (imm16 imm16 nil 16 nil nil)
				   (imm32 imm32 nil 32 nil nil)
				   (al-imm8 al imm8 8 nil nil)
				   (ax-imm16 ax imm16 16 t nil)
				   (eax-imm32 (reg eax) imm32 32 nil nil)
				   (rm8-1 (rm8) 1 8 nil nil)
				   (rm16-1 (rm16) 1 16 t nil)
				   (rm32-1 (rm32) 1 32 nil nil)
				   (rm8-cl (rm8) cl 8 nil nil)
				   (rm16-cl (rm16) cl 16 t nil)
				   (rm32-cl (rm32) cl 32 nil nil)
				   (rm8-imm8 (rm8) imm8 8 nil nil)
				   (rm16-imm8 (rm16) imm8 16 t nil)
				   (rm16-imm16 (rm16) imm16 16 t nil)
				   (rm32-imm8 (rm32) imm8 32 nil nil)
				   (rm32-imm32 (rm32) imm32 32 nil nil)
				   (rm16-simm8 (rm16) (width 16 (sex imm8)) 16 t nil)
				   (rm32-simm8 (rm32) (width 32 (sex imm8)) 32 nil nil)
				   (rm8-r8 (rm8) (r8) 8 nil nil)
				   (rm16-r16 (rm16) (r16) 16 t nil)
				   (rm32-r32 (rm32) (r32) 32 nil nil)
				   (r8-rm8 (r8) (rm8) 8 nil nil)
				   (r16-rm16 (r16) (rm16) 16 t nil)
				   (r32-rm32 (r32) (rm32) 32 nil nil)
				   (r16-rm8 (r16) (rm8) 16 t nil)
				   (r32-rm8 (r32) (rm8) 32 nil nil)
				   (ax-moffs32 ax (width 16 (mem imm32)) 16 t nil)
				   (eax-moffs32 (reg eax) (mem imm32) 32 nil nil)
				   (moffs32-ax (width 16 (mem imm32)) ax 16 t nil)
				   (moffs32-eax (mem imm32) (reg eax) 32 nil nil)
				   (+r8-imm8 (subreg 0 7 opcode-reg gpr) imm8 8 nil nil)
				   (+r16-imm16 (subreg 0 15 opcode-reg gpr) imm16 16 t nil)
				   (+r32-imm32 (reg opcode-reg gpr) imm32 32 nil nil)
				   (+st opcode-reg nil 3 nil nil))))))
    (assert (not (null params)))
    params))

(defmacro define-std-unary-insn (name modes effect)
  `(progn
    ,@(mapcar #'(lambda (mode)
		  (destructuring-bind (mode-name opcode . xo)
		      mode
		    (destructuring-bind (op dummy op-width op-size-prefix mem-only)
			(insn-mode-params mode-name)
		      `(define-intel-insn ,name
			,mode-name ,opcode ,(car xo)
			,(subst op-width 'op-width (subst (/ op-width 8) 'op-byte-width (subst op 'dst effect)))))))
	      modes)))

(defmacro define-std-binary-insn (name modes effect)
  `(progn
    ,@(mapcar #'(lambda (mode)
		  (destructuring-bind (mode-name opcode . xo)
		      mode
		    (destructuring-bind (dst src op-width op-size-prefix mem-only)
			(insn-mode-params mode-name)
		      `(define-intel-insn ,name
			,mode-name ,opcode ,(car xo)
			,(subst op-width 'op-width (subst (/ op-width 8) 'op-byte-width (subst src 'src (subst dst 'dst effect))))))))
	      modes)))

#|
		      `(define-insn ,(intern (string-concat (symbol-name name) "-" (symbol-name mode-name)))
			(,(if (= (length opcode) 1) `(opcode8 ,(car opcode)) `(opcode16 ,(+ (ash (cadr opcode) 8) (car opcode))))
			 ,@(if xo (list `(xo ,xo)) nil))
			
			,(asm-for-mode name mode-name)))))
	      modes)))
  nil)
|#

(defun generate-intel-insn-recognizer (machine action)
  (labels ((decode-immediate-if-necessary (insn)
	     (case (intel-insn-mode insn)
	       ((al-imm8 rm8-imm8 rm16-imm8 rm32-imm8 rm16-simm8 rm32-simm8 +r8-imm8 imm8 simm8)
		(format t "imm8 = i386_decode_imm8(intp);~%"))
	       ((imm16 ax-imm16 rm16-imm16 +r16-imm16)
		(format t "imm16 = i386_decode_imm16(intp);~%"))
	       ((eax-imm32 rm32-imm32 +r32-imm32 imm32 ax-moffs32 eax-moffs32 moffs32-ax moffs32-eax)
		(format t "imm32 = i386_decode_imm32(intp);~%"))))
	   (generate-16-32-pair (insn1 insn2 modrm-decoded-p)
	     (multiple-value-bind (insn32 insn16)
		 (let ((mode (intel-insn-mode insn1)))
		   (if (or (eq mode 'no-args) (not (fourth (insn-mode-params mode))))
		       (values insn1 insn2)
		       (values insn2 insn1)))
	       (assert (not (null insn32)))
	       (when (member (intel-insn-mode insn32) '(m16-noprefix m32 m64 rm8 rm16 rm32
							rm8-1 rm16-1 rm32-1 rm8-cl rm16-cl rm32-cl
							rm8-imm8 rm16-imm8 rm16-imm16 rm32-imm8 rm32-imm32
							rm16-simm8 rm32-simm8 rm8-r8 rm16-r16 rm32-r32
							r16-rm8 r32-rm8
							r8-rm8 r16-rm16 r32-rm32))
		 (unless modrm-decoded-p
		   (format t "i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);~%"))
		 (format t "i386_decode_sib(intp, opcode2, &scale, &index, &base, &disp8, &disp32);~%"))
	       (format t "if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {~%")
	       (if (null insn16)
		   (format t "assert(0);~%")
		   (progn
		     (decode-immediate-if-necessary insn16)
		     (format t "next_pc = pc = intp->pc;~%")
		     (funcall action insn16)))
	       (format t "} else {~%")
	       (decode-immediate-if-necessary insn32)
	       (format t "next_pc = pc = intp->pc;~%")
	       (funcall action insn32)
	       (format t "}~%break;~%")))
	   (generate-case (insns modrm-decoded-p)
	     (assert (<= (length insns) 2))
	     (let ((opcode (car (last (intel-insn-opcode (first insns))))))
	       (if (member (intel-insn-mode (first insns)) '(+r16 +r32 +r8-imm8 +r16-imm16 +r32-imm32 +st))
		   (progn
		     (dotimes (i 8)
		       (format t "case ~A :~%" (+ opcode i)))
		     (format t "opcode_reg = opcode~:[2~;~] - ~A;~%" (null (second (intel-insn-opcode (first insns)))) opcode))
		   (format t "case ~A :~%" opcode))
	       (generate-16-32-pair (first insns) (second insns) modrm-decoded-p)))
	   (generate-switch-for-opcode2 (insns)
	     (format t "i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);~%")
	     (format t "switch (opcode2) {~%")
	     (dolist (opcode2 (remove-duplicates (mapcar* #'(lambda (x) (second (intel-insn-opcode x))) insns)))
	       (let ((insns2 (remove-if-not #'(lambda (x) (eql opcode2 (second (intel-insn-opcode x)))) insns)))
		 (generate-case insns2 nil)))
	     (format t "default :~%switch (reg) {~%")
	     (dolist (extended-opcode (remove-duplicates (mapcar* #'intel-insn-extended-opcode insns)))
	       (let ((insnsx (remove-if-not #'(lambda (x) (eql extended-opcode (intel-insn-extended-opcode x))) insns)))
		 (format t "case ~A :~%" extended-opcode)
		 (generate-16-32-pair (first insnsx) (second insnsx) t)))
	     (format t "default :~%assert(0);~%}~%}~%break;~%")
	     (assert (null (remove-if-not #'(lambda (x) (and (null (second (intel-insn-opcode x)))
							     (null (intel-insn-extended-opcode x))))
					  insns)))))
    (format t "i386_decode_opcode(intp, &prefix_flags, &opcode, &opcode2);~%switch (opcode) {~%")
    (dolist (opcode (remove-duplicates (mapcar #'(lambda (x) (first (intel-insn-opcode x))) (machine-insns machine))))
      (let ((insns (remove-if-not #'(lambda (x) (= opcode (first (intel-insn-opcode x)))) (machine-insns machine))))
	(if (every #'(lambda (x) (and (= (length (intel-insn-opcode x)) 1)
				      (null (intel-insn-extended-opcode x)))) insns)
	    (generate-case insns nil)
	    (progn
	      (assert (notany #'(lambda (x) (and (= (length (intel-insn-opcode x)) 1)
						 (null (intel-insn-extended-opcode x)))) insns))
	      (format t "case ~A :~%" opcode)
	      (generate-switch-for-opcode2 insns)))))
    (format t "default:~%assert(0);~%}~%")))

(defun generate-intel-interpreter ()
  (with-open-file (out "i386_interpreter.c" :direction :output :if-exists :supersede)
    (let* ((*standard-output* out)
	   (*cse-bindings* nil)
	   (*insn-field-accessor* #'(lambda (name begin end) (dcs name)))
	   (interpreter (with-output-to-string (str-out)
			  (let ((*standard-output* str-out))
			    (format t "void interpret_i386_insn (interpreter_t *intp) {
word_8 opcode, opcode2;
word_32 pc, next_pc;
int prefix_flags;~%")
			    (generate-intel-insn-recognizer *i386* #'(lambda (insn)
								       (when (contains-jump-p (intel-insn-effect insn))
									 (format t "intp->have_jumped = 1;~%"))
								       (dolist (expr (intel-insn-effect insn))
									 (format t "~A;~%" (generate-interpreter expr nil)))))
			    (format t "intp->pc = next_pc;~%}~%")))))
      (format t "static word_8 mod, reg, rm, scale, index, base, disp8, opcode_reg, imm8;
static word_16 imm16;
static word_32 disp32, imm32;~%")
      (generate-cse-bindings-code)
      (princ interpreter)
      (generate-register-dumper *i386*))))

(defun generate-intel-compiler ()
  (with-open-file (out (format nil "i386_compiler.c") :direction :output :if-exists :supersede)
    (let ((*standard-output* out)
	  (*insn-field-accessor* #'(lambda (name begin end) (dcs name)))
	  (*generated-exprs* nil)
	  (*analyzed-bits-register* (lookup-register 'eflags)))
      (generate-registers-and-insns-code *i386*)
      (format t "static word_8 mod, reg, rm, scale, index, base, disp8, opcode_reg, imm8;
static word_16 imm16;
static word_32 disp32, imm32, pc;~%")
      (let ((main (with-output-to-string (out)
		    (let ((*standard-output* out))
		      (format t "void compile_i386_insn (interpreter_t *intp, word_32 to_be_killed) {
word_8 opcode, opcode2;
word_32 next_pc;
int prefix_flags;
void **env = 0;~%")
		      (generate-intel-insn-recognizer *i386* #'(lambda (insn)
								 (dolist (expr (intel-insn-effect insn))
								   (princ (generate-compiler nil expr nil)))
								 (format t "generated_insn_index = ~A;~%" (position insn (machine-insns *i386*)))))
		      (format t "~%#ifdef COLLECT_STATS~%++num_translated_insns;~%#endif~%intp->pc = next_pc;~%}~%")))))
	(dolist (func *generated-exprs*)
	  (destructuring-bind (dummy1 dummy2 proto dummy3)
	      func
	    (format t "~A;~%" proto)))
	(dolist (func *generated-exprs*)
	  (destructuring-bind ((dummy1 . expr) dummy2 proto code)
	      func
	    (format t "~A~%/*~%~A~%*/~%{~%~A}~%~%" proto (expr-to-sexp expr) code)))
	(princ main)
	(values)))))

(defun generate-intel-disassembler ()
  (labels ((op-disasm (op)
	     (cadr (assoc op '((1 "fputs(\"1\", stdout);")
			       ((r8) "i386_disassemble_r8(stdout, reg);")
			       (al "fputs(\"al\", stdout);")
			       (cl "fputs(\"cl\", stdout);")
			       ((r16) "i386_disassemble_r16(stdout, reg);")
			       (ax "fputs(\"ax\", stdout);")
			       ((r32) "i386_disassemble_r32(stdout, reg);")
			       ((reg eax) "fputs(\"eax\", stdout);")
			       ((rm8) "i386_disassemble_rm8(stdout, mod, rm, scale, index, base, disp8, disp32);")
			       ((rm16) "i386_disassemble_rm16(stdout, mod, rm, scale, index, base, disp8, disp32);")
			       ((rm32) "i386_disassemble_rm32(stdout, mod, rm, scale, index, base, disp8, disp32);")
			       ((m64) "i386_disassemble_rm32(stdout, mod, rm, scale, index, base, disp8, disp32);")
			       ((subreg 0 7 opcode-reg gpr) "i386_disassemble_r8(stdout, opcode_reg);")
			       ((subreg 0 15 opcode-reg gpr) "i386_disassemble_r16(stdout, opcode_reg);")
			       ((reg opcode-reg gpr) "i386_disassemble_r32(stdout, opcode_reg);")
			       (imm8 "printf(\"$0x%x\", imm8);")
			       ((width 16 (sex imm8)) "printf(\"$0x%x\", imm8 & 0x80 ? 0xff00 | imm8 : imm8);")
			       ((width 32 (sex imm8)) "printf(\"$0x%x\", imm8 & 0x80 ? 0xffffff00 | imm8 : imm8);")
			       (simm8 "printf(\"$0x%x\", imm8 & 0x80 ? 0xffffff00 | imm8 : imm8);")
			       (imm16 "printf(\"$0x%x\", imm16);")
			       (imm32 "printf(\"$0x%x\", imm32);")
			       ((mem imm32) "printf(\"0x%x\", imm32);")
			       ((width 16 (mem imm32)) "printf(\"0x%x\", imm32);")
			       (opcode-reg "printf(\"%d\", opcode_reg);"))
			  :test #'equal))))
    (with-open-file (out (format nil "i386_disassembler.c") :direction :output :if-exists :supersede)
      (let ((*standard-output* out)
	    (*insn-field-accessor* #'(lambda (name begin end) (dcs name))))
	(format t "void disassemble_i386_insn (interpreter_t *intp) {
word_8 opcode, opcode2;
word_32 pc, next_pc;
int prefix_flags;
word_8 mod, reg, rm, scale, index, base, disp8, opcode_reg, imm8;
word_16 imm16;
word_32 disp32, imm32;~%")
	(generate-intel-insn-recognizer *i386* #'(lambda (insn)
						   (let ((name (intel-insn-name insn))
							 (mode (intel-insn-mode insn)))
						     (if (eq mode 'no-args)
							 (format t "fputs(\"~A\n\", stdout);~%" (dcs name))
							 (destructuring-bind (op1 op2 width size-prefix mem-only)
							     (insn-mode-params mode)
							   (let ((op1-disasm (op-disasm op1))
								 (op2-disasm (op-disasm op2)))
							     (if (null op2)
								 (progn
								   (assert (not (null op1-disasm)))
								   (format t "fputs(\"~A \", stdout);~%~A~%" (dcs name) op1-disasm))
								 (progn
								   (assert (and (not (null op1-disasm)) (not (null op2-disasm))))
								   (format t "fputs(\"~A \", stdout);~%~A~%fputs(\",\", stdout);~%~A~%"
									   (dcs name) op1-disasm op2-disasm)))))))))
	(format t "intp->pc = next_pc;~%}~%")))))

(defun generate-intel-livenesser ()
  (with-open-file (out (format nil "i386_livenesser.c") :direction :output :if-exists :supersede)
    (let ((*standard-output* out)
	  (*insn-field-accessor* #'(lambda (name begin end) (dcs name))))
      (format t "void liveness_i386_insn (interpreter_t *intp, word_32 *_live, word_32 *_killed) {
word_8 opcode, opcode2;
word_32 pc, next_pc;
int prefix_flags;
word_8 mod, reg, rm, scale, index, base, disp8, opcode_reg, imm8;
word_16 imm16;
word_32 disp32, imm32;
word_32 live = *_live, killed = 0;~%")
      (generate-intel-insn-recognizer *i386* #'(lambda (insn)
						 (generate-live-killed (lookup-register 'eflags) (intel-insn-effect insn))))

      (format t "*_live = live;
*_killed = killed;
intp->pc = next_pc;~%}~%"))))

(defun generate-intel-jump-analyzer ()
  (with-open-file (out (format nil "i386_jump_analyzer.c") :direction :output :if-exists :supersede)
    (let ((*standard-output* out)
	  (*insn-field-accessor* #'(lambda (name begin end) (dcs name))))
      (format t "void jump_analyze_i386_insn (interpreter_t *intp, int *_num_targets, word_32 *targets, int *_can_fall_through, int *_can_jump_indirectly) {
word_8 opcode, opcode2;
word_32 pc, next_pc;
int prefix_flags;
word_8 mod, reg, rm, scale, index, base, disp8, opcode_reg, imm8;
word_16 imm16;
word_32 disp32, imm32;
int num_targets = 0;
int can_fall_through, can_jump_indirectly;~%")
      (generate-intel-insn-recognizer *i386* #'(lambda (insn)
						 (let* ((exprs (intel-insn-effect insn))
							(can-jump-indirectly (can-jump-indirectly-p exprs)))
						   (format t "can_fall_through = ~:[0~;1~];
can_jump_indirectly = ~:[0~;1~];~%"
							   (can-fall-through-p exprs) can-jump-indirectly)
						   (when (not can-jump-indirectly)
						     (dolist (expr exprs)
						       (generate-direct-jump-targets expr))))))
      (format t "intp->pc = next_pc;
*_num_targets = num_targets;
*_can_fall_through = can_fall_through;
*_can_jump_indirectly = can_jump_indirectly;~%}~%"))))

(defun generate-all-intel-files ()
  (generate-intel-interpreter)
  (generate-intel-compiler)
  (generate-intel-disassembler)
  (generate-intel-livenesser)
  (generate-intel-jump-analyzer))

(define-std-binary-insn add
    ((al-imm8 (#x04))
     (ax-imm16 (#x05))
     (eax-imm32 (#x05))
     (rm8-imm8 (#x80) 0)
     (rm16-imm16 (#x81) 0)
     (rm32-imm32 (#x81) 0)
     (rm16-simm8 (#x83) 0)
     (rm32-simm8 (#x83) 0)
     (rm8-r8 (#x00))
     (rm16-r16 (#x01))
     (rm32-r32 (#x01))
     (r8-rm8 (#x02))
     (r16-rm16 (#x03))
     (r32-rm32 (#x03)))
  ((set of (+overflow dst src))
   (set cf (+carry dst src))
   (set dst (+ dst src))
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-binary-insn adc
    ((al-imm8 (#x14))
     (ax-imm16 (#x15))
     (eax-imm32 (#x15))
     (rm8-imm8 (#x80) 2)
     (rm16-imm16 (#x81) 2)
     (rm32-imm32 (#x81) 2)
     (rm16-simm8 (#x83) 2)
     (rm32-simm8 (#x83) 2)
     (rm8-r8 (#x10))
     (rm16-r16 (#x11))
     (rm32-r32 (#x11))
     (r8-rm8 (#x12))
     (r16-rm16 (#x13))
     (r32-rm32 (#x13)))
  ((set of (logor (+overflow dst src) (+overflow (+ dst src) (width op-width (zex cf)))))
   (set cf (logor (+carry dst src) (+carry (+ dst src) (width op-width (zex cf)))))
   (set dst (+ (+ dst src) (width op-width (zex cf))))
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-binary-insn and
    ((al-imm8 (#x24))
     (ax-imm16 (#x25))
     (eax-imm32 (#x25))
     (rm8-imm8 (#x80) 4)
     (rm16-imm16 (#x81) 4)
     (rm32-imm32 (#x81) 4)
     (rm16-simm8 (#x83) 4)
     (rm32-simm8 (#x83) 4)
     (rm8-r8 (#x20))
     (rm16-r16 (#x21))
     (rm32-r32 (#x21))
     (r8-rm8 (#x22))
     (r16-rm16 (#x23))
     (r32-rm32 (#x23)))
  ((set dst (logand dst src))
   (set of 0)
   (set cf 0)
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-unary-insn call
    ((imm32 (#xe8)))
  ((set (reg esp) (- (reg esp) 4))
   (set (mem (reg esp)) pc)
   (jump-relative dst)))

(define-std-unary-insn call
    ((rm32 (#xff) 2))
  ((set (reg esp) (- (reg esp) 4))
   (set (mem (reg esp)) pc)
   (jump-absolute dst)))

(define-std-simple-insn cdq (#x99)
  ((if (bit-set-p (reg eax) 31)
       (set (reg edx) (mask 0 31))
       (set (reg edx) 0))))

(define-std-simple-insn cld (#xfc)
  ((set df 0)))

(define-std-binary-insn cmp
    ((al-imm8 (#x3c))
     (ax-imm16 (#x3d))
     (eax-imm32 (#x3d))
     (rm8-imm8 (#x80) 7)
     (rm16-imm16 (#x81) 7)
     (rm32-imm32 (#x81) 7)
     (rm16-simm8 (#x83) 7)
     (rm32-simm8 (#x83) 7)
     (rm8-r8 (#x38))
     (rm16-r16 (#x39))
     (rm32-r32 (#x39))
     (r8-rm8 (#x3a))
     (r16-rm16 (#x3b))
     (r32-rm32 (#x3b)))
  ((let ((temp (width op-width (- dst src))))
     (set cf (-carry dst (width op-width src)))
     (set zf (if (= temp (width op-width 0)) 1 0))
     (set sf (if (bit-set-p temp (- op-width 1)) 1 0))
     (set of (+overflow (width op-width dst) (width op-width (+ (bitneg src) 1)))))))

(define-std-unary-insn dec
    ((rm8 (#xfe) 1)
     (rm16 (#xff) 1)
     (rm32 (#xff) 1)
     (+r16 (#x48))
     (+r32 (#x48)))
  ((set of (+overflow dst (width op-width (bitneg 0))))
   (set dst (- dst 1))
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-unary-insn div
    ((rm8 (#xf6) 6))
  ((let ((dividend ax)
	 (divisor (width 16 (zex (rm8)))))
     (set al (promote 8 (logand (/ dividend divisor) (width 16 255))))
     (set ah (promote 8 (% dividend divisor))))))

(define-std-unary-insn div
    ((rm32 (#xf7) 6))
  ((let ((dividend (width 64 (logor (shiftl (zex (reg edx)) 32) (zex (reg eax)))))
	 (divisor (width 64 (zex (rm32)))))
     (set (reg eax) (promote 32 (logand (/ dividend divisor) (width 64 (mask 0 31)))))
     (set (reg edx) (promote 32 (% dividend divisor))))))

(define-std-unary-insn fadd
    ((m64 (#xdc) 0))
  ((set (array-reg fpst top) (+f (array-reg fpst top) (bits-to-double dst)))))

(define-std-unary-insn fcom
    ((m64 (#xdc) 2))
  ((set fc0 (if (<f (array-reg fpst top) (bits-to-double dst)) 1 0))
   (set fc2 0)
   (set fc3 (if (=f (array-reg fpst top) (bits-to-double dst)) 1 0))))

(define-std-unary-insn fcom
    ((+st (#xd8 #xd0)))
  ((set fc0 (if (<f (array-reg fpst top) (array-reg fpst (+ top dst))) 1 0))
   (set fc2 0)
   (set fc3 (if (=f (array-reg fpst top) (array-reg fpst (+ top dst))) 1 0))))

(define-std-simple-insn fcomp (#xd8 #xd9)
  ((set fc0 (if (<f (array-reg fpst top) (array-reg fpst (+ top (width 3 1)))) 1 0))
   (set fc2 0)
   (set fc3 (if (=f (array-reg fpst top) (array-reg fpst (+ top (width 3 1)))) 1 0))
   (set top (+ top 1))))

(define-std-unary-insn fidivr
    ((m32 (#xda) 7))
  ((set (array-reg fpst top) (/f (integer-to-double dst) (array-reg fpst top)))))

(define-std-unary-insn fild
    ((m16-noprefix (#xdf) 0)
     (m32 (#xdb) 0))			;FIXME: m64
  ((set top (- top 1))
   (set (array-reg fpst top) (integer-to-double dst))))

(define-std-unary-insn fld
    ((m64 (#xdd) 0))
  ((set top (- top 1))
   (set (array-reg fpst top) (bits-to-double dst))))

(define-std-unary-insn fld
    ((+st (#xd9 #xc0)))
  ((set (array-reg fpst (- top (width 3 1))) (array-reg fpst (+ top dst)))
   (set top (- top 1))))

(define-std-unary-insn fldcw
    ((m16-noprefix (#xd9) 5))
  ((set (reg fpsw) dst)))

(define-std-simple-insn fnstsw (#xdf #xe0)
  ((set ax (reg fpsw))))

(define-std-unary-insn fstcw
    ((m16-noprefix (#xd9) 7))
  ((set dst (reg fpsw))))

(define-std-unary-insn fstp
    ((m64 (#xdd) 3))
  ((set dst (double-to-bits (array-reg fpst top)))
   (set top (+ top 1))))

(define-std-unary-insn fstp
    ((+st (#xdd #xd8)))
  ((set (array-reg fpst (+ top dst)) (array-reg fpst top))
   (set top (+ top 1))))

(define-std-unary-insn idiv
    ((rm8 (#xf6) 7))
  ((let ((dividend al)
	 (divisor dst))
     (set al (/s dividend divisor))
     (set ah (%s dividend divisor)))))

(define-std-unary-insn idiv
    ((rm32 (#xf7) 7))
  ((let ((dividend (reg eax))
	 (divisor dst))
     (set (reg eax) (/s dividend divisor))
     (set (reg edx) (%s dividend divisor)))))

(define-std-unary-insn imul
    ((rm8 (#xf6) 5))
  ((set ax (* (sex dst) (sex al)))
   (set cf (if (or (= ah (width 8 0)) (= ah (width 8 255))) 0 1))
   (set of cf)))

(define-std-unary-insn imul
    ((rm32 (#xf7) 5))
  ((let ((op1 (reg eax))
	 (op2 dst))
     (set (reg eax) (* op1 op2))
     (set (reg edx) (promote 32 (width 64 (shiftr (* (sex op1) (sex op2)) 32))))
     (set cf (if (or (= (reg edx) 0) (= (reg edx) (bitneg 0))) 0 1))
     (set of cf))))

(define-std-binary-insn imul
    ((r16-rm16 (#x0f #xaf))
     (r32-rm32 (#x0f #xaf)))
  ((let ((temp (promote 32 (width 64 (shiftr (* (sex src) (sex dst)) 32)))))
     (set cf (if (or (= temp 0) (= temp (bitneg 0))) 0 1))
     (set of cf))
   (set dst (* src dst))))

(define-std-unary-insn inc
    ((rm8 (#xfe) 0)
     (rm16 (#xff) 0)
     (rm32 (#xff) 0)
     (+r16 (#x40))
     (+r32 (#x40)))
  ((set of (+overflow dst (width op-width 1)))
   (set dst (+ dst 1))
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-unary-insn int
    ((imm8 (#xcd)))
  ((syscall)))

(define-std-unary-insn ja
    ((simm8 (#x77))
     (imm32 (#x0f #x87)))
  ((if (and (= cf (width 1 0)) (= zf (width 1 0)))
       (jump-relative dst)
       (nop))))

(define-std-unary-insn jae
    ((simm8 (#x73))
     (imm32 (#x0f #x83)))
  ((if (= cf (width 1 0))
       (jump-relative dst)
       (nop))))

(define-std-unary-insn jb
    ((simm8 (#x72))
     (imm32 (#x0f #x82)))
  ((if (= cf (width 1 1))
       (jump-relative dst)
       (nop))))

(define-std-unary-insn jbe
    ((simm8 (#x76))
     (imm32 (#x0f #x86)))
  ((if (or (= cf (width 1 1)) (= zf (width 1 1)))
       (jump-relative dst)
       (nop))))

(define-std-unary-insn je
    ((simm8 (#x74))
     (imm32 (#x0f #x84)))
  ((if (= zf (width 1 1))
       (jump-relative dst)
       (nop))))

(define-std-unary-insn jg
    ((simm8 (#x7f))
     (imm32 (#x0f #x8f)))
  ((if (and (= zf (width 1 0)) (= sf of))
       (jump-relative dst)
       (nop))))

(define-std-unary-insn jge
    ((simm8 (#x7d))
     (imm32 (#x0f #x8d)))
  ((if (= sf of)
       (jump-relative dst)
       (nop))))

(define-std-unary-insn jl
    ((simm8 (#x7c))
     (imm32 (#x0f #x8c)))
  ((if (= sf of)
       (nop)
       (jump-relative dst))))

(define-std-unary-insn jle
    ((simm8 (#x7e))
     (imm32 (#x0f #x8e)))
  ((if (or (= zf (width 1 1)) (not (= sf of)))
       (jump-relative dst)
       (nop))))

(define-std-unary-insn jmp
    ((simm8 (#xeb))
     (imm32 (#xe9)))
  ((jump-relative dst)))

(define-std-unary-insn jmp
    ((rm32 (#xff) 4))
  ((jump-absolute dst)))

(define-std-unary-insn jne
    ((simm8 (#x75))
     (imm32 (#x0f #x85)))
  ((if (= zf (width 1 0))
       (jump-relative dst)
       (nop))))

(define-std-unary-insn jns
    ((simm8 (#x79))
     (imm32 (#x0f #x89)))
  ((if (= sf (width 1 0))
       (jump-relative dst)
       (nop))))

(define-std-binary-insn lea
    ((r32-rm32 (#x8d)))
  ((set dst (ea))))

(define-std-binary-insn mov
    ((rm8-r8 (#x88))
     (rm16-r16 (#x89))
     (rm32-r32 (#x89))
     (r8-rm8 (#x8a))
     (r16-rm16 (#x8b))
     (r32-rm32 (#x8b))
     (ax-moffs32 (#xa1))
     (eax-moffs32 (#xa1))
     (moffs32-ax (#xa3))
     (moffs32-eax (#xa3))
     (+r8-imm8 (#xb0))
     (+r16-imm16 (#xb8))
     (+r32-imm32 (#xb8))
     (rm8-imm8 (#xc6) 0)
     (rm16-imm16 (#xc7) 0)
     (rm32-imm32 (#xc7) 0))
  ((set dst src)))

(define-std-binary-insn movsx
    ((r16-rm8 (#x0f #xbe))
     (r32-rm8 (#x0f #xbe)))
  ((set dst (sex src))))

(define-std-binary-insn movzx
    ((r16-rm8 (#x0f #xb6))
     (r32-rm8 (#x0f #xb6)))
  ((set dst (zex src))))

(define-std-unary-insn mul
    ((rm8 (#xf6) 4))
  ((set ax (* (zex dst) (zex al)))
   (set cf (if (= ah (width 8 0)) 0 1))
   (set of cf)))

(define-std-unary-insn mul
    ((rm32 (#xf7) 4))
  ((let ((op1 (reg eax))
	 (op2 dst))
     (set (reg eax) (* op1 op2))
     (set (reg edx) (promote 32 (width 64 (shiftr (* (zex op1) (zex op2)) 32))))
     (set cf (if (= (reg edx) 0) 0 1))
     (set of cf))))

(define-std-unary-insn neg
    ((rm8 (#xf6) 3)
     (rm16 (#xf7) 3)
     (rm32 (#xf7) 3))
  ((set cf (if (= dst (width op-width 0)) 0 1))
   (set of (if (= dst (width op-width (bitneg 0))) 1 0)) ;is this correct?
   (set dst (neg dst))
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-simple-insn nop (#x90)
  ((nop)))

(define-std-unary-insn not
    ((rm8 (#xf6) 2)
     (rm16 (#xf7) 2)
     (rm32 (#xf7) 2))
  ((set dst (bitneg dst))))

(define-std-binary-insn or
    ((al-imm8 (#x0c))
     (ax-imm16 (#x0d))
     (eax-imm32 (#x0d))
     (rm8-imm8 (#x80) 1)
     (rm16-imm16 (#x81) 1)
     (rm32-imm32 (#x81) 1)
     (rm16-simm8 (#x83) 1)
     (rm32-simm8 (#x83) 1)
     (rm8-r8 (#x08))
     (rm16-r16 (#x09))
     (rm32-r32 (#x09))
     (r8-rm8 (#x0a))
     (r16-rm16 (#x0b))
     (r32-rm32 (#x0b)))
  ((set dst (logor dst src))
   (set cf 0)
   (set of 0)
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-unary-insn pop
    ((m32 (#x8f) 0)
     (+r32 (#x58)))
  ((set dst (mem (reg esp)))
   (set (reg esp) (+ (reg esp) op-byte-width))))

(define-std-unary-insn push
    ((rm32 (#xff) 6)
     (+r32 (#x50))
     (simm8 (#x6a))
     (imm32 (#x68)))
  ((set (mem (- (reg esp) 4)) dst)
   (set (reg esp) (- (reg esp) 4))))

(define-std-simple-insn repne_scasb (#xf2 #xae)	;FIXME: flags!
  ((if (= (reg ecx) 0)
       (nop)
       (dowhile (and (= zf (width 1 0)) (not (= (reg ecx) 0)))
		(let ((temp (width 8 (- al (mem (reg edi))))))
		  (set cf (-carry al (width 8 (mem (reg edi)))))
		  (set zf (if (= temp (width 8 0)) 1 0))
		  (set sf (if (bit-set-p temp 7) 1 0)))
		(set (reg edi) (+ (reg edi) 1))
		(set (reg ecx) (- (reg ecx) 1))))))

(define-std-simple-insn rep_movsb (#xf3 #xa4) ;FIXME: flags!
  ((if (= (reg ecx) 0)
       (nop)
       (dowhile (not (= (reg ecx) 0))
		(set (width 8 (mem (reg edi))) (mem (reg esi)))
		(set (reg esi) (+ (reg esi) 1))
		(set (reg edi) (+ (reg edi) 1))
		(set (reg ecx) (- (reg ecx) 1))))))

(define-std-simple-insn rep_movsd (#xf3 #xa5) ;FIXME: flags!
  ((if (= (reg ecx) 0)
       (nop)
       (dowhile (not (= (reg ecx) 0))
		(set (mem (reg edi)) (mem (reg esi)))
		(set (reg esi) (+ (reg esi) 4))
		(set (reg edi) (+ (reg edi) 4))
		(set (reg ecx) (- (reg ecx) 1))))))

(define-std-simple-insn ret (#xc3)
  ((set (reg esp) (+ (reg esp) 4))
   (jump-absolute (mem (- (reg esp) 4)))))

(define-std-unary-insn ret
    ((imm16 (#xc2)))
  ((set (reg esp) (+ (+ (reg esp) (zex dst)) 4))
   (jump-absolute (mem (- (- (reg esp) (zex dst)) 4)))))

(define-std-binary-insn sar		;FIXME: cf, of
    ((rm8-1 (#xd0) 7)
     (rm8-cl (#xd2) 7)
     (rm8-imm8 (#xc0) 7)
     (rm16-1 (#xd1) 7)
     (rm16-cl (#xd3) 7)
     (rm16-imm8 (#xc1) 7)
     (rm32-1 (#xd1) 7)
     (rm32-cl (#xd3) 7)
     (rm32-imm8 (#xc1) 7))
  ((set dst (ashiftr dst (zex (width 8 (logand src #x1f)))))
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-unary-insn sete
    ((rm8 (#x0f #x94)))
  ((set dst (zex zf))))

(define-std-unary-insn setg
    ((rm8 (#x0f #x9f)))
  ((set dst (if (and (= zf (width 1 0)) (= sf of)) 1 0))))

(define-std-unary-insn setl
    ((rm8 (#x0f #x9c)))
  ((set dst (if (= sf of) 0 1))))

(define-std-unary-insn setle
    ((rm8 (#x0f #x9e)))
  ((set dst (if (or (= zf (width 1 1)) (not (= sf of))) 1 0))))

(define-std-unary-insn setne
    ((rm8 (#x0f #x95)))
  ((set dst (logxor (zex zf) 1))))

(define-std-binary-insn shl		;FIXME: cf, of
    ((rm8-1 (#xd0) 4)
     (rm8-cl (#xd2) 4)
     (rm8-imm8 (#xc0) 4)
     (rm16-1 (#xd1) 4)
     (rm16-cl (#xd3) 4)
     (rm16-imm8 (#xc1) 4)
     (rm32-1 (#xd1) 4)
     (rm32-cl (#xd3) 4)
     (rm32-imm8 (#xc1) 4))
  ((set dst (shiftl dst (zex (width 8 (logand src #x1f)))))
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-binary-insn shr		;FIXME: cf, of
    ((rm8-1 (#xd0) 5)
     (rm8-cl (#xd2) 5)
     (rm8-imm8 (#xc0) 5)
     (rm16-1 (#xd1) 5)
     (rm16-cl (#xd3) 5)
     (rm16-imm8 (#xc1) 5)
     (rm32-1 (#xd1) 5)
     (rm32-cl (#xd3) 5)
     (rm32-imm8 (#xc1) 5))
  ((set dst (shiftr dst (zex (width 8 (logand src #x1f)))))
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-binary-insn sub
    ((al-imm8 (#x2c))
     (ax-imm16 (#x2d))
     (eax-imm32 (#x2d))
     (rm8-imm8 (#x80) 5)
     (rm16-imm16 (#x81) 5)
     (rm32-imm32 (#x81) 5)
     (rm16-simm8 (#x83) 5)
     (rm32-simm8 (#x83) 5)
     (rm8-r8 (#x28))
     (rm16-r16 (#x29))
     (rm32-r32 (#x29))
     (r8-rm8 (#x2a))
     (r16-rm16 (#x2b))
     (r32-rm32 (#x2b)))
  ((set cf (-carry dst (width op-width src)))
   (set of (+overflow (width op-width dst) (width op-width (+ (bitneg src) 1))))
   (set dst (- dst src))
   (set-sf dst op-width)
   (set-zf dst op-width)))

(define-std-binary-insn test
    ((al-imm8 (#xa8))
     (ax-imm16 (#xa9))
     (eax-imm32 (#xa9))
     (rm8-imm8 (#xf6) 0)
     (rm16-imm16 (#xf7) 0)
     (rm32-imm32 (#xf7) 0)
     (rm8-r8 (#x84))
     (rm16-r16 (#x85))
     (rm32-r32 (#x85)))
  ((let ((temp (logand dst src)))
     (set cf 0)
     (set of 0)
     (set-sf temp op-width)
     (set-zf temp op-width))))

(define-std-binary-insn xor
    ((al-imm8 (#x34))
     (ax-imm16 (#x35))
     (eax-imm32 (#x35))
     (rm8-imm8 (#x80) 6)
     (rm16-imm16 (#x81) 6)
     (rm32-imm32 (#x81) 6)
     (rm16-simm8 (#x83) 6)
     (rm32-simm8 (#x83) 6)
     (rm8-r8 (#x30))
     (rm16-r16 (#x31))
     (rm32-r32 (#x31))
     (r8-rm8 (#x32))
     (r16-rm16 (#x33))
     (r32-rm32 (#x33)))
  ((set dst (logxor dst src))
   (set cf 0)
   (set of 0)
   (set-sf dst op-width)
   (set-zf dst op-width)))

(defparameter *i386* *this-machine*)
