(new-machine 'i386)

(defparameter *word-bits* 32)

(defparameter *single-bits* 32)
(defparameter *double-bits* 64)

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

#|
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

(defun asm-for-mode (insn mode &optional reg)
  nil)

(define-insn-macro r8 ()
  (case reg
    ((0 1 2 3) (subreg 0 7 reg gpr))
    ((4 5 6 7) (subreg 8 15 (- reg 4) gpr))))

(define-insn-macro r32 ()
  (reg reg gpr))

(define-insn-macro rm8 ()
  (case mod
    (0 (case rm
	 ((0 1 2 3 6 7) (width 8 (mem (reg rm gpr))))
	 (4 (width 8 (mem (sob-address))))
	 (5 (width 8 (mem (+ disp32 (sob-address)))))))
    (1 (case rm
	 ((0 1 2 3 5 6 7) (width 8 (mem (+ (reg rm gpr) (sex disp8)))))
	 (4 (width 8 (mem (+ disp8 (sob-address)))))))
    (2 (case rm
	 ((0 1 2 3 5 6 7) (width 8 (mem (+ (reg rm gpr) disp32))))
	 (4 (width 8 (mem (+ disp32 (sob-address)))))))
    (3 (subreg 0 7 rm gpr))))

(define-insn-macro m32 ()
  (case mod
    (0 (case rm
	 ((0 1 2 3 6 7) (width 32 (mem (reg rm gpr))))
	 (4 (width 32 (mem (sob-address))))
	 (5 (width 32 (mem (+ disp32 (sob-address)))))))
    (1 (case rm
	 ((0 1 2 3 5 6 7) (width 32 (mem (+ (reg rm gpr) (sex disp8)))))
	 (4 (width 32 (mem (+ disp8 (sob-address)))))))
    (2 (case rm
	 ((0 1 2 3 5 6 7) (width 32 (mem (+ (reg rm gpr) disp32))))
	 (4 (width 32 (mem (+ disp32 (sob-address)))))))))

(define-insn-macro rm32 ()
  (case mod
    ((0 1 2) (m32))
    (3 (reg rm gpr))))

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
			       

  

(defmacro define-std-binary-insn (name modes effect)
  (prin1
  `(progn
    ,@(mapcar #'(lambda (mode)
		  (destructuring-bind (mode-name opcode &optional xo)
		      mode
		    (destructuring-bind (dst src op-width)
			(cdr (assoc mode-name '((al-imm8 al imm8 8) (eax-imm32 (reg eax) imm32 32) (rm8-imm8 (rm8) imm8 8) (rm32-imm32 (rm32) imm32 32)
						(rm32-simm8 (rm32) (sex imm8) 32) (rm8-r8 (rm8) (r8) 8) (rm32-r32 (rm32) (r32) 32) (r8-rm8 (r8) (rm8) 8)
						(r32-rm32 (r32) (rm32) 32))))
		      `(define-insn ,(intern (string-concat (symbol-name name) "-" (symbol-name mode-name)))
			(,(if (= (length opcode) 1) `(opcode8 ,(car opcode)) `(opcode16 ,(+ (ash (cadr opcode) 8) (car opcode))))
			 ,@(if xo (list `(xo ,xo)) nil))
			,(subst op-width 'op-width (subst src 'src (subst dst 'dst effect)))
			,(asm-for-mode name mode-name)))))
	      modes)))
  nil)

(define-std-binary-insn add
    ((al-imm8 (#x04))
     (eax-imm32 (#x05))
     (rm8-imm8 (#x80) 0)
     (rm32-imm32 (#x81) 0)
     (rm32-simm8 (#x83) 0)
     (rm8-r8 (#x00))
     (rm32-r32 (#x01))
     (r8-rm8 (#x02))
     (r32-rm32 (#x03)))
  ((set dst (+ dst src))))

(define-std-unary-insn pop
    ((m32 (#8f) 0)
     (+r32 (#58)))
  ((set dst (mem (reg esp)))
   (set (reg esp) (+ (reg esp) op-width))))
