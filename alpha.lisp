(new-machine 'alpha 'little nil)

(setf (machine-insn-bits *this-machine*) 32)
(setf (machine-word-bits *this-machine*) 64)

(setf (machine-single-bits *this-machine*) 32)
(setf (machine-double-bits *this-machine*) 64)

(define-register-class 'cr 'integer 64
  '(fpcr))

(define-register-class 'gpr 'integer 64
  '(r0 r1 r2 r3 r4 r5 r6 r7
    r8 r9 r10 r11 r12 r13 r14 r15
    r16 r17 r18 r19 r20 r21 r22 r23
    r24 r25 r26 r27 r28 r29 r30))

(define-register-class 'fpr 'float 64
  '(f0 f1 f2 f3 f4 f5 f6 f7
    f8 f9 f10 f11 f12 f13 f14 f15
    f16 f17 f18 f19 f20 f21 f22 f23
    f24 f25 f26 f27 f28 f29 f30))

(define-fields
    '((opcd 26 31)
      (ra 21 25)
      (rb 16 20)
      (memory-disp 0 15)
      (jump-target-hint 0 13)
      (jump-hint 14 15)
      (memory-function 0 15)
      (branch-disp 0 20)
      (sbz 13 15)
      (imm-select 12 12)
      (imm 13 20)
      (operate-function 5 11)
      (rc 0 4)
      (fa 21 25)
      (fb 16 20)
      (float-function 5 15)
      (fc 0 4)
      (pal-code 0 25)))

(define-operand-order
    '(ra fa memory-disp rb imm fb rc fc branch-disp))

;;;; macros

(defmacro define-operate-insn (name opcd function effect &optional fields &key (imm-form t) (generate t))
  (let ((reg-form `(define-insn ,name
		    ((opcd ,opcd)
		     (operate-function ,function)
		     (imm-select 0)
		     (sbz 0)
		     ,@fields)
		    ,(my-macroexpand effect `((op-b . ,#'(lambda () '(if (= rb (width 5 31)) (width 64 0) (reg rb gpr))))
					      (long-op-b . ,#'(lambda () '(if (= rb (width 5 31)) (width 32 0) (subreg 0 31 rb gpr))))
					      (word-op-b . ,#'(lambda () '(if (= rb (width 5 31)) (width 16 0) (subreg 0 15 rb gpr))))
					      (byte-op-b . ,#'(lambda () '(if (= rb (width 5 31)) (width 8 0) (subreg 0 7 rb gpr))))))
		    (,(format nil "~A $%u,$%u,$%u" (string-downcase (symbol-name name))) ra rb rc)
		    :generate ,generate)))
    (if imm-form
	`(progn
	  ,reg-form
	  (define-insn ,(intern (concatenate 'string (symbol-name name) "_IMM"))
	      ((opcd ,opcd)
	       (operate-function ,function)
	       (imm-select 1)
	       ,@fields)
	    ,(my-macroexpand effect `((op-b . ,#'(lambda () '(width 64 (zex imm))))
				      (long-op-b . ,#'(lambda () '(width 32 (zex imm))))
				      (word-op-b . ,#'(lambda () '(width 16 (zex imm))))
				      (byte-op-b . ,#'(lambda () 'imm))))
	    (,(format nil "~A $%u,%u,$%u" (string-downcase (symbol-name name))) ra imm rc)
	    :generate ,generate))
	reg-form)))

(defmacro define-float-insn (name opcd function effect &optional fields)
  `(define-insn ,name
    ((opcd ,opcd)
     (float-function ,function)
     ,@fields)
    ,effect
    (,(format nil "~A $f%u,$f%u,$f%u" (string-downcase (symbol-name name))) fa fb fc)))

(defmacro define-conditional-branch-insn (name opcd condition)
  `(define-insn ,name
    ((opcd ,opcd))
    ((if ,condition
	 (jump-relative (+ (shiftl (sex branch-disp) 2) 4))
	 (nop)))
    (,(format nil "~A 0x%x" (string-downcase (symbol-name name))) (width 64 (+ addr (+ (shiftl (sex branch-disp) 2) 4))))))

(defmacro define-unconditional-branch-insn (name opcd)
  `(define-insn ,name
    ((opcd ,opcd))
    ((set-ra (+ pc 4))
     (jump-relative (+ (shiftl (sex branch-disp) 2) 4)))
    (,(format nil "~A $%u,0x%x" (string-downcase (symbol-name name))) ra (width 64 (+ addr (+ (shiftl (sex branch-disp) 2) 4))))))

(defmacro define-conditional-move-insn (name function condition)
  `(define-operate-insn ,name #x11 ,function
    ((if ,condition
	 (set-rc (op-b))
	 (nop)))))

(defmacro define-compare-insn (name function condition)
  `(define-operate-insn ,name #x10 ,function
    ((set-rc (if ,condition 1 0)))))

(defmacro define-float-compare-insn (name function condition &optional fields)
  `(define-float-insn ,name #x16 ,function
    ((set-fc (if ,condition 2.0 0.0))) ,fields))

(defmacro define-memory-insn (name opcd function fields effect)
  `(define-insn ,name
    ((opcd ,opcd)
     (memory-function ,function)
     ,@fields)
    ,effect
    (,(format nil "~A $%u,$%u" (string-downcase (symbol-name name))) ra rb)))

(defmacro define-conditional-float-move-insn (name function condition)
  `(define-float-insn ,name #x17 ,function
    ((if ,condition
	 (set-fc (fop fb))
	 (nop)))))

(defmacro define-load-store-insn (name fields effect &key (generate t))
  `(define-insn ,name
    ,fields
    ,effect
    (,(format nil "~A $%u,%d($%u)" (string-downcase (symbol-name name))) ra memory-disp rb)
    :generate ,generate))

(defmacro define-float-load-store-insn (name fields effect)
  `(define-insn ,name
    ,fields
    ,effect
    (,(format nil "~A $f%u,%d($%u)" (string-downcase (symbol-name name))) fa memory-disp rb)))

(define-insn-macro byte-op (x)
  (if (= x (width 5 31))
      (width 8 0)
      (subreg 0 7 x gpr)))

(define-insn-macro word-op (x)
  (if (= x (width 5 31))
      (width 16 0)
      (subreg 0 15 x gpr)))

(define-insn-macro long-op (x)
  (if (= x (width 5 31))
      (width 32 0)
      (subreg 0 31 x gpr)))

(define-insn-macro op (x)
  (if (= x (width 5 31))
      0
      (reg x gpr)))

(define-insn-macro set-ra (val)
  (if (= ra (width 5 31))
      (nop)
      (set (reg ra gpr) val)))

(define-insn-macro set-rc (val)
  (if (= rc (width 5 31))
      (nop)
      (set (reg rc gpr) val)))

(define-insn-macro fop (x)
  (if (= x (width 5 31))
      (width 64 0.0)
      (reg x fpr)))

(define-insn-macro set-fa (val)
  (if (= fa (width 5 31))
      (nop)
      (set (reg fa fpr) val)))

(define-insn-macro set-fc (val)
  (if (= fc (width 5 31))
      (nop)
      (set (reg fc fpr) val)))

(define-insn-macro mem-src ()
  (mem (+ (op rb) (sex memory-disp))))

(define-insn-macro set-mem (w value)
  (set (width w (mem (+ (op rb) (sex memory-disp)))) value))

;;;; insns

(define-operate-insn addl #x10 #x00
  ((set-rc (sex (+ (long-op ra) (long-op-b))))))

(define-operate-insn addq #x10 #x20
  ((set-rc (+ (op ra) (op-b)))))
  
(define-float-insn adds #x16 #x080
  ((set-fc (+f (fop fa) (fop fb)))))

(define-float-insn addt #x16 #x0a0
  ((set-fc (+f (fop fa) (fop fb)))))

(define-operate-insn amask #x11 #x61
  ((set-rc (logand (op-b) (bitneg #x207)))) ;all but mvi
  ((ra 31))
  :generate nil)

(define-operate-insn and #x11 #x00
  ((set-rc (logand (op ra) (op-b)))))

(define-conditional-branch-insn beq #x39
  (= (op ra) 0))

(define-conditional-branch-insn bge #x3e
  (>=s (op ra) 0))

(define-conditional-branch-insn bgt #x3f
  (>s (op ra) 0))

(define-operate-insn bic #x11 #x08
  ((set-rc (logand (op ra) (bitneg (op-b))))))

(define-operate-insn bis #x11 #x20
  ((set-rc (logor (op ra) (op-b)))))

(define-conditional-branch-insn blbc #x38
  (not (bit-set-p (op ra) 0)))

(define-conditional-branch-insn blbs #x3c
  (bit-set-p (op ra) 0))

(define-conditional-branch-insn ble #x3b
  (<=s (op ra) 0))

(define-conditional-branch-insn blt #x3a
  (<s (op ra) 0))

(define-conditional-branch-insn bne #x3d
  (not (= (op ra) 0)))

(define-unconditional-branch-insn br #x30)

(define-unconditional-branch-insn bsr #x34)

(define-insn call_pal
    ((opcd #x00))
  ((call-pal (zex pal-code)))
  ("call_pal %u" pal-code))

(define-conditional-move-insn cmoveq #x24
  (= (op ra) 0))

(define-conditional-move-insn cmovge #x46
  (>=s (op ra) 0))

(define-conditional-move-insn cmovgt #x66
  (>s (op ra) 0))

(define-conditional-move-insn cmovlbc #x16
  (not (bit-set-p (op ra) 0)))

(define-conditional-move-insn cmovlbs #x14
  (not (bit-set-p (op ra) 0)))

(define-conditional-move-insn cmovle #x64
  (<=s (op ra) 0))

(define-conditional-move-insn cmovlt #x44
  (<s (op ra) 0))

(define-conditional-move-insn cmovne #x26
  (not (= (op ra) 0)))

(define-operate-insn cmpbge #x10 #x0f
  ((set-rc (+ (op ra) (op-b))))
  ()
  :generate nil)	;FIXME

(define-compare-insn cmpeq #x2d
  (= (op ra) (op-b)))

(define-compare-insn cmple #x6d
  (<=s (op ra) (op-b)))

(define-compare-insn cmplt #x4d
  (<s (op ra) (op-b)))

(define-float-compare-insn cmpteq #x0a5
  (=f (fop fa) (fop fb)))

(define-float-compare-insn cmptle #x0a7
  (<=f (fop fa) (fop fb)))

(define-float-compare-insn cmptlt #x0a6
  (<f (fop fa) (fop fb)))

(define-float-compare-insn cmptun #x0a4
  0					;FIXME
  ((fa dont-care)
   (fb dont-care)))

(define-compare-insn cmpule #x3d
  (<= (op ra) (op-b)))

(define-compare-insn cmpult #x1d
  (< (op ra) (op-b)))

(define-float-insn cpys #x17 #x020
  ((set-fc (+f (fop fa) (fop fb)))))	;FIXME

(define-float-insn cpyse #x17 #x022
  ()					;FIXME
  ((fa dont-care)
   (fb dont-care)
   (fc dont-care)))

(define-float-insn cpysn #x17 #x021
  ((set-fc (+f (fop fa) (fop fb)))))	;FIXME

(define-operate-insn ctlz #x1c #x32	;should have special asm since ra is not used
  ((set-rc (leading-zeros (op rb))))
  ((ra 31)) :imm-form nil)

(define-operate-insn ctpop #x1c #x30	;should have special asm since ra is not used
  ((set-rc (population (op rb))))
  ((ra 31)) :imm-form nil)

(define-operate-insn cttz #x1c #x33	;should have special asm since ra is not used
  ((set-rc (trailing-zeros (op rb))))
  ((ra 31)) :imm-form nil)

(define-float-insn cvtlq #x17 #x010
  ()					;FIXME
  ((fa dont-care)
   (fb dont-care)
   (fc dont-care)))

(define-float-insn cvtql #x17 #x030
  ((set-fc (integer-to-double (double-to-bits (fop fb))))) ;FIXME (negative values are complemented, long)
  ((fa 31)))

(define-float-insn cvtqs #x16 #x0bc
  ()					;FIXME
  ((fa dont-care)
   (fb dont-care)
   (fc dont-care)))

(define-float-insn cvtqt #x16 #x0be
  ((set-fc (integer-to-double (double-to-bits (fop fb)))))
  ((fa 31)))

(define-float-insn cvtqtc #x16 #x03e
  ((set-fc (integer-to-double (double-to-bits (fop fb)))))
  ((fa 31)))

(define-float-insn cvtst #x16 #x2ac
  ()					;FIXME
  ((fa dont-care)
   (fb dont-care)
   (fc dont-care)))

(define-float-insn cvttq #x16 #x0af
  ((set-fc (bits-to-double (double-to-integer (fop fb)))))
  ((fa 31)))

(define-float-insn cvttqc #x16 #x02f
  ((set-fc (bits-to-double (double-to-integer (fop fb)))))
  ((fa 31)))

(define-float-insn cvtts #x16 #x0ac
  ()					;FIXME
  ((fa dont-care)
   (fb dont-care)
   (fc dont-care)))

(define-float-insn divs #x16 #x083
  ((set-fc (/f (fop fa) (fop fb)))))

(define-float-insn divt #x16 #x0a3
  ((set-fc (/f (fop fa) (fop fb)))))

(define-memory-insn ecb #x18 #xe800
  ((ra dont-care)
   (rb dont-care))
  ((nop)))

(define-operate-insn eqv #x11 #x48
  ((set-rc (logxor (op ra) (bitneg (op-b))))))

(define-memory-insn excb #x18 #x0400
  ((ra dont-care)
   (rb dont-care))			;rb is actually significant here
  ((nop)))

(define-operate-insn extbl #x12 #x06
  ((set-rc (logand (shiftr (op ra) (* (logand (op-b) 7) 8)) (mask 0 7)))))

(define-operate-insn extlh #x12 #x6a
  ((set-rc (logand (shiftl (op ra) (- 64 (* (logand (op-b) 7) 8))) (mask 0 31)))))

(define-operate-insn extll #x12 #x26
  ((set-rc (logand (shiftr (op ra) (* (logand (op-b) 7) 8)) (mask 0 31)))))

(define-operate-insn extqh #x12 #x7a
  ((set-rc (shiftl (op ra) (- 64 (* (logand (op-b) 7) 8))))))

(define-operate-insn extql #x12 #x36
  ((set-rc (shiftr (op ra) (* (logand (op-b) 7) 8)))))

(define-operate-insn extwh #x12 #x5a
  ((set-rc (logand (shiftl (op ra) (- 64 (* (logand (op-b) 7) 8))) (mask 0 15)))))

(define-operate-insn extwl #x12 #x16
  ((set-rc (logand (shiftr (op ra) (* (logand (op-b) 7) 8)) (mask 0 15)))))

(define-conditional-branch-insn fbeq #x31
  (=f (fop fa) 0.0))

(define-conditional-branch-insn fbge #x36
  (>=f (fop fa) 0.0))

(define-conditional-branch-insn fbgt #x37
  (>f (fop fa) 0.0))

(define-conditional-branch-insn fble #x33
  (<=f (fop fa) 0.0))

(define-conditional-branch-insn fblt #x32
  (<f (fop fa) 0.0))

(define-conditional-branch-insn fbne #x35
  (not (=f (fop fa) 0.0)))

(define-conditional-float-move-insn fcmoveq #x02a
  (=f (fop fa) 0.0))

(define-conditional-float-move-insn fcmovge #x02d
  (>=f (fop fa) 0.0))

(define-conditional-float-move-insn fcmovgt #x02f
  (>f (fop fa) 0.0))

(define-conditional-float-move-insn fcmovle #x02e
  (<=f (fop fa) 0.0))

(define-conditional-float-move-insn fcmovlt #x02c
  (<f (fop fa) 0.0))

(define-conditional-float-move-insn fcmovne #x02b
  (not (=f (fop fa) 0.0)))

(define-memory-insn fetch #x18 #x8000
  ((ra dont-care)
   (rb dont-care))			;rb is actually significant here
  ((nop)))

(define-memory-insn fetch_m #x18 #xa000
  ((ra dont-care)
   (rb dont-care))			;rb is actually significant here
  ((nop)))

(define-float-insn ftois #x1c #x078
  ((set-rc (zex (single-to-bits (double-to-single (fop fa))))))	;FIXME
  ((fb 31)))

(define-float-insn ftoit #x1c #x070
  ((set-rc (double-to-bits (fop fa))))
  ((fb 31)))

(define-insn implver
    ((opcd #x11)
     (operate-function #x6c)
     (imm-select 1)
     (imm 1)
     (ra 31))
  ((set-rc 0))
  ("implver $%u" rc)
  :generate nil)

(define-operate-insn insbl #x12 #x0b
  ((set-rc (logand (shiftl (op ra) (* (logand (op-b) 7) 8))
		   (shiftl (mask 0 7) (* (logand (op-b) 7) 8))))))

(define-operate-insn inslh #x12 #x67
  ((set-rc (logand (shiftr (op ra) (- 64 (* (logand (op-b) 7) 8)))
		   (shiftr (mask 0 31) (- 64 (* (logand (op-b) 7) 8)))))))

(define-operate-insn insll #x12 #x2b
  ((set-rc (logand (shiftl (op ra) (* (logand (op-b) 7) 8))
		   (shiftl (mask 0 31) (* (logand (op-b) 7) 8))))))

(define-operate-insn insqh #x12 #x77
  ((set-rc (logand (shiftr (op ra) (- 64 (* (logand (op-b) 7) 8)))
		   (shiftr (mask 0 63) (- 64 (* (logand (op-b) 7) 8)))))))

(define-operate-insn insql #x12 #x3b
  ((set-rc (logand (shiftl (op ra) (* (logand (op-b) 7) 8))
		   (shiftl (mask 0 63) (* (logand (op-b) 7) 8))))))

(define-operate-insn inswh #x12 #x57
  ((set-rc (logand (shiftr (op ra) (- 64 (* (logand (op-b) 7) 8)))
		   (shiftr (mask 0 15) (- 64 (* (logand (op-b) 7) 8)))))))

(define-operate-insn inswl #x12 #x1b
  ((set-rc (logand (shiftl (op ra) (* (logand (op-b) 7) 8))
		   (shiftl (mask 0 15) (* (logand (op-b) 7) 8))))))

(define-float-insn itofs #x14 #x004
  ((set-fc (single-to-double (bits-to-single (long-op ra))))) ;FIXME
  ((rb 31)))

(define-float-insn itoft #x14 #x024
  ((set-fc (bits-to-double (op ra))))
  ((rb 31)))

(define-insn jmp
    ((opcd #x1a)
     (memory-disp dont-care))
  ((set-ra (+ pc 4))
   (jump-absolute (+ pc (logand (op rb) (mask 2 63)))))
  ("jmp $%u,($%u),%u" ra rb jump-hint))

(define-load-store-insn lda
    ((opcd #x08))
  ((set-ra (+ (op rb) (sex memory-disp)))))

(define-load-store-insn ldah
    ((opcd #x09))
  ((set-ra (+ (op rb) (shiftl (sex memory-disp) 16)))))

(define-load-store-insn ldbu
    ((opcd #x0a))
  ((set-ra (zex (width 8 (mem-src))))))

(define-load-store-insn ldwu
    ((opcd #x0c))
  ((set-ra (zex (width 16 (mem-src))))))

(define-load-store-insn ldl
    ((opcd #x28))
  ((set-ra (sex (width 32 (mem-src))))))

(define-load-store-insn ldl_l
    ((opcd #x2a))
  ((set-ra (sex (width 32 (mem-src)))))
  :generate nil)

(define-load-store-insn ldq
    ((opcd #x29))
  ((set-ra (mem-src))))

(define-load-store-insn ldq_l
    ((opcd #x2b))
  ((set-ra (mem-src)))
  :generate nil)

(define-load-store-insn ldq_u
    ((opcd #x0b))
  ((set-ra (mem (logand (+ (sex memory-disp) (op rb)) (mask 3 63))))))

(define-float-load-store-insn lds
    ((opcd #x22))
  ((set-fa (single-to-double (bits-to-single (width 32 (mem-src)))))))

(define-float-load-store-insn ldt
    ((opcd #x23))
  ((set-fa (bits-to-double (mem-src)))))

(define-memory-insn mb #x18 #x4000
  ((ra dont-care)
   (rb dont-care))
  ((nop)))

(define-float-insn mf_fpcr #x17 #x025
  ((set-fa (bits-to-double (reg fpcr))))
  ((fb fa)
   (fc fa)))

(define-operate-insn mskbl #x12 #x02
  ((set-rc (logand (op ra) (shiftl (mask 0 7) (* (logand (op-b) 7) 8))))))

(define-operate-insn msklh #x12 #x62
  ((set-rc (logand (op ra) (shiftr (mask 0 31) (- 64 (* (logand (op-b) 7) 8)))))))

(define-operate-insn mskll #x12 #x22
  ((set-rc (logand (op ra) (shiftl (mask 0 31) (* (logand (op-b) 7) 8))))))

(define-operate-insn mskqh #x12 #x72
  ((set-rc (logand (op ra) (shiftr (mask 0 63) (- 64 (* (logand (op-b) 7) 8)))))))

(define-operate-insn mskql #x12 #x32
  ((set-rc (logand (op ra) (shiftl (mask 0 53) (* (logand (op-b) 7) 8))))))

(define-operate-insn mskwh #x12 #x52
  ((set-rc (logand (op ra) (shiftr (mask 0 15) (- 64 (* (logand (op-b) 7) 8)))))))

(define-operate-insn mskwl #x12 #x12
  ((set-rc (logand (op ra) (shiftl (mask 0 15) (* (logand (op-b) 7) 8))))))

(define-float-insn mt_fpcr #x17 #x024
  ((set (reg fpcr) (double-to-bits (fop fa))))
  ((fb fa)
   (fc fa)))

(define-operate-insn mull #x13 #x00
  ((set-rc (sex (* (long-op ra) (long-op-b))))))

(define-operate-insn mulq #x13 #x20
  ((set-rc (* (op ra) (op-b)))))

(define-float-insn muls #x16 #x082
  ((set-fc (*f (fop fa) (fop fb)))))

(define-float-insn mult #x16 #x0a2
  ((set-fc (*f (fop fa) (fop fb)))))

(define-operate-insn ornot #x11 #x28
  ((set-rc (logor (op ra) (bitneg (op-b))))))

(define-memory-insn rpcc #x18 #xc000
  ((ra dont-care)			;ra is set to the pcc
   (rb dont-care))
  ((not-implemented)))

(define-operate-insn s4addl #x10 #x02
  ((set-rc (sex (width 32 (+ (shiftl (long-op ra) 2) (long-op-b)))))))

(define-operate-insn s4addq #x10 #x22
  ((set-rc (+ (shiftl (op ra) 2) (op-b)))))

(define-operate-insn s4subl #x10 #x0b
  ((set-rc (sex (width 32 (- (shiftl (long-op ra) 2) (long-op-b)))))))

(define-operate-insn s4subq #x10 #x2b
  ((set-rc (- (shiftl (op ra) 2) (op-b)))))

(define-operate-insn s8addl #x10 #x12
  ((set-rc (sex (width 32 (+ (shiftl (long-op ra) 2) (long-op-b)))))))

(define-operate-insn s8addq #x10 #x32
  ((set-rc (+ (shiftl (op ra) 2) (op-b)))))

(define-operate-insn s8subl #x10 #x1b
  ((set-rc (sex (width 32 (- (shiftl (long-op ra) 2) (long-op-b)))))))

(define-operate-insn s8subq #x10 #x3b
  ((set-rc (- (shiftl (op ra) 2) (op-b)))))

(define-operate-insn sextb #x1c #x00
  ((set-rc (sex (byte-op-b))))
  ((ra 31)))

(define-operate-insn sextw #x1c #x01
  ((set-rc (sex (word-op-b))))
  ((ra 31)))

(define-operate-insn sll #x12 #x39
  ((set-rc (shiftl (op ra) (logand (op-b) #x3f)))))

(define-float-insn sqrts #x14 #x08b
  ((set-fc (sqrt (fop fb))))
  ((fa 31)))

(define-float-insn sqrtt #x14 #x0ab
  ((set-fc (sqrt (fop fb))))
  ((fa 31)))

(define-operate-insn sra #x12 #x3c
  ((set-rc (ashiftr (op ra) (logand (op-b) #x3f)))))

(define-operate-insn srl #x12 #x34
  ((set-rc (shiftr (op ra) (logand (op-b) #x3f)))))

(define-load-store-insn stb
    ((opcd #x0e))
  ((set-mem 8 (byte-op ra))))

(define-load-store-insn stl
    ((opcd #x2c))
  ((set-mem 32 (long-op ra))))

(define-load-store-insn stl_c
    ((opcd #x2e))
  ((set-mem 32 (long-op ra))
   (set-ra 1)))

(define-load-store-insn stq
    ((opcd #x2d))
  ((set-mem 64 (op ra))))

(define-load-store-insn stq_c
    ((opcd #x2f))
  ((set-mem 64 (op ra))
   (set-ra 1)))

(define-load-store-insn stq_u
    ((opcd #x0f))
  ((set (mem (logand (+ (sex memory-disp) (op rb)) (mask 3 63))) (op ra))
   (set-ra 1)))

(define-float-load-store-insn sts
    ((opcd #x26))
  ((set-mem 32 (single-to-bits (double-to-single (fop fa))))))

(define-float-load-store-insn stt
    ((opcd #x27))
  ((set-mem 64 (double-to-bits (fop fa)))))

(define-load-store-insn stw
    ((opcd #x0d))
  ((set-mem 16 (word-op ra))))

(define-operate-insn subl #x10 #x09
  ((set-rc (sex (- (long-op ra) (long-op-b))))))

(define-operate-insn subq #x10 #x29
  ((set-rc (- (op ra) (op-b)))))

(define-float-insn subs #x16 #x081
  ((set-fc (-f (fop fa) (fop fb)))))

(define-float-insn subt #x16 #x0a1
  ((set-fc (-f (fop fa) (fop fb)))))

(define-memory-insn trapb #x18 #x0000
  ((ra dont-care)
   (rb dont-care))
  ((nop)))

(define-operate-insn umulh #x13 #x30
  ((set-rc (promote 64 (shiftr (* (width 128 (zex (op ra)))
				  (width 128 (zex (op-b))))
			       64)))))

(define-memory-insn wh64 #x18 #xf800
  ((ra dont-care)
   (rb dont-care))
  ((nop)))

(define-memory-insn wmb #x18 #x4400
  ((ra dont-care)
   (rb dont-care))
  ((nop)))

(define-operate-insn xor #x11 #x40
  ((set-rc (logxor (op ra) (op-b)))))

(define-operate-insn zap #x12 #x30
  ((set-rc (logand (op ra) (bitneg (maskmask 8 (byte-op-b)))))))

(define-operate-insn zapnot #x12 #x31
  ((set-rc (logand (op ra) (maskmask 8 (byte-op-b))))))

;;;; mnemonics

(define-mnemonic fmov (s d)
  (cpys s s d))

(define-mnemonic fnegt (s d)
  (cpysn s s d))

(define-mnemonic mov (s d)
  (bis s s d))

(define-mnemonic negq (s d)
  (subq 31 s d))

(define-mnemonic not (s d)
  (ornot 31 s d))

;;;; generators

(define-generator
  :inputs ((x integer (<= 64) reg)
	   (lt integer (= 4) const)
	   (gt integer (= 4) const)
	   (eq integer (= 4) const))
  :result (res integer 4)
  :pattern (if (<s x 0)
	       lt
	       (if (>s x 0)
		   gt
		   eq))
  :code ((cmovlt_imm x lt res)
	 (cmoveq_imm x eq res)
	 (cmovgt_imm x gt res)))

(define-generator
  :inputs ((d integer (= 16) const)
	   (b integer (= 32) reg)
	   (rhs integer (= 32) reg))
  :pattern (set (mem (+ b (sex d)) 32) rhs)
  :code ((stl rhs d b)))

(define-generator
  :inputs ((d integer (= 16) const)
	   (b integer (= 32) reg))
  :result (res integer 32)
  :pattern (mem (+ b (sex d)) 32)
  :code ((ldl res d b)))

(defparameter *alpha* *this-machine*)
