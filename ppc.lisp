(new-machine 'ppc)

(defparameter *insn-bits* 32)
(defparameter *word-bits* 32)

(defparameter *single-bits* 32)
(defparameter *double-bits* 64)

(define-register-class 'spr 'integer 32
  '(lr cr xer ctr fpscr))

(define-register-class 'gpr 'integer 32
  '(gpr0 gpr1 gpr2 gpr3 gpr4 gpr5 gpr6 gpr7
    gpr8 gpr9 gpr10 gpr11 gpr12 gpr13 gpr14 gpr15
    gpr16 gpr17 gpr18 gpr19 gpr20 gpr21 gpr22 gpr23
    gpr24 gpr25 gpr26 gpr27 gpr28 gpr29 gpr30 gpr31))

(define-register-class 'fpr 'float 64
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

(define-operand-order
    '(crm rs frs rd frd crfd crfs d ra fra frc rb frb uimm simm bo bi bd crbd crba crbb sh mb me))

;;;; macros

(defmacro define-rc-insn (name result-reg-field fields effect asm)
  (let* ((name-string (string-downcase (symbol-name name)))
	 (name-dot (intern (string-concat (symbol-name name) ".")))
	 (name-dot-string (string-downcase (symbol-name name-dot))))
    `(progn
      (define-insn ,name
	  (,@fields
	   (rc 0))
	,effect
	(,(format nil (car asm) name-string) ,@(cdr asm)))
      (define-insn ,name-dot
	  (,@fields
	   (rc 1))
	(,@effect
	 (set cr-0 (logor (if (<s (reg ,result-reg-field gpr) 0)
			      8
			      (if (>s (reg ,result-reg-field gpr) 0)
				  4
				      2))
			  (zex xer-so))))
	(,(format nil (car asm) name-dot-string) ,@(cdr asm))))))

;;;; insns

(define-insn add
    ((opcd 31)
     (xo9 266)
     (oe 0)
     (rc 0))
  ((set (reg rd gpr) (+ (reg ra gpr) (reg rb gpr))))
  ("add r%u,r%u,r%u" rd ra rb))

(define-rc-insn adde rd
    ((opcd 31)
     (xo9 138)
     (oe 0))
  ((let ((old-ca (width 32 (zex xer-ca))))
     (set (reg rd gpr) (+ (+ (reg ra gpr) (reg rb gpr)) (zex xer-ca)))
     (set xer-ca (logor (+carry (reg ra gpr) (reg rb gpr))
			(+carry (+ (reg ra gpr) (reg rb gpr)) (zex old-ca))))))
  ("~A r%u,r%u,r%u" rd ra rb))

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

(define-rc-insn and ra
    ((opcd 31)
     (xo1 28))
  ((set (reg ra gpr) (logand (reg rs gpr) (reg rb gpr))))
  ("~A r%u,r%u,r%u" ra rs rb))

(define-rc-insn andc ra
    ((opcd 31)
     (xo1 60))
  ((set (reg ra gpr) (logand (reg rs gpr) (bitneg (reg rb gpr)))))
  ("~A r%u,r%u,r%u" ra rs rb))

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
     (bi dont-care)
     (rb 0)
     (xo1 528)
     (lk 0))
  ((jump-absolute (logand (reg ctr) #xfffffffc)))
  ("bctr"))

(define-insn bdnz			;branch with decrement if not zero
    ((opcd 16)
     (bo 16)
     (bi dont-care)
     (aa 0)
     (lk 0))
  ((set (reg ctr) (- (reg ctr) 1))
   (if (= (reg ctr) 0)
       (nop)
       (jump-relative (shiftl (sex bd) 2))))
  ("bdnz 0x%x" (width 32 (+ addr (shiftl (sex li) 2)))))

(define-insn bdz			;branch with decrement if zero
    ((opcd 16)
     (bo 18)
     (bi dont-care)
     (aa 0)
     (lk 0))
  ((set (reg ctr) (- (reg ctr) 1))
   (if (= (reg ctr) 0)
       (jump-relative (shiftl (sex bd) 2))
       (nop)))
  ("bdz 0x%x" (width 32 (+ addr (shiftl (sex li) 2)))))

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
     (bi dont-care)
     (rb 0)
     (xo1 16)
     (lk 0))
  ((jump-absolute (logand (reg lr) #xfffffffc)))
  ("blr"))

(define-insn blrl
    ((opcd 19)
     (bo 20)
     (bi dont-care)
     (rb 0)
     (xo1 16)
     (lk 1))
  ((let ((old-lr (reg lr)))
     (set (reg lr) (+ pc 4))
     (jump-absolute (logand old-lr #xfffffffc))))
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

(define-insn creqv
    ((opcd 19)
     (xo1 289)
     (rc 0))
  ((set (numbered-subreg 1 (- (width 5 31) crbd) cr)
	(if (= (numbered-subreg 1 (- (width 5 31) crba) cr) (numbered-subreg 1 (- (width 5 31) crbb) cr)) 1 0)))
  ("creqv crb%u,crb%u,crb%u" crbd crba crbb))

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

(define-rc-insn eqv ra
  ((opcd 31)
   (xo1 284))
  ((set (reg ra gpr) (bitneg (logxor (reg rs gpr) (reg rb gpr)))))
  ("~A r%u,r%u,r%u" ra rs rb))

(define-insn extsb
    ((opcd 31)
     (xo1 954)
     (rb 0)
     (rc 0))
  ((set (reg ra gpr) (width 32 (sex (subreg 0 7 rs gpr)))))
  ("extsb r%u,r%u" ra rs))

(define-rc-insn extsh ra
    ((opcd 31)
     (xo1 922)
     (rb 0))
  ((set (reg ra gpr) (width 32 (sex (subreg 0 15 rs gpr)))))
  ("~A r%u,r%u" ra rs))

(define-insn fabs
    ((opcd 63)
     (fra 0)
     (xo1 264)
     (rc 0))
  ((set (reg frd fpr) (bits-to-double (logand (double-to-bits (reg frb fpr)) #x7fffffffffffffff))))
  ("fabs fr%u,fr%u" frd frb))

(define-insn fadd
    ((opcd 63)
     (frc 0)
     (xo5 21)
     (rc 0))
  ((set (reg frd fpr) (+f (reg fra fpr) (reg frb fpr))))
  ("fadd fr%u,fr%u,fr%u" frd fra frb))

(define-insn fadds
    ((opcd 59)
     (frc 0)
     (xo5 21)
     (rc 0))
  ((set (reg frd fpr) (+f (reg fra fpr) (reg frb fpr))))
  ("fadds fr%u,fr%u,fr%u" frd fra frb))

(define-insn fcmpu
    ((opcd 63)
     (crdz 0)
     (xo1 0)
     (rc 0))
  ((set (numbered-subreg 4 (- (width 3 7) crfd) cr)
	(if (<f (reg fra fpr) (reg frb fpr))
	    8
	    (if (>f (reg fra fpr) (reg frb fpr))
		4
		2))))
  ("fcmpu cr%u,fr%u,fr%u" crfd fra frb))

(define-insn fctiwz
    ((opcd 63)
     (fra 0)
     (xo1 15)
     (rc 0))
  ((set (reg frd fpr) (bits-to-double (zex (width 32 (double-to-integer (reg frb fpr)))))))
  ("fctiwz fr%u,fr%u" frd frb))

(define-insn fdiv
    ((opcd 63)
     (frc 0)
     (xo5 18)
     (rc 0))
  ((set (reg frd fpr) (/f (reg fra fpr) (reg frb fpr))))
  ("fdiv fr%u,fr%u,fr%u" frd fra frb))

(define-insn fdivs
    ((opcd 59)
     (frc 0)
     (xo5 18)
     (rc 0))
  ((set (reg frd fpr) (/f (reg fra fpr) (reg frb fpr))))
  ("fdiv fr%u,fr%u,fr%u" frd fra frb))

(define-insn fmadd
    ((opcd 63)
     (xo5 29)
     (rc 0))
  ((set (reg frd fpr) (+f (*f (reg fra fpr) (reg frc fpr)) (reg frb fpr))))
  ("fmadd fr%u,fr%u,fr%u,fr%u" frd fra frc frb))

(define-insn fmadds
    ((opcd 59)
     (xo5 29)
     (rc 0))
  ((set (reg frd fpr) (+f (*f (reg fra fpr) (reg frc fpr)) (reg frb fpr))))
  ("fmadds fr%u,fr%u,fr%u,fr%u" frd fra frc frb))

(define-insn fmr
    ((opcd 63)
     (fra 0)
     (xo1 72)
     (rc 0))
  ((set (reg frd fpr) (reg frb fpr)))
  ("fmr fr%u,fr%u" frd frb))

(define-insn fmsub
    ((opcd 63)
     (xo5 28)
     (rc 0))
  ((set (reg frd fpr) (-f (*f (reg fra fpr) (reg frc fpr)) (reg frb fpr))))
  ("fmsub fr%u,fr%u,fr%u,fr%u" frd fra frc frb))

(define-insn fmsubs
    ((opcd 59)
     (xo5 28)
     (rc 0))
  ((set (reg frd fpr) (-f (*f (reg fra fpr) (reg frc fpr)) (reg frb fpr))))
  ("fmsubs fr%u,fr%u,fr%u,fr%u" frd fra frc frb))

(define-insn fmul
    ((opcd 63)
     (frb 0)
     (xo5 25)
     (rc 0))
  ((set (reg frd fpr) (*f (reg fra fpr) (reg frc fpr))))
  ("fmul fr%u,fr%u,fr%u" frd fra frc))

(define-insn fmuls
    ((opcd 59)
     (frb 0)
     (xo5 25)
     (rc 0))
  ((set (reg frd fpr) (*f (reg fra fpr) (reg frc fpr))))
  ("fmuls fr%u,fr%u,fr%u" frd fra frc))

(define-insn fneg
    ((opcd 63)
     (fra 0)
     (xo1 40)
     (rc 0))
  ((set (reg frd fpr) (fneg (reg frb fpr))))
  ("fneg fr%u,fr%u" frd frb))

(define-insn frsp
    ((opcd 63)
     (fra 0)
     (xo1 12)
     (rc 0))
  ((set (reg frd fpr) (reg frb fpr)))
  ("frsp fr%u,fr%u" frd frb))

(define-insn fsub
    ((opcd 63)
     (frc 0)
     (xo5 20)
     (rc 0))
  ((set (reg frd fpr) (-f (reg fra fpr) (reg frb fpr))))
  ("fsub fr%u,fr%u,fr%u" frd fra frb))

(define-insn fsubs
    ((opcd 59)
     (frc 0)
     (xo5 20)
     (rc 0))
  ((set (reg frd fpr) (-f (reg fra fpr) (reg frb fpr))))
  ("fsubs fr%u,fr%u,fr%u" frd fra frb))

(define-insn lbz
    ((opcd 34))
  ((set (reg rd gpr) (zex (width 8 (mem (if (= ra (width 5 0))
					    (sex d)
					    (+ (reg ra gpr) (sex d))))))))
  ("lbz r%u,%d(r%u)" rd d ra))

(define-insn lbzu
    ((opcd 35))
  ((set (reg rd gpr) (zex (width 8 (mem (+ (reg ra gpr) (sex d))))))
   (set (reg ra gpr) (+ (reg ra gpr) (sex d))))	;FIXME: aliasing!!!!!
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
  ((set (reg frd fpr) (bits-to-double (mem (if (= ra (width 5 0))
					       (sex d)
					       (+ (reg ra gpr) (sex d)))))))
  ("lfd fr%u,%d(r%u)" frd d ra))

(define-insn lfdx
    ((opcd 31)
     (xo1 599)
     (rc 0))
  ((set (reg frd fpr) (bits-to-double (mem (if (= ra (width 5 0))
					       (reg rb gpr)
					       (+ (reg ra gpr) (reg rb gpr)))))))
  ("lfdx fr%u,r%u,r%u" frd ra rb))

(define-insn lfs
    ((opcd 48))
  ((set (reg frd fpr) (single-to-double (bits-to-single (mem (if (= ra (width 5 0))
								 (sex d)
								 (+ (reg ra gpr) (sex d))))))))
  ("lfs fr%u,%d(r%u)" frd d ra))

(define-insn lfsx
    ((opcd 31)
     (xo1 535)
     (rc 0))
  ((set (reg frd fpr) (single-to-double (bits-to-single (mem (if (= ra (width 5 0))
								 (reg rb gpr)
								 (+ (reg ra gpr) (reg rb gpr))))))))
  ("lfsx fr%u,r%u,r%u" frd ra rb))

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
   (set (reg ra gpr) (+ (reg ra gpr) (sex d))))	;FIXME: aliasing!!!!!!1
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
  ((set (reg rd fpr) (bits-to-double (zex (reg fpscr)))))
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
  ((set (reg fpscr) (promote 32 (logand (double-to-bits (reg rb fpr)) (width 64 #xffffffff)))))
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

(define-rc-insn nand ra
    ((opcd 31)
     (xo1 476))
  ((set (reg ra gpr) (bitneg (logand (reg rs gpr) (reg rb gpr)))))
  ("~A r%u,r%u,r%u" ra rs rb))

(define-insn neg
    ((opcd 31)
     (rb 0)
     (oe 0)
     (xo9 104)
     (rc 0))
  ((set (reg rd gpr) (neg (reg ra gpr))))
  ("neg r%u,r%u" rd ra))

(define-rc-insn nor ra
    ((opcd 31)
     (xo1 124))
  ((set (reg ra gpr) (bitneg (logor (reg rs gpr) (reg rb gpr)))))
  ("~A r%u,r%u,r%u" ra rs rb))

(define-rc-insn or ra
    ((opcd 31)
     (xo1 444))
  ((set (reg ra gpr) (logor (reg rs gpr) (reg rb gpr))))
  ("~A r%u,r%u,r%u" ra rs rb))

(define-rc-insn orc ra
    ((opcd 31)
     (xo1 412))
  ((set (reg ra gpr) (logor (reg rs gpr) (bitneg (reg rb gpr)))))
  ("~A r%u,r%u,r%u" ra rs rb))

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
  

(define-rc-insn rlwinm ra		;rotate left word immediate then AND with mask
    ((opcd 21))
  ((set (reg ra gpr) (logand (rotl (reg rs gpr) sh) (mask (width 5 (- 31 me)) (width 5 (- 31 mb))))))
  ("~A r%u,r%u,%u,%u,%u" ra rs sh mb me))

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

(define-rc-insn srawi ra
    ((opcd 31)
     (xo1 824))
  ((set xer-ca (if (<s (reg rs gpr) 0)
		   (if (= (bitneg (logand (- (shiftl 1 sh) 1) (reg rs gpr))) 0)
		       0
		       1)
		   0))
   (set (reg ra gpr) (ashiftr (reg rs gpr) sh)))
  ("~A r%u,r%u,%u" ra rs sh))

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
  ((set (mem (+ (if (= ra (width 5 0)) 0 (reg ra gpr)) (sex d)) 64) (double-to-bits (reg rs fpr))))
  ("stfd fr%u,%d(r%u)" rs d ra))

(define-insn stfdx
    ((opcd 31)
     (xo1 727)
     (rc 0))
  ((set (mem (+ (if (= ra (width 5 0)) 0 (reg ra gpr)) (reg rb gpr)) 64) (double-to-bits (reg frs fpr))))
  ("stfdx fr%u,r%u,r%u" frs ra rb))

(define-insn stfs
    ((opcd 52))
  ((set (mem (+ (if (= ra (width 5 0)) 0 (reg ra gpr)) (sex d))) (single-to-bits (double-to-single (reg rs fpr)))))
  ("stfs fr%u,%d(r%u)" rs d ra))

(define-insn stfsx
    ((opcd 31)
     (xo1 663)
     (rc 0))
  ((set (mem (+ (if (= ra (width 5 0)) 0 (reg ra gpr)) (reg rb gpr))) (single-to-bits (double-to-single (reg frs fpr)))))
  ("stfsx fr%u,r%u,r%u" frs ra rb))

(define-insn sth
    ((opcd 44))
  ((set (mem (if (= ra (width 5 0))
		 (sex d)
		 (+ (reg ra gpr) (sex d))) 16)
	(subreg 0 15 rs gpr)))
  ("sth r%u,%d(r%u)" rs d ra))

(define-insn sthx
    ((opcd 31)
     (xo1 407)
     (rc 0))
  ((set (mem (+ (if (= ra (width 5 0)) 0 (reg ra gpr)) (reg rb gpr)) 16) (subreg 0 15 rs gpr)))
  ("sthx r%u,r%u,r%u" rs ra rb))

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

(define-rc-insn subf rd
    ((opcd 31)
     (xo9 40)
     (oe 0))
  ((set (reg rd gpr) (- (reg rb gpr) (reg ra gpr))))
  ("~A r%u,r%u,r%u" rd ra rb))

(define-rc-insn subfc rd
    ((opcd 31)
     (xo9 8)
     (oe 0))
  ((set xer-ca (logor (+carry (bitneg (reg ra gpr)) (reg rb gpr))
		      (+carry (+ (bitneg (reg ra gpr)) (reg rb gpr)) 1)))
   (set (reg rd gpr) (- (reg rb gpr) (reg ra gpr))))
  ("~A r%u,r%u,r%u" rd ra rb))

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

(define-rc-insn xor ra
    ((opcd 31)
     (xo1 316))
  ((set (reg ra gpr) (logxor (reg rs gpr) (reg rb gpr))))
  ("~A r%u,r%u,r%u" ra rs rb))

(define-insn xori
    ((opcd 26))
  ((set (reg ra gpr) (logxor (reg rs gpr) (zex uimm))))
  ("xori r%u,r%u,%u" ra rs uimm))

(define-insn xoris
    ((opcd 27))
  ((set (reg ra gpr) (logxor (reg rs gpr) (shiftl (zex uimm) 16))))
  ("xoris r%u,r%u,%u" ra rs uimm))
