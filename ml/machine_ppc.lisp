;; machine_ppc.lisp

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

(setq *fields* '())
(setq *registers* '())
(setq *conditions* '())
(setq *insns* '())
(setq *machine-macros* '())

(deffields ((rs 5) (rd 5) (ra 5) (rb 5) (srs 5) (sh 5) (mb 5) (me 5) (simm 16) (uimm 16) (d 16)))

(defregisters
  ((lr spr 0 integer)
   (cr spr 1 integer)
   (xer spr 2 integer)
   (ctr spr 3 integer)
   (fpscr spr 4 integer)
   (r0 gpr 0 integer)
   (r1 gpr 1 integer)
   (r2 gpr 2 integer)
   (r3 gpr 3 integer)
   (r4 gpr 4 integer)
   (r5 gpr 5 integer)
   (r6 gpr 6 integer)
   (r7 gpr 7 integer)
   (r8 gpr 8 integer)
   (r9 gpr 9 integer)
   (r10 gpr 10 integer)
   (r11 gpr 11 integer)
   (r12 gpr 12 integer)
   (r13 gpr 13 integer)
   (r14 gpr 14 integer)
   (r15 gpr 15 integer)
   (r16 gpr 16 integer)
   (r17 gpr 17 integer)
   (r18 gpr 18 integer)
   (r19 gpr 19 integer)
   (r20 gpr 20 integer)
   (r21 gpr 21 integer)
   (r22 gpr 22 integer)
   (r23 gpr 23 integer)
   (r24 gpr 24 integer)
   (r25 gpr 25 integer)
   (r26 gpr 26 integer)
   (r27 gpr 27 integer)
   (r28 gpr 28 integer)
   (r29 gpr 29 integer)
   (r30 gpr 30 integer)
   (r31 gpr 31 integer)))

(defconditions
  ((cr0-lt cr 31)
   (cr0-gt cr 30)
   (cr0-eq cr 29)
   (cr0-so cr 28)
   (xer-so xer 31)))

;;;; insn definition macros

(defmacro defrcinsn (name args result-reg body)
  `(progn
     (definsn ,name ,args
       ,body)
     (definsn ,(intern (string-concat (string name) ".")) ,args
       (seq
	,body
	(seq
	 (set cr0-lt (<is (register (gpr ,result-reg)) 0))
	 (seq
	  (set cr0-gt (<is 0 (register (gpr ,result-reg))))
	  (seq
	   (set cr0-eq (=i (register (gpr ,result-reg)) 0))
	   (set cr0-so xer-so))))))))

;;;; machine macros

(defmachinemacro ppc-mask (begin-bit end-bit)
  (mask (-i 31 end-bit) (-i 31 begin-bit)))

;(defmachinemacro set-rc0-bits (val)
;  (seq
;   (set cr0-lt (<is 4 val 0))
;   (seq
;    (set cr0-gt (<is 4 0 val))
;    (seq
;     (set cr0-eq (=i 4 val 0))
;     (set cr0-so xer-so)))))

;;;; insns

(defrcinsn add () rd
  (set (gpr rd) (+i (register (gpr ra)) (register (gpr rb)))))

;; addo
;; addc
;; addco
;; adde
;; addeo

(definsn addi ((ra 0 32) (simm (0)))
  (set (gpr rd) (if (int-zero-p ra)
		    (sex 2 simm)
		  (+i (register (gpr ra)) (sex 2 simm)))))

;; addic
;; addic.

(definsn addis ((ra 0 32) (simm (0)))
  (set (gpr rd) (if (int-zero-p ra)
		    (shiftl simm 16)
		  (+i (register (gpr ra)) (shiftl simm 16)))))

;; addme
;; addmeo
;; addze
;; addzeo

(defrcinsn and () ra
  (set (gpr ra) (bit-and (register (gpr rs)) (register (gpr rb)))))

(defrcinsn andc () ra
  (set (gpr ra) (bit-and (register (gpr rs)) (bit-neg (register (gpr rb))))))

(definsn andi. ((uimm (0)))
  (seq
   (set (gpr ra) (bit-and (register (gpr rs)) (zex 2 uimm)))
   (seq
    (set cr0-lt (<is (register (gpr ra)) 0))
    (seq
     (set cr0-gt (<is 0 (register (gpr ra))))
     (seq
      (set cr0-eq (=i (register (gpr ra)) 0))
      (set cr0-so xer-so))))))

(definsn andis. ((uimm (0)))
  (seq
   (set (gpr ra) (bit-and (register (gpr rs)) (shiftl (zex 2 uimm) 16)))
   (seq
    (set cr0-lt (<is (register (gpr ra)) 0))
    (seq
     (set cr0-gt (<is 0 (register (gpr ra))))
     (seq
      (set cr0-eq (=i (register (gpr ra)) 0))
      (set cr0-so xer-so))))))

;; b
;; bctrl
;; bdnz
;; bdz
;; beq
;; beq+
;; beqlr
;; bl
;; blr
;; blrl
;; bne
;; bne-
;; bnelr
;; bnelr+
;; cmplw
;; cmplwi
;; cmpw
;; cmpwi
;; cntlzw
;; crand
;; crandc
;; creqv
;; crnand
;; crnor
;; cror
;; crorc
;; crxor
;; dcbst
;; dcbz

(defrcinsn divw () rd
  (set (gpr rd) (/is 4 0 (register (gpr ra)) (register (gpr rb)))))

;; divwo

(defrcinsn divwu () rd
  (set (gpr rd) (/iu 4 0 (register (gpr ra)) (register (gpr rb)))))

;; divwuo

(defrcinsn eqv () ra
  (set (gpr ra) (bit-xor (register (gpr rs)) (bit-neg (register (gpr rb))))))

(definsn extsb ()
  (set (gpr ra) (sex 1 (register (gpr rs)))))

(defrcinsn extsh () ra
  (set (gpr ra) (sex 2 (register (gpr rs)))))

;; icbi
;; isync

(definsn lbz ((ra 0 32) (d (0)))
  (set (gpr rd) (zex 1 (load-byte (if (int-zero-p ra)
				      (sex 2 d)
				    (+i (register (gpr ra)) (sex 2 d)))))))

(definsn lbzu ((d (0)))
  (seq
   (set (gpr ra) (+i (register (gpr ra)) (sex 2 d)))
   (set (gpr rd) (zex 1 (load-byte (register (gpr ra)))))))

(definsn lbzux ()
  (seq
   (set (gpr ra) (+i (register (gpr ra)) (register (gpr rb))))
   (set (gpr rd) (zex 1 (load-byte (register (gpr ra)))))))

(definsn lbzx ((ra 0 32))
  (set (gpr rd) (zex 1 (load-byte (if (int-zero-p ra)
				      (register (gpr rb))
				    (+i (register (gpr ra)) (register (gpr rb))))))))

(definsn lha ((ra 0 32) (d (0)))
  (set (gpr rd) (sex 2 (load big-endian 2 (if (int-zero-p ra)
					      (sex 2 d)
					    (+i (register (gpr ra)) (sex 2 d)))))))

(definsn lhau ((d (0)))
  (seq
   (set (gpr ra) (+i (register (gpr ra)) (sex 2 d)))
   (set (gpr rd) (sex 2 (load big-endian 2 (register (gpr ra)))))))

(definsn lhax ((ra 0 32))
  (set (gpr rd) (sex 2 (load big-endian 2 (if (int-zero-p ra)
					      (register (gpr rb))
					    (+i (register (gpr ra)) (register (gpr rb))))))))

;; lhbrx

(definsn lhz ((ra 0 32) (d (0)))
  (set (gpr rd) (zex 2 (load big-endian 2 (if (int-zero-p ra)
					      (sex 2 d)
					    (+i (register (gpr ra)) (sex 2 d)))))))

(definsn lhzu ((ra 0 32))
  (seq
   (set (gpr ra) (+i (register (gpr ra)) (sex 2 d)))
   (set (gpr rd) (zex 2 (load big-endian 2 (register (gpr ra)))))))

;; lhzux

(definsn lhzx ((ra 0 32))
  (set (gpr rd) (zex 2 (load big-endian 2 (if (int-zero-p ra)
					      (register (gpr rb))
					    (+i (register (gpr ra)) (register (gpr rb))))))))

;; lwbrx

(definsn lwz ((ra 0 32) (d (0)))
  (set (gpr rd) (load big-endian 4 (if (int-zero-p ra)
				       (sex 2 d)
				     (+i (register (gpr ra)) (sex 2 d))))))

(definsn lwzu ((ra 0 32))
  (seq
   (set (gpr ra) (+i (register (gpr ra)) (sex 2 d)))
   (set (gpr rd) (load big-endian 4 (register (gpr ra))))))

(definsn lwzux ()
  (seq
   (set (gpr ra) (+i (register (gpr ra)) (register (gpr rb))))
   (set (gpr rd) (load big-endian 4 (register (gpr ra))))))

(definsn lwzx ((ra 0 32))
  (set (gpr rd) (load big-endian 4 (if (int-zero-p ra)
				       (register (gpr rb))
				     (+i (register (gpr ra)) (register (gpr rb)))))))

;; mcrf
;; mcrxr
;; mfcr
;; mfctr
;; mflr
;; mfxer
;; mtcrf
;; mtctr
;; mtlr
;; mtxer

(definsn mulhw ()
  (set (gpr rd) (ashiftr 8 (*i (sex 4 (register (gpr ra)))
			       (sex 4 (register (gpr rb))))
			 32)))

(definsn mulhwu ()
  (set (gpr rd) (ashiftr 8 (*i (zex 4 (register (gpr ra)))
			       (zex 4 (register (gpr rb))))
			 32)))

(definsn mulli ()
  (set (gpr rd) (*i (register (gpr ra)) (sex 2 simm))))

(defrcinsn mullw () rd
  (set (gpr rd) (*i (register (gpr ra)) (register (gpr rb)))))

;; mullwo

(defrcinsn nand () ra
  (set (gpr ra) (bit-neg (bit-and (register (gpr rs)) (register (gpr rb))))))

(defrcinsn neg () rd
  (set (gpr rd) (int-neg (register (gpr ra)))))

;; nego

(defrcinsn nor () ra
  (set (gpr ra) (bit-neg (bit-or (register (gpr rs)) (register (gpr rb))))))

(defrcinsn or () ra
  (set (gpr ra) (bit-or (register (gpr rs)) (register (gpr rb)))))

(defrcinsn orc () ra
  (set (gpr ra) (bit-or (register (gpr rs)) (bit-neg (register (gpr rb))))))

(definsn ori ()
  (set (gpr ra) (bit-or (register (gpr rs)) (zex 2 uimm))))

(definsn oris ()
  (set (gpr ra) (bit-or (register (gpr rs)) (shiftl (zex 2 uimm) 16))))

(definsn rlwimi ((sh 0 32) (mb 0 32) (me 0 32))
  (set (gpr ra) (bit-or (bit-and (rotl 4 (register (gpr rs)) sh) (ppc-mask mb me))
			(bit-and (register (gpr ra)) (bit-neg (ppc-mask mb me))))))
		  
(definsn rlwinm ((sh 0 32) (mb 0 32) (me 0 32))
  (set (gpr ra) (bit-and (rotl 4 (register (gpr rs)) sh) (ppc-mask mb me))))

(definsn rlwnm ((mb 0 32) (me 0 32))
  (set (gpr ra) (bit-and (rotl 4 (register (gpr rs)) (bit-and (register (gpr rb)) #x1f))
			 (ppc-mask mb me))))

;; sc

(definsn slw ()
  (set (gpr ra) (shiftl (register (gpr rs)) (bit-and (register (gpr rb)) #x3f))))

;; sraw
;; srawi

(defrcinsn srw () ra
  (set (gpr ra) (lshiftr 4 (register (gpr rs)) (bit-and (register (gpr rb)) #x1f))))

(definsn stb ((ra 0 32) (d (0)))
  (store big-endian 1 (if (int-zero-p ra)
			  (sex 2 d)
			  (+i (register (gpr ra)) (sex 2 d)))
	 (register (gpr srs))))

(definsn stbu ((ra 0 32) (srs 0 32) (d (0)))
  (if (=i ra srs)
      (seq
       (store big-endian 1 (+i (register (gpr ra)) (sex 2 d)) (register (gpr srs)))
       (set (gpr ra) (+i (register (gpr ra)) (sex 2 d))))
      (seq
       (set (gpr ra) (+i (register (gpr ra)) (sex 2 d)))
       (store big-endian 1 (register (gpr ra)) (register (gpr srs))))))

;; stbux

(definsn stbx ((ra 0 32))
  (store big-endian 1 (if (int-zero-p ra)
			  (register (gpr rb))
			  (+i (register (gpr ra)) (register (gpr rb))))
	 (register (gpr srs))))

(definsn sth ((ra 0 32) (d (0)))
  (store big-endian 2 (if (int-zero-p ra)
			  (sex 2 d)
			  (+i (register (gpr ra)) (sex 2 d)))
	 (register (gpr srs))))

;; sthbrx

(definsn sthu ((ra 0 32) (srs 0 32) (d (0)))
  (if (=i ra srs)
      (seq
       (store big-endian 2 (+i (register (gpr ra)) (sex 2 d)) (register (gpr srs)))
       (set (gpr ra) (+i (register (gpr ra)) (sex 2 d))))
      (seq
       (set (gpr ra) (+i (register (gpr ra)) (sex 2 d)))
       (store big-endian 2 (register (gpr ra)) (register (gpr srs))))))
      
;; sthux

(definsn sthx ((ra 0 32))
  (store big-endian 2 (if (int-zero-p ra)
			  (register (gpr rb))
			  (+i (register (gpr ra)) (register (gpr rb))))
	 (register (gpr srs))))

(definsn stw ((ra 0 32) (d (0)))
  (store big-endian 4 (if (int-zero-p ra)
			  (sex 2 d)
			  (+i (register (gpr ra)) (sex 2 d)))
	 (register (gpr srs))))

;; stwbrx

(definsn stwu ((ra 0 32) (srs 0 32) (d (0)))
  (if (=i ra srs)
      (seq
       (store big-endian 4 (+i (register (gpr ra)) (sex 2 d)) (register (gpr srs)))
       (set (gpr ra) (+i (register (gpr ra)) (sex 2 d))))
      (seq
       (set (gpr ra) (+i (register (gpr ra)) (sex 2 d)))
       (store big-endian 4 (register (gpr ra)) (register (gpr srs))))))

(definsn stwux ((ra 0 32) (srs 0 32))
  (if (=i ra srs)
      (seq
       (store big-endian 4 (+i (register (gpr ra)) (register (gpr rb))) (register (gpr srs)))
       (set (gpr ra) (+i (register (gpr ra)) (register (gpr rb)))))
      (seq
       (set (gpr ra) (+i (register (gpr ra)) (register (gpr rb))))
       (store big-endian 4 (register (gpr ra)) (register (gpr srs))))))

(definsn stwx ((ra 0 32))
  (store big-endian 4 (if (int-zero-p ra)
			  (register (gpr rb))
			  (+i (register (gpr ra)) (register (gpr rb))))
	 (register (gpr srs))))

(defrcinsn subf () rd
  (set (gpr rd) (-i (register (gpr rb)) (register (gpr ra)))))

;; subfo
;; subfc
;; subfco
;; subfe
;; subfeo
;; subfic
;; subfme
;; subfmeo
;; subfze
;; subfzeo

;; sync

(defrcinsn xor () ra
  (set (gpr ra) (bit-xor (register (gpr rs)) (register (gpr rb)))))

(definsn xori ()
  (set (gpr ra) (bit-xor (register (gpr rs)) (zex 2 uimm))))

(definsn xoris ()
  (set (gpr ra) (bit-xor (register (gpr rs)) (shiftl (zex 2 uimm) 16))))
