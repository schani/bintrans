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
(setq *insns* '())
(setq *machine-macros* '())

(deffields ((rs 5) (rd 5) (ra 5) (rb 5) (srs 5) (sh 5) (mb 5) (me 5) (simm 16) (uimm 16) (d 16)))

;;;; machine macros

(defmachinemacro ppc-mask (begin-bit end-bit)
  (mask (-i 31 end-bit) (-i 31 begin-bit)))

;;;; insns

(definsn add ()
  (set rd (+i (register ra) (register rb))))

;; addo
;; addc
;; addco
;; adde
;; addeo

(definsn addi ((ra 0 32) (simm (0)))
  (set rd (if (int-zero-p 8 ra)
	      (sex 2 simm)
	    (+i (register ra) (sex 2 simm)))))

;; addic
;; addic.

(definsn addis ((ra 0 32) (simm (0)))
  (set rd (if (int-zero-p 8 ra)
	      (shiftl simm 16)
	    (+i (register ra) (shiftl simm 16)))))

;; addme
;; addmeo
;; addze
;; addzeo

(definsn and ()
  (set ra (bit-and (register rs) (register rb))))

(definsn andc ()
  (set ra (bit-and (register rs) (bit-neg (register rb)))))

;; andi.
;; andis.
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

(definsn divw ()
  (set rd (/is 4 0 (register ra) (register rb))))

;; divwo

(definsn divwu ()
  (set rd (/iu 4 0 (register ra) (register rb))))

;; divwuo

(definsn eqv ()
  (set ra (bit-xor (register rs) (bit-neg (register rb)))))

(definsn extsb ()
  (set ra (sex 1 (register rs))))

(definsn extsh ()
  (set ra (sex 2 (register rs))))

;; icbi
;; isync

(definsn lbz ((ra 0 32) (d (0)))
  (set rd (zex 1 (load-byte (if (int-zero-p 8 ra)
				(sex 2 d)
			      (+i (register ra) (sex 2 d)))))))

(definsn lbzu ((d (0)))
  (seq
   (set ra (+i (register ra) (sex 2 d)))
   (set rd (zex 1 (load-byte (register ra))))))

(definsn lbzux ()
  (seq
   (set ra (+i (register ra) (register rb)))
   (set rd (zex 1 (load-byte (register ra))))))

(definsn lbzx ((ra 0 32))
  (set rd (zex 1 (load-byte (if (int-zero-p 8 ra)
				(register rb)
			      (+i (register ra) (register rb)))))))

(definsn lha ((ra 0 32) (d (0)))
  (set rd (sex 2 (load big-endian 2 (if (int-zero-p 8 ra)
					(sex 2 d)
				      (+i (register ra) (sex 2 d)))))))

(definsn lhau ((d (0)))
  (seq
   (set ra (+i (register ra) (sex 2 d)))
   (set rd (sex 2 (load big-endian 2 (register ra))))))

(definsn lhax ((ra 0 32))
  (set rd (sex 2 (load big-endian 2 (if (int-zero-p 8 ra)
					(register rb)
				      (+i (register ra) (register rb)))))))

;; lhbrx

(definsn lhz ((ra 0 32) (d (0)))
  (set rd (zex 2 (load big-endian 2 (if (int-zero-p 8 ra)
					(sex 2 d)
				      (+i (register ra) (sex 2 d)))))))

(definsn lhzu ((ra 0 32))
  (seq
   (set ra (+i (register ra) (sex 2 d)))
   (set rd (zex 2 (load big-endian 2 (register ra))))))

;; lhzux

(definsn lhzx ((ra 0 32))
  (set rd (zex 2 (load big-endian 2 (if (int-zero-p 8 ra)
					(register rb)
				      (+i (register ra) (register rb)))))))

;; lwbrx

(definsn lwz ((ra 0 32) (d (0)))
  (set rd (load big-endian 4 (if (int-zero-p 8 ra)
				 (sex 2 d)
			       (+i (register ra) (sex 2 d))))))

(definsn lwzu ((ra 0 32))
  (seq
   (set ra (+i (register ra) (sex 2 d)))
   (set rd (load big-endian 4 (register ra)))))

(definsn lwzux ()
  (seq
   (set ra (+i (register ra) (register rb)))
   (set rd (load big-endian 4 (register ra)))))

(definsn lwzx ((ra 0 32))
  (set rd (load big-endian 4 (if (int-zero-p 8 ra)
				 (register rb)
			       (+i (register ra) (register rb))))))

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
  (set rd (ashiftr 8 (*i (sex 4 (register ra))
			 (sex 4 (register rb)))
		   32)))

(definsn mulhwu ()
  (set rd (ashiftr 8 (*i (zex 4 (register ra))
			 (zex 4 (register rb)))
		   32)))

(definsn mulli ()
  (set rd (*i (register ra) (sex 2 simm))))

(definsn mullw ()
  (set rd (*i (register ra) (register rb))))

;; mullwo

(definsn nand ()
  (set ra (bit-neg (bit-and (register rs) (register rb)))))

(definsn neg ()
  (set rd (int-neg (register ra))))

;; nego

(definsn nor ()
  (set ra (bit-neg (bit-or (register rs) (register rb)))))

(definsn or ()
  (set ra (bit-or (register rs) (register rb))))

(definsn orc ()
  (set ra (bit-or (register rs) (bit-neg (register rb)))))

(definsn ori ()
  (set ra (bit-or (register rs) (zex 2 uimm))))

(definsn oris ()
  (set ra (bit-or (register rs) (shiftl (zex 2 uimm) 16))))

(definsn rlwimi ((sh 0 32) (mb 0 32) (me 0 32))
  (set ra (bit-or (bit-and (rotl 4 (register rs) sh) (ppc-mask mb me))
		  (bit-and (register ra) (bit-neg (ppc-mask mb me))))))
		  
(definsn rlwinm ((sh 0 32) (mb 0 32) (me 0 32))
  (set ra (bit-and (rotl 4 (register rs) sh) (ppc-mask mb me))))

(definsn rlwnm ((mb 0 32) (me 0 32))
  (set ra (bit-and (rotl 4 (register rs) (bit-and (register rb) #x1f))
		   (ppc-mask mb me))))

;; sc

(definsn slw ()
  (set ra (shiftl (register rs) (bit-and (register rb) #x3f))))

;; sraw
;; srawi

(definsn srw ()
  (set ra (lshiftr 4 (register rs) (bit-and (register rb) #x1f))))

(definsn stb ((ra 0 32) (d (0)))
  (store big-endian 1 (if (int-zero-p 8 ra)
			  (sex 2 d)
			  (+i (register ra) (sex 2 d)))
	 (register srs)))

(definsn stbu ((ra 0 32) (srs 0 32) (d (0)))
  (if (=i 8 ra srs)
      (seq
       (store big-endian 1 (+i (register ra) (sex 2 d)) (register srs))
       (set ra (+i (register ra) (sex 2 d))))
      (seq
       (set ra (+i (register ra) (sex 2 d)))
       (store big-endian 1 (register ra) (register srs)))))

;; stbux

(definsn stbx ((ra 0 32))
  (store big-endian 1 (if (int-zero-p 8 ra)
			  (register rb)
			  (+i (register ra) (register rb)))
	 (register srs)))

(definsn sth ((ra 0 32) (d (0)))
  (store big-endian 2 (if (int-zero-p 8 ra)
			  (sex 2 d)
			  (+i (register ra) (sex 2 d)))
	 (register srs)))

;; sthbrx

(definsn sthu ((ra 0 32) (srs 0 32) (d (0)))
  (if (=i 8 ra srs)
      (seq
       (store big-endian 2 (+i (register ra) (sex 2 d)) (register srs))
       (set ra (+i (register ra) (sex 2 d))))
      (seq
       (set ra (+i (register ra) (sex 2 d)))
       (store big-endian 2 (register ra) (register srs)))))
      
;; sthux

(definsn sthx ((ra 0 32))
  (store big-endian 2 (if (int-zero-p 8 ra)
			  (register rb)
			  (+i (register ra) (register rb)))
	 (register srs)))

(definsn stw ((ra 0 32) (d (0)))
  (store big-endian 4 (if (int-zero-p 8 ra)
			  (sex 2 d)
			  (+i (register ra) (sex 2 d)))
	 (register srs)))

;; stwbrx

(definsn stwu ((ra 0 32) (srs 0 32) (d (0)))
  (if (=i 8 ra srs)
      (seq
       (store big-endian 4 (+i (register ra) (sex 2 d)) (register srs))
       (set ra (+i (register ra) (sex 2 d))))
      (seq
       (set ra (+i (register ra) (sex 2 d)))
       (store big-endian 4 (register ra) (register srs)))))

(definsn stwux ((ra 0 32) (srs 0 32))
  (if (=i 8 ra srs)
      (seq
       (store big-endian 4 (+i (register ra) (register rb)) (register srs))
       (set ra (+i (register ra) (register rb))))
      (seq
       (set ra (+i (register ra) (register rb)))
       (store big-endian 4 (register ra) (register srs)))))

(definsn stwx ((ra 0 32))
  (store big-endian 4 (if (int-zero-p 8 ra)
			  (register rb)
			  (+i (register ra) (register rb)))
	 (register srs)))

(definsn subf ()
  (set rd (-i (register rb) (register ra))))

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

(definsn xor ()
  (set ra (bit-xor (register rs) (register rb))))

(definsn xori ()
  (set ra (bit-xor (register rs) (zex 2 uimm))))

(definsn xoris ()
  (set ra (bit-xor (register rs) (shiftl (zex 2 uimm) 16))))
