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

(deffields ((rs 5) (rd 5) (ra 5) (rb 5) (sh 5) (mb 5) (me 5) (simm 16) (d 16)))

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
				(sex 4 (+i (register ra) (sex 2 d))))))))

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
			      (sex 4 (+i (register ra) (register rb))))))))

(definsn rlwimi ((sh 0 32) (mb 0 32) (me 0 32))
  (set ra (bit-or (bit-and (rotl 4 (register rs) sh) (ppc-mask mb me))
		  (bit-and (register ra) (bit-neg (ppc-mask mb me))))))
		  
(definsn rlwinm ((sh 0 32) (mb 0 32) (me 0 32))
  (set ra (bit-and (rotl 4 (register rs) sh) (ppc-mask mb me))))

(definsn rlwnm ((mb 0 32) (me 0 32))
  (set ra (bit-and (rotl 4 (register rs) (bit-and (register rb) #x1f))
		   (ppc-mask mb me))))
