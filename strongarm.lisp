;; strongarm.lisp

;; bintrans

;; Copyright (C) 2003 Mark Probst

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

(new-machine 'arm 'big nil)

(setf (machine-insn-bits *this-machine*) 32)
(setf (machine-word-bits *this-machine*) 32)

(define-register-class 'spr 'integer 32
		       '(cpsr fpscr))

(define-register-class 'gpr 'integer 32
		       '(r0 r1 r2 r3 r4 r5 r6 r7
			 r8 r9 r10 r11 r12 r13 r14 r15))

(define-fields
    '((a 21 21)
      (b 22 22)
      (bit20 20 20)
      (bit21 21 21)
      (bit22 22 22)
      (bit23 23 23)
      (bit24 24 24)
      (bit4 4 4)
      (bit7 7 7)
      (bits4-7 4 7)
      (bits8-11 8 11)
      (cond 28 31)
      (cp-num 8 11)
      (cp-opcode3 21 23)
      (cp-opcode4 20 23)
      (crd 12 15)
      (crm 0 3)
      (crn 16 19)
      (h 5 5)
      (i 25 25)
      (ignore0-23 0 23)
      (ignore0-3 0 3)
      (ignore5-24 5 24)
      (l1 20 20)
      (l2 24 24)
      (mask 16 19)
      (n 22 22)
      (offset12 0 11)
      (offset24 0 23)
      (offset4-1 0 3)
      (offset4-2 8 11)
      (offset8 0 7)
      (opcode1 25 27)
      (opcode2 21 24)
      (opcode-mrs-1 23 27)
      (opcode-mul 22 27)
      (p 24 24)
      (pd 22 22)
      (ps 22 22)
      (rd 12 15)
      (reg-list 0 15)
      (rm 0 3)
      (rn 16 19)
      (rotate 8 11)
      (rs 8 11)
      (shift-control 4 4)
      (shift-type 5 6)
      (shift-amount 7 11)
      (shift-register 8 11)
      (s1 20 20)
      (s2 6 6)
      (s3 22 22)
      (u 23 23)
      (w 21 21)))

(defmacro define-cond-insn (name fields effect asm)
  `(define-insn ,name ,fields ,effect
    (,(format nil "~A%s ~A" name (car asm)) ("cond_names[~A]" cond) ,@(cdr asm))))

(defmacro define-cond-s-insn (name s fields effect asm)
  `(define-insn ,name ,fields ,effect
    (,(format nil "~A%s%s ~A" name (car asm)) ("cond_names[~A]" cond) ("(~A ? \"S\" : \"\")" ,s) ,@(cdr asm))))

(defmacro define-operate-insn (name opcode effect asm)
  `(progn
    (define-cond-s-insn ,name s1
      ((bit26 0)
       (bit27 0)
       (opcode2 ,opcode)
       (i 0)
       (shift-control 0))
      ,(my-macroexpand effect `((op2 . ,#'(lambda () '())))) ;FIXME
      (,(format nil (car asm) "r%d, %s %d") ,@(cdr asm) rm ("shift_type_names[~A]" shift-type) shift-amount))
    (define-cond-s-insn ,name s1
      ((bit26 0)
       (bit27 0)
       (opcode2 ,opcode)
       (i 0)
       (shift-control 1))
      ,(my-macroexpand effect `((op2 . ,#'(lambda () '()))))
      (,(format nil (car asm) "r%d, %s r%d") ,@(cdr asm) rm ("shift_type_names[~A]" shift-type) rs))
    (define-cond-s-insn ,name s1
      ((bit26 0)
       (bit27 0)
       (opcode2 ,opcode)
       (i 1)
       ,(my-macroexpand effect `((op2 . ,#'(lambda () '()))))
       (,(format nil (car asm) "0x%x") ,@(cdr asm) (width 32 (rotl (zex offset8) (* 2 rotate))))))))

(define-cond-insn b
    ((opcode1 5)
     (l 0))
  ((not-implemented))
  ("0x%x" (width 32 (+ (+ addr 8) (shiftl (sex offset24) 2)))))

(define-cond-insn bl
    ((opcode1 5)
     (l 1))
  ((not-implemented))
  ("0x%x" (width 32 (+ (+ addr 8) (shiftl (sex offset24) 2)))))

(define-operate-insn and 0
    ((not-implemented))
  ("r%d, r%d, ~A" rd rn))

(define-operate-insn tst 8
    ((not-implemented))
  ("r%d, ~A" rn))

(define-cond-insn msr
    ((bit26 0)
     (bit27 0)
     (bit23 0)
     (bit24 1)
     (bit20 0)
     (bit21 1)
     (rd 15)
     (i 0))
  ((not-implemented))
  ("%s_%s, r%d" ("psr_dest_names[~A]" pd) ("psr_flag_names[~A]" mask) rm))

(define-cond-insn msr
    ((bit26 0)
     (bit27 0)
     (bit23 0)
     (bit24 1)
     (bit20 0)
     (bit21 1)
     (rd 15)
     (i 1))
  ((not-implemented))
  ("%s_%s, 0x%x" ("psr_names[~A]" pd) ("psr_flag_names[~A]" mask) (width 32 (rotl (zex offset8) (* 2 rotate)))))

(define-cond-insn mrs
    ((opcode-mrs-1 2)
     (opcode-mrs-2 15)
     (offset12 0))
  ((not-implemented))
  ("r%d, %s" rd ("psr_names[~A]" ps)))

(define-cond-s-insn mul s1
  ((opcode-mul 0)
   (a 0)
   (bits4-7 9))
  ((not-implemented))
  ("r%d, r%d, r%d" rn rm rs))

(define-cond-s-insn mla s1
  ((opcode-mul 0)
   (a 1)
   (bits4-7 9))
  ((not-implemented))
  ("r%d, r%d, r%d, r%d" rn rm rs rd))
