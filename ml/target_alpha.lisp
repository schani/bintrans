;; target_alpha.lisp

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

(setq *matchers* '())

(defmacro defopmatcher (name rhs c-format &key (zero nil))
  (let ((a-rhs (subst '(register ?ra) 'a rhs)))
    `(progn
      (defmatcher ,name
	(set ?rs ,(subst '(register ?rb) 'b a-rhs))
	1
	(,(format nil "~~A = ~A;" c-format) rs ra rb)
	(,(format nil "emit(COMPOSE_~A(~~A, ~~A, ~~A));" name) ra rb rs))
      (defmatcher ,(intern (string-concat (string name) "-IMM"))
	(set ?rs ,(subst '(any-int ?i) 'b a-rhs))
	(when (int-zero-p (lshiftr 8 i 8))
	  1)
	(,(format nil "~~A = ~A;" c-format) rs ra i)
	(,(format nil "emit(COMPOSE_~A_IMM(~~A, ~~A, ~~A));" name) ra i rs))
      ,(if zero
	   `(defmatcher ,(intern (string-concat (string name) "-31"))
	      (set ?rs ,(subst '(register ?rb) 'b (subst '0 'a rhs)))
	      1
	      (,(format nil "~~A = ~A;" (format nil c-format "0LL" "~A")) rs rb)
	      (,(format nil "emit(COMPOSE_~A(31, ~~A, ~~A));" name) rb rs))
	 '()))))

(defmacro defldmatcher (name rhs c-format mnemonic)
  `(progn
     (defmatcher ,name
       (set ?ra ,(subst '(register ?rb) 'ea rhs))
       2
       (,(format nil "~~A = ~A;" c-format) ra rb)
       (,(format nil "emit(COMPOSE_~A(~~A, 0, ~~A));" mnemonic) ra rb))
     (defmatcher ,(intern (string-concat (string name) "-DISP"))
       (set ?ra ,(subst '(+i (register ?rb) (any-int ?disp)) 'ea rhs))
       (when (zero-or-full-p (ashiftr 8 disp 16))
	 2)
       (,(format nil "~~A = ~A;" (format nil c-format "~A + ~A")) ra disp rb)
       (,(format nil "emit(COMPOSE_~A(~~A, ~~A & 0xffff, ~~A));" mnemonic) ra disp rb))))

(defmacro defstmatcher (name width mnemonic)
  `(progn
    (defmatcher ,name
      (store little-endian ,width (register ?ra) (register ?rb))
      2
      (,(format nil "mem_store_~A(~~A, ~~A);" (* width 8)) ra rb)
      (,(format nil "emit(COMPOSE_~A(~~A, 0, ~~A));" mnemonic) rb ra))
    (defmatcher ,(intern (string-concat (string name) "-DISP"))
      (store little-endian ,width (+i (register ?ra) (any-int ?disp)) (register ?rb))
      (when (zero-or-full-p (ashiftr 8 disp 16))
	2)
      (,(format nil "mem_store_~A(~~A + ~~A, ~~A);" (* width 8)) ra disp rb)
      (,(format nil "emit(COMPOSE_~A(~~A, ~~A & 0xffff, ~~A));" mnemonic) rb disp ra))))

; we don't add the immediate version of addq because it performs a subset of
; the functionality of lda
(defmatcher addq
  (set ?rs (+i (register ?ra) (register ?rb)))
  1
  ("~A = ~A + ~A;" rs ra rb)
  ("emit(COMPOSE_ADDQ(~A, ~A, ~A));" ra rb rs))

(defopmatcher addl (sex 4 (+i a b)) "sex_32(~A + ~A)")

(defmatcher addl-31
  (set ?rs (sex 4 (register ?ra)))
  1
  ("~A = sex_32(~A);" rs ra)
  ("emit(COMPOSE_ADDL(~A, 31, ~A));" ra rs))

(defopmatcher eqv (bit-xor a (bit-neg b)) "~A ^ ~~(~A)")

(defmatcher lda
  (set ?rs (+i (register ?ra) (any-int ?i)))
  (when (zero-or-full-p (ashiftr 8 i 15))
    1)
  ("~A = ~A + ~A;" rs ra i)
  ("emit(COMPOSE_LDA(~A, ~A & 0xffff, ~A));" rs i ra))

(defmatcher lda-31
  (set ?rs (any-int ?i))
  (when (zero-or-full-p (ashiftr 8 i 15))
    1)
  ("~A = ~A;" rs i)
  ("emit(COMPOSE_LDA(~A, ~A & 0xffff, 31));" rs i))

(defmatcher ldah
  (set ?rs (+i (register ?ra) (any-int ?i)))
  (when (and (int-zero-p (bit-and i #xffff))
	     (zero-or-full-p (ashiftr 8 i 31)))
    1)
  ("~A = ~A + ~A;" rs ra i)
  ("emit(COMPOSE_LDAH(~A, (~A >> 16) & 0xffff, ~A));" rs i ra))

(defmatcher ldah-31
  (set ?rs (any-int ?i))
  (when (and (int-zero-p (bit-and i #xffff))
	     (zero-or-full-p (ashiftr 8 i 31)))
    1)
  ("~A = ~A;" rs i)
  ("emit(COMPOSE_LDAH(~A, (~A >> 16) & 0xffff, 31));" rs i))

(defmatcher load-int
  (set ?rs (any-int ?i))
  2
  ("~A = ~A;" rs i)
  ("emit_load_integer_64(~A, ~A);" rs i))

(defmatcher mov
  (set ?rs (register ?ra))
  1
  ("~A = ~A;" rs ra)
  ("emit(COMPOSE_MOV(~A, ~A));" ra rs))

(defopmatcher and (bit-and a b) "~A & ~A")

(defmatcher bic-imm-for-and
  (set ?rs (bit-and (register ?ra) (any-int ?i)))
  (when (full-mask-p (ashiftr 8 i 8))
    1)
  ("~A = ~A & ~A;" rs ra i)
  ("emit(COMPOSE_BIC_IMM(~A, unary_BitNeg(~A), ~A));" ra i rs))

(defmatcher zapnot-imm
  (set ?rs (bit-and (register ?ra) (any-int ?i)))
  (when (user-op "IsMaskMask" i 8)
    1)
  ("~A = ~A & ~A;" rs ra i)
  ("emit(COMPOSE_ZAPNOT_IMM(~A, reverse_maskmask(~A, 8), ~A));" ra i rs))

(defopmatcher bis (bit-or a b) "~A | ~A")
(defopmatcher bic (bit-and a (bit-neg b)) "~A | ~A")

(defopmatcher mull (sex 4 (*i a b)) "sex_32(~A * ~A)")
(defopmatcher mulq (*i a b) "~A * ~A")

(defmatcher neg
  (set ?rs (bit-neg (register ?rb)))
  1
  ("~A = ~~~A;" rs rb)
  ("emit(COMPOSE_NOT(~A, ~A));" rb rs))

(defmatcher negl
  (set ?rs (sex 4 (int-neg (register ?rb))))
  1
  ("~A = sex_32(~~~A);" rs rb)
  ("emit(COMPOSE_NEGL(~A, ~A));" rb rs))

(defmatcher negq
  (set ?rs (int-neg (register ?rb)))
  1
  ("~A = ~~~A;" rs rb)
  ("emit(COMPOSE_NEGQ(~A, ~A));" rb rs))

(defmatcher ornot
  (set ?rs (bit-or (register ?ra) (bit-neg (register ?rb))))
  1
  ("~A = ~A | ~~~A;" rs ra rb)
  ("emit(COMPOSE_ORNOT(~A, ~A, ~A));" ra rb rs))

(defopmatcher sll (shiftl a b) "~A << ~A")
(defopmatcher sra (ashiftr 8 a b) "ashiftr_64(~A, ~A)")
(defopmatcher srl (lshiftr 8 a b) "~A >> ~A")

(defmatcher zapnot-imm-srl
  (set ?rs (lshiftr (?width (1 2 4)) (register ?ra) (register ?rb)))
  2
  ("~A = (~A & width_mask(~A)) >> ~A;" rs ra width rb)
  ("emit(compose_width_zapnot(~A, ~A, ~A)); emit(COMPOSE_SRL(~A, ~A, ~A));" ra width rs rs rb rs))

(defmatcher zapnot-imm-srl-imm
  (set ?rs (lshiftr (?width (1 2 4)) (register ?ra) (any-int ?i)))
  2
  ("~A = (~A & width_mask(~A)) >> ~A;" rs ra width i)
  ("emit(compose_width_zapnot(~A, ~A, ~A)); emit(COMPOSE_SRL_IMM(~A, ~A, ~A));" ra width rs rs i rs))

(defopmatcher srl (lshiftr 8 a b) "~A >> ~A")

(defmatcher extbl-imm-8
  (set ?rs (bit-and (lshiftr 8 (register ?ra) (any-int ?i)) #xff))
  (when (and (int-zero-p (bit-and i 7))
	     (int-zero-p (lshiftr 8 i 6)))
    1)
  ("~A = (~A >> ~A) & 0xff;" rs ra i)
  ("emit(COMPOSE_EXTBL_IMM(~A, ~A >> 3, ~A));" ra i rs))

(defmatcher extbl-imm-4
  (set ?rs (bit-and (lshiftr 4 (register ?ra) (any-int ?i)) #xff))
  (when (and (int-zero-p (bit-and i 7))
	     (int-zero-p (lshiftr 8 i 5)))
    1)
  ("~A = (~A >> ~A) & 0xff;" rs ra i)
  ("emit(COMPOSE_EXTBL_IMM(~A, ~A >> 3, ~A));" ra i rs))

(defmatcher extbl-imm-4-3
  (set ?rs (lshiftr 4 (register ?ra) 24))
  1
  ("~A = (~A >> 24) & 0xff;" rs ra)
  ("emit(COMPOSE_EXTBL_IMM(~A, 3, ~A));" ra rs))

(defmatcher extwl-imm-8
  (set ?rs (bit-and (lshiftr 8 (register ?ra) (any-int ?i)) #xffff))
  (when (and (int-zero-p (bit-and i 7))
	     (int-zero-p (lshiftr 8 i 6)))
    1)
  ("~A = (~A >> ~A) & 0xffff;" rs ra i)
  ("emit(COMPOSE_EXTWL_IMM(~A, ~A >> 3, ~A));" ra i rs))

(defmatcher extwl-imm-4
  (set ?rs (bit-and (lshiftr 4 (register ?ra) (any-int ?i)) #xffff))
  (when (and (int-zero-p (bit-and i 7))
	     (int-zero-p (lshiftr 8 i 5)))
    1)
  ("~A = (~A >> ~A) & 0xffff;" rs ra i)
  ("emit(COMPOSE_EXTWL_IMM(~A, ~A >> 3, ~A));" ra i rs))

(defmatcher extwl-imm-4-2
  (set ?rs (lshiftr 4 (register ?ra) 16))
  1
  ("~A = (~A >> 16) & 0xffff;" rs ra)
  ("emit(COMPOSE_EXTWL_IMM(~A, 2, ~A));" ra rs))

(defmatcher sextb
  (set ?rs (sex 1 (register ?rb)))
  1
  ("~A = sex_8(~A);" rs rb)
  ("emit(COMPOSE_SEXTB(~A, ~A));" rb rs))

(defmatcher sextw
  (set ?rs (sex 2 (register ?rb)))
  1
  ("~A = sex_16(~A);" rs rb)
  ("emit(COMPOSE_SEXTW(~A, ~A));" rb rs))

(defmatcher subl
  (set ?rs (sex 4 (-i (register ?ra) (register ?rb))))
  1
  ("~A = sex_32(~A - ~A);" ra ra rb)
  ("emit(COMPOSE_SUBL(~A, ~A, ~A));" ra rb rs))

(defmatcher subl-imm
    (set ?rs (sex 4 (+i (register ?ra) (any-int ?i))))
  (when (int-zero-p (lshiftr 8 (int-neg i) 8))
    1)
  ("~A = sex_32(~A + ~A);" rs ra i)
  ("emit(COMPOSE_SUBL_IMM(~A, -~A, ~A));" ra i rs))

(defopmatcher subq (-i a b) "~A - ~A")

(defopmatcher xor (bit-xor a b) "~A ^ ~A")

(defmatcher zapnot-imm-1
  (set ?rs (zex 1 (register ?ra)))
  1
  ("~A = zex_8(~A);" rs ra)
  ("emit(COMPOSE_ZAPNOT_IMM(~A, 1, ~A));" ra rs))

(defmatcher zapnot-imm-3
  (set ?rs (zex 2 (register ?ra)))
  1
  ("~A = zex_16(~A);" rs ra)
  ("emit(COMPOSE_ZAPNOT_IMM(~A, 3, ~A));" ra rs))

(defmatcher zapnot-imm-15
  (set ?rs (zex 4 (register ?ra)))
  1
  ("~A = zex_32(~A);" rs ra)
  ("emit(COMPOSE_ZAPNOT_IMM(~A, 15, ~A));" ra rs))

(defmatcher zapnot-imm-sll-srl-imm-bis
  (set ?rs (rotl 4 (register ?ra) (register ?rb)))
  4
  ("{ word_64 tmp = (~A & 0xffffffff) << ~A; ~A = tmp | (tmp >> 32); }" ra rb rs)
  ("{ reg_t tmp;
      emit(COMPOSE_ZAPNOT_IMM(~A, 15, ~A));
      emit(COMPOSE_SLL(~A, ~A, ~A));
      tmp = alloc_tmp_integer_reg();
      emit(COMPOSE_SRL_IMM(~A, 32, tmp));
      emit(COMPOSE_BIS(~A, tmp, ~A));
      free_tmp_integer_reg(tmp); }"
   ra rs				;zapnot_imm
   rs rb rs				;sll
   rs					;srl
   rs rs))				;bis

;;; extract

(defmatcher extract-and-imm
  (set ?rs (extract (register ?ra) 0 ?l))
  (when (<iu l 8)
    1)
  ("~A = bit_extract(~A, 0, ~A);" rs ra l)
  ("emit(COMPOSE_AND_IMM(~A, (1 << ~A) - 1, ~A));" ra l rs))

(defmatcher extract-zapnot-imm
  (set ?rs (extract (register ?ra) 0 ?l))
  (when (int-zero-p (bit-and l 7))
    1)
  ("~A = bit_extract(~A, 0, ~A);" rs ra l)
  ("emit(COMPOSE_ZAPNOT_IMM(~A, (1 << (~A >> 3)) - 1, ~A));" ra l rs))

(defmatcher extract-srl
  (set ?rs (extract (register ?ra) ?s ?l))
  (when (=i (+i s l) 64)
    1)
  ("~A = bit_extract(~A, ~A, ~A);" rs ra s l)
  ("emit(COMPOSE_SRL_IMM(~A, 64 - ~A, ~A));" ra s rs))

(defmatcher extract-extbl-imm
  (set ?rs (extract (register ?ra) ?s 8))
  (when (int-zero-p (bit-and s 7))
    1)
  ("~A = bit_extract(~A, ~A, 8);" rs ra s)
  ("emit(COMPOSE_EXTBL_IMM(~A, ~A >> 3, ~A));" ra s rs))

(defmatcher extract-extwl-imm
  (set ?rs (extract (register ?ra) ?s 16))
  (when (int-zero-p (bit-and s 7))
    1)
  ("~A = bit_extract(~A, ~A, 16);" rs ra s)
  ("emit(COMPOSE_EXTWL_IMM(~A, ~A >> 3, ~A));" ra s rs))

(defmatcher extract-sll-srl
  (set ?rs (extract (register ?ra) ?s ?l))
  2
  ("~A = bit_extract(~A, ~A, ~A);" rs ra s l)
  ("{ reg_t tmp = alloc_tmp_integer_reg();
      emit(COMPOSE_SLL_IMM(~A, 64 - ~A - ~A, tmp));
      emit(COMPOSE_SRL_IMM(tmp, 64 - ~A, ~A));
      free_tmp_integer_reg(tmp); }"
   ra s l				;sll
   l rs))				;srl

;;; insert

(defmatcher insert-full
  (set ?rs (insert (register ?ra) (register ?rb) ?s ?l))
  7
  ("~A = bit_insert(~A, ~A, ~A, ~A);" rs ra rb s l)
  ("{ reg_t mask_tmp = alloc_tmp_integer_reg(), source_tmp;
      emit(COMPOSE_NOT(31, mask_tmp));
      emit(COMPOSE_SLL_IMM(mask_tmp, 64 - ~A, mask_tmp));
      emit(COMPOSE_SRL_IMM(mask_tmp, 64 - (~A + ~A), mask_tmp));
      source_tmp = alloc_tmp_integer_reg();
      emit(COMPOSE_SLL_IMM(~A, ~A, source_tmp));
      emit(COMPOSE_BIC(source_tmp, mask_tmp, source_tmp));
      emit(COMPOSE_BIC(~A, mask_tmp, ~A));
      free_tmp_integer_reg(mask_tmp);
      emit(COMPOSE_BIS(source_tmp, ~A, ~A));
      free_tmp_integer_reg(source_tmp); }"
   l					;sll
   s l					;srl
   rb s					;sll
   ra rs				;bic
   rs rs))				;bis

;;; loads

(defldmatcher ldbu
  (load-byte ea)
  "mem_load_8(~A)"
  "LDBU")

(defldmatcher ldbu-zex
  (zex 1 (load-byte ea))
  "mem_load_8(~A)"
  "LDBU")

(defldmatcher ldwu
  (load little-endian 2 ea)
  "mem_load_16(~A)"
  "LDWU")

(defldmatcher ldwu-zex
  (zex 2 (load little-endian 2 ea))
  "mem_load_16(~A)"
  "LDWU")

(defldmatcher ldl
  (load little-endian 4 ea)
  "mem_load_32(~A)"
  "LDL")

(defldmatcher ldl-sex
  (sex 4 (load little-endian 4 ea))
  "sex_32(mem_load_32(~A))"
  "LDL")

;;; stores

(defstmatcher stb 1 "STB")
(defstmatcher stw 2 "STW")
(defstmatcher stl 4 "STL")
(defstmatcher stq 8 "STQ")

;;; comparisons

(defopmatcher cmpeq (condition-to-int (=i a b)) "(~A == ~A)")
(defopmatcher cmplt (condition-to-int (<is a b)) "(((sword_64)~A) < ((sword_64)~A))" :zero t)
(defopmatcher cmple (condition-to-int (<=is a b)) "(((sword_64)~A) <= ((sword_64)~A))" :zero t)
(defopmatcher cmpult (condition-to-int (<iu a b)) "~A < ~A" :zero t)
(defopmatcher cmpule (condition-to-int (<=iu a b)) "~A <= ~A" :zero t)
