(setq *matchers* '())

(defmatcher lda
  (set ?rs (any-int ?i))
  (when (zero-or-full-p 8 (ashiftr 8 i 15))
    1)
  ("~A = ~A;" rs i))

(defmatcher ldah
  (set ?rs (any-int ?i))
  (when (and (int-zero-p 8 (bit-and i #xffff))
	     (zero-or-full-p 8 (ashiftr 8 i 31)))
    1)
  ("~A = ~A;" rs i))

(defmatcher load-int
  (set ?rs (any-int ?i))
  2
  ("~A = ~A;" rs i))

(defmatcher mov
  (set ?rs (register ?ra))
  1
  ("~A = ~A;" rs ra))

(defmatcher and
  (set ?rs (bit-and (register ?ra) (register ?rb)))
  1
  ("~A = ~A & ~A;" rs ra rb))

(defmatcher and-imm
  (set ?rs (bit-and (register ?ra) (any-int ?i)))
  (when (int-zero-p 8 (lshiftr 8 i 8))
    1)
  ("~A = ~A & ~A;" rs ra i))

(defmatcher zapnot-imm
  (set ?rs (bit-and (register ?ra) (any-int ?i)))
  (when (user-op "IsMaskMask" i 8)
    1)
  ("~A = ~A & ~A;" rs ra i))

(defmatcher bis
  (set ?rs (bit-or (register ?ra) (register ?rb)))
  1
  ("~A = ~A | ~A;" rs ra rb))

(defmatcher bic
  (set ?rs (bit-and (register ?ra) (bit-neg (register ?rb))))
  1
  ("~A = ~A & ~~~A;" rs ra rb))

(defmatcher neg
  (set ?rs (bit-neg (register ?rb)))
  1
  ("~A = ~~~A;" rs rb))

(defmatcher sll
  (set ?rs (shiftl (register ?ra) (register ?rb)))
  1
  ("~A = ~A << ~A;" rs ra rb))

(defmatcher sll-imm
  (set ?rs (shiftl (register ?ra) (any-int ?a)))
  1
  ("~A = ~A << ~A;" rs ra a))

(defmatcher zapnot-imm-srl
  (set ?rs (lshiftr (?width (1 2 4)) (register ?ra) (register ?rb)))
  2
  ("~A = (~A & width_mask(~A)) >> ~A;" rs ra width rb))

(defmatcher zapnot-imm-srl-imm
  (set ?rs (lshiftr (?width (1 2 4)) (register ?ra) (any-int ?a)))
  2
  ("~A = (~A & width_mask(~A)) >> ~A;" rs ra width a))

(defmatcher srl
  (set ?rs (lshiftr 8 (register ?ra) (register ?rb)))
  1
  ("~A = ~A >> ~A;" rs ra rb))

(defmatcher srl-imm
  (set ?rs (lshiftr 8 (register ?ra) (any-int ?a)))
  1
  ("~A = ~A >> ~A;" rs ra a))

(defmatcher extbl-imm-8
  (set ?rs (bit-and (lshiftr 8 (register ?ra) (any-int ?a)) #xff))
  (when (and (int-zero-p 8 (bit-and a 7))
	     (int-zero-p 8 (lshiftr 8 a 6)))
    1)
  ("~A = (~A >> ~A) & 0xff;" rs ra a))

(defmatcher extbl-imm-4
  (set ?rs (bit-and (lshiftr 4 (register ?ra) (any-int ?a)) #xff))
  (when (and (int-zero-p 8 (bit-and a 7))
	     (int-zero-p 8 (lshiftr 8 a 5)))
    1)
  ("~A = (~A >> ~A) & 0xff;" rs ra a))

(defmatcher extwl-imm-8
  (set ?rs (bit-and (lshiftr 8 (register ?ra) (any-int ?a)) #xffff))
  (when (and (int-zero-p 8 (bit-and a 7))
	     (int-zero-p 8 (lshiftr 8 a 6)))
    1)
  ("~A = (~A >> ~A) & 0xffff;" rs ra a))

(defmatcher extwl-imm-4
  (set ?rs (bit-and (lshiftr 4 (register ?ra) (any-int ?a)) #xffff))
  (when (and (int-zero-p 8 (bit-and a 7))
	     (int-zero-p 8 (lshiftr 8 a 5)))
    1)
  ("~A = (~A >> ~A) & 0xffff;" rs ra a))

(defmatcher subq
  (set ?rs (-i (register ?ra) (register ?rb)))
  1
  ("~A = ~A - ~A;" rs ra rb))

(defmatcher zapnot-imm-sll-srl-bis
  (set ?rs (rotl 4 (register ?ra) (register ?rb)))
  4
  ("{ word_64 tmp = (~A & 0xffffffff) << ~A; ~A = tmp | (tmp >> 32); }" ra rb rs))
