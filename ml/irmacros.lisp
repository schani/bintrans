(setq *ir-macros* '())

;(defirmacro mask (begin-bit end-bit)
;  (if (<iu 8 end-bit begin-bit)
;      (bit-or (shiftl -1 begin-bit)
;	      (lshiftr 8 -1 (-i 63 end-bit)))
;    (shiftl (lshiftr 8 -1 (-i 63 (-i end-bit begin-bit))) begin-bit)))

(defirmacro -i (a b)
  (+i a (int-neg b)))

(defirmacro simple-mask (begin-bit end-bit)
  (bit-mask begin-bit (+i (-i end-bit begin-bit) 1)))

(defirmacro mask (begin-bit end-bit)
  (if (<iu 8 end-bit begin-bit)
      (bit-neg (simple-mask (+i end-bit 1) (-i begin-bit 1)))
    (simple-mask begin-bit end-bit)))

(defirmacro rotl (width x a)
  (bit-or (shiftl x a) (lshiftr width x (-i (const int (*i width 8)) a))))

(defirmacro full-mask-p (width x)
  (=i width x -1))

(defirmacro zero-or-full-p (width x)
  (or (int-zero-p width x)
      (full-mask-p width x)))
