(setq *ir-macros* '())

(defirmacro mask (begin-bit end-bit)
  (if (<iu 8 end-bit begin-bit)
      (bit-or (shiftl -1 begin-bit)
	      (lshiftr 8 -1 (-i 63 end-bit)))
    (shiftl (lshiftr 8 -1 (-i 63 (-i end-bit begin-bit))) begin-bit)))

(defirmacro rotl (width x a)
  (bit-or (shiftl x a) (lshiftr width x (-i (*i width 8) a))))
