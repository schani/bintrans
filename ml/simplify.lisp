(setq *simplifies* '())

;; arithmetic

(defsimplify (int-neg (int-neg ?x))
  x)

(defsimplify (+i ?x 0)
    x)

(defsimplify (*i ?x 1)
    x)

(defsimplify (*i ? 0)
    0)

;; shifts

(defsimplify (shiftl ?x 0)
    x)

(defsimplify (shiftl 0 ?)
    0)

(defsimplify (lshiftr ? ?x 0)
    x)

(defsimplify (lshiftr ? 0 ?)
    0)

(defsimplify (ashiftr ? ?x 0)
    x)

(defsimplify (ashiftr ? 0 ?)
    0)

;; sign extends

(defsimplify (sex 4 (sex 1 ?x))
    (sex 1 x))

(defsimplify (sex 4 (sex 2 ?x))
    (sex 2 x))

(defsimplify (sex 2 (sex 1 ?x))
    (sex 1 x))

(defsimplify (sex 4 (ashiftr 8 ?x 32))
  (ashiftr 8 x 32))

;; conditions

(defsimplify (int-to-condition (condition-to-int ?x))
  x)

(defsimplify (condition-to-int (int-to-condition ?x))
  (bit-and x 1))
