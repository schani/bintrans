(setq *simplifies* '())

;; arithmetic

(defsimplify (+i ?x 0)
    x)

(defsimplify (-i ?x 0)
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
