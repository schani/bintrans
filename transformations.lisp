(setf *transformations* nil)

(define-transformation
    (match x :type integer)
    (+ x 0))

(define-transformation
    (set (subregister (match reg) (match class) (match number) 0 (match w)) (shiftr (match v) (match a)))
    (set (register reg class number) (shiftr (logand (widen v) (mask 0 w)) (widen a))))

(define-transformation
    (set (target-subregister (match reg) (match class) 0 (match w)) (shiftr (match v) (match a)))
    (set (target-register reg class) (shiftr (logand (widen v) (mask 0 w)) (widen a))))

(define-transformation
    (set (subregister (match reg) (match class) (match number) 0 (match w)) (match rhs))
    (set (register reg class number) (zex rhs)))

(define-transformation
    (set (subregister (match reg) (match class) (match number) 0 (match w)) (match rhs))
    (set (register reg class number) (sex rhs)))

(define-transformation
    (set (target-subregister (match reg) (match class) 0 (match w)) (match rhs))
    (set (target-register reg class) (zex rhs)))

(define-transformation
    (set (target-subregister (match reg) (match class) 0 (match w)) (match rhs))
    (set (target-register reg class) (sex rhs)))

#|
(<- (widen (shiftl ?v ?a :width source-bits)
	   (strip source-bits (shiftl ?wv ?a :width target-bits)))
    (widen ?v ?wv))

(<- (machine-map (set (subreg 0 ?w ?reg ?class) (shiftl ?v ?a))
		 (set (reg ?treg ?tclass) (shiftl ?wv ?wa)))
    (register-map ?reg ?class ?treg ?tclass ?)
    (widen ?v ?wv)
    (machine-map ?a ?wa))

(<- (machine-map (set (subreg 0 ?w ?reg ?class) (shiftr ?v ?a))
		 (set (reg ?reg ?class) (shiftr (logand ?wv (mask 0 ?maskend)) ?wa)))
    (register-map ?reg ?class ?treg ?tclass ?)
    (machine-map ?v ?wv)
    (machine-map ?a ?wa)
    (is ?maskend (- ?w 1)))
|#
