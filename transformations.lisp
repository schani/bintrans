(setf *transformations* nil)

(define-transformation
    (neg (match x))
    (- 0 x))

(define-transformation
    (set (subregister (match reg) (match class) (match number) (match begin) (match end) (match named)) (match rhs))
    (set (register reg class number) (logor (logand (register reg class number) (bitneg (mask begin end))) (shiftl (zex rhs) begin))))

(define-transformation
    (set (target-subregister (match reg) (match class) 0 (match w)) (shiftr (match v) (match a)))
    (set (target-register reg class) (shiftr (logand (widen v) (mask 0 w)) (widen a))))

(define-transformation
    (set (target-subregister (match reg) (match class) 0 (match w)) (match rhs))
    (set (target-register reg class) (zexpand rhs)))

(define-transformation
    (set (target-subregister (match reg) (match class) 0 (match w)) (match rhs))
    (set (target-register reg class) (sexpand rhs)))

(define-transformation
    (set (target-subregister (match reg) (match class) 0 (match w)) (match rhs))
    (set (target-register reg class) (sex rhs)))

(define-transformation
    (set (target-register (match reg) (match class)) (zex (subregister (match sreg) (match sclass) (match snumber) 0 (match end) (match named))))
    (set (target-register reg class) (logand (widen (register sreg sclass snumber)) (mask 0 end))))

(define-transformation
    (set (numbered-subregister (match reg) (match class) (match number) (match w) (match index)) (match rhs))
    (set (register reg class number) (logor (logand (register reg class number) (bitneg (mask (* (zex index) (zex w))
											      (+ (* (zex index) (zex w)) (- (zex w) 1)))))
					    (shiftl (zex rhs) (* (zex index) (zex w))))))

(define-transformation
    (zex (match v :width w))
    (logand (zexpand v) (mask 0 (- (zex w) 1))))

(define-transformation
    (zex (match v :width w))
    (logand (sexpand v) (mask 0 (- (zex w) 1))))

(define-transformation
    (zex (numbered-subregister (match reg) (match class) (match number) (match w) (match index)))
    (logand (shiftr (widen (register reg class number)) (* (zex w) (zex index)))
	    (mask 0 (- (zex w) 1))))

(define-transformation
    (zex (logor (match op1) (match op2)))
    (logor (zex op1) (zex op2)))

(define-transformation
    (zex (logxor (match op1) (match op2)))
    (logxor (zex op1) (zex op2)))

(define-transformation
    (zex (shiftr (match v) (match a)))
    (shiftr (zex v) a))

(define-transformation
    (zex (+carry (match op1 :width w) (match op2)))
    (shiftr (+ (zex op1) (zex op2)) w))

(define-transformation
    (rotl (match v :width w) (match a))
    (logor (shiftl v a) (shiftr v (- w a))))

#|
(eval (make-integer-expr required-width w))

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
