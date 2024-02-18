(define (my-even? a)
  (= (* (quotient a 2) 2) a))

(define (my-odd? a)
  (not (my-even? a)))

(define (my-expt x y)
  (cond
    ((< y 0) (/ 1 (my-expt x (- y))))
    ((= y 0) 1)
    (else (* (my-expt x (- y 1)) x))))
