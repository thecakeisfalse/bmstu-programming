
(define (factorize expr)
  (define +? (equal? (car expr) '+))
  (define expt (car (reverse (cadr expr))))
  (define x (cadr (cadr expr)))
  (define y (cadr (caddr expr)))
  (cond
    ((and (not +?) (= expt 2)) `(* (- ,x ,y) (+ ,x ,y)))
    ((and (not +?) (= expt 3)) `(* (- ,x ,y) (+ (expt ,x 2) (* ,x ,y) (expt ,y 2))))
    ((and  +? (= expt 3)) `(* (+ ,x ,y) (+ (expt ,x 2) (- (* ,x ,y)) (expt ,y 2))))
    (else #f)))

