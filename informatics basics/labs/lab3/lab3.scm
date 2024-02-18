(load "trace.scm")

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace (car xss)))) ; Здесь...
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace xss)))))) ; ... и здесь

;(zip '(1 2 3) '(one two three))

; Пример процедуры с ошибкой
;
(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0) 1) ; Ошибка здесь!
    (else 1)))

(load "unit-test.scm")

(define the-tests
  (list (test (signum -2) -1)
        (test (signum 0) 0)
        (test (signum 2) 1)))

(run-tests the-tests)

(define counter
  (let ((n 0))
    (lambda ()
      (set! n (+ 1 n))
      n)))

(trace (counter))

(define counter-tests
  (list (test (counter) 2)
        (test (counter) 7)
        (test (counter) 4)))

(run-tests counter-tests)

(load "ref.scm")

(define ref-tests
  (list (test (ref '(1 2 3) 1) 2)
        (test (ref #(1 2 3) 1) 2)
        (test (ref "123" 1) #\2)
        (test (ref "123" 3) #f)
        (test (ref '(1 2 3) 1 0) '(1 0 2 3))
        (test (ref #(1 2 3) 1 0) #(1 0 2 3))
        (test (ref "123" 1 #\0) "1023")
        (test (ref "123" 1 0) #f)
        (test (ref "123" 3 #\4) "1234")
        (test (ref "123" 5 #\4) #f)
        ))

(run-tests ref-tests)

(load "factorize.scm")

(define factorize-tests
  `(,(test ((lambda ()
              (eval
               `((lambda
                     (x y)
                   ,(factorize '(- (expt x 2) (expt y 2)))) 1 2)
               (interaction-environment)))) -3)
    ,(test ((lambda ()
              (eval
               `((lambda
                     (x y)
                   ,(factorize '(- (expt x 3) (expt (- y 2) 3)))) 1 2)
               (interaction-environment)))) 1)
    ,(test ((lambda ()
              (eval
               `((lambda
                     (x y)
                   ,(factorize '(+ (expt (+ x 12) 3) (expt (- y 2) 3)))) 5 2)
               (interaction-environment)))) 4913)))

(run-tests factorize-tests)
