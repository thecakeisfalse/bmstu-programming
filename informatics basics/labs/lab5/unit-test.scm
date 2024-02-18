(define-syntax test
  (syntax-rules ()
    ((test expr correct) (list 'expr correct))))

(define (run-test test)
  (write (car test))
  (define v (eval (car test) (interaction-environment)))
  (if (equal? v (cadr test))
      (begin
        (display " ok")
        (newline)
        0)
      (begin
        (display " FAIL")
        (newline)
        (display "  Expected: ")
        (write (cadr test))
        (newline)
        (display "  Returned: ")
        (write v)
        (newline)
        1)))

(define (run-tests tests)
  (define (run-tests-rec tests)
    (if (null? tests)
        0
        (+ (run-test (car tests)) (run-tests-rec (cdr tests)))))
  (= (run-tests-rec tests) 0))
