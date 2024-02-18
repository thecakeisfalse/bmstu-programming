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
        #t)
      (begin
        (display " FAIL")
        (newline)
        (display "  Expected: ")
        (write (cadr test))
        (newline)
        (display "  Returned: ")
        (write v)
        (newline)
        #f)))

(define (run-tests tests)
  (define (run-tests-rec tests fl)
    (if (null? tests)
        fl
        (run-tests-rec (cdr tests)
                       (and (run-test (car tests)) fl))))
  (run-tests-rec tests #t))
