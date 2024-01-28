(define *assertion* #f)

(define call/cc call-with-current-continuation)
(define (use-assertions) (call/cc (lambda (cc) (set! *assertion* cc))))

(define-syntax assert
  (syntax-rules ()
    ((assert cond?)
     (and *assertion*
          ((lambda ()
             (or cond?
                 ((lambda ()
                    (display "FAILED: ")
                    (write 'cond?)
                    (newline)
                    (*assertion*))))))))))
