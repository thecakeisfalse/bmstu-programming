(define-syntax memoized-lambda
  (syntax-rules ()
    ((_ (args) . body)
     (let ((table '()))
       (lambda (args)
         (if (assoc args table)
             (cadr (assoc args table))
             (let ((res (begin . body)))
               (set! table (cons (list args res) table))
               res)))))))

(define-syntax define-memoized
  (syntax-rules ()
    ((_ (f . args) . body)
     (define f
       (memoized-lambda args . body)))))
