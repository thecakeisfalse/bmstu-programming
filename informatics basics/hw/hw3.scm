
;; homework 3

(define (prod-deriv args)
  (define result '())
  (let loop ((times (- (length args) 1)) (vec (list->vector args)))
    (if (< times 0)
        (cons '+ result)
        (begin
          (vector-set! vec times (derivative (vector-ref vec times)))
          (set! result (cons (cons '* (vector->list vec)) result))
          (loop (- times 1) (list->vector args))))))

(define (div-deriv arg1 arg2)
  (list '/ (list '- (list '* arg2 (derivative arg1)) (list '* arg1 (derivative arg2))) (list 'expt arg2 2)))

(define (complex-deriv expr)
  (cond
    ((equal? (car expr) 'exp) (list '* expr (derivative (cadr expr))))
    ((equal? (car expr) 'expt)
     (cond
       ((equal? (derivative (cadr expr)) 0)
        (list '* expr (list 'log (cadr expr)) (derivative (caddr expr))))
       (else (derivative (list 'exp (list '* (list 'log (cadr expr)) (caddr expr)))))))
    ((equal? (car expr) 'log)
     (cond
       ((= (length (cdr expr)) 1) (list '* (list '/ '1 (cadr expr)) (derivative (cadr expr))))
       (else (derivative (list '/ (list 'log (cadr expr)) (list 'log (caddr expr)))))))
    ((equal? (car expr) 'sin) (list '* (list 'cos (cadr expr)) (derivative (cadr expr))))
    ((equal? (car expr) 'cos) (list '* (list '- (list 'sin (cadr expr))) (derivative (cadr expr))))))

(define (derivative expr)
  (simplify
   (cond
     ((null? expr) '())
     ((number? expr) '0)
     ((not (list? expr)) '1)
     ((equal? (car expr) '+) (cons '+ (map derivative (cdr expr))))
     ((equal? (car expr) '-) (cons '- (map derivative (cdr expr))))
     ((equal? (car expr) '*) (prod-deriv (cdr expr)))
     ((equal? (car expr) '/) (div-deriv (cadr expr) (cons '* (cddr expr))))
     (else (complex-deriv expr)))))

(define (delete pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (cons (car xs) (delete pred? (cdr xs))))))

(define (any? pred? xs)
  (if (null? xs)
      #f
      (or (pred? (car xs)) (any? pred? (cdr xs)))))

(define (czero? x) (equal? x '0))
(define (cone? x) (equal? x '1))

(define (func arg prev)
  (if (null? arg)
      (list prev '())
      (if (and (list? (car arg)) (equal? (caar arg) 'log))
          (list (append prev (cdr arg)) (car arg))
          (func (cdr arg) (cons (car arg) prev)))))

(define (simplify expr)
  (cond
    ((null? expr) '())
    ((not (list? expr)) expr)
    ((equal? (car expr) '+)
     (let ((subexpr (delete czero? (map simplify (cdr expr)))))
       (cond
         ((null? subexpr) 0)
         ((> (length subexpr) 1) (cons (car expr) subexpr))
         (else (car subexpr)))))
    ((equal? (car expr) '*)
     (let ((subexpr (delete cone? (map simplify (delete cone? (cdr expr))))))
       (cond
         ((null? subexpr) 1)
         ((any? czero? subexpr) 0)
         ((> (length subexpr) 1) (cons (car expr) subexpr))
         (else (car subexpr)))))
    ((equal? (car expr) '-)
     (if (= (length (cdr expr)) 1)
         (begin
           (let ((subexpr (map simplify (cdr expr))))
             (if (null? (delete czero? subexpr)) '0 (cons '- subexpr))))
         (simplify (list '+ (simplify (cadr expr)) (cons '- (map simplify (cddr expr)))))))
    ((equal? (car expr) 'exp)
     (begin
       (let* ((subexpr (car (map simplify (cdr expr))))
              (subsubexpr (if (list? subexpr) (func (cdr subexpr) '()) (list subexpr '()))))
         (if (not (null? (cadr subsubexpr)))
             (cond
               ((equal? (car subexpr) '*)
                (simplify (list 'expt (cadadr subsubexpr) (cons '* (car subsubexpr)))))
               ((equal? (car subexpr) '+)
                (simplify (list '* (cadadr subsubexpr) (list 'expt (list '* (caar subsubexpr)))))))
             (list 'exp subexpr)))))
    (else (cons (car expr) (map simplify (cdr expr))))))