
;; A
(define-syntax when
  (syntax-rules ()
    ((_ cond? . actions) (if cond? (begin . actions)))))

(define-syntax unless
  (syntax-rules ()
    ((_ cond? . actions) (if (not cond?) (begin . actions)))))

;; B

(define-syntax for
  (syntax-rules (as in)
    ((for x in xs . actions)
     (let loop ((xs-copy xs))
       (if (not (null? xs-copy))
           (let ((x (car xs-copy)))
             (begin . actions)
             (loop (cdr xs-copy))))))
    ((for xs as x . actions)
     (for x in xs . actions))))

;; V

(define-syntax while
  (syntax-rules ()
    ((_ cond? . actions)
     (let loop ()
       (if cond?
           (begin
             (begin . actions)
             (loop)))))))

;; G

(define-syntax repeat
  (syntax-rules (until)
    ((repeat actions until cond?)
     (let loop ()
       (begin
         (begin . actions)
         (if (not cond?)
             (loop)))))))

;; D

(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl)
     (newline))
    ((cout << endl . exprs)
     (begin
       (newline)
       (cout . exprs)))
    ((cout << expr)
     (display expr))
    ((cout << expr . exprs)
     (begin
       (display expr)
       (cout . exprs)))))
