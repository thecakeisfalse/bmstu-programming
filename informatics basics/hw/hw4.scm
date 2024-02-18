
;; hw4-memo-lazy

(define memoized-factorial
  (let ((table '()))
    (lambda (n)
      (if (assoc n table)
          (cadr (assoc n table))
          (begin
            (cond
              ((<= n 0)
               (set! table (cons `(,n 1) table)))
              (else
               (set! table (cons `(,n ,(* n (memoized-factorial (- n 1)))) table))))
            (cadr (assoc n table)))))))

(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (lazy-car lazy-pair)
  (if (pair? lazy-pair)
      (car lazy-pair)
      lazy-pair))

(define (lazy-cdr lazy-pair)
  (if (pair? lazy-pair)
      (force (cdr lazy-pair))
      '()))

(define (lazy-head lazy-pair k)
  (if (= k 0)
      '()
      (cons (lazy-car lazy-pair)
            (lazy-head (lazy-cdr lazy-pair) (- k 1)))))

(define (lazy-ref lazy-pair k)
  (cond
    ((= k 0) (lazy-car lazy-pair))
    (else (lazy-ref (lazy-cdr lazy-pair) (- k 1)))))

(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))

(define (+factorials n m)
  (lazy-cons n (+factorials (* n m) (+ m 1))))

(define lazy-factorial
  (let ((+fact (+factorials 1 1)))
    (lambda (n)
      (lazy-ref +fact n))))

;; hw4-read-words

(define (read-words)
  (let loop ((result '()) (word '()))
    (cond
      ((eof-object? (peek-char))
       (reverse (map list->string result)))
      ((char-whitespace? (peek-char))
       (read-char)
       (if (null? word)
           (loop result word)
           (loop (cons (reverse word) result) '())))
      (else (loop result (cons (read-char) word))))))

;; hw4-struct

(define-syntax define-struct
  (syntax-rules ()
    ((_ struct-name struct-fields)
     (let ((ie (interaction-environment))
           (name (symbol->string 'struct-name))
           (fields (map symbol->string 'struct-fields)))
       (eval `(define (,(string->symbol (string-append "make-" name)) . values)
                (list ',(string->symbol (string-append "_struct-" name))
                      ','struct-fields
                      (list->vector values))) ie)
       (eval `(define (,(string->symbol (string-append name "?")) obj)
                (and (list? obj)
                     (not (null? obj))
                     (equal? (car obj)
                             ',(string->symbol (string-append "_struct-" name)))
                     (list? (cadr obj)))) ie)
       (let loop ((temp fields) (i 0))
         (eval `(define (,(string->symbol (string-append "set-" name "-" (car temp) "!")) obj value)
                     (vector-set! (caddr obj) ,i value)) ie)
         (eval `(define (,(string->symbol (string-append name "-" (car temp))) obj)
                     (vector-ref (caddr obj) ,i)) ie)
         (or (null? (cdr temp)) (loop (cdr temp) (+ i 1))))
       (values)))))

;; hw4-data

(define-syntax define-data
  (syntax-rules ()
    ((_ name (def))
     (let ((ie (interaction-environment)))
       (eval `(define (,(car 'def) . params)
                (list '_algebra ','name ',(car 'def) ,(- (length 'def) 1) params)) ie)))
    ((_ name (def . defs))
     (let ((ie (interaction-environment)))
       (define-data name defs)
       (define-data name (def))
       (eval `(define (,(string->symbol
                         (string-append (symbol->string 'name) "?")) obj)
                (and (list? obj) (not (null? obj))
                     (equal? (car obj) '_algebra) (equal? (cadr obj) ','name)
                     (let ((r (reverse obj)))
                       (and (list? (car r)) (number? (cadr r))
                            (= (length (car r)) (cadr r)))))) ie)))))

(define-syntax match
  (syntax-rules ()
    ((_ obj ((name . params) pattern))
     (if (equal? 'name (caddr obj))
         (apply (lambda params pattern) (car (reverse obj)))))
    ((_ obj ((name . params) pattern) . other)
     (begin
       (if (equal? 'name (caddr obj))
           (match obj ((name . params) pattern))
           (match obj . other))))))

