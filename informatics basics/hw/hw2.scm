
;; вспомогательные функкции

(define (any? pred? xs)
  (or (and (not (null? xs)) (pred? (car xs)))
      (and (> (length xs) 1) (any? pred? (cdr xs)))))

(define (all? pred? xs)
  (not (any? (lambda (x) (not (pred? x))) xs)))

(define (count x xs)
  (if (null? xs)
      0
      (if (eq? (car xs) x)
          (+ (count x (cdr xs)) 1)
          (count x (cdr xs)))))

;; hw2-lists

(define (my-range a b d)
  (if (< a b)
      (cons a (my-range (+ a d) b d))
      '()))

(define (my-flatten xs)
  (define (my-flatten-rec xs a)
    (cond
      ((null? xs) a)
      ((list? xs)
       (my-flatten-rec (car xs)
                       (my-flatten-rec (cdr xs) a)))
      (else (cons xs a))))
  (my-flatten-rec xs '()))

(define (my-filter pred? xs)
  (cond
    ((null? xs) '())
    ((pred? (car xs))
     (cons (car xs) (my-filter pred? (cdr xs))))
    (else (my-filter pred? (cdr xs)))))

(define (my-fold-left op xs)
  (define (fold-rec op a xs)
    (if (null? xs)
        a
        (fold-rec op (op a (car xs)) (cdr xs))))
  (fold-rec op (car xs) (cdr xs)))

(define (my-fold-right op xs)
  (cond
    ((null? xs) '())
    ((null? (cdr xs)) (car xs))
    (else (op (car xs) (my-fold-right op (cdr xs))))))

;; hw2-lists

(define (list->set xs)
  (cond
    ((null? xs) '())
    ((= (count (car xs) xs) 1)
     (cons (car xs) (list->set (cdr xs))))
    (else (list->set (cdr xs)))))

(define (set? xs)
  (all? (lambda (x) (= (count x xs) 1)) xs))

(define (union xs ys)
  (list->set (append xs ys)))

(define (intersection xs ys)
  (cond
    ((null? xs) '())
    ((my-element? (car xs) ys)
     (cons (car xs) (intersection (cdr xs) ys)))
    (else (intersection (cdr xs) ys))))

(define (difference xs ys)
  (cond
    ((null? xs) '())
    ((my-element? (car xs) ys) (difference (cdr xs) ys))
    (else (cons (car xs) (difference (cdr xs) ys)))))

(define (symmetric-difference xs ys)
  (union (difference xs ys) (difference ys xs)))

(define (set-eq? xs ys)
  (and (eqv? '() (difference xs ys))
       (eqv? '() (difference ys xs))))

;; hw2-strings

(define (string-trim-left s)
  (define (string-trim-left-rec l)
    (cond
      ((null? l) '())
      ((char-whitespace? (car l)) (string-trim-left-rec (cdr l)))
      (else l)))
  ((lambda (s) (list->string (string-trim-left-rec (string->list s)))) s))

(define (string-trim-right s)
  (define (last-not-space l i k)
    (cond
      ((null? l) (+ i 1))
      ((char-whitespace? (car l)) (last-not-space (cdr l) i (+ k 1)))
      (else (last-not-space (cdr l) k (+ k 1)))))
  (define (string-trim-right-rec l i)
    (if (or (null? l) (<= i 0))
        '()
        (cons (car l) (string-trim-right-rec (cdr l) (- i 1)))))
  (list->string
   ((lambda (l) (string-trim-right-rec l (last-not-space l 1e10 0))) (string->list s))))

(define (string-trim s)
  (string-trim-right (string-trim-left s)))

(define (string-prefix? a b)
  (and (not (> (string-length a) (string-length b)))
       (equal? (substring b 0 (string-length a)) a)))

(define (string-suffix? a b)
  (define l1 (string-length a))
  (define l2 (string-length b))
  (and (<= l1 l2)
       (equal? (substring b (- l2 l1) l2) a)))

(define (string-infix? a b)
  (define (string-infix-rec a b)
    (or (string-prefix? a (list->string b))
        (and (not (null? b)) (string-infix-rec a (cdr b)))))
  (string-infix-rec a (string->list b)))

(define (string-split str sep)
  (define (string->result str result index)
    (cond
      ((= index (string-length str)) '())
      ((null? result)
       (cons (substring str index (string-length str))
             (string->result str '() (string-length str))))
      (else
       (cons (substring str index (car result))
             (string->result str (cdr result)
                             (+ (car result) (string-length sep)))))))

  (let loop ((s (string-append str sep))
             (m (string-length sep))
             (n (string-length str))
             (index 0)
             (result '()))
    (cond
      ((>= index n)
       (string->result str result 0))
      ((equal? (substring s index (+ index m)) sep)
       (loop s m n (+ index m) (append result `(,index))))
      (else (loop s m n (+ index 1) result)))))

;; hw2-multivectors

(define (multi-vector? m)
  (and (list? m) (equal? (car m) '_mv)))

(define (make-multi-vector sizes . fill)
  (list '_mv sizes
        (make-vector (apply * sizes)
                     (if (null? fill) 0 (car fill)))))

(define (index->position indices sizes)
  (if (null? indices)
      0
      (+ (* (car indices) (apply * (cdr sizes)))
         (index->position (cdr indices) (cdr sizes)))))

(define (multi-vector-set! m indices x)
  (vector-set! (caddr m) (index->position indices (cadr m)) x))

(define (multi-vector-ref m indices)
  (vector-ref (caddr m) (index->position indices (cadr m))))

;; hw2-composition

(define (o . fs)
  (define (o-rec fs x)
    (if (null? fs)
      x
      ((car fs) (o-rec (cdr fs) x))))
  (lambda (x) (o-rec fs x)))

;; hw2-ltr-achieve

(define (list-trim-right l)
  (define (last-not-space l i k)
    (cond
      ((null? l) (+ i 1))
      ((char-whitespace? (car l)) (last-not-space (cdr l) i (+ k 1)))
      (else (last-not-space (cdr l) k (+ k 1)))))
  (define (list-trim-right-rec l i)
    (if (or (null? l) (<= i 0))
      '()
      (cons (car l) (list-trim-right-rec (cdr l) (- i 1)))))
  (list-trim-right-rec l (last-not-space l 1e10 0)))

