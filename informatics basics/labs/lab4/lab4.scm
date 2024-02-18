
;; 1
(load "assert.scm")
(use-assertions)

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

;(map 1/x '(1 2 3 4 5))
;(map 1/x '(-2 -1 0 1 2))

;; 2
(define (load-data filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop
        ((line (read p)) (result '()))
        (if (eof-object? line)
            (reverse result)
            (loop (read p) (cons line result)))))))

(define (save-data data filename)
  (call-with-output-file filename
    (lambda (p)
      (write data p))))

(define (count-lines filename)
  (with-input-from-file filename
    (lambda ()
      (begin
        (let loop ((prev #f) (count 0))
          (let ((ch (read-char)))
            (cond
              ((eof-object? ch)
               (+ count (if prev 0 1)))
              (else
               (loop (equal? ch #\newline)
                     (+ count (if (and (equal? ch #\newline) (not prev)) 1 0)))))))))))

;(save-data '(1 2 3 4 5) "test.txt")
;(load-data "test.txt")
;(count-lines "test.txt")

;; 3
(load "memoization.scm")

(define (t n)
  (cond
    ((<= n 1) 0)
    ((= n 2) 1)
    (else (+ (t (- n 1)) (t (- n 2)) (t (- n 3))))))

;(t 50)

(define t
  (let ((table '()))
    (lambda (n)
      (if (assoc n table)
          (cadr (assoc n table))
          (begin

            (cond
              ((<= n 1) (set! table (cons `(,n 0) table)))
              ((= n 2) (set! table (cons `(,n 1) table)))
              (else
               (set! table (cons `(,n ,(+ (t (- n 1)) (t (- n 2)) (t (- n 3)))) table))))
            (cadr (assoc n table)))))))

;(t 100)

;(define-memoized (t n)
;  (cond
;    ((<= n 1) 0)
;    ((= n 2) 1)
;    (else (+ (t (- n 1)) (t (- n 2)) (t (- n 3))))))

;(t 10)

;; 4
(define-syntax my-if
  (syntax-rules ()
    ((_ true? opt1 opt2)
     (let ((f1 (delay opt1))
           (f2 (delay opt2)))
       (force (or (and true? f1) f2))))))

;(define counter
;  (let ((n 0))
;    (lambda ()
;      (set! n (+ 1 n))
;      n)))

;(my-if (= (counter) 2)
;       'aa
;       'bb)

;(my-if #t #f (/ 1 0))
;(my-if #f (/ 1 0) 1)

;; 5
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val) ...) body)
     ((lambda (var ...) body) val ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () body) body)
    ((my-let* ((var val)) body)
     (my-let ((var val)) body))
    ((my-let* ((var val) . xs) body)
     (my-let ((var val)) (my-let* xs body)))))

(define (f x y)
  (let ((x (+ y 12))
        (y (* x 3)))
    (+ x y)))

(define (f1 x y)
  (my-let ((x (+ y 12))
           (y (* x 3)))
          (+ x y)))

;(f1 12 2)

;; 6
(load "imperative.scm")

(let ((x 1))
  (when (> x 0) (display "x > 0") (newline))
  (unless (= x 0) (display "x != 0") (newline)))

(for i in '(1 2 3)
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))

(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))
