;; ??
;; ==
;; signed-number ::= sign signed-number | number
;; frac ::= signed-number separator number
;; fracs ::= frac fracs | ε
;;
;; Лексика
;; =======
;; number ::= DIGIT number-tail
;; number-tail ::= number | ε
;; sign ::= + | -
;; separator ::= /
;; spaces ::= SPACE spaces | ε
;; token ::= number | sign | separator
;; tokens ::= token tokens | spaces tokens | ε
;;

(load "stream.scm")

;; =======================
;; Вспомогательные функции
;; =======================

(define (all? pred? xs)
  (or (null? xs)
      (and (pred? (car xs))
           (all? pred? (cdr xs)))))

(define char-digit? char-numeric?)

(define (char-sign? ch)
  (and (memq ch '(#\+ #\-)) #t))

(define (char-sep? ch)
  (equal? ch #\/))

(define (char->digit ch)
  (- (char->integer ch)
     (char->integer #\0)))

(define (list->integer xs)
  (define (list->integer-rec xs)
    (if (null? xs)
        0
        (+ (* 10 (list->integer-rec (cdr xs)))
           (char->digit (car xs)))))
  (list->integer-rec (reverse xs)))

;; ======================
;; Лексический анализатор
;; ======================

(define (tokenize str)
  (let* ((EOF (integer->char 0))
         (stream (make-stream (string->list str) EOF)))

    (call-with-current-continuation
     (lambda (error)
       (define result (tokens stream error))
       (and (equal? (peek stream) EOF) result)))))

;; tokens ::= token tokens | spaces tokens | ε
(define (tokens stream error)
  (define (start-token? char)
    (or (char-sign? char)
        (char-sep? char)
        (char-digit? char)))
  (cond
    ((char-whitespace? (peek stream))
     (spaces stream error)
     (tokens stream error))
    ((start-token? (peek stream))
     (cons (token stream error)
           (tokens stream error)))
    (else '())))

;; spaces ::= SPACE spaces | ε
(define (spaces stream error)
  (cond
    ((char-whitespace? (peek stream))
     (next stream))
    (else #t)))

;; token ::= number | sign | separator
(define (token stream error)
  (cond
    ((char-sign? (peek stream))
     (next stream))
    ((char-sep? (peek stream))
     (next stream))
    ((char-digit? (peek stream))
     (number stream error))
    (else (error #f))))

;; number ::= DIGIT number-tail
(define (number stream error)
  (cond
    ((char-digit? (peek stream))
     (list->integer
      (cons (next stream)
            (number-tail stream error))))
    (else (error #f))))

;; number-tail ::= number | ε
(define (number-tail stream error)
  (cond
    ((char-digit? (peek stream))
     (cons (next stream)
           (number-tail stream error)))
    (else '())))

;; =========================
;; Синтаксический анализатор
;; =========================

(define (parse tokens axiom)
  (define stream
    (make-stream tokens))

  (call-with-current-continuation
   (lambda (error)
     (let ((result (axiom stream error)))
       (and (equal? (peek stream) #f) result)))))

;; fracs ::= frac fracs | ε
(define (fracs stream error)
  (cond
    ((peek stream)
     (cons
      (frac stream error)
      (fracs stream error)))
    (else '())))

;; frac ::= signed-number separator number
(define (frac stream error)
  (cond
    ((or (char-sign? (peek stream))
         (number? (peek stream)))
     (list (signed-number stream error)
           (if (char-sep? (peek stream))
               (next stream)
               (error #f))
           (non-zero-number stream error)))
    (else (error #f))))

;; signed-number ::= sign signed-number | number
(define (signed-number stream error)
  (cond
    ((char-sign? (peek stream))
     (cons (next stream)
           (signed-number stream error)))
    ((number? (peek stream))
     (list (next stream)))
    (else (error #f))))

(define (non-zero-number stream error)
  (cond
    ((and
      (number? (peek stream))
      (> (peek stream) 0))
     (list (next stream)))
    (else (error #f))))

;; ===================
;; Упрощение выражения
;; ===================

(define (simplify-frac-tree tree)
  (define (list->number list)
    (cond
      ((null? (cdr list)) (car list))
      ((equal? (car list) #\-)
       (- (list->number (cdr list))))
      ((equal? (car list) #\+)
       (list->number (cdr list)))))
  (and tree (/ (list->number (car tree)) (list->number (caddr tree)))))

(define (simplify-fracs tree)
  (and tree (append (map simplify-frac-tree tree))))

;; ================
;; Основные функции
;; ================

;; S ::= frac
(define (check-frac expr)
  (define tkns (tokenize expr))
  (and tkns (parse tkns frac) #t))

;; S ::= frac
(define (scan-frac expr)
  (define tkns (tokenize expr))
  (and tkns (simplify-frac-tree (parse tkns frac))))

;; S ::= fracs
(define (scan-many-fracs expr)
  (define tkns (tokenize expr))
  (and tkns (simplify-fracs (parse tkns fracs))))

;; ======================================ТЕСТИРОВАНИЕ========================================

(load "unit-test.scm")

(define check-frac-tests
  (list 
   (test (check-frac "110/111") #t)
   (test (check-frac "-4/3") #t)
   (test (check-frac "+5/10") #t)
   (test (check-frac "5.0/10") #f)
   (test (check-frac "FF/10") #f)
   (test (check-frac "/") #f)
   (test (check-frac "1/") #f)
   (test (check-frac "/1") #f)
   (test (check-frac "") #f)
   (test (check-frac "+/1") #f)
   (test (check-frac "+1 1/1") #f)
   (test (check-frac "2/") #f)
   (test (check-frac "/2") #f)
   (test (check-frac "+/2") #f)
   (test (check-frac "+1/") #f)
   (test (check-frac "-2/1") #t)
   (test (check-frac "-2/1/12") #f)
   (test (check-frac "-2//12") #f)
   (test (check-frac "5/0") #f)
   (test (check-frac "/") #f)
   (test (check-frac "-334//234/4/234/342///4//2342") #f)
   (test (check-frac "-10/0") #f)))

(define scan-frac-tests
  (list
   (test (scan-frac "110/111") 110/111)
   (test (scan-frac "-4/3") -4/3)
   (test (scan-frac "+-----+---+-5/10") -5/10)
   (test (scan-frac "5.0/10") #f)
   (test (scan-frac "FF/10") #f)
   (test (scan-frac "-2/1") -2/1)
   (test (scan-frac "-20/123") -20/123)))

(define scan-many-fracs-tests
  (list
   (test (scan-many-fracs "\t1/2 1/3\n\n10/8") '(1/2 1/3 5/4))
   (test (scan-many-fracs "") '())
   (test (scan-many-fracs " 1/2 ") '(1/2))
   (test (scan-many-fracs "1/2-1/2") '(1/2 -1/2))
   (test (scan-many-fracs "1/2 -1/2") '(1/2 -1/2))
   (test (scan-many-fracs "") '())
   (test (scan-many-fracs " ") '())
   (test (scan-many-fracs "\t\t\t\t\t\t\t    1/2     ") '(1/2))
   (test (scan-many-fracs " /2 ") #f)
   (test (scan-many-fracs " 2/ ") #f)
   (test (scan-many-fracs "1/2 -2/1") '(1/2 -2/1))
   (test (scan-many-fracs "   1/2  ") '(1/2))
   (test (scan-many-fracs "1234/234 2345/432 \t\t\n343/324") '(1234/234 2345/432 343/324))
   (test (scan-many-fracs "1/2 ** 2/3") #f)))

(define tests
  (append check-frac-tests scan-frac-tests scan-many-fracs-tests))

(run-tests tests)