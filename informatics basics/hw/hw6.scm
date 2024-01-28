
;; homework6

;; Лексика (в форме EBNF):
;; =======================
;; tokens  = { token | spaces }
;; token   = special | word | number
;; special = "+" | "-" | "*" | "/" | "^" | "(" | ")"
;; word    = letter { letter }
;; integer = digit { digit }
;; number  = ["+" | "-"] integer ["." integer] ["e" ["+" | "-"] integer]
;; digit   = [0-9]
;; spaces  = " " | "\t" | "\n"
;; letter  = [a-z] | [A-Z]
;;
;; Синтаксис:
;; ==========
;; Expr    ::= Term Expr'
;; Expr'   ::= AddOp Expr | ε
;; Term    ::= Factor Term'
;; Term'   ::= MulOp Term | ε
;; Factor  ::= Power Factor'
;; Factor' ::= PowOp Factor | ε
;; Power   ::= value | "(" Expr ")" | unaryMinus Power

;; ================
;; Код "stream.scm"
;; ================

;; Конструктор потока
(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))

;; Запрос текущего символа
(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))

;; Запрос первых двух символов
(define (peek2 stream)
  (if (null? (car stream))
      (cadr stream)
      (if (null? (cdar stream))
          (list (caar stream))
          (list (caar stream) (cadar stream)))))

;; Продвижение вперёд
(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))

;; =======================
;; Вспомогательные функции
;; =======================

(define (char-special? char)
  (or (and (memq char '(#\+ #\- #\* #\/ #\^ #\( #\))) #t)
      (and (memq char '(+ - * / ^ "(" ")")) #t)))

(define char-letter? char-alphabetic?)
(define char-digit? char-numeric?)

(define (char-sign? char)
  (or (equal? char #\+) (equal? char #\-)))

(define (char-number? char)
  (or (char-digit? char) (char-sign? char)))

(define (char-eof? char)
  (equal? char (integer->char 0)))

(define (char->symbol char)
  (string->symbol (string char)))

(define (char->special char)
  (cond
    ((or (equal? char #\() (equal? char #\)))
     (string char))
    (else (char->symbol char))))

(define (list->number l)
  (string->number (list->string l)))

(define (list->symbol l)
  (string->symbol (list->string (map char-downcase l))))

(define (symbol->scheme s)
  (if (equal? s '^) 'expt s))

;; ======================
;; Лексический анализатор
;; ======================

(define (tokenize str)
  (let* ((EOF (integer->char 0))
         (stream (make-stream (string->list str) EOF)))

    (call-with-current-continuation
     (lambda (error)
       (let ((result (tokens stream error)))
         (and (char-eof? (peek stream)) result))))))

(define (tokens stream error)
  (cond
    ((char-eof? (peek stream)) '())
    ((char-special? (peek stream))
     (cons (char->special (next stream))
           (tokens stream error)))
    ((char-whitespace? (peek stream))
     (next stream)
     (tokens stream error))
    ((char-number? (peek stream))
     (cons (list->number (number stream error))
           (tokens stream error)))
    ((char-letter? (peek stream))
     (cons (list->symbol (word stream error))
           (tokens stream error)))
    (else (error #f))))

(define (number stream error)
  (let loop ((e #f) (dot #f) (prev (peek stream)) (res `(,(next stream))))
    (cond
      ((and (char-alphabetic? (peek stream)) (not (equal? (peek stream) #\e)))
       (error #f))
      ((char-digit? (peek stream))
       (loop e dot (peek stream) (cons (next stream) res)))
      ((and (char-sign? (peek stream)) (equal? prev #\e))
       (loop e dot (peek stream) (cons (next stream) res)))
      ((equal? (peek stream) #\e)
       (and e (error #f))
       (loop #t dot (peek stream) (cons (next stream) res)))
      ((equal? (peek stream) #\.)
       (and (or e dot) (error 'number))
       (loop e #t (peek stream) (cons (next stream) res)))
      (else (reverse res)))))

(define (word stream error)
  (cond
    ((or (char-eof? (peek stream))
         (char-special? (peek stream))
         (char-whitespace? (peek stream)))
     '())
    ((char-letter? (peek stream))
     (cons (next stream) (word stream error)))
    (else (error #f))))

;; =========================
;; Синтаксический анализатор
;; =========================

(define (parse tokens)
  (define stream (make-stream tokens))

  (and tokens
       (if (not stream) '()
           (call-with-current-continuation
            (lambda (error)
              (let ((result (expr stream error)))
                (and (not (peek stream)) result)))))))

(define (expr stream error)
  (let loop ((t (term stream error)))
    (if (or (equal? (peek stream) '+)
            (equal? (peek stream) '-))
        (loop `(,t ,(next stream) ,(term stream error)))
        t)))

(define (term stream error)
  (let loop ((f (factor stream error)))
    (if (or (equal? (peek stream) '*)
            (equal? (peek stream) '/))
        (loop `(,f ,(next stream) ,(factor stream error)))
        f)))

(define (factor stream error)
  (let ((p (power stream error)))
    (if (equal? (peek stream) '^)
        `(,p ,(next stream) ,(factor stream error))
        p)))

(define (power stream error)
  (let ((ch (next stream)))
    (cond ((equal? ch '-)
           (list '- (power stream error)))
          ((equal? ch "(")
           (let ((e (expr stream error)))
             (if (equal? (next stream) ")")
                 e
                 (error #f))))
          ((or (number? ch) (symbol? ch)) ch)
          (else (error #f)))))

;; ===============
;; Преобразователь
;; ===============

(define (tree->scheme tree)
  (cond
    ((boolean? tree) #f)
    ((and (list? tree) (= (length tree) 3))
     (let ((a (car tree)) (op (cadr tree)) (b (caddr tree)))
       `(,(symbol->scheme op) ,(tree->scheme a) ,(tree->scheme b))))
    (else tree)))
