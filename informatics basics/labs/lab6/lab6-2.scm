;; Program    ::= Articles Body
;; Articles   ::= Article Articles | ε
;; Article    ::= define word Program end ε
;; Body       ::= if Body ElsePart endif Body | integer Body | word Body | ε
;; ElsePart   ::= else Body | ε

(load "stream.scm")

;; =======================
;; Вспомогательные функции
;; =======================

(define keywords
  '(+ - * / mod neg
      = > < not and or
      drop swap dup over rot depth))

(define reserved
  '(define end if endif else))

(define (integer-token? token)
  (number? token))

(define (reserved-token? token)
  (and (memq token reserved) #t))

(define (keyword-token? token)
  (and (memq token keywords) #t))

(define (variable-token? token)
  (and (not (keyword-token? token))
       (not (reserved-token? token))
       (not (integer-token? token))))

(define (word-token? token)
  (and token
       (or (keyword-token? token)
           (variable-token? token))))

;; =========================
;; Синтаксический анализатор
;; =========================

(define (parse tokens)
  (define stream
    (make-stream (vector->list tokens)))
  (define axiom program)

  (call-with-current-continuation
   (lambda (error)
     (let ((result (axiom stream error)))
       (and (equal? (peek stream) #f) result)))))

;; Program ::= Articles Body ε
(define (program stream error)
  (list (articles stream error)
        (body stream error)))

;; Articles ::= Article Articles | ε
(define (articles stream error)
  (cond
    ((equal? (peek stream) 'define)
     (cons (article stream error)
           (articles stream error)))
    (else '())))

;; Article ::= define word Program end ε
(define (article stream error)
  (if (equal? (peek stream) 'define)
      (next stream)
      (error #f))

  (define result
    (append (if (word-token? (peek stream))
                (list (next stream))
                (error #f))
            (program stream error)))

  (if (equal? (peek stream) 'end)
      (next stream)
      (error #f))

  result)

;; Body ::= if Body ElsePart endif Body | integer Body | word Body | ε
(define (body stream error)
  (cond
    ((equal? (peek stream) 'if) (if-expr stream error))
    ((integer-token? (peek stream)) (integer-expr stream error))
    ((word-token? (peek stream)) (word-expr stream error))
    (else '())))

(define (if-expr stream error)  
  (define result
    (append
     (if (equal? (peek stream) 'if)
         (list (next stream))
         (error #f))
  
     (list (body stream error))
     (else-expr stream error)))

  (if (equal? (peek stream) 'endif)
      (begin (next stream) '())
      (error #f))

  (cons result (body stream error)))

;; ElsePart ::= else Body | ε
(define (else-expr stream error)
  (cond
    ((equal? (peek stream) 'else)
     (next stream)
     (list (body stream error)))
    (else '())))

(define (word-expr stream error)
  (cons (if (word-token? (peek stream))
            (next stream)
            (error #f))
        (body stream error)))

(define (integer-expr stream error)
  (cons (if (integer-token? (peek stream))
            (next stream)
            (error #f))
        (body stream error)))

(parse #(define a define b 1 end define c 2 end b c end a + 1 2 -))
(parse #(define -- 1 - end
         define =0? dup 0 = end
         define =1? dup 1 = end
         define factorial
           =0? if drop 1 exit endif
           =1? if drop 1 exit endif
           dup --
           factorial
           *
         end
         0 factorial
         1 factorial
         2 factorial
         3 factorial
         4 factorial ))

(parse #(if a b else c d endif))
(parse #(if a else endif))
(parse #(define mod end define mod1 end mod mod1))
(parse #(define a define b 1 end define c 2 end b c d end if a mod 12345 endif))
(parse #(if endif))
(parse #(define a define b body end end a))
