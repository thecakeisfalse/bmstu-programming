
;; hw1-day-of-week

(define (day-of-week day month year)
  ((lambda (year)
     (modulo (+ day
                (quotient (* 31 (if (< month 3)
                                    (+ month 10)
                                    (- month 2))) 12)
                year
                (quotient year 4)
                (- (quotient year 100))
                (quotient year 400)) 7)) (- year (if (< month 3) 1 0))))

;; hw1-square-root

(define (square-equation a b c)
  (let ((D (- (* b b) (* 4 a c))))
    (cond
      ((< D 0) '())
      ((= D 0) (list (/ (- b) (* 2 a))))
      (else
       (list (/ (- (- b) (sqrt D)) (* 2 a))
             (/ (+ (- b) (sqrt D)) (* 2 a)))))))

;; hw1-gcd-lcm

(define (my-gcd a b)
  (if (> b a)
      (my-gcd (abs b) (abs a))
      (if (or (= a 0) (= b 0))
          (+ a b)
          (my-gcd b (modulo a b)))))

(define (my-lcm a b)
  (/ (* (abs a) (abs b)) (my-gcd a b)))

(define (prime? n)
  (define (prime-rec n d)
    (or (> (* d d) n)
        (and (> (modulo n d) 0)
             (prime-rec n (+ d 1)))))
  (prime-rec n 2))
