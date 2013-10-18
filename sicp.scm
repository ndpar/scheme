#lang racket

; p.12
(define (square x)
  (* x x))

; p.13
(define (sum-of-squares x y)
  (+ (square x) (square y)))

; Exercise 1.2, p.21
;(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
;   (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3, p.21
(define (sicp-1-3 x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y z) (< y x)) (sum-of-squares x z))
        ((and (< z x) (< z y)) (sum-of-squares x y))))

; p.23
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

; Exercise 1.7, p.25
;(sqrt (square 0.01))
;(my-sqrt (square 0.01))
;(my-sqrt (square (+ 1e+30 1e+6)))

(define (good-enough-2? prev guess)
  (< (abs (- prev guess)) 0.001))

(define (sqrt-iter-2 prev guess x)
  (if (good-enough-2? prev guess)
      guess
      (sqrt-iter-2 guess (improve guess x)
                   x)))

(define (my-sqrt-2 x)
  (sqrt-iter-2 0.0 1.0 x))

;(my-sqrt-2 (square 0.01)) ; works now for small numbers
;(my-sqrt-2 (square (+ 1e+30 1e+6))) ; still not working for big numbers

; Exercise 1.8, p.26
; Cube root
(define (cbrt x)
  (define (good-enough? prev guess)
    (< (abs (- prev guess)) 0.001))
  (define (improve guess)
    (/ (+ (* 2 guess) (/ x guess guess)) 3))
  (define (cbrt-iter prev guess)
    (if (good-enough? prev guess)
        guess
        (cbrt-iter guess (improve guess))))
  (cbrt-iter 0.0 1.0))

; Exercise 1.10, p.36
; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;(= (A 1 10) 1024)
;(= (A 2 4) 65536 (A 3 3))

; Exercise 1.11, p.42
; Tree recursion vs linear iteration
(define (f1 n)
  (cond ((< n 3) n)
        (else (+ (* 1 (f1 (- n 1)))
                 (* 2 (f1 (- n 2)))
                 (* 3 (f1 (- n 3)))))))

(define (f2 n)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (iter 2 1 0 n))

; Exercise 1.12, p.42
; Pascal's triangle
(define (pascal m n)
  (if (or (= n 1) (= m n))
      1
      (+ (pascal (- m 1) (- n 1))
         (pascal (- m 1) n))))

; Exercise 1.15, p.44
(define (cube x) (* x x x))
(define (p x)
  (display " *** ")
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; Exercise 1.16, p.46
; Iterative exponentiation by successive squaring
; Time complexity Θ(log n), space Θ(1)
(define (expt x n)
  (define (iter a x n)
    (cond ((= n 0) a)
          ((even? n) (iter a (* x x) (/ n 2)))
          (else (iter (* a x) x (- n 1)))))
  (iter 1 x n))

; Exercise 1.17, p.46
; Recursive multiplication by successive doubling
; Time and space complexity Θ(log n)
(define (double n) (+ n n))
(define (halve n) (quotient n 2))

(define (recur-mult m n)
  (cond ((= n 0) 0)
        ((= n 1) m)
        ((even? n) (recur-mult (double m) (halve n)))
        (else (+ m (recur-mult m (- n 1))))))

; Exercise 1.18, p.47
; Iterative multiplication by successive doubling
; Time complexity Θ(log n), space Θ(1)
(define (russian m n)
  (define (iter a m n)
    (cond ((= n 0) a)
          ((even? n) (iter a (double m) (halve n)))
          (else (iter (+ a m) m (- n 1)))))
  (iter 0 m n))

; p.50
; Deterministic primality test
; Time complexity: Θ(√n)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next td)
;    (if (= td 2) 3 (+ td 2))) ; Exercise 1.23, p.54
    (+ td 1))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (and (> n 1)
       (= (smallest-divisor n) n)))

; p.51
; Probabilistic primality test
; Time complexity: Θ(log n)
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Exercise 1.22, p.54
; Primality tests: order of growth
(define (search-for-primes n count)
  (cond ((= count 0) (newline) (display "done"))
        ((even? n) (search-for-primes (+ n 1) count))
;        ((fast-prime? n 3) (timed-prime-test n) ; Exercise 1.24, p.55
        ((prime? n) (timed-prime-test n)
                    (search-for-primes (+ n 2) (- count 1)))
        (else (search-for-primes (+ n 2) count))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
;  (cond ((fast-prime? n 3) ; Exercise 1.24, p.55
  (cond ((prime? n)
         (report-prime (- (current-inexact-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " ** ")
  (display elapsed-time))

;(timed-prime-test 37) ; to warm interpreter
;(timed-prime-test 1009)
;(timed-prime-test 10007)
;(timed-prime-test 100003)
;(timed-prime-test 1000003)

; Exercise 1.27, p.55
(define (carmichael-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (iter a prod)
    (if (= 1 a)
        prod
        (iter (- a 1) (and (try-it a) prod))))
  (iter (- n 1) #t))

;(carmichael-test 561)
;(carmichael-test 1105)
;(carmichael-test 1729)
;(carmichael-test 2465)
;(carmichael-test 2821)
;(carmichael-test 6601)

; Exercise 1.29, p.60
; Simpson's rule for numerical integration
; N must be even
(define (inc n) (+ n 1))

(define (dec n) (- n 1))

(define (integral f a b n)
  (let ([h (/ (- b a) n)])
    (define (y k)
      (f (+ a (* k h))))
    (define (coef k)
      (cond ((or (= k 0) (= k n)) 1)
            ((odd? k) 4)
            ((even? k) 2)))
    (define (term k)
      (* (coef k) (y k)))
    (* (/ h 3) (sum term 0 inc n))))

; (integral cube 0 1 100)
; (integral cube 0 1 1000)

; (integral cube 0.0 1.0 100)
; (integral cube 0.0 1.0 1000)

; Exercise 1.30, p.60
; Itirative summation
(define (sum term a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i) (+ (term i) result))))
  (iter a 0))

; Exercise 1.31, p.60
; Recursive product
(define (prod-rec factor a next b)
  (if (> a b)
      1
      (* (factor a)
         (prod-rec factor (next a) next b))))

; Iterative product
(define (product factor a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i) (* (factor i) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (π/4 iter)
  (define (π-factor i)
    (* 1.0 (dec i) (inc i) (/ (square i))))
  (define (π-next i)
    (+ i 2))
  (product π-factor 3 π-next iter))

; Exercise 1.32, p.61
; Recursive accumulator
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

; Iterative accumulator
(define (accumulate combiner null-value term a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i) (combiner (term i) result))))
    (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

; Exercise 1.33, p.61
(define (filtered-accumulate combiner null-value pred term a next b)
  (define (iter i result)
    (cond ((> i b) result)
          ((pred i) (iter (next i) (combiner (term i) result)))
          (else (iter (next i) result))))
  (iter a null-value))

(define (square-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

(define (rel-prime-prod n)
  (define (rel-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 rel-prime? identity 1 inc (dec n)))