#lang racket

; p.12
(define (square x)
  (* x x))

; p.13
(define (sum-of-squares x y)
  (+ (square x) (square y)))

; Exercise 1.2, p.21
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

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
(sqrt (square 0.01))
(my-sqrt (square 0.01))
(my-sqrt (square (+ 1e+30 1e+6)))

(define (good-enough-2? prev guess)
  (< (abs (- prev guess)) 0.001))

(define (sqrt-iter-2 prev guess x)
  (if (good-enough-2? prev guess)
      guess
      (sqrt-iter-2 guess (improve guess x)
                   x)))

(define (my-sqrt-2 x)
  (sqrt-iter-2 0.0 1.0 x))

(my-sqrt-2 (square 0.01)) ; works now for small numbers
(my-sqrt-2 (square (+ 1e+30 1e+6))) ; still not working for big numbers

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

(= (A 1 10) 1024)
(= (A 2 4) 65536 (A 3 3))

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