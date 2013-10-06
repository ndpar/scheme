#lang racket

; Exercise 1.5, p.21
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; To check if language is lazy run
; (test 0 (p))
; Scheme is not lazy; the test will run forever.


; Clojure is not lazy either

; user=> (defn p [] (p))
; #'user/p
; user=> (defn test [x y]
;   #_=>   (if (= x 0) 0 y))
; #'user/test
; user=> (test 0 (p))
; StackOverflowError   user/p (form-init814678507212014687.clj:1)


; Proof of 'if' being a spepcial form: run
; (if-test 0 0)
(define (if-test x y)
  (if (= x 0) 0 (/ x y)))


; Exercise 1.6, p.25
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; cond-implementation of 'if' is not special form: run
; (new-if-test 0 0)
(define (new-if-test x y)
  (new-if (= x 0) 0 (/ x y)))
