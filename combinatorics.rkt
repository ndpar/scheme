#lang racket

;; One-level-deep flatten
(define (flatten-1 coll)
  (foldr append null coll))

(define (flat-map f coll)
  (flatten-1 (map f coll)))

(define (scale a vector)
  (map (λ (v) (* a v)) vector))

(define (rational->real vector)
  (map (λ (r) (rationalize r 0.01)) vector))


;; Calculates different ways of distributing
;; (integer) total sum between n people
(define (distribute n total)
  (if (= 1 n)
      (list (list total))
      (let ([ts (range (+ 1 total))])
        (flat-map (λ (t)
                    (map (λ (a) (cons t a))
                         (distribute (- n 1) (- total t))))
                  ts))))

(= 56 (length (distribute 4 5)))


;; Calculates different percentage allocations
;; between n accounts, with step as unit fraction
(define (allocations n step)
  (map (λ (x) (rational->real (scale step x)))
       (distribute n (/ step))))

;(= 176851 (length (allocations 4 1/100)))
(= 286 (length (allocations 4 1/10)))