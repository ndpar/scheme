#lang racket

; http://en.wikipedia.org/wiki/Miller–Rabin_primality_test

(define (devides? d n)
  (= (remainder n d) 0))

(define (square x)
  (* x x))


; Fast modular exponentiation by repeated squaring.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


; Return a random integer from the range [lower..upper].
(define (rand lower upper)
  (+ lower (random (+ (- upper lower) 1))))


; Return two values D and S such that NUMBER = DIVISOR^S * D,
; and D is not divisible by DIVISOR.
(define (factor-out number divisor)
  (define (iter d s)
    (if (devides? divisor d)
        (iter (/ d divisor) (+ s 1))
        (values d s)))
  (iter number 0))


; Test N for primality by performing the Miller-Rabin test K times.
; Return #f if N is composite, and #t if N is probably prime.
(define (miller-rabin-test n k)
  (cond ((= n 1) #f)
        ((< n 4) #t)
        ((even? n) #f)
        (else
         (let-values ([(d s) (factor-out (- n 1) 2)])
           (define (strong-liar? a)
             (define (iter c)
               (if (<= s c)
                   #f
                   (let ([m (expmod a (* d (expt 2 c)) n)])
                     (cond ((= m 1) #f)
                           ((= m (- n 1)) #t)
                           (else (iter (+ c 1)))))))
             (let ([x (expmod a d n)])
               (if (or (= x 1) (= x (- n 1)))
                   #t
                   (iter 1))))
           (define (test count)
             (if (= count k)
                 #t
                 (if (strong-liar? (rand 2 (- n 2)))
                     (test (+ 1 count))
                     #f)))
           (test 0)))))
