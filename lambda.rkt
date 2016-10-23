#lang lazy

;; In this exercise we express Booleans, Lists, and Numbers in terms of lambdas.
;; The code is based on Lecture 6.6 of 'Functional Programming in Haskell'
;; taught by University of Glasgow on FutureLearn platform:
;;
;; https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/110703
;;
;; All forms below are lazy so we can implement our own if-form. Therefore to
;; test them, we have to force the expressions.
;;
;; https://docs.racket-lang.org/lazy/

;; To run all tests in this file, execute this command
;; raco test lambda.rkt

(module+ test
  (require rackunit)

  ; poor man's quickcheck
  (define (test-eq left right)
    (check-equal? (! (left 1 2)) (! (right 1 2)))))

;; Booleans

(define (TRUE x y) x)
(define (FALSE x y) y)

(module+ test
  (check-equal? (! (TRUE 1 2)) 1)
  (check-equal? (! (FALSE 1 2)) 2))

;; Boolean Algebra

(define (AND g h)
  (g (h TRUE FALSE) FALSE))

(module+ test
  (test-eq TRUE (AND TRUE TRUE))
  (test-eq FALSE (AND FALSE TRUE))
  (test-eq FALSE (AND TRUE FALSE))
  (test-eq FALSE (AND FALSE FALSE)))

(define (OR g h)
  (g TRUE (h TRUE FALSE)))

(module+ test
  (test-eq TRUE (OR TRUE TRUE))
  (test-eq TRUE (OR FALSE TRUE))
  (test-eq TRUE (OR TRUE FALSE))
  (test-eq FALSE (OR FALSE FALSE)))

(define (NOT g)
  (g FALSE TRUE))

(module+ test
  (test-eq FALSE (NOT TRUE))
  (test-eq TRUE (NOT FALSE)))

(define (XOR g h)
  (AND (OR g h) (OR (NOT g) (NOT h))))

(module+ test
  (test-eq FALSE (XOR TRUE TRUE))
  (test-eq TRUE (XOR FALSE TRUE))
  (test-eq TRUE (XOR TRUE FALSE))
  (test-eq FALSE (XOR FALSE FALSE)))

(define (XNOR g h)
  (NOT (XOR g h)))

(module+ test
  (test-eq TRUE (XNOR TRUE TRUE))
  (test-eq FALSE (XNOR FALSE TRUE))
  (test-eq FALSE (XNOR TRUE FALSE))
  (test-eq TRUE (XNOR FALSE FALSE)))

;; Conditions

(define (IF-THEN-ELSE c t e)
  ; this is where we need laziness
  ; we don't want t and e evaluated when they are not needed
  (c t e)) 

(module+ test
  (check-equal? 1 (! (IF-THEN-ELSE TRUE 1 (/ 2 0))))
  (check-equal? 2 (! (IF-THEN-ELSE FALSE (/ 1 0) 2))))

;; Lists

(define (EMPTY f) TRUE)

(define (EMPTY? ls)
  (ls (λ [x xs] FALSE)))

(module+ test
  (test-eq TRUE (EMPTY? EMPTY))
  (test-eq FALSE (EMPTY? (CONS 42 EMPTY)))
  (test-eq TRUE (EMPTY? (TAIL (CONS 42 EMPTY)))))

(define ((CONS x xs) c)
  (c x xs))

(define (HEAD ls)
  (ls (λ [x _] x)))

(define (TAIL ls)
  (ls (λ [_ y] y)))

(module+ test
  (check-equal? 1 (! (HEAD (CONS 1 EMPTY))))
  (check-equal? 1 (! (HEAD (TAIL (CONS 2 (CONS 1 EMPTY)))))))

(define (FOLDL f acc ls)
  (IF-THEN-ELSE
   (EMPTY? ls)
   acc
   (FOLDL f (f (HEAD ls) acc) (TAIL ls))))

(module+ test
  (check-equal? 0 (! (FOLDL + 0 EMPTY)))
  (check-equal? 6 (! (FOLDL + 0 (CONS 3 (CONS 2 (CONS 1 EMPTY)))))))

(define (MAP f ls)
  (IF-THEN-ELSE
   (EMPTY? ls)
   EMPTY
   (CONS (f (HEAD ls)) (MAP f (TAIL ls)))))

(module+ test
  (! (let* [(ls (CONS 3 (CONS 2 (CONS 1 EMPTY))))
            (result (MAP (λ [x] (+ 1 x)) ls))]
       (check-equal? 4 (! (HEAD result)))
       (check-equal? 3 (! (HEAD (TAIL result))))
       (check-equal? 2 (! (HEAD (TAIL (TAIL result))))))))

(define (REVERSE ls)
  (FOLDL (λ [e acc] (CONS e acc)) EMPTY ls))

(module+ test
  (! (let [(result (REVERSE (CONS 3 (CONS 2 (CONS 1 EMPTY)))))]
       (check-equal? 1 (! (HEAD result)))
       (check-equal? 2 (! (HEAD (TAIL result))))
       (check-equal? 3 (! (HEAD (TAIL (TAIL result))))))))

(define (CONCAT ls1 ls2)
  (FOLDL (λ [e acc] (CONS e acc)) ls2 (REVERSE ls1)))

(module+ test
  (! (let [(result (CONCAT (CONS 4 (CONS 3 EMPTY)) (CONS 2 (CONS 1 EMPTY))))]
       (check-equal? 4 (! (HEAD result)))
       (check-equal? 3 (! (HEAD (TAIL result))))
       (check-equal? 2 (! (HEAD (TAIL (TAIL result)))))
       (check-equal? 1 (! (HEAD (TAIL (TAIL (TAIL result)))))))))

(define (LENGTH ls)
  (FOLDL (λ [_ acc] (add1 acc)) 0 ls))

(module+ test
  (check-equal? 0 (! (LENGTH EMPTY)))
  (check-equal? 1 (! (LENGTH (CONS 1 EMPTY))))
  (check-equal? 2 (! (LENGTH (CONS 2 (CONS 1 EMPTY))))))

;; Signed Integers

; We define an integer as a list of booleans, in thermometer encoding, and with
; the following definitions:

; We define usigned 0 as a 1-element list containing FALSE. To get signed
; integers we simply define the first bit of the list as the sign bit. We
; define unsigned and signed versions of 0.

; It is essentially Church numerals, positive and negative.

(define UZERO (CONS FALSE EMPTY)) ; [FALSE]

(define ZERO  (CONS TRUE UZERO))  ; [TRUE, FALSE]
(define +ZERO (CONS TRUE UZERO))  ; [TRUE, FALSE]
(define -ZERO (CONS FALSE UZERO)) ; [FALSE, FALSE]

(define +ONE (CONS TRUE (CONS TRUE UZERO)))
(define -ONE (CONS FALSE (CONS TRUE UZERO)))

(define (POS? n) (HEAD n))
(define (NEG? n) (NOT (HEAD n)))
(define (ZERO? n) (NOT (HEAD (TAIL n))))
(define (SIGN n) (HEAD n))

(define (NEG n)
  (CONS (NOT (HEAD n)) (TAIL n)))

(define (INC n)
  (IF-THEN-ELSE
   (POS? n)
   (CONS TRUE (CONS TRUE (TAIL n)))
   (IF-THEN-ELSE
    (ZERO? n)
    +ONE
    (CONS FALSE (TAIL (TAIL n))))))

(define +TWO (INC +ONE))
(define +THREE (INC +TWO))
(define +FOUR (INC +THREE))
(define +FIVE (INC +FOUR))
(define +SIX (INC +FIVE))

(define (DEC n)
  (IF-THEN-ELSE
   (ZERO? n)
   -ONE
   (IF-THEN-ELSE
    (NEG? n)
    (CONS FALSE (CONS TRUE (TAIL n)))
    (CONS TRUE (TAIL (TAIL n))))))

(define -TWO (DEC -ONE))
(define -THREE (DEC -TWO))
(define -FOUR (DEC -THREE))
(define -FIVE (DEC -FOUR))
(define -SIX (DEC -FIVE))

(module+ test
  (define (to-int n)
    (IF-THEN-ELSE
     (ZERO? n)
     0
     (IF-THEN-ELSE
      (POS? n)
      (add1 (to-int (DEC n)))
      (sub1 (to-int (INC n))))))

  (check-equal? (! (to-int +THREE)) 3)
  (check-equal? (! (to-int -THREE)) -3)
  
  (define (test-eq? n m)
    (check-equal? (! (to-int n)) (! (to-int m)))))

(define (ABS n)
  (IF-THEN-ELSE
   (NEG? n)
   (NEG n)
   n))

(module+ test
  (test-eq? (ABS ZERO) ZERO)
  (test-eq? (ABS +ONE) +ONE)
  (test-eq? (ABS -ONE) +ONE))

(define (ADD n m)
  (IF-THEN-ELSE
   (ZERO? m)
   n
   (IF-THEN-ELSE
    (POS? m)
    (ADD (INC n) (DEC m))
    (ADD (DEC n) (INC m)))))

(module+ test
  (test-eq? (ADD ZERO ZERO) ZERO)
  (test-eq? (ADD +THREE +TWO) +FIVE)
  (test-eq? (ADD -THREE +TWO) -ONE)
  (test-eq? (ADD +THREE -TWO) +ONE)
  (test-eq? (ADD -THREE -TWO) -FIVE))

(define (SUB n m)
  (IF-THEN-ELSE
   (ZERO? m)
   n
   (IF-THEN-ELSE
    (POS? m)
    (SUB (DEC n) (DEC m))
    (SUB (INC n) (INC m)))))

(module+ test
  (test-eq? (SUB ZERO ZERO) ZERO)
  (test-eq? (SUB +THREE +ONE) +TWO)
  (test-eq? (SUB +THREE -ONE) +FOUR)
  (test-eq? (SUB -THREE +ONE) -FOUR)
  (test-eq? (SUB -THREE -ONE) -TWO))

(define (REPLICATE n times acc)
  (IF-THEN-ELSE
   (ZERO? times)
   acc
   (REPLICATE n (DEC times) (ADD n acc))))

(define (MULT n m)
  (IF-THEN-ELSE
   (ZERO? m)
   ZERO
   (IF-THEN-ELSE
    (POS? m)
    (REPLICATE n m ZERO)
    (NEG (REPLICATE n (NEG m) ZERO)))))

(module+ test
  (test-eq? (MULT +ONE ZERO) ZERO)
  (test-eq? (MULT ZERO -ONE) ZERO)
  (test-eq? (MULT +THREE +TWO) +SIX)
  (test-eq? (MULT +THREE -TWO) -SIX)
  (test-eq? (MULT -THREE -TWO) +SIX)
  (test-eq? (MULT -THREE +TWO) -SIX))

(define (TAKE what from times acc)
  (IF-THEN-ELSE
   (OR (NEG? times) (NEG? (SUB from what)))
   acc
   (TAKE what (SUB from what) (DEC times) (INC acc))))

(define (DIV n m)
  (let [(result (TAKE (ABS m) (ABS n) (ABS m) ZERO))]
    (IF-THEN-ELSE
     ;(XNOR (HEAD n) (HEAD m)) ; same signs
     (OR (AND (POS? n) (POS? m)) (AND (NEG? n) (NEG? m)))
     result
     (NEG result))))

(module+ test
  (test-eq? (DIV ZERO -ONE) ZERO)
  (test-eq? (DIV +SIX +TWO) +THREE)
  (test-eq? (DIV +SIX -TWO) -THREE)
  (test-eq? (DIV -SIX -TWO) +THREE)
  (test-eq? (DIV -SIX +TWO) -THREE))

(define (REM n m)
  (SUB n (MULT (DIV n m) m)))

(module+ test
  (test-eq? (REM +FIVE +THREE) +TWO)
  (test-eq? (REM +FIVE -THREE) +TWO)
  (test-eq? (REM -FIVE -THREE) -TWO)
  (test-eq? (REM -FIVE +THREE) -TWO))
