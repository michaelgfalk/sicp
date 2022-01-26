#lang sicp
; Chapter 1.2

; Exercise 1.9
; Which of these procedures evolves an interative process, and which a
; recursive?
(define (recursive-sum a b)
  (if (= a 0) b (inc (recursive-sum (dec a) b))))
; Consider (recursive-sum 3 2)
; (recursive-sum 3 2)
; (inc (recursive-sum 2 2))
; (inc (inc (recursive-sum 1 2))
; (inc (inc (inc (recursive-sum 0 2))
; (inc (inc (inc 2))
; (inc (inc 3))
; (inc 4)
; (5)
(define (iterative-sum a b)
  (if (= a 0) b (iterative-sum (dec a) (inc b))))
; (iterative-sum 3 2)
; (iterative-sum 2 3)
; (iterative-sum 1 4)
; (iterative-sum 0 5)
; 5

; Exercise 1.10
; A version of Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))


; (A 1 10)      ; else 
; (A 0 (A 1 9)) ; else
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
; ... which resolves into 2^10
(A 1 10)
(define (raise x power)
  (define (raise-iter val power count)
    (if (= count power)
        val
        (raise-iter (* val x) power (inc count))))
  (raise-iter x power 1))
(raise 2 10)

; (A 2 4) ; else
; (A 1 (A 2 3)) ; else
; (A 1 (A 1 (A 2 2))) ; else
; (A 1 (A 1 (A 1 (A 2 1)))) ; (= y 1) is #t 
; (A 1 (A 1 (A 1 2))) ; else
; (A 1 (A 1 (A 0 (A 1 1)))) ; (= y 1) is #t
; (A 1 (A 1 (A 0 2))) ; (= x 0) is #t
; (A 1 (A 1 4)) ; (A 1 4) == 2^4
; (A 1 16) == 2^16
(A 2 4)
(raise 2 16)

; (A 3 3) ; else
; (A 2 (A 3 2)) ; else
; (A 2 (A 2 (A 3 1))) ; (= y 1) is #t
; (A 2 (A 2 2)) ; else
; (A 2 (A 1 (A 2 1))) ; (= y 1) is #t
; (A 2 (A 1 2)) ; (A 1 2) == 2^2
; (A 2 4) ; (A 2 4) == 2^16
(A 3 3)
(raise 2 16)

; Give concise mathematical definitions for the following:

(define (f n) (A 0 n)) ; f(n) = 2n
(f 7)
(f 22)

(define (g n) (A 1 n)) ; g(n) = 2^n
(g 4)
(g 5)

(define (h n) (A 2 n)) ; h(n) = 2^(h(n-1))
; (h 1) == 2 = 2
; (h 2) == 2^(h 1) = 2^2
; (h 3) == 2^(h 2) = 2^4
; (h 4) == 2^(h 3) = 2^16
; (h 5) == 2^(h 4) = 2^65536

; The coin example
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                 (- kinds-of-coins 1))
                 (cc (- amount
                     (first-denomination
                      kinds-of-coins))
                  kinds-of-coins)))))
; Returns value of highest-denomination coin available
; given # types of coins available (e.g. if 5 coins are available,
; then the highest-demonination coin is a half-dollar, $0.50
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; a = 10, n = 2, denoms = (1 5)
; num_ways = (+ (:a 10 :n 1) (:a (- 10 5) :n 2))
; num_ways = (+ (<:a counts down to 0>)
;               (<:a splits once (nickel for the rest or pennies),
;                and each split counts down to zero)
; num_ways = 3
(count-change 100)

; Exercise 1.11
; Recursive definition of f(n)
(define (fr n)
  (if (< n 3)
      n
      (+ (fr (- n 1))
         (* 2 (fr (- n 2)))
         (* 3 (fr (- n 3))))))

; Iterative definition of f(n)
; Each step is the linear combination of the three previous terms
; First three terms are 0 1 2 (and endlessly backward in the negative
; numbers...
(define (fi n)
  (define (func-iter t1 t2 t3 count)
    (if (> count (- n 3))
        t1
        (func-iter (+ t1 (* 2 t2) (* 3 t3)) t1 t2 (inc count))))
  (if (< n 3)
      n
      (func-iter 2 1 0 0)))

(fr 5) 
(fi 5) ; Expected value == 25


; Exercise 1.12
; Write a recursive function that computes the nth number in pascal's
; triangle
; Let n be the index of the nth number of pascal's triangle  
(define (pascal n)
  ; Calculate the row of the nth number
  (define (row n)
    (define (row-count n r)
      (if (<= n r)
          r
          (row-count (- n r) (inc r))))
    (row-count n 1))
  ; Which n is on the right edge of a row?
  (define (right-edge r)    
    (define (row-count current-row total r)
      (if (= current-row r)
          (+ total current-row)
          (row-count (inc current-row) (+ total current-row) r)))
      (row-count 1 0 r))
  ; Which n is on the left edge?
  (define (left-edge r)
    (- (right-edge r) (dec r)))
  ; edge? predicate
  (define (edge? n)
    (define n-row (row n))
    (cond ((= n (right-edge n-row)) #t)
          ((= n (left-edge n-row)) #t)
          (else #f)))
  ; If it's an edge, 1, otherwise compute the sum of the two
  ; numbers above using the row index
  (if (edge? n)
      1 
      (+ (pascal (- n (row n)))
         (pascal (- n (dec (row n)))))))

; The first 15 pascal numbers
; Expected value == 1 1 1 1 2 1 1 3 3 1 1 4 6 4 1
(map pascal (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

; Exercise 1.14
;
; What is the order of complexity of the count-change procedure in terms of
; space and time?
;
; The order is the same. Ø(n) = a for both. Each time a cent is added to
; the total, this results in one extra split in each branch of the tree
; that *doesn't* end by running out of denominations of coins. This is
; because each cent requires one extra subtraction from the total. Since
; each subtraction splits the tree, space and number of steps grow together.
; Every time you subtract a penny from the total, for example, the computer
; also considers the case where you try and make change from the total with
; no coins.
; There is actually a slight addition to the growth in space and time. Each
; new denomination that is added to the available denominations adds an
; additional non-terminating branch, which will split again every time the
; amount increases. As the amount gets larger, so do the non-terminal branches
; for each denomination. (e.g. if you change 172 cents, this will create
; a new branch each for using one half-dollar, two half-dollars and three
; half-dollars). So you could say that Ø(n) = ad. Could you?

; Exercise 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1)) angle
      (p (sine (/ angle 3.0)))))
; a. How many times is p called if you compute sine of 12.5?
;
; It is called each time the procedure recurs. You need to divide 12.5
; by 3 six times to get it below 0.1, so p would be called six times:
; (sine 12.5) => (p (sine (/ 12.5 3))) => (p (sine 4.1)) =>
; (p (p (sine (/ 4.1 3))) etc. until angle < 0.1

; b. What is the order of growth in space and steps as a increases?
;
; Ø(n) = log3(a) for both space and steps
; Each time you multiply a by three, you will require an additional division
; and an additional call to (p)
; See the below example:
(define (d3 x) (/ x 3.0))
(d3 (d3 (d3 (d3 3))))
(d3 (d3 (d3 (d3 (d3 9)))))
(d3 (d3 (d3 (d3 (d3 (d3 27))))))

; Exercise 1.16
; Design an iterative version of fast-expt
; Here is the recursive version:
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(define (square n)
  (* n n))

; Here is the iterative version
; Each step looks at the product ab^n
; The state variable a keeps track of odd indices. Each time an odd index is encountered,
; a is multiplied by the base, and the index is decremented by 1. The index is even again,
; and the successive squaring can continue.
(define (fast-expt-iter b n)
  (define (step a b n)
    (cond ((= n 1) (* a b))
          ((even? n) (step a (square b) (/ n 2)))
          (else (step (* a b) b (dec n)))))
  (step 1 b n))

; Expected value == 4294967296
(fast-expt 4 16)
(fast-expt-iter 4 16)

; Expected value == 30517578125
(fast-expt 5 15)
(fast-expt-iter 5 15)

; Exercise 1.17
; Recursive multiplication procedure in logarithmic number of steps
(define (halve x)
  (/ x 2))
(define (double x)
  (+ x x))
(define (recur-prod x y)
  (cond ((= y 1) x)
        ((even? y) (recur-prod (double x) (halve y)))
        (else (+ x (recur-prod x (dec y))))))

; Exercise 1.18
; Design an iterative version.
(define (iter-prod x y)
  (define (step a x y)
    (cond ((= y 1) (+ a x))
          ((even? y) (step a (double x) (halve y)))
          (else (step (+ a x) x (dec y)))))
  (step 0 x y))

; Expected value == 8449
(* 71 119)
(recur-prod 71 119)
(iter-prod 71 119)

; Exercise 1.18

; Iterative implementation of Fibonacci numbers that computes
; in logarithmic time
; Expanding Tpq leads to the following values for p' and q':
; p' = (+ (square p) (square q))
; q' = (* q (+ (* 2 p) q))

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (* q (+ (* 2 p) q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 10) ; Expected value == 55
(fib 12) ; Expected value == 144

; Exercise 1.20

; How many remainders are computed by the below gcd algorithm under normal-
; order evaluation for (gcd 206 40)? How does this compare to the applicative-
; order evaluation?
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Substitution under normal-order evaluation. 18 remainders are computed.
(gcd 206 40)
(= 0 40)
(gcd 40
     (remainder 206 40))
(= (remainder 206 40) 0) ; 1
(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40)))
(= 0
       (remainder 40 (remainder 206 40))) ; 2,3
(gcd (remainder 40
                (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40
                           (remainder 206 40))))
(= 0 (remainder (remainder 206 40)
                (remainder 40
                           (remainder 206 40)))) ; 4,5,6,7
(gcd (remainder (remainder 206 40)
                 (remainder 40
                            (remainder 206 40)))
      (remainder (remainder 40 (remainder 206 40))
                 (remainder (remainder 206 40)
                            (remainder 40
                                       (remainder 206 40)))))
(= 0 (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40
                                      (remainder 206 40))))) ; #t! 8,9,10,11,12,13,14
(remainder (remainder 206 40)
                 (remainder 40
                            (remainder 206 40))) ; 15,16,17,18

; Substitution under applicative-order evaluation. 4 remainders computed.
(gcd 206 40)
(= 0 40)
(gcd 40 (remainder 206 40)) ; 1
(= 0 6)
(gcd 6 (remainder 40 6)) ; 2
(= 0 4)
(gcd 4 (remainder 6 4)) ; 3
(= 0 2)
(gcd 2 (remainder 4 2)) ; 4
(= 0 0)
2

; Section 1.2.6


(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder
                      (square (expmod base
                                      (/ exp 2)
                                      m)) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; Write a search-for-primes procedure and test whether it actually takes
; (root n) time to compute
(define (search-for-primes from max)
  (define (search-iter i primes)
    (cond ((= (length primes) max) (newline) primes)
          ((prime? i) (timed-prime-test i) (search-iter (+ 2 i) (cons i primes)))
          (else (search-iter (+ 2 i) primes))))
  (if (even? from)
      (search-iter (inc from) '())
      (search-iter from '())))

; In each case, n increases by a factor of 10, so the time should increase
; by (sqrt 10) == 3.16
(define test-primes (append
                     (search-for-primes 1000 3)
                     (search-for-primes 10000 3)
                     (search-for-primes 100000 3)))

(* (sqrt 10) 3)
(* (sqrt 10) 9)

; The order of growth estimate seems accurate

; Exercise 1.23

; Write a faster version of (smallest-divisor n) that skips the even numbers
; to cut computation time, then repeat the time estimates for the nine primes
; found above

; Does this change reduce the computation time by a factor of two? If not,
; why not?

(define (my-fast-prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (define (next n)
        (if (= n 2)
            3
            (+ n 2)))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (fast-prime-test n)
  (newline)
  (display n)
  (start-fast-test n (runtime)))
(define (start-fast-test n start-time)
  (if (my-fast-prime? n)
      (report-prime (- (runtime) start-time))))

(display "Test using faster smallest-divisor procedure")
(newline)
(for-each fast-prime-test test-primes)
(newline)

; The change seems to reduce the computation time by roughly (sqrt 2)
; rather than by 2

(/ 25.0 17.0)
(sqrt 2)

; I can't see why this is... is it to do with the speed of the (remainder)
; procedure which is called by (divides?)? Is it faster to compute the
; remainder of an even number? So when we cut out the even numbers,
; we cut out the tests that were faster to perform?

; Exercise 1.24

; Try the time calcuations using the Fermant test

; In theory, the order of growth for this test is O(log n)
(define (fermant-prime-test n)
  (newline)
  (display n)
  (start-fermant-test n (runtime)))
(define (start-fermant-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(newline)
(display "Test using Fermant's test")
(newline)
(for-each fermant-prime-test test-primes)
(newline)
(/ (log 100000) (log 10000))
(/ 31.0 26.0)

; Increasing n seems to increase the computation time by *less* than
; expected.

; I think this is because the underlying expmod procedure also only grows
; logarithmically with the size of n, which becomes the exponent in the
; expmod procedure and is reduced efficiently through successive
; squaring.

; Exercise 1.25

; Why is the following expmod procedure inferior to the one above?
; (define (expmod base exp m)
;   (remainder (fast-expt base exp) m))

; The problem is that (fast-expt base exp) will be an enormous number when testing
; for a large prime. For example, to test if the number 100043 is prime, then any
; number between 1 and 100043 will be raised to the power of 100043.

; Consider the following example:
; (fermat-test 100043)
; (try-it (+ 1 (random (- 100043 1)))
; (= (expmod 90088 100043 100043) 90088))

; Using the new expmod, this would result in:

(remainder (fast-expt 90088 100043) 100043)

; This would first calculate an enormous number, then takes its remainder
; with 100043.

; Using the original expmod, it would become:

(remainder (* 90088 (expmod 90088 100042 100043))
           100043)
(remainder (* 90088 (remainder (square (expmod 90088 (/ 100042 2) 100043))
                               100043))
           100043)
(remainder (* 90088 1) 100043)

; The equation uses the identity xy % m = ((x % m) * (y % m)) % m
; The expression (* (x % m) (y % m)) can never be greater than (m - 1)^2
; This is because x % m < m.
; Thus this algorithm which takes the remainder at each recursion ensures
; that the computer never has to deal with titantically large numbers such
; as 90088^100043
; BTW – in 2022, my Mac laptop easily deals with these large numbers, so long
; as you don't try to print them to the screen!

; Exercise 1.26

; What is wrong with Louis Reasoner's `fast-prime? test?

;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (* (expmod base (/ exp 2) m)
;                       (expmod base (/ exp 2) m))
;                    m))
;        (else
;         (remainder (* base
;                       (expmod base (- exp 1) m))
;                    m))))

; The key problem is in lines four and five. Each time the procedure recurs,
; it sets off two further recursions – which then set off two further recursions...
; Since the procedure took roughly log_2(n) steps to run, if you multiply each
; even step by two, you exactly cancel out the saving in calculations!

; In the original expmod procuedure, these excessive branches of the recursion
; are prevented by enclosing the squaring operation in the (square) function.
; This evaluates the two operands ONCE, and multiplies them by each other,
; rather than setting off the extra recursions.

; Exercise 1.27

(define (complete-fermant-test? n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (try-step a)
    (cond ((= a (- 1 n)) #t)
          ((try-it a) (try-it (inc a)))
          (else #f)))
  (try-step 1))

(complete-fermant-test? 561); Returns #t, even though...
(smallest-divisor 561) ; (remainder 561 3) == 0

; Exercise 1.28

; Implement the Miller-Rabin test by modifying `expmod
; Exp will always be even in this case, because we are testing
; n for primality, and Miller-Rabin asks us to raise a to the power of
; n-1. – We won't test an even number for primality! So n-1 will be odd.

(define (expmod-signal base exp m)
  (define (check a)
    (if (and (not (or (= a 1)
                      (= a (- m 1))))
             (= (remainder (square a) m) 1))
        0
        (remainder (square a) m)))
  (cond ((= exp 0) 1)
        ((even? exp) (check (expmod-signal base (/ exp 2) m)))
        (else
         (remainder
          (* base (expmod-signal base (- exp 1) m))
          m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-signal a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (miller-rabin-prime? n (dec times)))
        (else #f)))

(define (mr-10 n)
  (miller-rabin-prime? n 10))

(map mr-10 test-primes) ; Should all be true
(map mr-10 (list 561 1105 1729 2465 2821 6601)) ; Should all be false
  