#lang sicp

; Exercise 1.29

; Implement Simpson's Rule

; Provided functions
(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (even? x)
  (= (remainder x 2) 0))

; My solution:
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (coeff k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          (else (+ 2 (* 2 (remainder k 2))))))
  (define (term k)
    (* (coeff k) (y k)))
  (* (/ h 3.0)
     (sum term 0 inc n)))

(integral cube 0 1 0.001)

(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)
  
; Exercise 1.30

; Write an iterative implementation of the above sum procedure
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Test using the pi procedure from the text

(define (pi-term x)
  (/ 1.0 (* x (+ x 2))))
(define (pi-next x)
  (+ x 4))

; It works! With a slight difference presumably due to rounding
(* 8 (sum pi-term 1 pi-next 100000))
(* 8 (sum-iter pi-term 1 pi-next 100000))

; Exercise 1.31

; Part a. Write a recursive implementation of the product of a series.

(define (product term a next b)
  (if (> a b)
      1.0
      (* (term a)
         (product term (next a) next b))))

; Part b. Write an iterative implementation of the product of a series.

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1.0))

; Use these product producedures to calculate Wallis's approximation of pi

(define (wallis-term k)
  (/ (+ 2 k (remainder k 2))
     (+ 2 k (remainder (+ k 1) 2))))

(* 4 (product wallis-term 0 inc 100000))
(* 4 (product-iter wallis-term 0 inc 100000))

; Exercise 1.32

; Part a. Show that sum and product can both be expressed by a higher-order procedure,
; called accumulate.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

; To prove that sum or product are equivalent, consider this substitution:
; (accumulate * 1.0 wallis-term 0 inc 100)
; (> 0 100) is false
; (* (wallis-term 1.0)
;    (accumulate * 1.0 wallis-term 1 100))
; Then recur until (= a 101), in which case the function terminates with the form
; (* (wallis-term 0)
;    (wallis-term 1)
;    (wallis-term 2)
;    ...
;    (wallis-term 100)
;    1.0)
; Which is precisely the final expression of (product wallis-term 0 inc 100)

; Part b. Write an iterative version of accumulate

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; Here the proof is even simpler. Consider this substitution

; (accumulate-iter + 0 wallis-term 0 inc 1000)
; (iter 0 0)
; (> a b) is false
; (iter (inc a) (+ (wallis-term a) 0)
; Which is identical to the expression of the first iteration of
; (sum wallis-term 0 inc 1000)

; Exercise 1.33

; Write a filtered-accumulate function that only accumulates values for which
; a meets a certain condition.

(define (filtered-accumulate combiner null-value filter? term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter? a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))

; Use this procedure to find:

; a. the sum of the squares of the prime numbers in the interval a to b

; The prime? predicate from Chapter 1.2

(define (prime? n)
  (define (divides? x y)
    (= (remainder y x) 0))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (= n (smallest-divisor n)))

(define (square x)
  (* x x))

(define (sum-prime-squares a b)
  (filtered-accumulate + 0 prime? square a inc b))

(sum-prime-squares 0 10) ; = 88
(+ (square 1) (square 2) (square 3) (square 5) (square 7)) ; = 88

; b. the sum of the relative primes of n in the interval a to b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (multiply-relative-primes n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (define (identity x)
    x)
  (filtered-accumulate * 1.0 relative-prime? identity 1 inc n))

(multiply-relative-primes 20)

; Exercise 1.34

(define (f g) (g 2))

; What happens if we ask the interpreter to evaluate (f f)?
; (f f)
; (f 2)
; (2 2)
; Error: 2 is not a procedure -- uncomment the first (or indeed the second) line
; bove to reproduce the error

; Exercise 1.35

; Show that the golden ratio is a fixed point of the transformation x --> 1 + 1/x, then
; compute the value using the fixed-point procedure

; φ^2 = φ + 1 ; definition of 'golden ratio'
; φ^2/φ = φ/φ + 1/φ
; φ = 1 + 1/φ
; which is the fixed point of the transformation
; x --> 1 + 1/x

(define tolerance 0.00001)
(define (close-enough? v1 v2)
  (< (abs (- v1 v2))
     tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Estimate for φ
(fixed-point (lambda (x) (+ 1.0 (/ 1.0 x)))
             1.0)

; Exercise 1.36

; Define a verbose fixed-point function so it prints its estimate each time
(define (fp-verbose f first-guess)
  (define (try guess i)
    (let ((next (f guess)))
      (cond ((close-enough? guess next)
             (display "Solved on Guess ")
             (display i)
             (display ": ")
             (display next)
             (newline)
             next)
            (else
             (display "Guess ")
             (display i)
             (display ": ")
             (display guess)
             (newline)
             (try next (inc i))))))
  (try first-guess 0))

; Compare with and without average damping. How many steps?
(display "Find solution to x^x = 1000 without average damping\n")
; x is equal to the fixed point of f(x) = log(1000)/log(x)
(define (log_div x)
  (/ (log 1000) (log x)))
(fp-verbose log_div 2)
(display "Find solution to x^x = 1000 with average damping\n")
(define (avg-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))
(fp-verbose (avg-damp log_div) 2)

; It took 33 steps without avg-damp, and only 8 steps with it.

; Exercise 1.37

; Write a procedure that approximates a continued fraction by computed
; the k-term finite continued fraction.
; The function takes three arguments:
;   n = a procedure to compute the numerator of term i
;   d = a procedure to computer the denominator of term i
;   k = how many terms to compute

(define (cont-frac n d k)
  (define (compute-term i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (compute-term (inc i))))))
  (compute-term 1))

; Use cont-frac to compute 1/φ to four decimal digits accuracy. What value of
; k is required to achieve this?

; 11 steps (just manual trial-and-error! I know I could have written a function that
; returned the required number of steps...)

(display "1/φ computed using fixed-point:\n")
(/ 1
   (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x)))
                1.0))
(display "1/φ computed using cont-frac:\n")
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

; Write iterative version of cont-frac

(define (cont-frac-iter n d k)
  (define (iter-step i acc)
    (if (= 0 i)
        acc
        (iter-step (dec i) (/ (n i) (+ (d i) acc)))))
  (iter-step k 0))

(display "1/φ computed using cont-frac-iter:\n")
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

; Exercise 1.38

; Use cont-frac to compute Euler's approximation of e - 2, where the denominator
; of successive terms are 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, etc.

(define (euler_d i)
  (if (= (remainder (inc i) 3) 0)
      (- i (/ (- i 2) 3))
      1))

(display "e computed using Euler's continued fraction expansion:\n")
(+ 2
   (cont-frac-iter (lambda (i) 1.0)
                   euler_d
                   1000))

; Exercise 1.39

; Write a procedure that computes Lambert's continued fraction for (tan x)

(define (tan-cf x k)
  (let ((n (lambda (i) ; numerator
             (if (= i 1)
                 x
                 (- (* x x)))))
        (d (lambda (i) ; denominator
             (dec (* 2.0 i)))))
    (cont-frac-iter n d k)))

; Benchmarking
(display "(tan 1) = ")
(tan 1)
(display "(tan-cf 1 10) = ")
(tan-cf 1 10)

; Exercise 1.40

; Define a procedure (cubic a b c) that can be used with `newtons-method` to approximate the
; roots of (x^3 + ax^2 + bx^3 + c)

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a (* x x)) (* b x) c)))

; Exercise 1.41

; Define a procedure (double proc) that returns a new procedure that applies proc to its argument
; twice [proc takes only one argument]

(define (double proc)
  (lambda (x)
    (proc (proc x))))

; What value is returned by
; First guess = 13
; Wrong! It adds 16, not 8. Why? (double double) applies inc four times, then the outer
; double squares this
(((double (double double)) inc) 5)
; Here's the substitution
; (double double)
; (lambda (x) (double (double x)))
; (lambda (x) ((lambda (x) (proc (proc x))) ((lambda (x) (proc (proc x))) x) ; 4 x proc
; So...
; (double (double double))
; (lambda (x)
;   ((double double) ((double double) x)))
; Apply to inc
; ((double double) ((double double) inc)))
; ((double double) ((lambda (x) (double (double x))) inc)))
; ((double double) ((double (double inc)))
; ((double double) (double (lambda (x) (inc (inc x)))))
; ((double double) (lambda (x) (inc (inc (inc (inc x)))))) ; Bit of a shortcut
; Now (double double) will apply the new proc another four times
; meaning that ultimately `inc` will be applied 16 times

; Or, more intuitively:
; (double (double double)) effectively means, 'apply (double double) to iteself'
; Now (double double) means 'apply the provided procedure four times to its input',
; so if we apply (double double) to (double double), we apply (double double) four
; times to its input. Each time, it quadruples the application of the input procedure,
; meaning that (double (double double)) will cause its input to be applied 4 x 4 = 16 times
; to its own operand.

; Exercise 1.42

; Define a higher-order function that takes two functions as input, and
; outputs a new function where they are sucessively applied

(define (compose f g)
  (lambda (x)
    (f (g x))))

(display "Compose test. Expected value = 49\n")
((compose square inc) 6)

; Exercise 1.43

; Define a more general version of 'double' that lets you repeat
; a function n times

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

(display "Repeated test. Expected value = 625\n")
((repeated square 2) 5)

; Exercise 1.44

; Define a smooth function, then extend it into an n-fold-smooth function
; The question asks that smooth *not* take dx as an input

(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3.0))))

(define (n-fold-smooth f n)
  (repeated smooth n))

; Exercise 1.45

; How many average-damps are required to compute the nth root as a fixed-point search?

; the nth root of x is the fixed point of y --> x/y^(n-1)

; Borrow the fast-expt procedure from Chapter 1.2
(define (fast-expt-iter b n)
  (define (even? x)
    (= (remainder x 2) 0))
  (define (step a b n)
    (cond ((= n 1) (* a b))
          ((even? n) (step a (square b) (/ n 2)))
          (else (step (* a b) b (dec n)))))
  (step 1 b n))

; Generate y --> x/y^(n-1)
(define (fp-root-func b n)
  (lambda (x)
    (/ b (fast-expt-iter x (dec n)))))

(newline)
(display "Average damp experiments:\n")
(display "5th root of 100: ")
(fixed-point (avg-damp (avg-damp (fp-root-func 100 5)))
             1.0)
(display "6th root of 100: ")
(fixed-point (avg-damp (avg-damp (fp-root-func 100 6)))
             1.0)
(display "7th root of 100: ")
(fixed-point (avg-damp (avg-damp (fp-root-func 100 7)))
             1.0)
(display "8th root of 100 (required a third avg-damp): ")
(fixed-point (avg-damp (avg-damp (avg-damp (fp-root-func 100 8))))
             1.0)
(display "15th root of 100: ")
(fixed-point (avg-damp (avg-damp (avg-damp (fp-root-func 100 15))))
             1.0)
(display "16th root of 100 (required a fourth avg-damp): ")
(fixed-point (avg-damp (avg-damp (avg-damp (avg-damp (fp-root-func 100 16)))))
             1.0)
; n = 2,3 - 1 average damp
; n = 4,5,6,7 - 2 average damps
; n = 8-15 - 3 average damps
; n = 16... to 31 - 4 average damps

; The number of average damps required goes up with the logarithm of 2...
(define (nth-root x n)
  (let ((f (fp-root-func x n))
        (trans (repeated avg-damp (floor (log n 2)))))
    (fixed-point (trans f) 1.0)))

(newline)
(display "The 123rd root of 5000: ")
(nth-root 123 5000)
(display "Average-damps required: ")
(floor (log 123 2))

; Exercise 1.46

; Define a general iterative-improve procedure
(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (if (good-enough? guess)
           guess
           (try (improve guess))))
    (try first-guess)))

; Use it to define new sqrt and fixed-point procedures
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (let ((average (lambda (a b) (/ (+ a b) 2)))
        (good-enough? (lambda (guess) (< (abs (- (square guess) x)) 0.001)))
        (improve (lambda (guess) (average guess (/ x guess)))))
    ((iterative-improve good-enough? improve) 1.0)))

(newline)
(display "Tests with iterative-improve:\n")
(display "sqrt of 49: ")
(sqrt 49)

(define (fp-abstract f)
  (let ((tolerance 0.00001)
        (good-enough? (lambda (guess) (< (abs (- guess (f guess)))
                                         tolerance)))
        (improve (lambda (guess) (f guess))))
    ((iterative-improve good-enough? improve) 1.0)))

(display "12th root of 4578: ")
(fp-abstract ((repeated avg-damp (floor (log 12 2)))
              (fp-root-func 4578 12)))
    