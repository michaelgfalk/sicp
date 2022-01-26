#lang sicp

; Chapter 1.1

; Exercise 1.5
; What is returned by the following expressions?
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

; Exercise 1.2
; Translate given expression into prefix notation
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3
; Define a procedure that takes three numbers as arguments and
; returns the sum of the squares of the two larger numbers
(define (sum-squares x y)
  (+ (* x x) (* y y)))
(define (sum-largest-squares x y z)
  (if (> x y)
      (if (> y z)
          (sum-squares x y)
          (sum-squares x z))
      (if (> z x)
          (sum-squares y z)
          (sum-squares x y))))

; These shoudl all return 13
(sum-largest-squares 1 2 3)
(sum-largest-squares 3 1 2)
(sum-largest-squares 2 3 1)

; Exercise 1.4
; Describe the operation of the following procedure
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; This cleverly avoids the need to change the sign of b before
; summing the operands, by choosing the procedure according to the
; sign of b. If b is positive, a sum is performed. If b is
; negative, a difference is performed. Since b is the second
; operand, this has the effect of summing the absolute value.

; Exercise 1.5
; What would the difference be if the below were applied in
; normal order? (lazy evaluation)
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;(test 0 (p))

; This gets stuck in an infinite loop in
; applicative evaluation, but will return a 0 in normal-order
; evaluation. The reason is that (p) has a circular definition –
; when you call (p), it returns (p), which returns (p), which
; returns (p) ... - In normal-order evaluation, it would never
; be evaluated, because the argument y would never be used in
; the procedure. We can simulate this in Racket by using 'delay':

(test 0 (delay (p))) ; This executes fine

; Exercise 1.6
; How does (new-if) affect the square root program?
(define (new-if predicate consequent alternative)
  (cond (predicate consequent)
        (else alternative)))

; Here is the new procedure:

(define (square x)
  (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

; When executed, it enters an infinite loop. (new-if) is a
; *function*, not a special form like if or cond. Therefore
; every time it is called, its arguments are evaluated. In this
; implementation of sqrt-iter, (sqrt-iter (improve guess x) x)) is
; evaluated every time, even if the guess passes good-enough?
; Thus the guess is endlessly improved until the computer runs
; out of memory.
; This looks forward to Chapter 5 - it explains why special forms
; like if and cond need a different evaluation rule to functions,
; even though they are expressions (unlike the if and cond
; statements common in other languages)

; Exercise 1.8
; Implement a cube-root procedure based on the provided formula
(define (cube x)
  (* x x x))
(define (cube-root x)
  (define (cube-good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve-cube-root guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (cube-iter guess x)
    (if (cube-good-enough? guess x)
        guess
        (cube-iter (improve-cube-root guess x) x)))
  (cube-iter 1.0 x))

(cube-root 45)

