#lang sicp

; Exercise 2.1

; Write a better make-rat procedure that normalises the sign

(define (gcd a b)
  (if (= b 0)
      a
      (gcd a (remainder a b))))

(define (make-rat n d)
  (let ((sign (if (> (* n d) 0)
                  1
                  -1))
        (g (abs (gcd n d))))
    (cons (* sign (abs (/ n g))) (abs (/ d g)))))

(display "(make-rat -9 27): ")
(make-rat -9 27)

; Exercise 2.2

; Define the constructors and selectors for a Cartesian geometry system

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (mean x y)
  (/ (+ x y) 2))

(define (midpoint-segment seg)
  (let ((x1 (x-point (start-segment seg)))
        (x2 (x-point (end-segment seg)))
        (y1 (y-point (start-segment seg)))
        (y2 (y-point (end-segment seg))))
    (make-point (mean x1 x2) (mean y1 y2))))

(display "Midpoint of (1,2) and (7,14): ")
(print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 7 14))))

; Exercise 2.3

; Implement a representation of rectangles, along with perimeter and area functions

;(define (make-rectangle p1 p2 p3 p4)
;  (list p1 p2 p3 p4))
;
;(define (p1 rectangle)
;  (car rectangle))
;
;(define (p2 rectangle)
;  (car (cdr rectangle)))
;
;(define (p3 rectangle)
;  (car (cdr (cdr rectangle))))
;
;(define (p4 rectangle)
;  (car (cdr (cdr (cdr rectangle)))))
;
;(define (height rect)
;  (abs (- (y-point (p1 rect)) (y-point (p4 rect)))))
;
;(define (breadth rect)
;  (abs (- (x-point (p1 rect)) (x-point (p2 rect)))))

(define (perimeter rectangle)
  (+ (* 2 (height rectangle))
     (* 2 (breadth rectangle))))

(define (area rectangle)
  (* (height rectangle)
     (breadth rectangle)))

; Now implement a different representation, for which the area and perimeter
; functions still work

; (If you uncomment these lines, comment the ones above, or the code won't compile)

(define (make-rectangle origin height breadth)
  (list origin height breadth))

(define (height rectangle)
  (car (cdr rectangle)))

(define (breadth rectangle)
  (car (cdr (cdr rectangle))))

(display "Rectangle test:\n")
;(define my-rect (make-rectangle
;                 (make-point 0 5)
;                 (make-point 10 5)
;                 (make-point 10 0)
;                 (make-point 0 0)))
(define my-rect (make-rectangle (make-point 0 0)
                                5
                                10))
(display "Perimeter of my-rect: ")
(perimeter my-rect)

; Exercise 2.4

; Verify that the below definitions of cons and car meet the requirements
; for the representation of a pair. Then write the implementation for car

(define (cons-2 x y)
  (lambda (m) (m x y)))

(define (car-2 z)
  (z (lambda (p q) p)))

; Prove that (car-2 z) returns y if z = (cons-2 x y)

;(car-2 z)
; = (car-2 (cons-2 x y))
; = (car-2 (lambda (m) (m x y)))
; = ((lambda (m) (m x y)) (lambda (p q) p))
; = ((lambda (p q) p) x y)
; = y

(define (cdr-2 z)
  (z (lambda (p q) q)))

(car-2 (cons-2 7 14)) ; Should return 7
(cdr-2 (cons-2 22 561)) ; Should return 561

; Exercise 2.5

; Show that we can represent pairs of nonnegative integers using
; only numbers and arithmetic operations if we represent the pair
; (a, b) as the integer that is the product 2^a * 3^b. Give the
; corresponding definitions of the procedures cons, car, and cdr.

(define (cons-int a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-int c)
  (define (car-iter i c)
    (if (= (remainder c 2) 1)
        i
        (car-iter (inc i) (/ c 2))))
  (car-iter 0 c))

(define (cdr-int c)
  (define (cdr-iter i c)
    (if (not (= (remainder c 3) 0))
        i
        (cdr-iter (inc i) (/ c 3))))
  (cdr-iter 0 c))

(display "Testing integer pairs:\n")
(display "(car-int (cons-int 22 44)): ")
(car-int (cons-int 22 44))
(display "(cdr-int (cons-int 300 2)): ")
(cdr-int (cons-int 300 2))

; Exercise 2.6

; Implement Church numerals directly, without using the provided
; zero and add-1 functions. Define one, two and +

; Deriving definition of Church one (add-1 zero) = one

;(add-1 zero) ; substitute for zero
;(add-1 (lambda (f) (lambda (x) x))) ; sub zero-lambda for n in add-1 body
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) ; apply zero proc
;(lambda (f) (lambda (x) (f ((lambda (x) x) x)))) ; apply add-1 proc
;(lambda (f) (lambda (x) (f x))) ; definition of one

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (+church f1 f2) (lambda (f) (lambda (x) ((f1 f) ((f2 f) x)))))
(define (*church f1 f2) (lambda (f) (lambda (x) ((f1 (f2 f)) x))))

; Exercise 2.7

; Write the selectors to go with Alyssa P. Hacker's interval constructor

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))


; Exercise 2.8

; Write a subtraction function for Alyssa's interval arithmetic program

(define (sub-interval a b)
  (make-interval
   (- (lower-bound a) (lower-bound b))
   (- (upper-bound a) (upper-bound b))))

; Exercise 2.9

; Consider the width of an interval. Show that if you add or subtract intervals,
; then the width of the sum or difference is a function of the widths of the inputs.
; Give examples showing that this is not true for multiplication or division.

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

; Substitution

;(width (sub-interval a b))
;(/ (- (lower-bound (sub-interval a b))
;      (upper-bound (sub-interval a b)))
;   2)
;(/ (- (- (lower-bound a) (lower-bound b)))   ; == (a - b) - (c - d)
;      (- (upper-bound a) (upper-bound b))  ; == a - c - b + d
;   2)                                       ; == (a - c) - (b - d)
;(/ (- (- (lower-bound a) (upper-bound a))
;      (- (lower-bound b) (upper-bound b)))
;   2)
;(/ (- (* -2 (width a))
;      (* -2 (width b)))
;   2)
; ∴ (width (sub-interval a b)) is a function of (width a) and (width b)

; Some examples that show this is not the case for multiplication and subtraction

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define int-1 (make-interval 0 6.0))
(define int-2 (make-interval 0 8.5))
(display "Width of int-1: ")
(width int-1)
(display "Width of int-2: ")
(width int-2)
(display "(width (mul-interval int-1 int-2)): ")
(width (mul-interval int-1 int-2))

(define int-3 (make-interval 1 7.0))
(define int-4 (make-interval 2 10.5))
(display "Width of int-3: ")
(width int-3)
(display "Width of int-4: ")
(width int-4)
(display "(width (mul-interval int-3 int-4)): ")
(width (mul-interval int-3 int-4))

; The same widths of a and b can result in different widths for a * b
; ∴ (width (mul-interval a b)) is not a function of (width a) and (width b)

; It is the same for division – who needs examples?

; Exercise 2.10

; Alyssa's definition of div-interval is not meaningful when either of the intervals
; spans zero. Add a check and throw an error if either interval fails this condition.

(define (no-zero-span? x)
  (if (< (lower-bound x) 0)
      (< (upper-bound x) 0)
      (> (upper-bound x) 0)))

(define (div-interval x y)
  (if (and (no-zero-span? x)
           (no-zero-span? y))  
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))
      (error "Input interval spans zero")))

; Change the below intervals to test (no-zero-span? x)
(div-interval (make-interval -22 -77) (make-interval 1 2))

; Exercise 2.11

; 'By testing the signs of the endpoints of the intervals, it is
; possible to break mul-interval into nine cases, only one
; of which requires more than two multiplications'...

(define (mul-interval-sillywise a b)
  (let ((al-pos (>= (lower-bound a) 0))
        (au-pos (>= (upper-bound a) 0))
        (bl-pos (>= (lower-bound b) 0))
        (bu-pos (>= (upper-bound b) 0)))
    (cond
      ((and al-pos au-pos bl-pos bu-pos) ; 1
       (make-interval (* (lower-bound a) (lower-bound b))
                      (* (upper-bound a) (upper-bound b))))
      ((and al-pos au-pos (not bl-pos) bu-pos) ; 2
       (make-interval (* (upper-bound a) (lower-bound b))
                      (* (upper-bound a) (upper-bound b))))
      ((and (not al-pos) au-pos bl-pos bu-pos) ; 3
       (make-interval (* (lower-bound a) (upper-bound b))
                      (* (upper-bound a) (upper-bound b))))
      ((and (not al-pos) (not au-pos) bl-pos bu-pos) ; 4
       (make-interval (* (lower-bound a) (upper-bound b))
                      (* (upper-bound a) (lower-bound b))))
      ((and al-pos au-pos (not bl-pos) (not bu-pos)) ; 5
       (make-interval (* (upper-bound a) (lower-bound b))
                      (* (lower-bound a) (upper-bound b))))
      ((and (not al-pos) au-pos (not bl-pos) bu-pos) ; 6 -- special case
       (let ((p1 (* (lower-bound a) (lower-bound b)))
             (p2 (* (lower-bound a) (upper-bound b)))
             (p3 (* (upper-bound a) (lower-bound b)))
             (p4 (* (upper-bound a) (upper-bound b))))
         (make-interval (min p1 p2 p3 p4)
                        (max p1 p2 p3 p4))))
      ((and (not al-pos) au-pos (not bl-pos) (not bu-pos)) ; 7
       (make-interval (* (upper-bound a) (lower-bound b))
                      (* (lower-bound a) (lower-bound b))))
      ((and (not al-pos) (not au-pos) (not bl-pos) bu-pos) ; 8
       (make-interval (* (lower-bound a) (upper-bound b))
                      (* (lower-bound a) (lower-bound b))))
      ((and (not al-pos) (not au-pos) (not bl-pos) (not bu-pos)) ; 9
       (make-interval (* (upper-bound a) (upper-bound b))
                      (* (lower-bound a) (lower-bound b)))))))

; Have a play to see how it goes
(mul-interval-sillywise (make-interval -22 -1) (make-interval -6000 -2000))

; Exercise 2.12

; Define a constructor make-center-percent that takes a centrepoint
; and percentage tolerance, and produces the desired interval. Also
; define a percentage selector that retrieves the percentage from the
; interval.

(define (make-center-width c p)
  (let ((w (* c p)))
    (make-interval (- c w) (+ c w))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percentage i)
  (/ (width i) (center i)))

(display "= expected-value 0.15: ")
(percentage (make-center-width 10 0.15))

; Exercise 2.13

; Derive a simple formula for the approximate percentage tolerance
; of the product of two intervals, assuming the tolerance of each is small.
; You may also assume that all numbers are positive.

; The simple formula is (percentage (mul-interval a b)) ≈ (+ (percentage a) (percentage b))

(let ((a (make-center-width 10 0.15))
      (b (make-center-width 200 0.01)))
  (display "(percentage (mul-interval a b)) => ")
  (display (percentage (mul-interval a b)))
  (display "\n(+ (percentage a) (percentage b)) => ")
  (display (+ (percentage a) (percentage b))))

; Let Ca, Ta define an interval a
; = [(Ca - (Ca (/ Ta 2))), (Ca + (* Ca (/ Ta 2))))
; = [(Ca(1 - (Ta / 2))), (Ca(1 + (Ta / 2)))]
; Let b be a similar interval, and Ca, Cb > Ta, Tb > 0
; Thus (mul-interval a b)
; = [(Ca (1 - (Ta / 2)))(Cb (1 - (Tb / 2))),
;    (Ca (1 + (Ta / 2)))(Cb (1 + (Tb / 2)))]
; Consider the upper bound of (mul-interval a b)
; = (CaCb(1 - (Ta / 2) - (Tb / 2) + (TaTb / 4))
; = (CaCb(1 - (Ta + Tb)/2) + CaCbTaTb/4
; By similar simplifcation, the lower bound of (mul-interval a b)
; = (CaCb(1 + (Ta + Tb)/2) + CaCbTaTb/4
; Now, if Ta and Tb are close to 0, then TaTb/4 is a very small number,
; and therefore the tolerance of (mul-interval a b) will be very close
; to (Ta + Tb)

; Exercise 2.14

; Examine why these two algebraically equivalent formulae for calculating the resistance
; of parallel resistors produce different answers in Alyssa's system:

; R_t = R_1R_2/(R_1 + R_2)
; R_t = 1 / ((1 / R_2) + (1 / R_2))

; Also Exercise 2.15 – Can you design a system without this defect?

; No, I can't! But I think I understand the problem (having read a little online as well).
; The issue is that when you transform one equation into another, you are relying on the fact
; that R_1 = R_1 and R_2 = R_2. But these identity relationships do not hold in interval
; arithmetic. This is because both R_1 and R_2 each represent a range of possible values.
; As you see below, if you divide R_1 by R_1, you don't get 1, but rather a new interval
; that represents the range of possible values given by dividing a number in R_1 by another
; number in R_1.

; To solve this problem, it would be necessary to introduce some kind of identity relationship,
; so that the machine can recognise when it is operating on the 'same' interval. Perhaps this
; would be easiet using object-oriented patterns? How should the case be treated when
; R_1 and R_2 have the same value? Intuitively, I would say that even if their centrepoint
; and tolerance were the same, it would still be necessary to treat them as different entities.
; In this case, the identity of different intervals would need to be checked using some kind
; of id number ... This would then need to be tracked when the interval is injected at
; different points of the expression (e.g. in version 1 of the equation above).

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define (center-percent i)
  (cons (center i) (percentage i)))

(let ((r1 (make-center-width 12 0.2))
      (r2 (make-center-width 55 0.5)))
  (newline)
  (newline)
  (display "r1 / r1: ")
  (display (center-percent (div-interval r1 r1)))
  (display "\nr1 / r2: ")
  (display (center-percent (div-interval r1 r2)))
  (display "\nr1 / (r2 * r2): ")
  (display (center-percent (mul-interval r1 (div-interval r2 r2))))
  (display "\n(par1 r1 r2): ")
  (display (center-percent (par1 r1 r2)))
  (display "\n(par2 r1 r2): ")
  (display (center-percent (par2 r1 r2))))