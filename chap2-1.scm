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

; Define the constructors and selectors for a Cartesian geomtry system

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




  
  