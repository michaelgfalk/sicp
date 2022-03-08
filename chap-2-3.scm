#lang sicp

; Exercise 2.53

; What will the interpreter print from each of the following?

(list 'a 'b 'c) ; => (a b c) ✓
(list (list 'george)) ; => ((george)) ✓
(cdr '((x1 x2) (y1 y2))) ; => ((y1 y2)) ✓
(cadr '((x1 x2) (y1 y2))) ; => (y1 y2) ✓
(pair? (car '(a short list))) ; => #t <- Watch out for (cars), Michael!
(memq 'red '((red shoes) (blue socks))) ; => #f ✓
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks) ✓

; Exercise 2.54

; Implement equal? as a procedure. It compares two lists, using the
; primite eq? to check whether each corresponding symbol is identical

(define (equal? list-1 list-2)
  (cond ((and (eq? list-1 '()) (eq? list-2 '()))
         #t)
        ((not (eq? (car list-1) (car list-2))) #f)
        (else (equal? (cdr list-1) (cdr list-2)))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

; Exercise 2.55

; Why does the interpreter print `quote` when Eva Lu Ator types the following
; expression?
(car ''abracadabra)

; There are two things going on here, the double quotation and the car.
; Eva's expression is exactly equivalent to
;     (car (quote (quote abracadabra)))
; Now, the expression (quote (quote abracadabra)) is a list, and the car
; of this list is quite obviously the symbol 'quote!

; Symbolic differentiation example

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2)) (+ a1 a2))
;        (else (list '+ a1 a2))))

;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m1 1) m2)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2)) (* m1 m2))
;        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

; Exercise 2.56

; Extend the basic differentiator the handle more kinds of expressions.
; Try d(u^n)/dx = nu^(n-1)*du/dx

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

; Exercise 2.57

; Extend the differentiation program so that it can handle sums
; and products of arbitrary numbers of (two or more) terms.

(define (make-sum a1 . rest)
  (cond ((null? (car rest)) a1)
        ((=number? a1 0) (make-sum (car rest) (cdr rest)))
        ((and (number? a1) (number? (car rest))) (make-sum (+ a1 (car rest)) (cdr rest)))
        (else (list '+ a1 (make-sum (car rest) (cdr rest))))))

(define (make-product m1 . rest)
  (cond ((null? (car rest)) m1)
        ((or (=number? m1 0) (=number? (car rest) 0)) 0)
        ((=number? m1 1) (make-product (car rest) (cdr rest)))
        ((and (number? m1) (number? (car rest))) (make-product (* m1 (car rest)) (cdr rest)))
        (else (list '* m1 (make-product (car rest) (cdr rest))))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (let ((n (exponent exp))
               (u (base exp)))
           (make-product n
                         (make-product (make-exponentiation u (make-sum n -1))
                                       (deriv u var)))))
        (else
         (error "unknown expression type: DERIV" exp))))

(display "Test exercise 2.56: ")
(deriv '(* (** x y) (** x 4)) 'x)
(display "Test exercise 2.57: ")
(deriv '(* x y (+ x 3)) 'x)

; Exercise 2.58

; a. Implement infix notation, assuming expressions are fully parenthised.
