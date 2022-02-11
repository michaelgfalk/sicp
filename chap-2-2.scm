#lang sicp

; Exercise 2.17

; Define a procedure last-pair that returns the final element of a non-empty list
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

(last-pair (list 23 72 149 34))

; Exercise 2.18

; Define a procedure reverse that returns a list of the same elements in reverse order

(define (reverse l)
  (define (rev-iter l-out l-in)
    (if (null? l-in)
        l-out
        (rev-iter (cons (car l-in) l-out) (cdr l-in))))
  (rev-iter '() l))

(reverse (list 1 4 9 16 25))

; Exercise 2.19

; Implement the helper procedures first-denomination, except-first-denomination and
; no-more? for the below count-change procedure, rewritten from Chap 1

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc 100 us-coins) ; => 292

; Does the order of the coin list affect the answer? Why or why not?
; I think it doesn't, but let's see:
(cc 100 (reverse us-coins))

; The reason that it doesn't matter is that the algorithm is a recursive
; tree-walk that considers every possible combination of coins. It will
; consider all combinations including the half doller whether the half-dollar
; is the first, seventh, or tenth item in the list.

; Exercise 2.20

; Use dotted-tail notation to define a procedure same-parity, which will
; return a list of all operands that have the same even-odd
; parity as the first operand

(define (same-parity . l)
  (define (same-parity-iter out-l in-l same-parity?)
    (cond ((null? in-l) out-l)
          ((same-parity? (car in-l)) (same-parity-iter (cons (car in-l) out-l)
                                                       (cdr in-l)
                                                       same-parity?))
          (else (same-parity-iter out-l (cdr in-l) same-parity?))))
  (let ((same-parity? (lambda (x) (= (remainder (car l) 2)
                              (remainder x 2)))))
    (reverse (same-parity-iter '() l same-parity?))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

; Exercise 2.21

; Complete the two implementations of square-list below

;(define (square-list items)
;  (if (null? items)
;      nil
;      (cons (expt (car items) 2) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (expt x 2)) items))

(square-list '(7 8 12 49))

; Exercise 2.22

; Why do Louis Reasoner's implementations of square-list return
; the answers in reverse order?

; The problem with his first implementation is that he cons's
; the new result onto the answer list each time, which puts the
; newest result at the beginning of the list.

; The problem with his second implementation is that he is creating
; a nested list of lists. He want 'cons' to splice two lists together,
; but this is not what it does. If you pass a list as the first argument
; to cons, then the car of the new list will be the *entire list* that was
; passed as the first argument. This is due to the 'closure' property
; of lists in Scheme. Thus his second implementation produces a nested
; list of lists, like so '('('(item_0^2) item_1^2) item_2^2) etc.

(define (for-each proc l)
  (cond ((null? l) #t)
        (else (proc (car l))
              (for-each proc (cdr l)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))