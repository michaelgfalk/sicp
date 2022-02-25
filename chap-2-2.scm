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

; Exercise 2.23

; Write a procedure for-each with does something each iteration,
; and returns an arbitrary value at the end to show it's finished.

(define (for-each proc l)
  (cond ((null? l) #t)
        (else (proc (car l))
              (for-each proc (cdr l)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

; Exercise 2.24

; My guess for the return value of the following expression:
; (1 (2 (3 4))
(list 1 (list 2 (list 3 4)))

; Box-and-pointer notation is difficult in a text file...
; But here is the tree:
;   /\
;  /  \
; 1   /\
;    /  \
;   2   /\
;      /  \
;     3    4

; Exercise 2.25

; Return 7 from each of the following lists
(cadr (caddr '(1 3 (5 7) 9)))
(caar '((7)))
(cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))

; Exercise 2.26

; Suppose these two lists
(define x (list 1 2 3))
(define y (list 4 5 6))

; What is returned by each of the following expressions?
; '(1 2 3 4 5 6) ; correct!
(append x y)
; '((1 2 3) 4 5 6) ; correct!
(cons x y)
; '((1 2 3) (4 5 6)) ; correct!
(list x y)

; Exercise 2.27

; Write a deep-reverse procedure that reverses the passed list *and* any
; lists within it, to an infinite depth.

(define (deep-reverse l)
  (define (dr-iter out-l in-l)
    (cond ((null? in-l) out-l)
          ((pair? (car in-l)) (dr-iter (cons (dr-iter '() (car in-l)) out-l)
                                       (cdr in-l)))
          (else (dr-iter (cons (car in-l) out-l) (cdr in-l)))))
  (dr-iter '() l))

(deep-reverse '((1 2) (3 4) (5 6 (7 8 9))))
(deep-reverse '(1 2 3))

; Exercise 2.28

; Write a procedured fringe that takes as argument a tree
; and returns a list whose elements are all the leaves of the tree
; arranged in left-to-right order.

(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define ftest (list (list 1 2) (list 3 4)))
(fringe ftest)
(fringe (list ftest (list ftest)))

; Exercise 2.29

; Here are the constructors for a 'mobile' data structure
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. write the selectors for left and right branches, and for branch length and structure

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; b. write a total-weight procedure that returns the weight of all the structures on
; a mobile (NB: structure can be a number representing the weight, or another mobile)

(define (total-weight mobile)
  (define (branch-weight branch)
    (let ((struct (branch-structure branch)))
      (if (not (pair? struct))
          struct
          (+ (branch-weight (left-branch struct))
             (branch-weight (right-branch struct))))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define test-mobile
  (make-mobile (make-branch 7
                            (make-mobile (make-branch 6 2)
                                         (make-branch 4 12)))
               (make-branch 3
                            (make-mobile (make-branch 2 (make-mobile (make-branch 1 7)
                                                                     (make-branch 7 12)))
                                         (make-branch 8 14)))))

(display "Expected value = 47: ")
(total-weight test-mobile)

; c. define a predicate 'balanced?' which measures if a mobile is
; balanced. This has two conditions...

(define (balanced? mobile)
  (define (weigh-if-balanced branch)
    (let ((s (branch-structure branch)))
      (if (not (pair? s))
          s
          (if (balanced? s)
              (total-weight s)
              #f))))
  (define (compare b1 b2)
    (let ((w1 (weigh-if-balanced b1))
          (w2 (weigh-if-balanced b2)))
      (if (and w1 w2)
          (= (* (branch-length b1) w1)
             (* (branch-length b2) w2))
          #f)))
  (compare (left-branch mobile) (right-branch mobile)))

(balanced? test-mobile)
(define simple-balanced-mobile (make-mobile (make-branch 3 12)
                                            (make-branch 9 4)))
(balanced? simple-balanced-mobile)
(define complex-balanced-mobile (make-mobile (make-branch 4 2)
                                             (make-branch 2 (make-mobile (make-branch 1 2)
                                                                         (make-branch 1 2)))))
(balanced? complex-balanced-mobile)

; d. What if the constructors changed to use cons instead of list? How much
; would your other programs need to change?

; The only necessary change would be to the right-branch and branch-structure selectors.
; Both of these would need to have the 'cadr' in the function body replaced with 'cdr'.

; Exercise 2.30

; Define a procedure square-tree that squares all items in a tree, and returns
; a new tree of the same structure

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (expt sub-tree 2)))
       tree))

(square-tree
       (list 1
             (list 2 (list 3 4) 5)
             (list 6 7)))

; Exercise 2.31

; Generalise the square-tree procedure to produce a generic tree-map procedure

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(tree-map (lambda (x) (expt x 2))
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

; Exercise 2.32

; Complete the below subsets function and explain why it works

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (l) (cons (car s) l)) rest)))))

(subsets (list 1 2 3))

; To see why this works, consider just the inner variable 'rest'

; If the input = '(1 2 3)
; rest (first recursion) = (subsets '(2 3))
; rest (second recursion) = (subsets '(3))
; rest (third recursion) = (subsets '())
; This then propagates back up the chain...
; (subsets '()) --> (list nil) --> '(())
; (subsets '(3)) --> (append '(()) (map (lambda (l) (cons (car '(3)) l) '())
; (subsets '(3)) --> '(() (3))
; (subsets '(2 3)) --> (append '(() 3) (map (lambda (l) (cons (car '(2 3)) l) '(() 3))
; --> '(() (3) (2) (2 3))
; The final return then maps along this sequence, consing 1 onto the start of each element,
; and providing the final subsets.

; Exercise 2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Complete the following programs, which re-define some common list operations
; as accumulations.

(define (new-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(new-map (lambda (x) (expt x 2)) '(1 2 3 4 5))

(define (new-append seq1 seq2)
  (accumulate cons seq2 seq1))

(new-append '(1 2 3) '(4 5 6))

(define (new-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(new-length '(1 2 3 4))

; Exercise 2.34

; Write a procedure that evaluates a polynomial using Horner's rule

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

; Exercise 2.35

; Redefine count-leaves using accumulate

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x)
                                   (count-leaves x)
                                   1))
                   t))) ; enumerator

(count-leaves (list 1 (list 2 (list 3 4)) 22))
  
; Exercise 2.36

; Complete the definition of accumulate-n, which should seperately accumulate values
; for each sub-list in a provided list of lists

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; Exercise 2.37

; A matrix can be represented as a list of lists, where each sub-list represents a row
; of the matrix. Given the dot-product procedure below, complete the other procedures.

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 4 5 6) (list 12 10 0))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(matrix-*-matrix (list '(1 2 3) '(4 5 6))
                 (list '(0 1 2 3 4) '(0 1 2 3 4) '(0 1 2 3 4)))

; Exercise 2.38

; Accumulate is also known as 'fold-right' because it accumulates the sequence
; by combining the first element with the accumualtion of all elements to the right.
; Basically it works from right to left. Here is the procedure that works in the
; opposite direction

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(accumulate / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(accumulate list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

; What property does an operation need to satisfy to produce the same result
; using either folding procedure?

; The operation must be commutative – the order of operands mustn't matter (e.g. +, *)

; Exercise 2.39

; Redefine the reverse procedure from ex 2.18 in terms of fold-right (i.e. accumulate)
; and fold-left

(define (right-reverse sequence)
  (accumulate (lambda (x y) (append y (list x))) nil sequence))

(define (left-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(right-reverse (list 10 20 30 40))
(left-reverse (list 10 20 30 40))


; Exercise 2.40

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (square x)
  (* x x))

(define (prime? n)
  (define (smallest-divisor n) (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b) (= (remainder b a) 0))
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; Define a procedure unique-pairs that generates the sequence of pairs (i,j) with
; 1 ≤ j < i ≤ n

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(prime-sum-pairs 20)

; Exercise 2.41

; Write a procedure to find all ordered triples of distinct positive integers i, j and
; k less than or equal to a given integer n that sum to a given integer s

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (pair) (append (list i) pair))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-sequence seq)
  (accumulate + 0 seq))

(define (find-triple-sums s n)
  (filter (lambda (triple) (= s (sum-sequence triple)))
          (unique-triples n)))

(display "All unique triples 1 ≤ k < j < i ≤ 10 that sum to 20:\n")
(find-triple-sums 20 10)

; Exercise 2.42

; Complete this procedure for solving the 'eight queens problem', by defining
; the other required procedures.

(define empty-board nil)

(define (adjoin-position row col positions)
  (cons (list row col) positions))

(define (row position) (car position))
(define (col position) (cadr position))

(define (queens-in-k positions k)
  (filter (lambda (position) (= (col position) k))
          positions))

(define (queens-not-in-k positions k)
  (filter (lambda (position) (not (= (col position) k)))
          positions))

(define (check queen-1 queen-2)
  (let ((ax (row queen-1))
        (ay (col queen-1))
        (bx (row queen-2))
        (by (col queen-2)))
    (cond ((= ax bx) #f)
          ((= (abs (- ax bx))
              (abs (- ay by))) #f)
          (else #t))))

(define (safe? k positions)
  (let ((kth-queen (car positions))
        (other-queens (cdr positions)))
    (define (check-iter unchecked)
      (cond ((null? unchecked) #t)
            ((check kth-queen (car unchecked)) (check-iter (cdr unchecked)))
            (else #f)))
    (check-iter other-queens)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(display "(queens 8) Expected value == 92: ")
(length (queens 8))

; Exercise 2.43

; How much slower does Louis Reasoner's queens program run than the one above, and why?

; He adjoins new positions using this code:
;(flatmap
; (lambda (new-row)
;   (map (lambda (rest-of-queens)
;          (adjoin-position new-row k rest-of-queens))
;        (queen-cols (- k 1))))
; (enumerate-interval 1 board-size))

; In Louis's implementation, queen-cols is called once each recursion for each row – that is,
; queen-cols is called k times per recursion.
; In the original implementation, queen-cols is called only once each recursion.
; If the original implementation takes T time to complete, Louis's implementation will take roughly
; T*k^k to complete, since the function is being called k times per k recursions, rather than once per
; k recursions.