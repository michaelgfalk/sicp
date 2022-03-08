#lang sicp
(#%require sicp-pict)

; Exercise 2.44

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


; Define the procedure up-split used by corner-split.

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split einstein 1))

; Exercise 2.45

; Define a general 'split' operation that can be used to more concisely
; express right-split and up-split

(define (split arranger splitter)
  (define (new-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (new-split painter (- n 1))))
          (arranger painter (splitter smaller smaller)))))
  new-split)

(define right-split-abstract (split beside below))
(define up-split-abstract (split below beside))

(display "New right-split-abstract procedure:\n")
(paint (right-split-abstract einstein 2))
(display "Original right-split procedure:\n")
(paint (right-split einstein 2))

; Exercise 2.46

; Implement the constructors and selectors for vector arithmetic

;(define (make-vect x y)
;  (list x y))
;
;(define (vector-xcor v)
;  (car v))
;
;(define (ycor-vect v)
;  (cadr v))
;
;(define (vector-add v1 v2)
;  (make-vect (+ (vector-xcor v1) (vector-xcor v2))
;             (+ (vector-ycor v1) (vector-ycor v2))))
;
;(define (vector-sub v1 v2)
;  (make-vect (- (vector-xcor v1) (vector-xcor v2))
;             (- (vector-ycor v1) (vector-ycor v2))))
;
;(define (vector-scale v s)
;  (make-vect (* (vector-xcor v) s)
;             (* (vector-ycor v) s)))

; Exercise 2.47

; Write the appropriate selectors for these two implementations of frames

;(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
;(define (make-frame-2 origin edge1 edge2) (cons origin (cons edge1 edge2)))
;
;(define (origin frame)
;  (car frame))
;
;(define (edge1 frame)
;  (cadr frame))
;
;(define (edge2)
;  (caddr frame))
;
;(define (edge2-2 frame) ; Only difference required for the second rep of frame
;  (cddr frame))

; Exercise 2.48

; Define constructors and selctors for line segments, based on the vector datatype above

;(define (make-segment v1 v2)
;  (list v1 v2))
;
;(define (start-segment s)
;  (car s))
;
;(define (end-segment s)
;  (cadr s))

; Exercise 2.49

; Define the following four procedures using segments->painter (provided as primitive in
; sicp-pict)

; a. the painter that draws the outline of the frame

(define outline
  (let ((segment-list (list (make-segment (make-vect 0.0 0.0)
                                          (make-vect 0.0 1.0))
                            (make-segment (make-vect 0.0 1.0)
                                          (make-vect 1.0 1.0))
                            (make-segment (make-vect 1.0 1.0)
                                          (make-vect 1.0 0.0))
                            (make-segment (make-vect 1.0 0.0)
                                          (make-vect 0.0 0.0)))))
    (segments->painter segment-list)))

(paint (superpose white outline)) ; using dark mode

; b. the painter that draws an 'X' by connecting opposite
;    corners of the frame

(define x-painter
  (let ((segment-list (list (make-segment (make-vect 0.0 0.0)
                                          (make-vect 1.0 1.0))
                            (make-segment (make-vect 1.0 0.0)
                                          (make-vect 0.0 1.0)))))
    (segments->painter segment-list)))

(paint (superpose white x-painter)) ; using dark mode

; c. the painter that draws and diamond shape by connecting the midpoints
;    of the sides of the frame

(define diamond
  (let ((segment-list (list (make-segment (make-vect 0.5 0.0)
                                          (make-vect 1.0 0.5))
                            (make-segment (make-vect 1.0 0.5)
                                          (make-vect 0.5 1.0))
                            (make-segment (make-vect 0.5 1.0)
                                          (make-vect 0.0 0.5))
                            (make-segment (make-vect 0.0 0.5)
                                          (make-vect 0.5 0.0)))))
    (segments->painter segment-list)))

(paint (superpose white diamond)) ; using dark mode

; d. the wave painter

(define wave
  (let ((segment-list (list (make-segment (make-vect 0.2 0.0)
                                          (make-vect 0.4 0.7))
                            (make-segment (make-vect 0.4 0.7)
                                          (make-vect 0.2 0.6))
                            (make-segment (make-vect 0.2 0.6)
                                          (make-vect 0.0 0.7))
                            (make-segment (make-vect 0.0 0.8)
                                          (make-vect 0.2 0.7))
                            (make-segment (make-vect 0.2 0.7)
                                          (make-vect 0.4 0.8))
                            (make-segment (make-vect 0.4 0.8)
                                          (make-vect 0.35 0.9))
                            (make-segment (make-vect 0.35 0.9)
                                          (make-vect 0.4 1.0))
                            (make-segment (make-vect 0.6 1.0)
                                          (make-vect 0.65 0.9))
                            (make-segment (make-vect 0.65 0.9)
                                          (make-vect 0.6 0.8))
                            (make-segment (make-vect 0.6 0.8)
                                          (make-vect 0.8 0.7))
                            (make-segment (make-vect 0.8 0.7)
                                          (make-vect 1.0 0.4))
                            (make-segment (make-vect 1.0 0.3)
                                          (make-vect 0.6 0.7))
                            (make-segment (make-vect 0.6 0.7)
                                          (make-vect 0.8 0.0))
                            (make-segment (make-vect 0.7 0.0)
                                          (make-vect 0.6 0.3))
                            (make-segment (make-vect 0.6 0.3)
                                          (make-vect 0.5 0.4))
                            (make-segment (make-vect 0.5 0.4)
                                          (make-vect 0.4 0.3))
                            (make-segment (make-vect 0.4 0.3)
                                          (make-vect 0.3 0.0)))))
        (segments->painter segment-list)))

(paint (superpose white wave))

; Exercise 2.50

; Define the transformation flip-horiz, rotate-counter-180 and rotate-counter-270
; transform-painter is provided as a primitive in sicp-pict in Racket

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(paint (flip-horiz einstein))

(define (rotate-counter-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(paint (rotate-counter-180 einstein))

(define (rotate-counter-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(paint (rotate-counter-270 einstein))

; Exercise 2.51

; Define the below operation for painters. Give two definitions - one similar
; to the beside operation in the textbook, another using beside with rotation

; NB: (below p1 p2) should put p1 on the bottom

(define (below-new p1 p2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter
            p1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
          (paint-top
           (transform-painter
            p2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(paint (below-new (rotate-counter-180 einstein)
                  einstein))

(define (below-rotate p1 p2)
  (let ((rotate-counter-90
         (lambda (painter)
           (transform-painter
            painter
            (make-vect 1.0 0.0)
            (make-vect 1.0 1.0)
            (make-vect 0.0 0.0)))))
    (rotate-counter-180
     (rotate-counter-270
      (beside (rotate-counter-270 p1)
              (rotate-counter-270 p2))))))

(paint (below-rotate (superpose white wave)
                     einstein))

; Exercise 2.52

; Modify the square-limit program at different levels of abstraction.

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (superpose white (square-limit wave 2)))

; a. Low level: add some segments to wave

(define new-wave
  (let ((segment-list (list (make-segment (make-vect 0.2 0.0)
                                          (make-vect 0.4 0.7))
                            (make-segment (make-vect 0.4 0.7)
                                          (make-vect 0.2 0.6))
                            (make-segment (make-vect 0.2 0.6)
                                          (make-vect 0.0 0.7))
                            (make-segment (make-vect 0.0 0.8)
                                          (make-vect 0.2 0.7))
                            (make-segment (make-vect 0.2 0.7)
                                          (make-vect 0.4 0.8))
                            (make-segment (make-vect 0.4 0.8)
                                          (make-vect 0.35 0.9))
                            (make-segment (make-vect 0.35 0.9)
                                          (make-vect 0.4 1.0))
                            (make-segment (make-vect 0.6 1.0)
                                          (make-vect 0.65 0.9))
                            (make-segment (make-vect 0.65 0.9)
                                          (make-vect 0.6 0.8))
                            (make-segment (make-vect 0.6 0.8)
                                          (make-vect 0.8 0.7))
                            (make-segment (make-vect 0.8 0.7)
                                          (make-vect 1.0 0.4))
                            (make-segment (make-vect 1.0 0.3)
                                          (make-vect 0.6 0.7))
                            (make-segment (make-vect 0.6 0.7)
                                          (make-vect 0.8 0.0))
                            (make-segment (make-vect 0.7 0.0)
                                          (make-vect 0.6 0.3))
                            (make-segment (make-vect 0.6 0.3)
                                          (make-vect 0.5 0.4))
                            (make-segment (make-vect 0.5 0.4)
                                          (make-vect 0.4 0.3))
                            (make-segment (make-vect 0.4 0.3)
                                          (make-vect 0.3 0.0))
                            (make-segment (make-vect 0.43 0.87)  ; three new segments to add a smile
                                          (make-vect 0.46 0.84))
                            (make-segment (make-vect 0.46 0.84)
                                          (make-vect 0.53 0.84))
                            (make-segment (make-vect 0.53 0.84)
                                          (make-vect 0.56 0.87))
                                          )))
        (segments->painter segment-list)))

(paint (superpose white (square-limit new-wave 2)))

; b. Change the pattern constructed by corner-split

(define (new-corner-split painter n)
  (if (= n 0)
      painter
      (let ((row (right-split painter (- n 1))))
        (below (flip-vert row)
               row))))

(paint (superpose white (new-corner-split new-wave 2)))

(define (new-square-limit painter n)
  (let ((quarter (new-corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (superpose white (new-square-limit new-wave 3)))

; c. Use the square-of-four procedure to redefine square limit in a new
; way

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (rotate-counter-90 painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0)
   (make-vect 1.0 1.0)
   (make-vect 0.0 0.0)))

(define (weird-square-limit painter n)
  (let ((combine4 (square-of-four rotate-counter-90
                                  rotate-counter-270
                                  (lambda (x) (rotate-counter-90 (flip-vert x)))
                                  (lambda (x) (rotate-counter-270 (flip-vert x))))))
    (combine4 (corner-split painter n))))

(paint (superpose white (weird-square-limit new-wave 2)))