#lang htdp/isl+

; A Shape is a function:
; [Posn -> Boolean]
; interpretation: if s is a shape and p a Posn, (s p)
; produces #true if p is in s, #false otherwise

; Shape Posn -> Boolean
(check-expect (inside? (mk-point 3 4) (make-posn 3 4)) #true)
(check-expect (inside? (mk-point 3 5) (make-posn 3 4)) #false)
(define (inside? s p)
  (s p))

; Number Number -> Shape
; represents a point at (x, y)
(define (mk-point x y)
  (lambda (p)
    (and (= (posn-x p) x) (= (posn-y p) y))))

(define a-simple-shape (mk-point 3 4))

; Number Number Posn -> Number
; computes distance b/w (x, y) and Posn p
(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p)))
           (sqr (- y (posn-y p))))))

; Number Number Number -> Shape
; creates a representation for a circle of radius r
;   located at (center-x, center-y)
(check-expect
 (inside? (mk-circle 3 4 5) (make-posn 0 0)) #true)
(check-expect
 (inside? (mk-circle 3 4 5) (make-posn 0 9)) #false)
(check-expect
 (inside? (mk-circle 3 4 5) (make-posn -1 3)) #true)
(define (mk-circle center-x center-y r)
  ; [Posn -> Boolean]
  (lambda (p)
    (<= (distance-between center-x center-y p) r)))

; Rect
; Number Number Number Number -> Shape
; represents a width by height rectangle whose
; upper-left corner is located at (ul-x, ul-y)
(check-expect (inside? (mk-rect 10 3 0 0)
                       (make-posn 0 0)) #true)
(check-expect (inside? (mk-rect 10 3 2 3)
                       (make-posn 4 5)) #true)
(check-expect (inside? (mk-rect 10 3 2 3)
                       (make-posn 13 5)) #false)
(define (mk-rect width height ul-x ul-y)
  (lambda (p)
    (and (<= ul-x (posn-x p) (+ ul-x width))
         (<= ul-y (posn-y p) (+ ul-y height)))))

; Shape Shape -> Shape
; combines two shapes into one
(check-expect (inside? union1 (make-posn 0 0)) #true)
(check-expect (inside? union1 (make-posn 0 9)) #false)
(check-expect (inside? union1 (make-posn -1 3)) #true)
(define (mk-combination s1 s2)
  ; Posn -> Boolean
  (lambda (p) (or (inside? s1 p)
                  (inside? s2 p))))

(define circle1 (mk-circle 3 4 5))
(define rectangle1 (mk-rect 0 3 10 3))
(define union1 (mk-combination circle1 rectangle1))
