#lang htdp/isl+

;; Exercise 295. Develop n-inside-playground?, a specification of the
;; random-posns function below. The function generates a predicate
;; that ensures that the length of the given list is some given count
;; and that all Posns in this list are within a WIDTH by HEIGHT
;; rectangle:

; distances in terms of pixels
(define WIDTH 300)
(define HEIGHT 300)

; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3) (n-inside-playground? 3))
(define (random-posns n)
  (build-list n (lambda (i) (make-posn (random WIDTH) (random HEIGHT)))))

; N -> [[List-of Posn] -> Boolean]
(define (n-inside-playground? n)
  (lambda (lop)
    (local (; Posn -> Boolean
            (define (inside-rect? p)
              (and (and (> (posn-x p) 0) (> HEIGHT (posn-x p)))
                   (and (> (posn-y p) 0) (> WIDTH (posn-y p))))))
      (cond
        [(and (eq? n (length lop))              ; same length as list
              (andmap inside-rect? lop)) #true] ; all posns are within rect
        [else #false]))))

; N -> [List-of Posn]
; Not Satisfies the Random Posn rule, but passes the Specification
; Test function
(check-satisfied (random-posns/bad 3) (n-inside-playground? 3))
(define (random-posns/bad n)
  (build-list n (lambda (i) (make-posn (- (- WIDTH 1) i) (- HEIGHT 1)))))
