#lang htdp/isl+

(require 2htdp/image)

(define BG (empty-scene 50 50))
(define DOT (circle 2 "solid" "red"))
(define (dots lop)
  (foldr (lambda (a-posn scene)
           (place-image DOT
                        (posn-x a-posn)
                        (posn-y a-posn)
                        scene))
         BG lop))

(define lop1 (list (make-posn 20 12) (make-posn 25 25)))

; [List-of Posn] -> [List-of Posn]
(define (keep-good lop)
  (filter (lambda (p) (<= (posn-y p) 20)) lop))
