#lang htdp/isl+

;; Exercise 285. Use map to define the function convert-euro, which
;; converts a list of US$ amounts into a list of € amounts based on an
;; exchange rate of US$1.06 per €.

;; Also use map to define convertFC, which converts a list of Fahrenheit
;; measurements to a list of Celsius measurements.

;; Finally, try your hand at translate, a function that translates a list
;; of Posns into a list of lists of pairs of numbers.

; [List-of Number] -> [List-of Number]
(define (convert-euro loUSD)
  (local ((define rate-per-euro 1.06)) (map (lambda (u) (* u rate-per-euro)) loUSD)))

(check-within (convertFC (list -40 0 80 100)) (list -40 -17.7 26.6 37.7) 0.1)
(define (convertFC lof)
  (map (lambda (f) (* 5/9 (- f 32))) lof))

; [List-of Posn] -> [List-of Number]
(check-expect (translate (list (make-posn 1 2) (make-posn 2 4))) (list (list 1 2) (list 2 4)))
(define (translate lop)
  (map (lambda (pos) (list (posn-x pos) (posn-y pos))) lop))
