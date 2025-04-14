#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-267) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 267. Use map to define the function convert-euro, which
;; converts a list of US$ amounts into a list of € amounts based on an
;; exchange rate of US$1.06 per € (on April 13, 2017).

;; Also use map to define convertFC, which converts a list of
;; Fahrenheit measurements to a list of Celsius measurements.

;; Finally, try your hand at translate, a function that translates a
;; list of Posns into a list of lists of pairs of numbers.

; Constants
(define EXRATE 1.06)

; [List-of Number] -> [List-of Number]
; converts list-of US to EURO currency
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (list 1.06 21.2 106)) (list 1 20 100))
(define (convert-euro lou)
  (local (; Number -> Number
          (define (usd-euro usd)
            (/ usd EXRATE)))
    (map usd-euro lou)))

; [List-of Number] -> [List-of Number]
; converts list-of Fahrenhiet to Celsius temperatures
;; (check-expect (convertFC (list 23 -34)))
(check-within (convertFC (list -40 0 80 100)) (list -40 -17.7 26.6 37.7) 0.1)
(define (convertFC lof)
  (local (; Number -> Number
          (define (f->c num)
            (* 5/9 (- num 32))))
    (map f->c lof)))

; [List-of Posn] -> [List-of [List-of Number]]
; converst list-of Posn to list-of Pair-of-Numbers
(check-expect (translate (list (make-posn 20 3) (make-posn 28 2)))
              (list (list 20 3) (list 28 2)))
(define (translate lop)
  (local (; Posn -> [List-of Number]
          (define (make-pair pos)
            (list (posn-x pos) (posn-y pos))))
    (map make-pair lop)))
