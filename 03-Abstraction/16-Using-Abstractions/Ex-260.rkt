#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-260) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; Constants
(define l1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                 12 11 10 9 8 7 6 5 4 3 2 1))

(define l2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                 17 18 19 20 21 22 23 24 25))

; Operator Nelon -> Number
; gives the smallest/largest number from the list
; by given comparison operator
(define (extract R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else (local (
                  (define from-rest (extract R (rest l))))
            (if (R (first l) from-rest)
                (first l)
                from-rest))]))
