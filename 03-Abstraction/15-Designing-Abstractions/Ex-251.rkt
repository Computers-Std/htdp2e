#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-251) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; [List-of Number] Operation onEnd-> Number
; computes the OPERATION of the numbers on l
(define (map-opr l opr onEnd)
  (cond
    [(empty? l) onEnd]
    [else (opr (first l)
               (map-opr (rest l) opr onEnd))]))

; [List-of Number] -> Number
; computes the sum of numbers on l
(check-expect (map-sum (list 1 2 3 )) 6)
(define (map-sum l)
  (map-opr l + 0))

; [List-of Number] -> Number
; computes the product of numbers on l
(check-expect (map-product (list 1 2 3 )) 6)
(define (map-product l)
  (map-opr l * 1))
