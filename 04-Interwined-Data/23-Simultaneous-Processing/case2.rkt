#lang htdp/isl+

; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on hours and wages/h assume the
; two lists are of equal length
(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list 5.65) (list 40))
              (list 226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))

(define (wages*.v2 hours wages/h)
  (cond
    [(empty? hours) '()]
    [else (cons
           (weekly-wage (first hours) (first wages/h))
           (wages*.v2 (rest hours) (rest wages/h)))]))

; Number Number -> Number
; computes the weekly wage from hours and pay-rate
(define (weekly-wage hours pay-rate)
  (* hours pay-rate))
