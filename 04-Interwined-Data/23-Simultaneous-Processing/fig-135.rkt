#lang htdp/isl+

; list-pick: [List-of Symbol] N[>= 0] -> Symbol
; determine the nth symbol from alos, counting from 0;
; signals an error if there is no nth symbol
(check-error (list-pick '() 0) "list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list too short")
(define (list-pick alos n)
  (cond
    [(empty? alos) (error "list too short")]
    [(= n 0) (first alos)]
    [(> n 0) (list-pick (rest alos) (sub1 n))]))
