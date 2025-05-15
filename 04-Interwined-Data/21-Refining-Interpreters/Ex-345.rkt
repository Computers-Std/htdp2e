#lang htdp/isl+

(define-struct add [left right])
; (make-add BSL-expr BSL-expr)

(define-struct mul [left right])
; (make-mul BSL-expr BSL-expr)

; BSL-expr is one of
; - Number
; - Add
; - Mul
; - BSL-expr

;; (+ 10 -10)
(make-add 10 -10)

;; (+ (* 20 3) 33)
(make-add (make-mul 20 3) 33)

;; (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9)))
(make-add (make-mul 3.14 (make-mul 2 3))
          (make-mul 3.14 (make-mul -1 -9)))

;; (make-add -1 2)
(+ -1 2)
;; (make-add (make-mul -2 -3) 33)
(+ (* -2 -3) 33)
;; (make-mul (make-add 1 (make-mul 2 3)) 3.14)
(* (+ 1 (* 2 3)) 3.14)
