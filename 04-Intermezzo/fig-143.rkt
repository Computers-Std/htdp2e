#lang htdp/isl+

(define-struct inex [mantissa sign exponent])
; An Inex is a structure:
;  (make-inex N99 S N99)
; An S is one of:
; - 1
; - -1
; An N99 is an N between 0 and 99 (inclusive)

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))

; Inex -> Number
; converts an Inex into its numeric equivalent
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt 10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))
