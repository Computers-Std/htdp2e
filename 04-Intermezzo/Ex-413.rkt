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
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1))) (make-inex m s e)]
    [else (error "bad values given")]))

; Inex -> Number
; converts an Inex into its numeric equivalent
(define (inex->number an-inex)
  (* (inex-mantissa an-inex) (expt 10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))
(define ERROR-EXPONENTS "Inequal Exponents")
(define ERROR-RANGE "Wrong Ranges")

; Inex -> Inex
(check-expect (inex* (create-inex 1 1 0) (create-inex 1 1 0)) (make-inex 1 1 0))
(check-expect (inex* (create-inex 3 1 4) (create-inex 50 -1 2)) (make-inex 15 1 3))
(check-expect (inex* (create-inex 3 1 4) (create-inex 50 1 2)) (make-inex 15 1 7))

(define (inex* i1 i2)
  (local ((define mantissa-product (* (inex-mantissa i1) (inex-mantissa i2)))
          (define exponent-product
            (+ (* (inex-exponent i1) (inex-sign i1))
               (* (inex-exponent i2) (inex-sign i2))))
          (define sign-product (if (negative? exponent-product) -1 1))
          (define exponent-in-limit? (<= 0 exponent-product 99))
          (define (closest m)
            (local ((define new-mantissa (round (/ m 10)))
                    (define new-exponent (add1 exponent-product)))
              (make-inex new-mantissa sign-product new-exponent))))
    (if exponent-in-limit?
        (if (> mantissa-product 99)
            (closest mantissa-product)
            (make-inex mantissa-product sign-product exponent-product))
        (error ERROR-RANGE))))
