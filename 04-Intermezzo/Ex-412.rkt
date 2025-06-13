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

; Inex Inex -> Inex
(check-error (inex+ (create-inex 1 1 0) (create-inex 1 -1 1)) ERROR-EXPONENTS)
(check-error (inex+ (create-inex 1 1 0) (create-inex 1 -1 0)) ERROR-EXPONENTS)
(check-expect (inex+ (create-inex 1 1 0) (create-inex 1 1 0)) (make-inex 2 1 0))
(define (inex+ i1 i2)
  (local ((define exponents=?
            (and (= (inex-sign i1) (inex-sign i2))
                 (= (inex-exponent i1) (inex-exponent i2))))
          (define mantissa-sum (+ (inex-mantissa i1) (inex-mantissa i2)))
          (define i-sign (inex-sign i1))
          (define i-exponent (inex-exponent i1))
          (define (closest m)
            (local ((define new-mantissa (round (/ m 10)))
                    (define negative-sign? (= -1 i-sign)))
              (cond
                [negative-sign?
                 (local ((define new-exp (- i-exponent 1)))
                   (if (< new-exp 0)
                       (error ERROR-RANGE)
                       (make-inex new-mantissa i-sign new-exp)))]
                [else
                 (local ((define new-exp (+ i-exponent 1)))
                   (if (> new-exp 99)
                       (error ERROR-RANGE)
                       (make-inex new-mantissa i-sign new-exp)))]))))
    (if exponents=?
        (if (> mantissa-sum 99)
            (closest mantissa-sum)
            (make-inex mantissa-sum i-sign i-exponent))
        (error ERROR-EXPONENTS))))

; Inex Inex -> Inex
(check-expect (inex2+ (create-inex 1 1 0) (create-inex 1 -1 1)) (create-inex 11 -1 1))
(define (inex2+ i1 i2)
  (local ((define good-exponents?
            (or (= (inex-exponent i1) (inex-exponent i2))
                (= 1 (abs (- (inex-exponent i1) (inex-exponent i2))))))
          ; Number -> Inex
          (define (number->inex n)
            (if (or (> n (inex->number MAX-POSITIVE)) (< n (inex->number MIN-POSITIVE)))
                (error ERROR-RANGE)
                (cond
                  [(or (integer? n) (> (round n) 9)) (convert (round n) 1 0)]
                  [(< n 1) (convert n -1 0)]
                  [else (convert (round (* 10 n)) -1 1)])))
          ; Inex -> Inex
          (define (convert m s e)
            (if (not (and (<= 0 e 99) (or (= s 1) (= s -1))))
                (error ERROR-RANGE)
                (cond
                  [(and (integer? m) (<= 1 m 99)) (make-inex m s e)]
                  [else
                   (convert (if (= 1 s)
                                (round (/ m 10))
                                (* m 10))
                            s
                            (add1 e))]))))
    (if good-exponents?
        (number->inex (+ (inex->number i1) (inex->number i2)))
        (error ERROR-EXPONENTS))))
