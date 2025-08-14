#lang htdp/isl+

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length:
;  n+1, n, n-1, .. , 2.
; interpretation represents a triangular matrix

(define M1 (list (list 2 2 3 10) (list 2 5 12 31) (list 4 1 -2 1)))
(define TM1 (list (list 2 2 3 10) (list 3 9 21) (list 1 2)))
(define M2 (list (list 2 3 3 8) (list 2 3 -2 3) (list 4 -2 2 4)))
(define TM2 (list (list 2 3 3 8) (list -8 -4 -12) (list -5 -5)))
(define M3 '((0 2 3 4) (0 2 3 6) (0 0 3 0)))
(define ERROR-ZEROS "Error: All leading coefficients are 0s")

; Equation Equation -> Equation
(define (subtract e1 e2)
  (cond
    [(not (= (length e1) (length e2))) (error "Err Length")]
    [else
     (local ((define multiple (/ (first e1) (first e2))))
       (if (integer? multiple)
           (map (lambda (x y) (- x (* multiple y))) (rest e1) (rest e2))
           (error "Err Remainder")))]))

; SOE -> TM
(check-expect (triangulateR M1) TM1)
(check-expect (triangulateR M2) TM2)
(check-error (triangulateR M3) ERROR-ZEROS)
(define (triangulateR soe)
  (local ((define top (first soe))
          (define bot (rest soe))
          ; List -> List
          (define (rotate L)
            (append (rest L) (list (first L)))))
    (cond
      [(empty? bot) (list top)]
      [(andmap (lambda (e) (zero? (first e))) soe) (error ERROR-ZEROS)]
      [(zero? (first top)) (triangulateR (rotate soe))]
      [else (cons top (triangulateR (map (lambda (e) (subtract e top)) bot)))])))
