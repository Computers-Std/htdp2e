#lang htdp/isl+

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length:
;  n+1, n, n-1, .. , 2.
; interpretation represents a triangular matrix

(define M (list (list 2 2 3 10) (list 2 5 12 31) (list 4 1 -2 1)))
(define TM (list (list 2 2 3 10) (list 3 9 21) (list 1 2)))
(define M2 (list (list 2 3 3 8) (list 2 3 -2 3) (list 4 -2 2 4)))
(define TM2 (list (list 2 3 3 8) (list -8 -4 -12) (list -5 -5)))
(define S '(1 1 2)) ; a Solution

; Equation Equation -> Equation
(check-expect (subtract '(2 2) '(2 5)) '(3))
(check-expect (subtract '(2 2 3 10) '(2 5 12 31)) '(3 9 21))
(check-expect (subtract '(3 9 21) '(-3 -8 -19)) '(1 2))
(define (subtract e1 e2)
  (cond
    [(not (= (length e1) (length e2))) (error "Err Length")]
    ;; [(zero? (first e2)) (subtract e2 e1)] BUG:
    [else
     (local ((define multiple (/ (first e1) (first e2))))
       (if (integer? multiple)
           (map (lambda (x y) (- x (* multiple y))) (rest e1) (rest e2))
           (error "Err Remainder")))]))

; SOE -> TM
; Triangulates the given system of equations.
(define (triangulate soe)
  (local ((define top (first soe)) (define bot (rest soe)))
    (cond
      [(empty? bot) (list top)]
      [else (cons top (triangulate (map (lambda (e) (subtract e top)) bot)))])))

; List -> List
(define (rotate L)
  (append (rest L) (list (first L))))

; SOE -> TM
(check-expect (triangulateR (list (list 2 2 3 10) (list 2 5 12 31)))
              (list (list 2 2 3 10) (list 3 9 21)))
(check-expect (triangulateR M) TM)
(check-expect (triangulateR M2) TM2)

(define (triangulateR soe)
  (local ((define top (first soe)) (define bot (rest soe)))
    (cond
      [(empty? bot) (list top)]
      [(zero? (first top)) (triangulateR (rotate soe))]
      [else (cons top (triangulateR (map (lambda (e) (subtract e top)) bot)))])))

;; (triangulateR '((2 2 2 6) (2 2 4 8) (2 2 1 2))) ERROR:
