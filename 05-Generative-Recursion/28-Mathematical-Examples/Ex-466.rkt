#lang htdp/isl+

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length:
;  n+1, n, n-1, .. , 2.
; interpretation represents a triangular matrix

(define M ; an SOE
  (list (list 2 2  3 10) ; an Equation
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define TM (list (list 2 2 3 10)
                 (list   3 9 21)
                 (list     1  2)))

; Equation Equation -> Equation
(define (subtract e1 e2)
  (cond
    [(not (= (length e1) (length e2))) (error "Err Length")]
    [else (local ((define multiple (/ (first e1) (first e2))))
            (if (integer? multiple)
                (map (lambda (x y) (- x (* multiple y)))
                     (rest e1) (rest e2))
                (error "Err Remainder")))]))

; SOE -> TM
; Triangulates the given system of equations.
(check-expect (triangulate (list (list 2 2 3 10)))
              (list (list 2 2 3 10)))
(check-expect (triangulate (list (list 2 2 3 10)
                                 (list 2 5 12 31)))
              (list (list 2 2 3 10)
                    (list   3 9 21)))
(check-expect (triangulate M) TM)
(define (triangulate soe)
  (local ((define row1 (first soe)))
    (cond
      [(empty? (rest soe)) (list row1)]
      [else (cons row1
                  (triangulate (map (lambda (e) (subtract e row1))
                                    (rest soe))))])))
; SOE -> TM
(define (triangulateF soe)
  (local ((define row1 (first soe)))
    (cond
      [(empty? (rest soe)) (list row1)]
      [else (cons row1
                  (triangulateF
                   (foldr (lambda (e le)
                            (cons (subtract e row1) le)) '() (rest soe))))])))
