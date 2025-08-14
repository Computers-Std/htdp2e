#lang htdp/isl+

(define M1 (list (list 2 2 3 10) (list 2 5 12 31) (list 4 1 -2 1)))
(define S1 '(1 1 2))

(define M2 (list (list 2 3 3 8) (list 2 3 -2 3) (list 4 -2 2 4)))
(define S2 '(1 1 1))

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
(define (triangulate soe)
  (local ((define top (first soe)) (define bot (rest soe))
                                   ; List -> List
                                   (define (rotate L)
                                     (append (rest L) (list (first L)))))
    (cond
      [(empty? bot) (list top)]
      [(andmap (lambda (e) (zero? (first e))) soe) (error ERROR-ZEROS)]
      [(zero? (first top)) (triangulate (rotate soe))]
      [else (cons top (triangulate (map (lambda (e) (subtract e top)) bot)))])))

; TM -> Solution
; Solves triangular system of equations.
(define (solve m)
  (local ((define (solve-each e l)
            (local ((define lhs (reverse (rest (reverse e))))
                    (define rhs (first (reverse e)))
                    (define known (foldr (lambda (c v t) (+ (* c v) t)) 0 (rest lhs) l)))
              (cons (/ (- rhs known) (first lhs)) l))))
    (foldr solve-each '() m)))

; SOE -> Solution
(check-expect (gauss M1) S1)
(check-expect (gauss M2) S2)
(define (gauss soe)
  (solve (triangulate soe)))
