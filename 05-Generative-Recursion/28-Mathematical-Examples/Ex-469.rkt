#lang htdp/isl+

; TM -> Solution
; Solves triangular system of equations.
(check-expect (solve (list (list 2 10))) '(5))
(check-expect (solve (list (list 3 3 21)
                           (list 2 10)))
              '(2 5))
(check-expect (solve (list (list -2 1 -2 -10)
                           (list 3 3 21)
                           (list 2 10)))
              '(1 2 5))
(check-expect (solve (list (list 2 3 3 8)
                           (list  -8 -4 -12)
                           (list     -5 -5)))
              '(1 1 1))
(define (solve m)
  (local ((define (solve-each e l)
            (local ((define lhs (reverse (rest (reverse e))))
                    (define rhs (first (reverse e)))
                    (define known
                      (foldr (lambda (c v t) (+ (* c v) t))
                             0 (rest lhs) l)))
              (cons (/ (- rhs known) (first lhs)) l))))
    (foldr solve-each '() m)))

; NOTE: This is not my solution; it went over my head, so I copied it :(
; TODO: Come back when you're confident.
