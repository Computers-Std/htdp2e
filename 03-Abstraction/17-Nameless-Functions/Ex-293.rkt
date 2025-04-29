#lang htdp/isl+

;; Exercise 293. Develop found?, a specification for the find function
;; & Use found? to formulate a check-satisfied test for find.

; [X] X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x)
         l
         (find x (rest l)))]))

(check-expect (find "a" '()) #false)
(check-expect (find "a" '("b" "c" "d")) #false)
(check-expect (find "a" '("a")) '("a"))
(check-expect (find "b" '("a" "b" "c")) '("b" "c"))
(check-satisfied (find "a" '()) (found? "a" '()))
(check-satisfied (find "a" '("b" "c" "d")) (found? "a" '("b" "c" "d")))
(check-satisfied (find "a" '("a")) (found? "a" '("a")))
(check-satisfied (find "b" '("a" "b" "c")) (found? "b" '("a" "b" "c")))
; [X] X [List-of X] -> [[List-of X] -> Boolean]
(define (found? x l)
  (lambda (l0)
    (local ((define l-len (length l))
            ; Number [List-of X] -> [List-of X]
            ; drops items until Number in l
            (define (drop-head n l)
              (cond
                [(zero? n) l]
                [else (drop-head (sub1 n) (rest l))])))
      (cond
        [(false? l0) (not (member? x l))]
        [(> (length l0) l-len) #false]
        [else (equal? l0 (drop-head (- l-len (length l0)) l))]))))
