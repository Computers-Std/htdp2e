#lang htdp/isl+

;; Exercise 436: Formulate a termination argument for food-create from Exercise-432

(define MAX 10)

; Posn -> Posn
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (cond
    [(and (>= 1 MAX) (equal? p (make-posn 0 0)))
     (error "Infinite Loop")]
    [else (local ((define candidate (make-posn (random MAX) (random MAX))))
            (if (equal? candidate p)
                (food-create p) candidate))]))

;; Posn -> Boolean
;; Use for testing only.
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
