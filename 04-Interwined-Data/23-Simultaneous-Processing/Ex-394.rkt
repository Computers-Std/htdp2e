#lang htdp/isl+

;; Exercise 394. Design merge. The function consumes two lists of
;; numbers, sorted in ascending order. It produces a single sorted
;; list of numbers that contains all the numbers on both inputs lists.
;; A number occurs in the output as many times as it occurs on the two
;; input lists together.

(define ls1 '(2 3 4 6 7))
(define ls2 '(3 5 7 9 11))

; [List-of Number] [List-of Number] -> [List-of Number]
(check-expect (merge ls1 ls2) '(2 3 3 4 5 6 7 7 9 11))
(check-expect (merge '() '(2 9)) '(2 9))
(check-expect (merge '(1 4 9) '(4 4 6)) '(1 4 4 4 6 9))
(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else
     (local ((define f1 (first l1))
             (define f2 (first l2)))
       (if (< f1 f2)
           (cons f1 (merge (rest l1) l2))
           (cons f2 (merge l1 (rest l2)))))]))

; NOTE: I am proud (came up myself) of this solution
