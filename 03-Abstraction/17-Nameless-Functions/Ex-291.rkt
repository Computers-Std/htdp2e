#lang htdp/isl+

;; Exercise 291. The fold functions are so powerful that you can
;; define almost any list-processing functions with them. Use fold to
;; define map-via-fold, which simulates map.

; [X -> X] [List-of X] -> [List-of X]
(check-expect (map-via-fold add1 (list 1 2 3)) (map add1 (list 1 2 3)))
(define (map-via-fold fun lox)
  (foldr (lambda (m n) (cons (fun m) n)) '() lox))
