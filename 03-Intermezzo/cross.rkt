#lang htdp/isl+

(require 2htdp/abstraction)

; [List-of X] [List-of Y] -> [List-of [List X Y]]
(check-satisfied (my-cross '(a b c) '(1 2))
                 (lambda (c) (= (length c) 6)))
(define (my-cross l1 l2)
  (local ((define (1cross a)
            (map (lambda (b1) (list a b1)) l2)))
    (foldr (lambda (a la) (append (1cross a) la)) '() l1)))

; [List-of X] [List-of Y] -> [List-of [List X Y]]
; generates all pairs of items from l1 and l2
(check-satisfied (cross '(a b c) '(1 2))
                 (lambda (c) (= (length c) 6)))
(define (cross l1 l2)
  (for*/list ([x1 l1][x2 l2])
    (list x1 x2)))
