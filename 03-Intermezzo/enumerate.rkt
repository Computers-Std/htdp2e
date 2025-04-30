#lang htdp/isl+

(require 2htdp/abstraction)

; [List-of X] -> [List-of [List N X]]
; pairs each item in lx with its relative index
(check-expect (my-enumerate '(a b c)) '((1 a) (2 b) (3 c)))
(define (my-enumerate lx)
  (map list (build-list (length lx) (lambda (i) (add1 i))) lx))

; [List-of X] -> [List-of [List N X]]
; pairs each item in lx with its relative index
(check-expect (enumerate '(a b c)) '((1 a) (2 b) (3 c)))
(define (enumerate lx)
  (for/list ([x lx] [ith (length lx)])
    (list (+ ith 1) x)))
