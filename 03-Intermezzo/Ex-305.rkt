#lang htdp/isl+

(require 2htdp/abstraction)

; Constants
(define EXRATE 1.06)

; [List-of Number] -> [List-of Number]
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (list 1.06 21.2 106)) (list 1 20 100))
(define (convert-euro lou)
  (for/list ([u lou])
    ((lambda (i) (/ i EXRATE)) u)))
