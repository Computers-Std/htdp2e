#lang htdp/isl+

; Any -> Boolean
(define (atom? n)
  (or (number? n) (string? n) (symbol? n)))
