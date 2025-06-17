#lang htdp/isl+

; [List-of Number] Number -> [List-of Number]
(define (smallers l n)
  (cond
    [(empty? l) '()]
    [else (if (<= (first l) n)
              (cons (first l) (smallers (rest l) n))
              (smallers (rest l) n))]))

;; (quick-sort< '(1 9 2 18 12 14 4 1)) ;; INFINITE LOOP

; When we use '<=' in smallers function, it will include the pivot and
; keeping it place(first) unchanged. This will perform recursion without
; changing(shortening) the data on each call, leads to infinte looping.
