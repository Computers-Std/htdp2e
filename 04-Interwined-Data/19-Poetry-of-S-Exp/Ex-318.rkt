#lang htdp/isl+

; S-expr -> Number
; determines the depth of S-expr
(check-expect (depth '()) 0)
(check-expect (depth 'hello) 1)
(check-expect (depth '(hello (world is yours))) 4)
(define (depth sexp)
  (local ((define (sl-depth sl)
            (cond
              [(empty? sl) 0]
              [else (+ (depth (first sl))
                       (sl-depth (rest sl)))]))
          (define (atom? n)
            (or (number? n) (string? n) (symbol? n))))
    (cond
      [(atom? sexp) 1]
      [else (sl-depth sexp)])))
