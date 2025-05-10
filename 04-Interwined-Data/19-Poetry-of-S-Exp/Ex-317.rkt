#lang htdp/isl+

; S-expr Symbol -> N
; counts all occurences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

(define (count sexp sy)
  (local ((define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else (+ (count (first sl) sy)
                       (count-sl (rest sl)))]))
          (define (count-atom at)
            (cond
              [(or (number? at) (string? at)) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)]))
          (define (atom? n)
            (or (number? n) (string? n) (symbol? n))))
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))
