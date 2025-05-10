#lang htdp/isl+

; S-expr Symbol Symbol -> S-expr
; replaces Symbol(old) with Symbol(new) in S-expr
(check-expect (substitute 'hello 'hello 'emacs) 'emacs)
(check-expect (substitute '(hello world) 'hello 'world)
              (list 'world 'world))
(check-expect (substitute '(hello hello world) 'hello 'hi)
              (list 'hi 'hi 'world))

(define (substitute sexp old new)
  (local ((define (atom? n)
            (or (number? n) (string? n) (symbol? n)))
          (define (sub-sl sl)
            (cond
              [(empty? (rest sl))
               (cons (first sl) '())]
              [else (cons (substitute (first sl) old new)
                          (sub-sl (rest sl)))])))
    (cond
      [(and (atom? sexp)
            (symbol=? sexp old)) new]
      [else (sub-sl sexp)])))
