#lang htdp/isl+

(define EPSILON 0.01)
(define R 160)

; [Number -> Number] Number Number -> Number
(define (integrate-rectangles f a b)
  (local ((define width (/ (- b a) R))
          (define mid (+ a (/ width 2)))
          (define (summation i)
            (* width (f (+ mid (* i width)))))
          (define (traverse r)
            (cond
              [(= r 0) (summation 0)]
              [else (+ (summation r)
                       (traverse (- r 1)))])))
    (traverse (- R 1))))

(check-within (integrate-rectangles (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-rectangles (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)
