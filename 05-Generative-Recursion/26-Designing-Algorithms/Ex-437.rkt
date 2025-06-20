#lang htdp/isl+

; [List-of X] -> Number
(define (special-length P)
  (local ((define (solve s)
            (cond
              [(= s 0) 0]
              [else 1]))
          (define (combine-solutions a b)
            (+ (solve (first a)) b)))
    (cond
      [(empty? P) (solve 0)]
      [else (combine-solutions P (special-length (rest P)))])))

; [List-of String] -> [List-of String]
(define (special-uppercase P)
  (local ((define (solve s)
            (cond
              [(empty? s) '()]
              [else (string-upcase s)]))
          (define (combine-solutions a b)
            (append (list (solve (first a))) b)))
    (cond
      [(empty? P) (solve P)]
      [else (combine-solutions P (special-uppercase (rest P)))])))

; [List-of Number] -> [List-of Number]
(define (special-negates P)
  (local ((define (solve s)
            (cond
              [(empty? s) '()]
              [else (* -1 s)]))
          (define (combine-solutions a b)
            (append (list (solve (first a))) b)))
    (cond
      [(empty? P) (solve P)]
      [else (combine-solutions P (special-negates (rest P)))])))

; Almost every Structural Recursive function can be written as
; Generative Recursive function, but may not the other way.
