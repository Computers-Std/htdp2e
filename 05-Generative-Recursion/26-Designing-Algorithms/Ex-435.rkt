#lang htdp/isl+

; [List-of Number] Number -> [List-of Number]
(define (smallers lon n)
  (cond
    [(empty? lon) '()]
    [else (if (< (first lon) n)
              (cons (first lon) (smallers (rest lon) n))
              (smallers (rest lon) n))]))

; [List-of Number] Number -> [List-of Number]
(define (largers lon n)
  (cond
    [(empty? lon) '()]
    [else (if (> (first lon) n)
              (cons (first lon) (largers (rest lon) n))
              (largers (rest lon) n))]))

; [List-of Number] -> [List-of Number]
; Assume all numbers are distinct
(check-expect (quick-sort< '(11 9 2 18 12 14 4 1)) '(1 2 4 9 11 12 14 18))
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon))
                  (define post-pivot (rest alon)))
            (append (quick-sort< (smallers post-pivot pivot))
                    (list pivot)
                    (quick-sort< (largers post-pivot pivot))))]))
