#lang htdp/isl+
; Exercise 426: The sorted version of a list of one item is the list
; itself, Modify quick-sort<, Evaluate the example again. How many
; steps does the revised algorithm save?

; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else
     (if (> (first alon) n)
         (cons (first alon) (largers (rest alon) n))
         (largers (rest alon) n))]))

; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else
     (if (< (first alon) n)
         (cons (first alon) (smallers (rest alon) n))
         (smallers (rest alon) n))]))

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon assume the numbers are all
; distinct
(define (quick-sort< alon)
  (cond
    [(or (empty? alon) (empty? (rest alon))) alon]
    [else
     (local ((define pivot (first alon)))
       (append (quick-sort< (smallers alon pivot))
               (list pivot)
               (quick-sort< (largers alon pivot))))]))
