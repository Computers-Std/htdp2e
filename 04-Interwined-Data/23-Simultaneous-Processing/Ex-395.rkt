#lang htdp/isl+

;; Exercise 395. Design take. It consumes a list l and a natural
;; number n. It produces the first n items from l or all of l if it is
;; too short.

;; Design drop. It consumes a list l and a natural number n. Its
;; result is l with the first n items removed or just ’() if l is too
;; short.

(define ls '(0 1 2 3 4 5 6 7 8 9))

; [List-of Number] Number -> [List-of Number]
; produces the first n items from l or just l if it is too short
(check-expect (take ls 6) (list 0 1 2 3 4 5))
(check-expect (take ls 0) '())
(define (take l n)
  (cond
    [(empty? l) l]
    [(= n 0) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of Number] Number -> [List-of Number]
; produces l with the first n items removed or just '() if l is too
; short
(check-expect (drop ls 6) (list 6 7 8 9))
(check-expect (drop ls 0) ls)
(define (drop l n)
  (cond
    [(empty? l) '()]
    [(= n 0) l]
    [else (drop (rest l) (sub1 n))]))
