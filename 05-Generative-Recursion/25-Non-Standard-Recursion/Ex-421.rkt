#lang htdp/isl+

;; Exercise 421. Is (bundle '("a" "b" "c") 0) a proper use of the
;; bundle function? What does it produce? Why?

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
(check-expect (bundle LETTERS 3) (list "abc" "def" "g"))
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else (cons (implode (take s n)) (bundle (drop s n) n))]))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(or (zero? n) (empty? l)) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(or (zero? n) (empty? l)) l]
    [else (drop (rest l) (sub1 n))]))


;; (bundle '("a" "b" "c") 0) -> Is not a Proper Usage
; Because the first cond cluase of the bundle function runs until
; (empty? s), but (bundle (drop s 0) 0) yeild same list back to the
; bundle function, which leads to an endless recursion.
