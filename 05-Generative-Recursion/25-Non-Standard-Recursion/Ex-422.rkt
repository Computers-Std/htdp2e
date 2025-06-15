#lang htdp/isl+

;; Exercise 422. Define the function list->chunks. It consumes a list l
;; of arbitrary data and a natural number n. The function’s result is a
;; list of list chunks of size n. Each chunk represents a sub-sequence
;; of items in l.

;; Use list->chunks to define bundle via function composition.

; [List-of X] N -> [List-of [List-of X]]
(check-expect (list->chunks '(1 2) 2) '((1 2)))
(check-expect (list->chunks '(1 2 3) 2) '((1 2) (3)))
(check-expect (list->chunks '(10 2 3 5 100 11 4) 3) '((10 2 3) (5 100 11) (4)))
(define (list->chunks l n)
  (cond
    [(or (zero? n) (empty? l)) '()]
    [else (cons (take l n) (list->chunks (drop l n) n))]))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take s n)
  (cond
    [(or (zero? n) (empty? s)) '()]
    [else (cons (first s) (take (rest s) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop s n)
  (cond
    [(or (zero? n) (empty? s)) s]
    [else (drop (rest s) (sub1 n))]))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n idea take n items and
; drop n at a time
(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(define (bundle s n)
  (map implode (list->chunks s n)))
