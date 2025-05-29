#lang htdp/isl+

;; Exercise 393. Figure 62 presents two data definitions for finite
;; sets. Design the union function for the representation of finite
;; sets of your choice. It consumes two sets and produces one that
;; contains the elements of both.

;; Design intersect for the same set representation. It consumes two
;; sets and produces the set of exactly those elements that occur in
;; both.

; A Son is one of:
; - '()
; - (cons Number Son)
;
; Constraint: if s is a Son, no number occurs twice in s

(define son1 '(2 3 4 7 9 6))
(define son2 '(3 4 9 7 2 8))

; Son Son -> Son
; produces all unique elements from both sets
(check-expect (union son1 son2) '(6 3 4 9 7 2 8))
(define (union s1 s2)
  (cond
    [(empty? s1) s2]
    [else
     (if (member? (first s1) s2)
         (union (rest s1) s2)
         (cons (first s1) (union (rest s1) s2)))]))

; Son Son -> Son
; produces intersection of two sets
(check-expect (intersect son1 son2) '(2 3 4 7 9))
(define (intersect s1 s2)
  (cond
    [(empty? s1) '()]
    [else
     (if (member? (first s1) s2)
         (cons (first s1) (intersect (rest s1) s2))
         (intersect (rest s1) s2))]))
