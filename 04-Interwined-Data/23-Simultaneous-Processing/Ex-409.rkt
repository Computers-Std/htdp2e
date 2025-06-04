#lang htdp/isl+

;; Exercise 409. Design reorder. The function consumes a database db
;; and list lol of Labels. It produces a database like db but with its
;; columns reordered according to lol.

(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)

; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]

; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions

; integrity constraint In (make-db sch con),
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

; Constants
(define school-schema `(("Name" ,string?) ("Age" ,integer?) ("Present" ,boolean?)))
(define school-content '(("Alice" 35 #true) ("Bob" 25 #false) ("Carol" 30 #true) ("Dave" 32 #false)))
(define school-db (make-db school-schema school-content))

;; DB [List-of Label] -> DB
; NOTE: Ignoring the Length and New Labels, produces new DB
(check-expect (reorder school-db '("Present" "Age" "Name"))
              '((#true 35 "Alice") (#false 25 "Bob") (#true 30 "Carol") (#false 32 "Dave")))
(check-expect (reorder school-db '("Age" "Name")) '((35 "Alice") (25 "Bob") (30 "Carol") (32 "Dave")))
(check-expect (reorder school-db '("Age" "Name" "Error"))
              '((35 "Alice") (25 "Bob") (30 "Carol") (32 "Dave")))

(define (reorder db lol)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define names (map first schema))
          ; String [List-of String] -> Number
          ; returns the index of s in list
          (define (index s los)
            (foldr (lambda (i base)
                     (if (string=? i s)
                         0
                         (add1 base)))
                   -1
                   los))
          ; Schema Schema -> [List-of Number]
          (define order
            (local ((define names (map first (db-schema school-db))))
              (foldr (lambda (l base)
                       (if (member? l names)
                           (cons (index l names) base)
                           base))
                     '()
                     lol)))
          ; Row -> Row
          (define (row-project row)
            (foldr (lambda (i base) (cons (list-ref row i) base)) '() order)))
    ; -- IN --
    (foldr (lambda (r base) (cons (row-project r) base)) '() content)))
