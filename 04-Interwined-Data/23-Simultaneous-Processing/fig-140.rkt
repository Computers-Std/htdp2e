#lang htdp/isl+

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
(define ERROR "Two aren't equally long lists")
(define ex-l0 '(10 20 30))
(define ex-l1 '(1 2 3))
(define ex-l2 '(4 5 #false))
(define school-schema `(("Name" ,string?) ("Age" ,integer?) ("Present" ,boolean?)))
(define school-content `(("Alice" 35 #true) ("Bob" 25 #false) ("Carol" 30 #true) ("Dave" 32 #false)))
(define school-db (make-db school-schema school-content))
(define projected-content `(("Alice" #true) ("Bob" #false) ("Carol" #true) ("Dave" #false)))
(define projected-schema `(("Name" ,string?) ("Present" ,boolean?)))
(define projected-db (make-db projected-schema projected-content))

(define (project db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          ; Spec -> Boolean
          ; does this spec belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
          ; Row -> Row
          ; retains those columns whose name is in labels
          (define (row-project row)
            (local ; All the specs to project
                ((define specs (map (lambda (s) (second (assoc s schema))) labels))
                 ; Cell -> Boolean
                 (define (keep-it? cell)
                   (foldr (lambda (i base) (or (i cell) base)) #false specs)))
              (filter keep-it? row))))
    (make-db (filter keep? schema) (map row-project content))))

;  Stop! Read this test carefully. What's wrong?

;; (check-expect (project school-db '("Name" "Present")) projected-db)
; FIXME;: Error - first argument of equality cannot be a function, given string?
