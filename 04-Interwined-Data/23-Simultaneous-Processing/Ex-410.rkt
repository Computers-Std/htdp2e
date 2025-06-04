#lang htdp/isl+

; Exercise 410. Design the function db-union, which consumes two
; databases with the exact same schema and produces a new database
; with this schema and the joint content of both. The function must
; eliminate rows with the exact same content

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

(define college-schema `(("Name" ,string?) ("Age" ,integer?) ("Present" ,boolean?)))
(define college-content
  '(("Carol" 30 #true) ("Ushakiran" 35 #true)
                       ("Chandra" 30 #true)
                       ("Shekara" 32 #false)
                       ("Saraswati" 25 #false)))
(define college-db (make-db college-schema college-content))

(define new-content
  '(("Alice" 35 #true) ("Bob" 25 #false)
                       ("Dave" 32 #false)
                       ("Carol" 30 #true)
                       ("Ushakiran" 35 #true)
                       ("Chandra" 30 #true)
                       ("Shekara" 32 #false)
                       ("Saraswati" 25 #false)))

; DB DB -> DB
(check-expect (db-union school-db college-db) (make-db school-schema new-content))

(define (db-union db1 db2)
  (local ((define schema1 (db-schema db1))
          (define schema2 (db-schema db2))
          (define content1 (db-content db1))
          (define content2 (db-content db2))
          (define content-project
            (foldr (lambda (row base)
                     (if (member? row content2)
                         base
                         (cons row base)))
                   content2
                   content1)))
    (if (equal? schema1 schema2)
        (make-db schema1 content-project)
        (error "DBs have different Schemas"))))
