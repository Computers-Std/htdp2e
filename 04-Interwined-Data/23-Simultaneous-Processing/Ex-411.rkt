#lang htdp/isl+

;; Exercise 411: Design join, a function that consumes two databases:
;; db-1 and db-2. The schema of db-2 starts with the exact same Spec
;; that the schema of db-1 ends in. The function creates a database
;; from db-1 by replacing the last cell in each row with the
;; translation of the cell in db-2.

(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)

; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]

; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any

(define school-schema `(("Name" ,string?) ("Age" ,integer?) ("Present" ,boolean?)))
(define school-content `(("Alice" 35 #true) ("Bob" 25 #false) ("Carol" 30 #true) ("Dave" 32 #false)))
(define school-db (make-db school-schema school-content))
; --
(define presence-schema `(("Present" ,boolean?) ("Description" ,string?)))
(define presence-content `((#true "presence") (#false "absence")))
(define presence-db (make-db presence-schema presence-content))

; DB DB -> DB
(define (join db-1 db-2)
  (local ((define schema-1 (db-schema db-1))
          (define schema-2 (db-schema db-2))
          (define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          )
    ...))
