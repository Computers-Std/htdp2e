#lang htdp/isl+

;; Exercise 408. Design the function select. It consumes a database, a
;; list of labels, and a predicate on rows. The result is a list of
;; rows that satisfy the given predicate, projected down to the given
;; set of labels.

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
; -- school-db
(define school-schema `(("Name" ,string?) ("Age" ,integer?) ("Present" ,boolean?)))
(define school-content '(("Alice" 35 #true) ("Bob" 25 #false) ("Carol" 30 #true) ("Dave" 32 #false)))
(define school-db (make-db school-schema school-content))
; -- project-db
(define projected-schema `(("Name" ,string?) ("Present" ,boolean?)))
(define projected-content '(("Alice" #true) ("Bob" #false) ("Carol" #true) ("Dave" #false)))
(define projected-db (make-db projected-schema projected-content))

; Functions
; DB [List-of Label] Predicate -> [List-of Row]
(check-expect (select.v1 school-db '("Name" "Present") (lambda (r) (false? (third r))))
              '(("Bob" #false) ("Dave" #false)))
(check-expect (select.v1 school-db '("Name" "Age") (lambda (r) (<= (second r) 30)))
              '(("Bob" 25) ("Carol" 30)))
(define (select.v1 db labels predicate)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          ; Spec -> Boolean
          (define (keep? c)
            (member? (first c) labels))
          (define mask (map keep? schema))
          (define pre-mask (map predicate content))
          ; Row -> Row
          (define (row-project row)
            (foldr (lambda (cell m base)
                     (if m (cons cell base) base))
                   '() row mask)))
    (foldr (lambda (row pm base)
             (if pm (cons (row-project row) base)
                 base))
           '() content pre-mask)))

(define (select.v2 db labels predicate)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          ; Spec -> Boolean
          (define (keep? c)
            (member? (first c) labels))
          (define mask (map keep? schema))
          ; Row -> Row
          (define (row-project row)
            (foldr (lambda (cell m base)
                     (if m (cons cell base) base))
                   '() row mask)))
    (map row-project (filter predicate content))))

; Select Parallel-version
(define (select.v3 db labels predicate)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          ; Spec -> Boolean
          (define (keep? c)
            (member? (first c) labels))
          (define mask (map keep? schema))
          ; Row -> Row
          (define (row-project row)
            (foldr (lambda (cell m base)
                     (if m (cons cell base) base))
                   '() row mask))
          ; Content -> Content
          (define (traverse c)
            (cond
              [(empty? c) '()]
              [else
               (if (predicate (first c))
                   (cons (row-project (first c)) (traverse (rest c)))
                   (traverse (rest c)))])))
    (traverse content)))

(check-expect (select.v3 school-db '("Name" "Present") (lambda (r) (false? (third r))))
              `(("Bob" #false) ("Dave" #false)))
(check-expect (select.v3 school-db '("Name" "Present") (lambda (r) (<= (second r) 30)))
              `(("Bob" #false) ("Carol" #true)))
(check-expect (select.v3 school-db '("Age") (lambda (r) (false? #false))) `((35) (25) (30) (32)))
