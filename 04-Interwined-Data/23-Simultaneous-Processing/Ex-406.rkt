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
(define school-schema `(("Name" ,string?)
                        ("Age" ,integer?)
                        ("Present" ,boolean?)))
(define school-content `(("Alice" 35 #true)
                         ("Bob" 25 #false)
                         ("Carol" 30 #true)
                         ("Dave" 32 #false)))
(define school-db (make-db school-schema school-content))

; Functions
(define (project.v1 db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          ; Spec -> Boolean
          (define (keep? c)
            (member? (first c) labels))
          ; Row -> Row
          ; retains those columns whose name is in labels
          (define (row-project row)
            (local ((define names (map first schema)))
              (row-filter row names)))
          ; Row [List-of Label] -> Row
          ; retains those cells whose corresponding element in names
          ; is also in labels
          (define (row-filter row names)
            (cond
              [(empty? names) '()]
              [else
               (if (member? (first names) labels)
                   (cons (first row)
                         (row-filter (rest row) (rest names)))
                   (row-filter (rest row) (rest names)))])))
    (make-db (filter keep? schema)
             (map row-project content))))

(define projected-content `(("Alice" #true)
                            ("Bob" #false)
                            ("Carol" #true)
                            ("Dave" #false)))

(define projected-schema `(("Name" ,string?)
                           ("Present" ,boolean?)))

(define projected-db (make-db projected-schema projected-content))
;; (project.v1 school-db '("Name" "Present"))
