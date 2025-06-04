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
(define (project db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          ; Spec -> Boolean
          (define (keep? c)
            (member? (first c) labels))
          ; Row [List-of Label] -> Row
          ; retains those cells whose corresponding element in names
          ; is also in labels
          ;; (define (row-filter row names) '())
          ; Row -> Row
          ; retains those columns whose name is in labels
          (define (row-project row)
            (row-filter row (map first schema) labels)))
    (make-db (filter keep? schema)
             (map row-project content))))

; Row [List-of Label] [List-of Label] -> Row
; retains those cells whose corresponding element in names
; is also in labels
(check-expect (row-filter '() '() '()) '())
(check-expect (row-filter '("Alice" 35 #true)
                          '("Name" "Age" "Present")
                          '("Name" "Present"))
              '("Alice" #true))
(check-expect (row-filter '("Bob" 25 #false)
                          '("Name" "Age" "Present")
                          '("Name" "Age" "Present"))
              '("Bob" 25 #false))
(check-expect (row-filter '("Bob" 25 #false)
                          '("Name" "Age" "Present")
                          '("Name"))
              '("Bob"))
(check-expect (row-filter '("Bob" 25 #false)
                          '("Name" "Age" "Present")
                          '())
              '())
(define (row-filter row names labels)
  (cond
    [(empty? names) '()]
    [else (if (member? (first names) labels)
              (cons (first row)
                    (row-filter (rest row) (rest names) labels))
              (row-filter (rest row) (rest names) labels))]))

(define projected-content `(("Alice" #true)
                            ("Bob" #false)
                            ("Carol" #true)
                            ("Dave" #false)))

(define projected-schema `(("Name" ,string?)
                           ("Present" ,boolean?)))

(define projected-db (make-db projected-schema projected-content))
;  Stop! Read this test carefully. What's wrong?

;; (check-expect (project school-db '("Name" "Present")) projected-db)
; FIXME;: Still Error - first argument of equality cannot be a function, given string?
