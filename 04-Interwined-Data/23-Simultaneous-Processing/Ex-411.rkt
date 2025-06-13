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

(define multi-presence-schema `(("Present" ,boolean?) ("Description" ,string?)))
(define multi-presence-content
  `((#true "presence") (#true "here") (#false "absence") (#false "there")))
(define multi-presence-db (make-db multi-presence-schema multi-presence-content))

(define school+presence-content
  '(("Alice" 35 "presence") ("Bob" 25 "absence") ("Carol" 30 "presence") ("Dave" 32 "absence")))

(define school+multi-presence-content
  '(("Alice" 35 "presence") ("Alice" 35 "here")
                            ("Bob" 25 "absence")
                            ("Bob" 25 "there")
                            ("Carol" 30 "presence")
                            ("Carol" 30 "here")
                            ("Dave" 32 "absence")
                            ("Dave" 32 "there")))

; DB DB -> DB
(check-expect (db-content (join school-db presence-db))
              school+presence-content)
(define (join db-1 db-2)
  (local ((define schema-1 (db-schema db-1))
          (define schema-2 (db-schema db-2))
          (define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          (define (translate cell)
            (second (assoc cell content-2)))
          (define (row-project row)
            (cond
              [(empty? row) '()]
              [else
               (cons (if (empty? (rest row))
                         (translate (first row))
                         (first row))
                     (row-project (rest row)))])))
    (make-db (join-schema schema-1 schema-2)
             (foldr (lambda (r base) (cons (row-project r) base))
                    '() content-1))))

; Schema Schema -> Schema
; Produce a schema like s1 but with last cell replaced with its
; corresponding cell in s2
(define (join-schema s1 s2)
  (cond
    [(empty? s1) '()]
    [else
     (cons (if (empty? (rest s1))
               (second s2)
               (first s1))
           (join-schema (rest s1) s2))]))

; DB DB -> DB
(check-expect (db-content (multi-join school-db multi-presence-db))
              school+multi-presence-content)
(define (multi-join db-1 db-2)
  (local
      ((define schema-1 (db-schema db-1))
       (define schema-2 (db-schema db-2))
       (define content-1 (db-content db-1))
       (define content-2 (db-content db-2))

       ; [List-of Any] Any -> [List-of Any]
       (define (replace-last row s)
         (cond
           [(empty? row) '()]
           [else
            (cons (if (empty? (rest row))
                      s
                      (first row))
                  (replace-last (rest row) s))]))

       ; Row -> [List-of Row]
       (define (translations r)
         (cond
           [(empty? r) '()]
           [else
            (if (empty? (rest r))
                (filter (lambda (p) (equal? (first p) (first r))) content-2)
                (translations (rest r)))]))

       ; Row -> [List-of Row]
       (define (row-project row)
         (foldr (lambda (i base) (cons (replace-last row (second i)) base))
                '() (translations row))))

    (make-db (join-schema schema-1 schema-2)
             (foldr (lambda (r base) (append (row-project r) base))
                    '() content-1))))

;; [05-06-2025] TODO: Maybe, this can be improved to use less computation.
