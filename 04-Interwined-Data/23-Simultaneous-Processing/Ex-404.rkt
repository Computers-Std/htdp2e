#lang htdp/isl+

;; Exercise 404. Design the function andmap2. It consumes a function f
;; from two values to Boolean and two equally long lists. Its result
;; is also a Boolean. Specifically, it applies f to pairs of
;; corresponding values from the two lists, and if f always produces
;; #true, andmap2 produces #true, too. Otherwise, andmap2 produces
;; #false. In short, andmap2 is like andmap but for two lists.

(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)

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

(define school-db (make-db
                   school-schema
                   school-content))

; Any Any -> Boolean
(define (f-ex v1 v2)
  (and (number? v1) (number? v2)))

; [Any Any -> Boolean] [List-of X] [List-of Y] -> Boolean
(check-expect (andmap2 f-ex ex-l0 ex-l1) #true)
(check-expect (andmap2 f-ex ex-l1 ex-l2) #false)
(check-error (andmap2 f-ex ex-l1 (cons "string" ex-l2)) ERROR)
(check-expect (andmap2 (lambda (s c) [(second s) c])
                       (db-schema school-db)
                       (first school-content))
              #true)
(define (andmap2 f l1 l2)
  (cond
    [(false? (= (length l1) (length l2))) (error ERROR)]
    [(and (empty? l1) (empty? l2)) #true]
    [else (and (f (first l1) (first l2)) (andmap2 f (rest l1) (rest l2)))]))
