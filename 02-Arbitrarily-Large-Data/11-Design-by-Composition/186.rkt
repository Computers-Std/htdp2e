#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l)
                       (sort> (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; Ex-145
;; NEList-of-temperatures -> Boolean
; List-of-numbers -> Boolean
(check-expect (sorted>?
               (cons 3 (cons 2 (cons 1 '())))) #true)
(define (sorted>? alon)
  (cond
    [(empty? (rest alon)) #true]
    [(>= (first alon) (first (rest alon)))
     (sorted>? (rest alon))]
    [else #false]))

(sorted>?
 (cons 2 (cons 2 (cons 1 '()))))

;; Testing Area
(define l1 (list 2 2 4 63 5))
(define l2 (list 2 34 0 -1 3))

(check-satisfied (sort> l1) sorted>?)
(check-satisfied (sort> l2) sorted>?)


; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))

(check-satisfied (sort>/bad l1) sorted>?) ; #true
