#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-187) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 187. Design a program that sorts lists of game players by score:

(define-struct gp [name score])
; A GamePlayer is a structure:
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who
; scored a maximum of s points

;; Hint Formulate a function that compares two elements of GamePlayer

; A List-of-gps is one of
; - '()
; - (cons GamePlayer List-of-gps)

;; Previous Sorting Funtions
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
; Testing area
(define l1 (list (make-gp "name1" 20)
                 (make-gp "name2" 90)
                 (make-gp "name3" 13)
                 (make-gp "name4" 1)
                 (make-gp "name5" -5)))

; List-of-gps -> List-of-gps
; produces a sorted version of Logp
(check-expect (sort-gp> l1)
              (list (make-gp "name2" 90)
                    (make-gp "name1" 20)
                    (make-gp "name3" 13)
                    (make-gp "name4" 1)
                    (make-gp "name5" -5)))
(define (sort-gp> logp)
  (cond
    [(empty? logp) '()]
    [(cons? logp) (insert-gp (first logp)
                             (sort-gp> (rest logp)))]))

; Player List-of-gps -> List-of-gps
; inserts player into sorted List-of-gps
(check-expect (insert-gp (make-gp "aaa" 10)
                         (list (make-gp "name3" 13)
                               (make-gp "name4" 1)))
              (list (make-gp "name3" 13)
                    (make-gp "aaa" 10)
                    (make-gp "name4" 1)))
(define (insert-gp p logp)
  (cond
    [(empty? logp) (cons p '())]
    [else (if (>= (gp-score p) (gp-score (first logp)))
              (cons p logp)
              (cons (first logp)
                    (insert-gp p (rest logp))))]))
