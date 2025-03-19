#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-238) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; Constants
(define l1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                 12 11 10 9 8 7 6 5 4 3 2 1))

(define l2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                 17 18 19 20 21 22 23 24 25))

; Nelon -> Number
; determines the smallest number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (min (first l) (inf (rest l)))]))

; Nelon -> Number
; determines the largest number on l
;; (define (sup l)
;;   (cond
;;     [(empty? (rest l))
;;      (first l)]
;;     [else (if (> (first l) (sup (rest l)))
;;               (first l)
;;               (sup (rest l)))]))

(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else (max (first l) (inf (rest l)))]))

; Nelon -> Number
; gives the smalles/largest number from the list
;; (check-expect (extract < l1) (inf l1))
(define (extract R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else (if (R (first l) (extract R (rest l)))
              (first l)
              (extract R (rest l)))]))

; Nelon -> Number
; gives the smalles/largest number from the list
;; (check-expect (extract < l1) (inf l1))
(define (extract-2 R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else (R (first l) (extract-2 R (rest l)))]))

(define (inf-1 l)
  (extract < l))
(define (sup-1 l)
  (extract > l))

(define (inf-2 l)
  (extract min l))
(define (sup-2 l)
  (extract max l))

;; (extract-2 min l1)
;; (extract-2 max l1)
