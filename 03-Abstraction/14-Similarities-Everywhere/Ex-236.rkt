#reader
(lib "htdp-intermediate-lambda-reader.ss" "lang")
((modname Ex-236) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; Lon -> Lon
; adds 1 to each item on l
(define (add1+ l)
  (cond
    [(empty? l) '()]
    [else (cons (add1 (first l)) (add1+ (rest l)))]))

; Lon -> Lon
; adds 5 to each item on l
(define (plus5 l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) 5) (plus5 (rest l)))]))

;; Abstraction

; Lon -> Lon
; add n to each item on l
(define (add-n n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n) (add-n n (rest l)))]))

(define l1 '(1 2 8 2 04 221 98))

; Lon -> Lon
;; One liners concrete funcs
(check-expect (add1+.v2 l1) (add1+ l1))
(define (add1+.v2 l)
  (add-n 1 l))

; Lon -> Lon
(check-expect (plus5.v2 l1) (plus5 l1))
(define (plus5.v2 l)
  (add-n 5 l))
