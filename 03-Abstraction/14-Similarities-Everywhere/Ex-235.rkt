#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-235) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

(define (contains-atom? l)
  (contains? "atom" l))

(define (contains-basic? l)
  (contains? "basic" l))

(define (contains-zoo? l)
  (contains? "zoo" l))
