#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-237) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; Operation Lon Number -> Lon
; extracts the numbers from Lon with given threshold (t) and Operator(R)
(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(R (first l) t) (cons (first l) (extract R (rest l) t))]
       [else (extract R (rest l) t)])]))

;; Good Use cases

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))
