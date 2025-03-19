#reader
(lib "htdp-intermediate-lambda-reader.ss" "lang")
((modname fig-88) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; Lon Number -> Lon
; select those numbers on l
; that are below t
(define (small l t)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(< (first l) t) (cons (first l) (small (rest l) t))]
       [else (small (rest l) t)])]))

; Lon Number -> Lon
; select those numbers on l
; that are above t
(define (large l t)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(> (first l) t) (cons (first l) (large (rest l) t))]
       [else (large (rest l) t)])]))

; Operation Lon Number -> Lon
; extracts the numbers from Lon with given threshold (t) and Operator(R)
(check-expect (extract < '() 5) (small '() 5))
(check-expect (extract < '(3) 5) (small '(3) 5))
(check-expect (extract < '(1 6 4) 5) (small '(1 6 4) 5))

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

;; Testing
(extract squared>? '(3 4 5) 10)
