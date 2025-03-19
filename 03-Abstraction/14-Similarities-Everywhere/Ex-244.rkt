#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-244) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

(define (f0 x) (x 10))
(f0 sub1)

(define (f1 x) (x f1))
;; (define (increment n) (add1 n))

(define (f2 x y) (x 'a y 'b))
