#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-253) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; [Number -> Boolean]
(define (num n)
  (number? n))

; [Boolean String -> Boolean]
(define (bool-str b s)
  (cond
    [(and b (string? s)) #true]
    [else false]))

; [Number Number Number -> Number]
(define (3xn n1 n2 n3)
  (* n1 n2 n3))

; [Number -> [List-of Number]]
(define (n-lon n)
  (cond
    [(= n 0) '()]
    [else (cons n (n-lon (sub1 n)))]))

; [[List-of Number] -> Boolean]
(define (lon-n l)
  (cond
    [(empty? l) #false]
    [else #true]))
