#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-256) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; [X] [X -> Number] [NEList-of X] -> X
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i,
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...

;; (define (argmax f lx)
;; ...)

(define (negative n)
  (* n -1))

;; (argmax negative (list 1 2 3 4 3 2 1)) => 1

(define (abs-diff x)
  (abs (- x 5)))

;; (argmin negative (list 1 2 3 4 3 2 1)) => 1

; [X] [X -> Number] [NEList-of X] ->
; find the (first) item in lx that minimizes f
; if (argmin f (list x-1 ... x-n)) == x-i,
; then (<= (f x-i) (f x-1)), (<= (f x-i) (f x-2)), ...
