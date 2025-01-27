;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-150) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 150. Design the function add-to-pi. It consumes a natural number n and adds
;; it to pi without using the primitive + operation.

(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

(define (add n x)
  (cond
    [(zero? n) x]
    [else (add1 (add (sub1 n) x))]))

;(check-expect (add 3 2) 5)

(define (multiply n x)
  (cond
    [(zero? n) 0]
    [else (add x (multiply (sub1 n) x))]))
(multiply 2 3)
;(check-expect (multiply 2 3) (* 2 3))
;(check-expect (multiply 3 3) (* 3 3))
;(check-expect (multiply 4 3) (* 4 3))

