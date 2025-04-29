#lang htdp/isl+

;; creates the list (list 0 ... (- n 1)) for any natural number n
(check-expect (fun1 4) (list 0 1 2 3))
(define (fun1 n)
  (build-list n (lambda (x) x)))

;; creates the list (list 1 ... n) for any natural number n;
(check-expect (fun1 4) (list 0 1 2 3))
(define (fun2 n)
  (build-list n (lambda (x) (add1 x))))

;; creates the list (list 1 1/2 ... 1/n) for any natural number n
(check-expect (fun1 4) (list 0 1 2 3))
(define (fun3 n)
  (build-list n (lambda (x) (/ 1 (+ x 1)))))

;; creates the list of the first n even numbers; and
(check-expect (fun4 4) (list 0 2 4 6))
(define (fun4 n)
  (build-list n (lambda (x) (* 2 x))))

;; creates a diagonal square of 0s and 1s; see exercise 262.
; Number -> [List-of [List-of Number]]
; interpretaion: given size of the matrix, produces the Identity
; Matrix (diagonal matrix)
(check-expect (diagonal-matrix 1) (list (list 1)))
(check-expect (diagonal-matrix 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define (diagonal-matrix s)
  (build-list s (lambda (x) (build-list s (lambda (y) (if (= y x) 1 0))))))

;; Finally, define tabulate from exercise 250 using build-list.
; Number Operation -> [List-of Number]
(check-expect (tabulate 0 sin) (list 0))
(check-expect (tabulate 0 sqrt) (list 0))
(define (tabulate num op)
  (if (= num 0)
      (list (op 0))
      (build-list num (lambda (x) (op (- num x))))))
