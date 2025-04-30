#lang htdp/isl+
(require 2htdp/abstraction)

;; 1. creates the list (list 0 ... (- n 1)) for any natural number n;
; Number -> [List-of Number]
(define (func1 n)
  (for/list ([i n])
    i))

;; 2. creates the list (list 1 ... n) for any natural number n;
(define (func2 n)
  (for/list ([i n])
    (add1 i)))

;; 3. creates the list (list 1 1/2 ... 1/n) for any natural number n;
(define (func3 n)
  (for/list ([i n])
    (/ 1 (add1 i))))

;; 4. creates the list of the first n even numbers; and
(define (first-evens n)
  (for/list ([i n])
    (* 2 i)))

;; 5. creates a diagonal square of 0s and 1s; see exercise 262.
(define (matrix n)
  (for/list ([i n])
    (for/list ([j n])
      (if (= (add1 j) (add1 i)) 1 0))))

;; Finally, define tabulate from exercise 250 using build-list.
; Number Operation -> [List-of Number]
(check-expect (tabulate 0 sin) (list 0))
(check-expect (tabulate 0 sqrt) (list 0))
(define (tabulate num op)
  (if (= num 0)
      (list (op 0))
      (for/list ([i num])
        (op (- num i)))))
