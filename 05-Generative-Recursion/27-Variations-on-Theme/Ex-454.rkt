#lang htdp/isl+

;; Exercise 454. Design create-matrix. The function consumes a number
;; n and a list of n2 numbers. It produces an image matrix

(check-expect (create-matrix 2 (list 1 2 3 4))
              (list (list 1 2) (list 3 4)))
(check-expect (create-matrix 3 '(9 8 7 6 5 4 3 2 1))
              (list (list 9 8 7) (list 6 5 4) (list 3 2 1)))

; Number [List-of Number] -> [List-of [List-of Number]]
(define (create-matrix n l)
  (cond
    [(or (empty? l) (= n 0)) '()]
    [else (cons (firstN n l)
                (create-matrix n (drop-firstN n l)))]))

; Number [List-of Number] -> [List-of Number]
(define (firstN n l)
  (cond
    [(or (empty? l) (= n 0)) '()]
    [else (cons (first l)
                (firstN (sub1 n) (rest l)))]))

; Number [List-of Number] -> [List-of Number]
(define (drop-firstN n l)
  (cond
    [(or (empty? l) (= n 0)) l]
    [else (drop-firstN (sub1 n) (rest l))]))
