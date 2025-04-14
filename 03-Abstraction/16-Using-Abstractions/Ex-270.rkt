#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-270) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 270. Use build-list to define a function that

;; creates the list (list 0 ... (- n 1)) for any natural number n;
(check-expect (fun1 4)
              (list 0 1 2 3))
(define (fun1 n)
  (cons 0 (build-list (- n 1) add1)))

;; creates the list (list 1 ... n) for any natural number n;
(check-expect (fun2 4)
              (list 1 2 3 4))
(define (fun2 n)
  (build-list n add1))

;; creates the list (list 1 1/2 ... 1/n) for any natural number n;
(check-expect (fun3 4)
              (list 1 1/2 1/3 1/4))
(define (fun3 n)
  (local (; Number -> Number
          (define (help3 m)
            (/ 1 (+ m 1))))
    (build-list n help3)))

;; creates the list of the first n even numbers; and
(check-expect (fun4 4)
              (list 0 2 4 6))
(define (fun4 n)
  (local (; Number -> Number
          (define (help4 m)
            (* 2 m)))
    (build-list n help4)))

;; creates a diagonal square of 0s and 1s; see exercise 262.

; Number -> [List-of [List-of Number]]
; interpretaion: given size of the matrix, produces the Identity
; Matrix (diagonal matrix)
(check-expect (diagonal-matrix 1) (list (list 1)))
(check-expect (diagonal-matrix 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define (diagonal-matrix size)
  (local ((define (diagonalize n)
            (local ((define (build-row pos)
                      (if (= pos n) 1 0)))
              (build-list size build-row))))
    (build-list size diagonalize)))

;; Finally, define tabulate from exercise 250 using build-list.
; Number Operation -> [List-of Number]
(check-expect (tabulate 0 sin) (list 0))
(check-expect (tabulate 0 sqrt) (list 0))
;; (check-expect (tabulate 1 sin) (list (sin 1)))
;; (check-expect (tabulate 2 sqrt) (list (sqrt 2) (sqrt 1)))
(define (tabulate num op)
  (local ((define (rever n)
            (op (- num n))))
    (if (= num 0)
        (list (op 0))
        (build-list num rever))))
