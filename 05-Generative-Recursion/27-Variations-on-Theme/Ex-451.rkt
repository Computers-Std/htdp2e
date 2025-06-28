#lang htdp/isl+

(define-struct table [length array])
; A Table is a structure:
;  (make-table N [N -> Number])

; An N is one of:
; – 0
; – (add1 N)
; interpretation represents the counting numbers

(define table1 (make-table 3 (lambda (i) (- i 3))))
(define table2 (make-table 10 (lambda (x) (* (- x 2) (- x 5)))))

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; Table -> Number
; finds the smallest index for a root of table
(check-expect (find-linear table1) 3)
(check-satisfied (find-linear table2) (lambda (i) (or (= i 2) (= i 5))))
(define (find-linear t)
  (local ((define (linear-helper i)
            (cond
              [(> i (table-length t)) (error "No root found")]
              [(zero? (round (table-ref t i))) i]
              [else (linear-helper (add1 i))])))
    (linear-helper 0)))

; Table -> Number
; finds the smallest index for a root of table (assuming the table is sorted)
; NOTE :: Assuming the "length" gives length+1 items
(check-expect (find-binary table1) 3)
(check-satisfied (find-binary table2) (lambda (i) (or (= i 2) (= i 5))))
(define (find-binary t)
  (local ((define array-fun (table-array t))
          (define len (table-length t))
          (define (binary-helper l r)
            (if (> l r)
                (error "No root found")
                (local ((define mid (floor (/ (+ l r) 2)))
                        (define a@mid (array-fun mid)))
                  (cond
                    [(zero? a@mid) mid]
                    [(< a@mid 0) (binary-helper (add1 mid) r)]
                    [else (binary-helper l (sub1 mid))])))))
    (if (= 0 len)
        (error "Empty table")
        (binary-helper 0 len))))
