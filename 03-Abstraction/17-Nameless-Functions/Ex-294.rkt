#lang htdp/isl+

;; Exercise 294. Develop is-index?, a specification for index: Use
;; is-index? to formulate a check-satisfied test for index.

; [X] X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))


;; [X] X [List-of X] -> [[Maybe N] -> Boolean]
(check-expect (index 10 '()) #false)
(check-expect (index 10 '(1 2 3)) #false)
(check-expect (index 10 '(10 20 30)) 0)
(check-expect (index 10 '(20 10 10)) 1)
(check-expect (index 10 '(20 30 10)) 2)
(check-satisfied (index 10 '()) (is-index? 10 '()))
(check-satisfied (index 10 '(1 2 3)) (is-index? 10 '(1 2 3)))
(check-satisfied (index 10 '(10 20 30)) (is-index? 10 '(10 20 30)))
(check-satisfied (index 10 '(20 30 10)) (is-index? 10 '(20 30 10)))
(define (is-index? n l)
  (lambda (l0)
    (local (; Number [List-of X] -> [List-of X]
            ; drop all items except First Number of items in ls
            (define (drop-tail n ls)
              (cond
                [(zero? n) '()]
                [else (cons (first ls) (drop-tail (sub1 n) (rest ls)))])))
      (or (and (false? l0) (not (member? n l))) ; Not Found Anywhere
          (and (< l0 (length l))                ; Found Somewhere
               (eq? (list-ref l l0) n))
          (not (member? n (drop-tail n l))))))) ; Not Elsewhere Before
