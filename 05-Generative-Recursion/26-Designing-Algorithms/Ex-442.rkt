#lang htdp/isl+

;; Exercise 442. Add sort< and quick-sort< to the definitions
;; area. Run tests on the functions to ensure that they work on basic
;; examples. Also develop create-tests, a function that creates large
;; test cases randomly. Then explore how fast each works on various
;; lists.

;; Does the experiment confirm the claim that the plain sort< function
;; often wins over quick-sort< for short lists and vice versa?

;; Determine the cross-over point. Use it to build a clever-sort
;; function that behaves like quick-sort< for large lists and like
;; sort< for lists below this cross-over point. Compare with exercise
;; 427.


;; [List-of Number] -> [List-of Number]
;; Produces a sorted version of alon.
(check-expect (quick-sort< '()) '())
(check-expect (quick-sort< (list 3 2 1 1 2 3)) (list 1 1 2 2 3 3))
(define (quick-sort< alon)
  (cond
    [(or (empty? alon) (empty? (rest alon))) alon]
    [else (local ((define pivot (first alon)))
            (append (quick-sort<
                     (filter (lambda (x) (< x pivot))
                             (rest alon)))
                    (filter (lambda (x) (= x pivot)) alon)
                    (quick-sort<
                     (filter
                      (lambda (x) (> x pivot))
                      (rest alon)))))]))

;; List-of-numbers -> List-of-numbers
;; Produces a sorted version of the list l.
(check-expect (sort< '()) '())
(check-expect (sort< (list 12 20 -5)) (list -5 12 20))
(define (sort< l)
  (cond
    [(empty? l) '()]
    [else
     (local ((define (insert n l)
               (cond
                 [(empty? l) (cons n '())]
                 [else (if (<= n (first l))
                           (cons n l)
                           (cons (first l) (insert n (rest l))))])))
       (insert (first l) (sort< (rest l))))]))

;; [[List-of Number] -> [List-of Number]] [List-of N] -> [List-of Number]
;; Runs a function f on a randomly generated lists
;; of the sizes from los and containing numbers from 0 to max.
(define (create-tests f los max)
  (local ((define (run s)
            (time (f (build-list s (lambda (i) (random max)))))))
    (map run los)))
