#lang htdp/isl+

;; Exercise 427, 428, 429

(define THRESHOLD 5)

; [List-of Number] -> [List-of Number]
(define (sort< l c)
  (local ((define (insert n l)
            (cond
              [(empty? l) (cons n '())]
              [else (if (or (c n (first l)) (= n (first l)))
                        (cons n l)
                        (cons (first l) (insert n (rest l))))])))
    (cond
      [(empty? l) '()]
      [(cons? l) (insert (first l) (sort< (rest l) c))])))

(define (quick-sort alon c)
  (local ; [List-of Number] Number -> [List-of Number]
      ((define (largers alon n)
         (filter (lambda (x) (> x n)) alon))
       ; [List-of Number] Number -> [List-of Number]
       (define (smallers alon n)
         (filter (lambda (x) (< x n)) alon))
       ; [List-of Number] -> [List-of Number]
       (define (threshold-function lon)
         (if (>= (length lon) THRESHOLD)
             (sort< lon c)
             (quick-sort lon c))))
    (cond
      [(or (empty? alon) (empty? (rest alon))) alon]
      [else
       (local ((define pivot (first alon))
               (define small-numbers (threshold-function (smallers alon pivot)))
               (define large-numbers (threshold-function (largers alon pivot))))
         (append (if (equal? c >) small-numbers large-numbers)
                 (list pivot)
                 (if (equal? c >) large-numbers small-numbers)))])))
