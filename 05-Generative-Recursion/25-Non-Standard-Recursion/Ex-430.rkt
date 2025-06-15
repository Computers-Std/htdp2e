#lang htdp/isl+

;; [15-06-2025] NOTE: Does quciksort can handle comaparison functions
;; other than <,>; if yes, then how it will perform partioning the
;; other sublist, which need an exact opposite version of given
;; comparision function.

(define (quick-sort alon c)
  (local ((define (largers alon n)
            (filter (lambda (x) (> x n)) alon))
          (define (smallers alon n)
            (filter (lambda (x) (< x n)) alon)))
    (cond
      [(or (empty? alon) (empty? (rest alon))) alon]
      [else
       (local ((define pivot (first alon))
               (define equals (filter (lambda (x) (= pivot x)) alon))
               (define small-numbers (quick-sort (smallers alon pivot) c))
               (define large-numbers (quick-sort (largers alon pivot) c)))
         (append (if (equal? c <) small-numbers large-numbers)
                 equals
                 (if (equal? c <) large-numbers small-numbers)))])))
