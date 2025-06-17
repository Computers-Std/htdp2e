#lang htdp/isl+

;; Exercise 433: checked version of bundle

; [List-of Number] -> [List-of Number]
; bundles sub-sequneces of s into strings of length n
; termination (bundle s 0) loops unless s is '()
(check-expect (bundle (explode "abcdefgh") 2) '("ab" "cd" "ef" "gh"))
(define (bundle s n)
  (local ((define (take l n)
            (cond
              [(or (zero? n) (empty? l)) '()]
              [else (cons (first l) (take (rest l) (sub1 n)))]))
          (define (drop l n)
            (cond
              [(or (zero? n) (empty? l)) l]
              [else (drop (rest l) (sub1 n))])))
    (cond
      [(empty? s) '()]
      [(zero? n) (error "Leads to Infinite Loop, if N <= 0")]
      [else (cons (implode (take s n)) (bundle (drop s n) n))])))
