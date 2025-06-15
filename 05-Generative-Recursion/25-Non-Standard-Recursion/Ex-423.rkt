#lang htdp/isl+

; String Number -> [List-of String]
(define (partition s n)
  (local ((define str-length (string-length s)))
    (cond
      [(or (zero? str-length) (zero? n)) '()]
      [else (if (>= str-length n)
                (append (list (substring s 0 n))
                        (partition (substring s n str-length) n))
                (append (list s) '()))])))

(define (bundle s n)
  (local ((define (list->chunks l n)
            (cond
              [(or (zero? n) (empty? l)) '()]
              [else (cons (take l n) (list->chunks (drop l n) n))]))
          (define (take s n)
            (cond
              [(or (zero? n) (empty? s)) '()]
              [else (cons (first s) (take (rest s) (sub1 n)))]))
          (define (drop s n)
            (cond
              [(or (zero? n) (empty? s)) s]
              [else (drop (rest s) (sub1 n))])))
    (map implode (list->chunks s n))))


(check-expect (equal? (partition "abcdefg" 3) (bundle (explode "abcdefg") 3))
              #true)
