#lang htdp/isl+

; A Token is one of:
;  - 1String
;  - String

; Line -> [List-of Token]
(define (tokenize l)
  (cond
    [(empty? l) '()]
    [(string-whitespace? (first l)) (tokenize (rest l))]
    [else (cons (first l) (tokenize (rest l)))]))

(check-expect (tokenize '("h" "o" "w" " " "a" "r" "e" " " "y" "o" "u"))
              '("h" "o" "w" "a" "r" "e" "y" "o" "u"))
(check-expect (tokenize '("hello" " " "world")) '("hello" "world"))
