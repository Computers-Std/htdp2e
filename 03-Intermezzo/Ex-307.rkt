#lang htdp/isl+
(require 2htdp/abstraction)

;; Exercise 307. Define find-name. The function consumes a name and a
;; list of names. It retrieves the first name on the latter that is
;; equal to, or an extension of, the former.

; String [List-of String] -> [Maybe String]
(check-expect (find-name "cat" '()) #false)
(check-expect (find-name "cat" '("dog" "fish")) #false)
(check-expect (find-name "cat" '("dog" "cat" "fish" "caterpillar")) "cat")
(check-expect (find-name "cat" '("dog" "caterpillar" "fish")) "caterpillar")
(define (find-name name alon)
  (for/or ([n alon])
    (if ((lambda (n1 n2)
           (or (string=? n1 n2)
               (and (> (string-length n1) (string-length n2))
                    (string=? (substring n1 0 (string-length n2)) n2))))
         n name)
        n #false)))
