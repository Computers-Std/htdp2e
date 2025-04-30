#lang htdp/isl+
(require 2htdp/abstraction)

;; Exercise 309. Design the function words-on-line, which determines
;; the number of Strings per item in a list of list of strings.

; [List-of [List-of String]] -> [List-of Number]
; determines number of Strings per item in a list of list of strings
(check-expect (words-on-line llos1) (list 3 2 3))
(define (words-on-line llos)
  (for/list ([ls llos])
    (match ls
      [list? (length ls)])))

(define llos1
  (list (list "sd" "sdf" "ert")
        (list "sdf" "sd")
        (list "sdf" "xcv" "cvb")))
