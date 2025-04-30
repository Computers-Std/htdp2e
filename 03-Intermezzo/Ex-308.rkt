#lang htdp/isl+
(require 2htdp/abstraction)

;; Exercise 308. Design the function replace, which substitutes the
;; area code 713 with 281 in a list of phone records.

(define-struct phone [area number])
; (make-phone Number Number)
; (make-phone 713 232334)

(define ph1 (make-phone 713 232348))
(define ph2 (make-phone 343 787348))
(define ph3 (make-phone 713 780048))

; [List-of Phone] -> [List-of Phone]
; replaces the area codes with 713 to 281
(check-expect (replace (list ph1 ph2 ph3))
              (list (make-phone 281 232348) (make-phone 343 787348) (make-phone 281 780048)))
(define (replace lop)
  (for/list ([p lop])
    (match p
      [(phone 713 n) (make-phone 281 n)]
      [p p])))
