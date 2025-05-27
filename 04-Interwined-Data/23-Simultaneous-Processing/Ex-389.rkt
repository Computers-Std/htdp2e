#lang htdp/isl+

(define-struct phonerecord [name number])
; A PhoneRecord is a structure:
;  (make-phonerecord String String)

(define lonames '("usha" "kiran" "pallavi"))
(define lonumbers '("234" "345" "456"))

; [List-of String] [List-of String] -> [List-of PhoneRecord]
(check-expect (zip lonames lonumbers)
              (list (make-phonerecord "usha" "234")
                    (make-phonerecord "kiran" "345")
                    (make-phonerecord "pallavi" "456")))
(define (zip names numbers)
  (cond
    [(empty? names) '()]
    [else (cons
           (make-phonerecord (first names) (first numbers))
           (zip (rest names) (rest numbers)))]))
