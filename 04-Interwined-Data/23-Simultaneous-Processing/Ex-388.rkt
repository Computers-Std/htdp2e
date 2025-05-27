#lang htdp/isl+

(define-struct employee [name ssn rate])
; An Employee is a structure:
; (make-employee String Number Number)

(define-struct record [ssn hours])
; A Record is a structure:
;  (make-record Number Number)
; Note: using ssn istead of name because, workers may have same names.

(define-struct pay [name wage])
; A Pay is a structure:
;  (make-pay String Number)

; Constants
(define emp1 (make-employee "usha" 001 25))
(define emp2 (make-employee "kiran" 002 35))
(define emp3 (make-employee "pallavi" 003 45))

(define rec1 (make-record 001 2))
(define rec2 (make-record 001 5))
(define rec3 (make-record 002 3))
(define rec4 (make-record 002 1))
(define rec5 (make-record 003 2))
(define rec6 (make-record 003 2))

(define loemp `(,emp1 ,emp2 ,emp3))
(define lorec `(,rec1 ,rec2 ,rec3 ,rec4 ,rec5 ,rec6))

; [List-of Employee] [List-of Record] -> [List-of Pay]
(check-expect (wages*.v2 loemp lorec)
              (list (make-pay "usha" 175)
                    (make-pay "kiran" 140)
                    (make-pay "pallavi" 180)))
(define (wages*.v2 loe lor)
  (cond
    [(empty? loe) '()]
    [else (cons
           (weekly-wage (first loe) lor)
           (wages*.v2 (rest loe) lor))]))

; Employee [List-of Record] -> Pay
(check-expect (weekly-wage emp1 lorec) (make-pay "usha" 175))
(check-expect (weekly-wage emp3 lorec) (make-pay "pallavi" 180))
(define (weekly-wage emp lor)
  (local ((define name (employee-name emp))
          (define ssn (employee-ssn emp))
          (define rate (employee-rate emp)))
    (make-pay name
              (foldr (lambda (rec wage)
                       (+ (* rate
                             (if (= ssn (record-ssn rec))
                                 (record-hours rec)
                                 0))
                          wage))
                     0
                     lor))))
