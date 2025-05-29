#lang htdp/isl+

(define-struct employee [name ssn rate])
; A Employee is a structure:
;  (make-employee String Number Number)

(define-struct card [ssn hours])
; A Card is a structure:
;  (make-card Number Number)

(define-struct wage [name pay])
; A Wage is a structure:
;  (make-wage String Number)

;; [28-05-2025] NOTE: I Couldn't make it
;; [28-05-2025] TODO: Solve this, with fresh mind
