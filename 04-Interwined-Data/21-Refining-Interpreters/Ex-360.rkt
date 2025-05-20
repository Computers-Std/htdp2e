#lang htdp/isl+

(define-struct add [left right])
(define-struct mul [left right])

(define ERROR-DEFCON "No such Constant defined")
(define ERROR-DEFUN "No such Function defined")

(define-struct fun [name arg])
; A Fun is a Structure:
;   (make-fun Symbol BSL-fun-expr)
; data representation of a function call with only one argument

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-fun-expr)
; - (make-mul BSL-var-expr BSL-fun-expr)
; - (make-fun Symbol BSL-fun-expr)

(define-struct defun [name pm body])
; A BSL-fun-def is a Structure:
;   (make-defun Symbol Symbol BSL-fun-expr)
; data representation of a Function Definition, consisting NAME,
; PARAMETER, BODY.

(define-struct defcon [name val])
; A BSL-con-def is a Structure:
;   (make-const Symbol Number)
; data representation of a Constant Definition, consisting NAME,
; VALUE

; A BSL-da-all is one of:
; - '()
; - (cons BSL-con-def BSL-da-all)
; - (cons BSL-fun-def BSL-da-all)

(define da1
  (list (make-defun 'f 'x (make-add 3 'x))
        (make-defun 'g 'y (make-fun 'f (make-mul 2 'y)))
        (make-defun 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v)))))

(define da2
  (list (make-defcon 'close-to-pi 3.14)
        (make-defcon 'ten 10)
        (make-defun 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r)))))

; BSL-da-all Symbol -> [Maybe BSL-con-def]
(define (lookup-con-def da x)
  (cond
    [(empty? da) (error ERROR-DEFCON)]
    [else (if (and (defcon? (first da))
                   (symbol=? (defcon-name (first da)) x))
              (first da)
              (lookup-con-def (rest da) x))]))

; BSL-da-all Symbol -> [Maybe BSL-fun-def]
(define (lookup-fun-def da x)
  (cond
    [(empty? da) (error ERROR-DEFUN)]
    [else (if (and (defun? (first da))
                   (symbol=? (defun-name (first da)) x))
              (first da)
              (lookup-fun-def (rest da) x))]))
