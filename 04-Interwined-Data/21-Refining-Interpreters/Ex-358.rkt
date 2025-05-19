#lang htdp/isl+

(define-struct add [left right])
(define-struct mul [left right])

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

; (define (f x) (+ 3 x))
(define f-def (make-defun 'f 'x (make-add 3 'x)))

; (define (g y) (f (* 2 y)))
(define g-def (make-defun 'g 'y (make-fun 'f (make-mul 2 'y))))

; (define (h v) (+ (f v) (g v)))
(define h-def (make-defun 'h 'v
                          (make-add (make-fun 'f 'v) (make-fun 'g 'v))))

; A BSL-fun-def* is a [List-of BSL-fun-def]
; definitions areas as List-of Definitions
(define da-fgh (list f-def g-def h-def))

; Wish

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g-def)
(define (lookup-def da f)
  (cond
    [(empty? da) (error "No Such Definition")]
    ;; [else (first (filter (lambda (func) (symbol=? (defun-name func) f)) da))]
    [else (if (symbol=? (defun-name (first da)) f)
              (first da)
              (lookup-def (rest da) f))]))
