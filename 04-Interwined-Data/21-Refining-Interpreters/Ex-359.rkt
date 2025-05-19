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

(define f-def (make-defun 'f 'x (make-add 3 'x)))
(define g-def (make-defun 'g 'y (make-fun 'f (make-mul 2 'y))))
(define h-def (make-defun 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v))))

; A BSL-fun-def* is a [List-of BSL-fun-def]
; definitions areas as List-of Definitions
(define da-fgh (list f-def g-def h-def))

; BSL-var-expr Symbol Number -> BSL-var-expr
(check-expect (subst (make-add 3 'x) 'x 5) (make-add 3 5))
(check-expect (subst (make-mul 5 (make-fun 'f2 (make-add 1 'k))) 'k 10)
              (make-mul 5 (make-fun 'f2 (make-add 1 10))))
(make-mul (make-fun 'i 5) (make-fun 'k (make-add 1 1)))
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v) (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v) (subst (mul-right ex) x v))]
    [(fun? ex) (make-fun (fun-name ex) (subst (fun-arg ex) x v))]))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g-def)
(define (lookup-def da f)
  (cond
    [(empty? da) (error "No Such Function defined")]
    ;; [else (first (filter (lambda (func) (symbol=? (defun-name func) f)) da))]
    [else
     (if (symbol=? (defun-name (first da)) f)
         (first da)
         (lookup-def (rest da) f))]))

; BSL-fun-expr BSL-fun-def* -> [Maybe Number]
; Mimics Dr.Racket's Interaction Area
(check-expect (eval-function* (make-fun 'f 3) da-fgh) 6)
(check-expect (eval-function* (make-add (make-fun 'f -12) (make-fun 'g 3)) da-fgh) 0)
(check-error (eval-function* (make-add 3 (make-fun 'e 3)) da-fgh) "No Such Function defined")
(define (eval-function* expr da)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (error "Not NUMERIC")]
    [(add? expr) (+ (eval-function* (add-left expr) da) (eval-function* (add-right expr) da))]
    [(mul? expr) (* (eval-function* (mul-left expr) da) (eval-function* (mul-right expr) da))]
    [(fun? expr)
     (local ; Find the definition in DA
         ((define find-def (lookup-def da (fun-name expr)))
          ; Eval the Arg before passin into DA's definition
          (define eval-arg (eval-function* (fun-arg expr) da))
          ; Substitute eval-arg in Function's PARAMETER
          (define subst-body (subst (defun-body find-def) (defun-pm find-def) eval-arg)))
       (eval-function* subst-body da))]))
