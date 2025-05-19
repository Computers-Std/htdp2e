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

; BSL-var-expr Symbol Number -> BSL-var-expr
(check-expect (subst.v2 (make-add 3 'x) 'x 5) (make-add 3 5))
(check-expect (subst.v2 (make-mul 1/2 (make-mul 'x 5)) 'x 5) (make-mul 1/2 (make-mul 5 5)))
(check-expect (subst.v2 (make-add (make-mul 'x 'x) (make-mul 'y 'y)) 'y 5)
              (make-add (make-mul 'x 'x) (make-mul 5 5)))
(check-expect (subst.v2 (make-fun 'f (make-add 2 'y)) 'y 3) (make-fun 'f (make-add 2 3)))
(check-expect (subst.v2 (make-mul 5 (make-fun 'f2 (make-add 1 'k))) 'k 10)
              (make-mul 5 (make-fun 'f2 (make-add 1 10))))
(make-mul (make-fun 'i 5) (make-fun 'k (make-add 1 1)))

(define (subst.v2 ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst.v2 (add-left ex) x v) (subst.v2 (add-right ex) x v))]
    [(mul? ex) (make-mul (subst.v2 (mul-left ex) x v) (subst.v2 (mul-right ex) x v))]
    [(fun? ex) (make-fun (fun-name ex) (subst.v2 (fun-arg ex) x v))]))

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> [Maybe Number]
(check-expect (eval-definition1 (make-mul 5 (make-fun 'g (make-add 1 (make-fun 'g (make-mul 2 3)))))
                                'g
                                'x
                                (make-add 'x 2))
              (* 5 (+ (+ 1 (+ (* 2 3) 2)) 2)))

(define (eval-definition1 expr f-name f-arg f-body)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (error "Not NUMERIC")]
    [(add? expr)
     (+ (eval-definition1 (add-left expr) f-name f-arg f-body)
        (eval-definition1 (add-right expr) f-name f-arg f-body))]
    [(mul? expr)
     (* (eval-definition1 (mul-left expr) f-name f-arg f-body)
        (eval-definition1 (mul-right expr) f-name f-arg f-body))]
    [(fun? expr)
     (if (not (symbol=? (fun-name expr) f-name))
         (error "Invalid Function")
         ;; (local ((define value (eval-definition2 (fun-arg expr) f-name f-arg f-body))
         ;;         (define plugd-in (subst.v2 f-body f-arg value)))
         ;;   (eval-definition2 plugd-in f-name f-arg f-body))
         (eval-definition1
          (subst.v2 f-body f-arg (eval-definition1 (fun-arg expr) f-name f-arg f-body))
          f-name
          f-arg
          f-body))]))

; NOTE: this form of recursion (non-termination) was not covered
; before, and the proper design of such functions are discussed in
; Generative Recursion.
