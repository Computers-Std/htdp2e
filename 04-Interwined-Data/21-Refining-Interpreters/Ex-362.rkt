#lang htdp/isl+

;;; -- Constants --

(define ERROR-DEFCON "No such Constant defined")
(define ERROR-DEFUN "No such Function defined")
(define WRONG "Invalid Expression")

;;; -- Data Definitions --

(define-struct add [left right])
(define-struct mul [left right])

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-fun-expr)
; - (make-mul BSL-var-expr BSL-fun-expr)
; - (make-fun Symbol BSL-fun-expr)

(define-struct fun [name arg])
; A Fun is a Structure:
;   (make-fun Symbol BSL-fun-expr)
; data representation of a Function call with one argument

(define-struct defcon [name val])
; A BSL-con-def is a Structure:
;   (make-const Symbol Number)
; data representation of a Constant Definition, consisting NAME,
; VALUE

(define-struct defun [name pm body])
; A BSL-fun-def is a Structure:
;   (make-defun Symbol Symbol BSL-fun-expr)
; data representation of a Function Definition, consisting NAME,
; PARAMETER, BODY.

; A BSL-da-all is one of:
; - '()
; - (cons BSL-con-def BSL-da-all)
; - (cons BSL-fun-def BSL-da-all)

;;; -- Evaluation --

; BSL-da-all Symbol -> [Maybe BSL-con-def]
(define (lookup-con-def da x)
  (cond
    [(empty? da) (error ERROR-DEFCON)]
    [else
     (if (and (defcon? (first da)) (symbol=? (defcon-name (first da)) x))
         (first da)
         (lookup-con-def (rest da) x))]))

; BSL-da-all Symbol -> [Maybe BSL-fun-def]
(define (lookup-fun-def da x)
  (cond
    [(empty? da) (error ERROR-DEFUN)]
    [else
     (if (and (defun? (first da)) (symbol=? (defun-name (first da)) x))
         (first da)
         (lookup-fun-def (rest da) x))]))

; BSL-fun-expr Symbol Symbol -> BSL-fun-expr
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (equal? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v) (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v) (subst (mul-right ex) x v))]
    [(fun? ex) (make-fun (fun-name ex) (subst (fun-arg ex) x v))]))

; BSL-fun-expr BSL-da-all -> [Maybe Number]
(define (eval-all expr da)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (defcon-val (lookup-con-def da expr))]
    [(add? expr) (+ (eval-all (add-left expr) da) (eval-all (add-right expr) da))]
    [(mul? expr) (* (eval-all (mul-left expr) da) (eval-all (mul-right expr) da))]
    [(fun? expr)
     (local ;Find the definition in DA
         ((define find-def (lookup-fun-def da (fun-name expr)))
          ; Eval the Arg before passin into DA's find-defun
          (define eval-arg (eval-all (fun-arg expr) da))
          ; Substitute eval-arg in Function's PARAMETER
          (define subst-body (subst (defun-body find-def) (defun-pm find-def) eval-arg)))
       (eval-all subst-body da))]))

;;; -- Parsing --

; Any -> Boolean
(define (atom? n)
  (or (number? n) (symbol? n) (string? n)))

; Atom -> BSL-fun-expr
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(symbol? s) s]
    [(string? s) (error WRONG)]))

; S-expr SL -> [Maybe Number]
(check-expect (parse '(f 5)) (make-fun 'f 5))
(check-expect (parse '(f (+ 1 1))) (make-fun 'f (make-add 1 1)))
(check-expect (parse '(f (* 1 z))) (make-fun 'f (make-mul 1 'z)))
(define (parse sexp)
  (cond
    [(atom? sexp) (parse-atom sexp)]
    [else (parse-sl sexp)]))

; S-expr SL -> BSL-fun-expr
(define (parse-sl se)
  (if (and (or (= (length se) 2) (= (length se) 3)) (symbol? (first se)))
      (cond
        [(symbol=? (first se) '+) (make-add (parse (second se)) (parse (third se)))]
        [(symbol=? (first se) '*) (make-mul (parse (second se)) (parse (third se)))]
        [(symbol? (first se)) (make-fun (first se) (parse (second se)))]
        [else (error WRONG)])
      (error WRONG)))

;;; -- Interpreter --

(check-expect (interpreter '(f 3) da1) (eval-all (make-fun 'f 3) da1))
(check-expect (interpreter '(area-of-circle 1) da2) (eval-all (make-fun 'area-of-circle 1) da2))
(check-error (interpreter '(f 4) '()) ERROR-DEFUN)
(define (interpreter expr da)
  (eval-all (parse expr) da))

;;; -- Testing Area --

(define da1
  (list (make-defun 'f 'x (make-add 3 'x))
        (make-defun 'g 'y (make-fun 'f (make-mul 2 'y)))
        (make-defun 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v)))))
(define da2
  (list (make-defcon 'close-to-pi 3.14)
        (make-defcon 'ten 10)
        (make-defun 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r)))
        (make-defun 'volume-of-10-cylinder 'r (make-mul 'ten (make-fun 'area-of-circle 'r)))))
