#lang htdp/isl+

(define-struct add [left right])
; (make-add BSL-expr BSL-expr)

(define-struct mul [left right])
; (make-mul BSL-expr BSL-expr)

(define (atom? n)
  (or (number? n) (string? n) (symbol? n)))

(define WRONG "Invalid Expression")

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr
(define (parse-sl s)
  (if (and (= (length s) 3) (symbol? (first s)))
      (cond
        [(symbol=? (first s) '+)
         (make-add (parse (second s)) (parse (third s)))]
        [(symbol=? (first s) '*)
         (make-mul (parse (second s)) (parse (third s)))]
        [else (error WRONG)])
      (error WRONG)))

; Atom -> BSL-expr
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

;; S-expr -> BSL-expr
(check-expect (parse 10) 10)
(check-error (parse '(+ 10)) WRONG)
(check-expect (parse '(+ 1 2)) (make-add 1 2))
(check-expect (parse '(* 1 2)) (make-mul 1 2))
(check-error (parse '(^ 1 2)) WRONG)
(check-expect (parse '(+ (* 30 13) (* 2 9)))
              (make-add (make-mul 30 13) (make-mul 2 9)))
