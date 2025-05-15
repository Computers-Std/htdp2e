#lang htdp/isl+

(define-struct add [left right])
; (make-add BSL-expr BSL-expr)

(define-struct mul [left right])
; (make-mul BSL-expr BSL-expr)

(define-struct eql [left right])

(define-struct bor [left right])
; (make-bor BSL-expr BSL-expr)

(define-struct band [left right])
; (make-band BSL-expr BSL-expr)

(define-struct bnot [val])
; (make-bnot BSL-expr)

(define (atom? n)
  (or (number? n) (string? n) (symbol? n) (boolean? n)))

(define WRONG "Invalid Expression")

;; -- Parsing --
; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr
(define (parse-sl s)
  (if (and (= (length s) 3) (symbol? (first s)))
      (cond
        [(symbol=? (first s) '+) (make-add (parse (second s)) (parse (third s)))]
        [(symbol=? (first s) '*) (make-mul (parse (second s)) (parse (third s)))]
        [(symbol=? (first s) '=) (make-eql (parse (second s)) (parse (third s)))]
        [(symbol=? (first s) 'or) (make-bor (parse (second s)) (parse (third s)))]
        [(symbol=? (first s) 'and) (make-band (parse (second s)) (parse (third s)))]
        [(symbol=? (first s) 'not) (make-bnot (parse (second s)) (parse (third s)))]
        [else (error WRONG)])
      (error WRONG)))

; Atom -> BSL-expr
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(boolean? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

(define (parse-bool s)
  (if (and (= (length s) 3) (symbol? (first s)))
      (cond
        [(symbol=? (first s) 'or) (make-bor (parse (second s)) (parse (third s)))]
        [else (error WRONG)])
      (error WRONG)))

; BSL-expr -> BSL-expr
(define (eval-expression bexp)
  (cond
    [(number? bexp) bexp]
    [(add? bexp) (+ (eval-expression (add-left bexp))
                    (eval-expression (add-right bexp)))]
    [(mul? bexp) (* (eval-expression (mul-left bexp))
                    (eval-expression (mul-right bexp)))]
    [(eql? bexp) (= (eval-expression (eql-left bexp))
                    (eval-expression (eql-right bexp)))]
    [(bor? bexp) (or (eval-expression (bor-left bexp))
                     (eval-expression (bor-right bexp)))]
    [(band? bexp) (and (eval-expression (band-left bexp))
                       (eval-expression (band-right bexp)))]
    [(bnot? bexp) (not (eval-expression bexp))]
    [else bexp]))

(define texp '(or #true (= 7 9)))

(check-expect (interpreter-expr '(or (= 2 0) (= 2 2))) #true)
;; (check-expect (interpreter-expr '(+ 12 23 23)) 58) ; What it takes to eval this
(check-expect (interpreter-expr '(+ 12 23)) 35)
(define (interpreter-expr sexp)
  (eval-expression (parse sexp)))
