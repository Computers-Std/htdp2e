#lang htdp/isl+

(define (atom? n)
  (or (number? n) (string? n) (symbol? n)))

; An S-expr is one of:
; - Atom
; - SL

; An SL is one of:
; - '()
; - (cons S-expr SL)

; An Atom is one of:
; - Number
; - String
; - Symbol


;;; First Step
; New S-expr is one of:
; - Atom
; - [List-of S-expr]

; S-expr Symbol -> Number
; counts all occurences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(define (count sexp sy)
  (local ((define (traverse lsxp)
            (cond
              [(empty? lsxp) 0]
              [else (+ (count (first lsxp) sy)
                       (traverse (rest lsxp)))])))
    (cond
      [(atom? sexp)
       (if (and (symbol? sexp)
                (symbol=? sexp sy)) 1 0)]
      [else (traverse sexp)])))

;;; Second Step
(check-expect (count* 'world 'hello) 0)
(check-expect (count* '(world hello) 'hello) 1)
(check-expect (count* '(((world) hello) hello) 'hello) 2)
; S-expr Symbol -> Number
(define (count* sexp sy)
  (cond
    [(atom? sexp)
     (if (and (symbol? sexp)
              (symbol=? sexp sy)) 1 0)]
    [else (foldr (lambda (sx ls)
                   (+ (count* sx sy) ls)) 0 sexp)]))

; foldr can generate as many Args as requested
; lamda will consume as many Args as required
