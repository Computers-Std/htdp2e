#lang htdp/isl+

(define (atom? n)
  (or (number? n) (string? n) (symbol? n)))

;; --- Old
(define (old-substitute sexp old new)
  (local ((define (atom? n)
            (or (number? n) (string? n) (symbol? n)))
          (define (sub-sl sl)
            (cond
              [(empty? (rest sl))
               (cons (first sl) '())]
              [else (cons (old-substitute (first sl) old new)
                          (sub-sl (rest sl)))])))
    (cond
      [(and (atom? sexp)
            (symbol=? sexp old)) new]
      [else (sub-sl sexp)])))

; S-expr Symbol Symbol -> S-expr
; replaces Symbol(old) with Symbol(new) in S-expr
(check-expect (substitute.v2 'hello 'hello 'emacs) 'emacs)
(check-expect (substitute.v2 '(hello world) 'hello 'world)
              (list 'world 'world))
(check-expect (substitute.v2 '(hello hello world) 'hello 'hi)
              (list 'hi 'hi 'world))

(define (substitute.v2 sexp old new)
  (local (; S-expr -> S-expr
          (define (for-sexp sexp)
            (cond
              [(atom? sexp) (for-atom sexp)]
              [else (for-sl sexp)]))
          ; SL -> S-expr
          (define (for-sl sl)
            (map for-sexp sl))
          ; Atom -> S-expr
          (define (for-atom at)
            (if (equal? at old) new at)))
    (for-sexp sexp)))

(check-expect (substitute.v3 'hello 'hello 'emacs) 'emacs)
(check-expect (substitute.v3 '(hello world) 'hello 'world)
              (list 'world 'world))
(check-expect (substitute.v3 '(hello hello world) 'hello 'hi)
              (list 'hi 'hi 'world))
(define (substitute.v3 sexp old new)
  (local (; S-expr -> S-expr
          (define (for-sexp sexp)
            (cond
              [(atom? sexp) (if (equal? sexp old) new sexp)]
              [else (map for-sexp sexp)])))
    (for-sexp sexp)))

; A we left with one local function which work with the only major argument
; we suffice it as main function

(check-expect (substitute.v4 'hello 'hello 'emacs) 'emacs)
(check-expect (substitute.v4 '(hello world) 'hello 'world)
              (list 'world 'world))
(check-expect (substitute.v4 '(hello hello world) 'hello 'hi)
              (list 'hi 'hi 'world))
(define (substitute.v4 sexp old new)
  (cond
  [(atom? sexp) (if (equal? sexp old)
                    new sexp)]
  [else (map (lambda (s) (substitute.v4 s old new)) sexp)]))

; we use lambda because it acts on each item of 'sexp' instead of the
; major arg "sexp" as whole.
