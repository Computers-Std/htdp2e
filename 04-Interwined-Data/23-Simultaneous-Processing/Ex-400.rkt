#lang htdp/isl+

(define dna0 '(a c g t h u))
(define dna1 '(a c g t h u))
(define dna2 '(a c g t h u x y q))
(define dna3 '(x y a c g t h u))

;; Design the function DNAprefix. The function takes two arguments,
;; both lists of 'a, 'c, 'g, and 't, symbols that occur in DNA
;; descriptions. The first list is called a pattern, the second one a
;; search string. The function returns #true if the pattern is
;; identical to the initial part of the search string; otherwise it
;; returns #false.

; [List-of Symbol] [List-of Symbol] -> [List-of Symbol]
; returns #true if the pattern is identical to the initial part of the
; search string; otherwise it returns #false
(check-expect (DNAprefix dna1 dna2) #true)
(check-expect (DNAprefix dna1 dna3) #false)
(check-expect (DNAprefix '(a) '()) #false)
(define (DNAprefix pattern search)
  (cond
    [(empty? pattern) #true]
    [(and (cons? pattern) (empty? search)) #false]
    [else (and (symbol=? (first pattern) (first search))
               (DNAprefix (rest pattern) (rest search)))]))


;; Also design DNAdelta. This function is like DNAprefix but returns
;; the first item in the search string beyond the pattern. If the
;; lists are identical and there is no DNA letter beyond the pattern,
;; the function signals an error. If the pattern does not match the
;; beginning of the search string, it returns #false. The function
;; must not traverse either of the lists more than once.

; [List-of Symbol] [List-of Symbol] -> [List-of Symbol]
(check-expect (DNAdelta dna1 dna2) 'x)
(check-error (DNAdelta dna1 dna0) "nothing beyond")
(check-expect (DNAdelta '(a) '()) #false)
(define (DNAdelta pattern search)
  (cond
    [(and (empty? pattern) (cons? search)) (first search)]
    [(empty? search)
     (if (cons? pattern)
         #false
         (error "nothing beyond"))]
    [else
     (if (symbol=? (first pattern) (first search))
         (DNAdelta (rest pattern) (rest search))
         #false)]))
