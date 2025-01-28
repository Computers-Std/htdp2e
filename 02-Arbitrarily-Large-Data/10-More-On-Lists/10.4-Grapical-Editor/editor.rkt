#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Approach: Use structures that combine two lists of 1Strings
(define-struct editor [pre post])
; An Editor is a Structure:
; (make-editor Lo1S lo1S)

; An Lo1S is one of:
; - '()
; - (cons 1String Lo1S)

;; -- Examples
(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))

;; data example 1:
;; (make-editor all good)
;; data example 2:
;; (make-editor lla good)


; Lo1S -> Lo1S
; produces a reverse version of the given list
(check-expect (rev (cons "a" (cons "b" (cons "c" '()))))
              (cons "c" (cons "b" (cons "a" '()))))
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-to-end (rev (rest l))
                      (first l))]))

; Lo1S 1String -> Lo1S
; create a new list by adding s to the end of l
(check-expect (add-to-end (cons "c" (cons "b" '())) "a")
              (cons "c" (cons "b" (cons "a" '()))))

(define (add-to-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l)
                (add-to-end (rest l) s))]))

