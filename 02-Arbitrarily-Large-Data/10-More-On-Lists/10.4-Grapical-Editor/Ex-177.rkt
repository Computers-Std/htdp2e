#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-177) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Approach: Use structures that combine two lists of 1Strings
(define-struct editor [pre post])
; An Editor is a Structure:
; (make-editor Lo1S lo1S)

; An Lo1S is one of:
; - '()
; - (cons 1String Lo1S)

; String String -> Editor
;; consumes two strings and produces an Editor
(check-expect (create-editor "" "") (make-editor '() '()))
(check-expect (create-editor "a" "") (make-editor (cons "a" '()) '()))
(check-expect (create-editor "ab" "c") (make-editor (cons "a" (cons "b" '())) (cons "c" '())))

(define (create-editor sl sr)
  (make-editor (explode sl) (explode sr)))
