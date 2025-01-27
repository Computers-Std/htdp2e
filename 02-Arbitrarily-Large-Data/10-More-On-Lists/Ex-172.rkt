#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-172) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)
(require racket/string)

;; Testing
(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))
(define lls2 (cons line0 (cons line0 '())))

; Exercise 172. Design the function collapse, which converts a list of
; lines into a string. The strings should be separated by blank spaces
; (" "). The lines should be separated with a newline ("\n").

; List-of-list-of-string -> String
; converts a list of lines into a string
(check-expect (collapse lls0) "")
(check-expect (collapse lls1) "hello world\n")
(check-expect (collapse (cons line0 '())) "hello world\n")
(check-expect (collapse lls2) "hello world\nhello world\n")

(define (collapse lls)
  (cond
    [(empty? lls) ""]
    [else (string-append (line-str (first lls))
                         (collapse (rest lls)))]))

;; (check-expect (collapse (read-words/line "poem.txt")) (read-file "poem.txt"))

; List-of-strings -> String
(define (line-str los)
  (cond
    [(empty? los) ""]
    [else (string-append (first los)
                         (if (empty? (rest los)) "\n" " ")
                         (line-str (rest los)))]))
(check-expect (line-str line0) "hello world\n")
(check-expect (line-str line1) "")

(write-file "poem.dat"
            (collapse (read-words/line "poem.txt")))
