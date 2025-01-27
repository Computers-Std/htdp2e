#reader(lib "htdp-beginner-reader.ss" "lang")((modname fig-68) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)

; An LN is one of:
; – '()
; – (cons Los LN)
; interpretation a list of lines, each is a list of Strings

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())

(define ln0 '())
(define ln1 (cons line0 (cons line1 '())))

; LN -> List-of-numbers
; determines the number of words on each line

(check-expect (words-on-line ln0) '())
(check-expect (words-on-line ln1) (cons 2 (cons 0 '())))

(define (words-on-line ln)
  (cond
    [(empty? ln) '()]
    [else (cons (length (first ln))
                (words-on-line (rest ln)))]))

;; ---
; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
   (read-words/line file-name)))

(file-statistic "poem.txt")
