#lang htdp/isl+

(define NEWLINE "\n")

; A File is one of:
; – '()
; – (cons "\n" File)
; – (cons 1String File)
; interpretation represents the content of a file
; "\n" is the newline character

; A Line is a [List-of 1String]

; File -> [List-of Line]
; converts a file into a list of lines
(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else (cons (first-line afile)
                (file->list-of-lines (remove-first-line afile)))]))

; File -> Line
; retrieves the prefix of afile up to the first occurence of NEWLINE
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))

; File -> File
; drops the prefix of afile upto the first occurence of NEWLINE
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))
