#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-175) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)

(define-struct info [lines words chars])
;; An Info is a structure
;;  (make-info Number Number Number)
;; (make-info l w c) represents an information about a file
;; with l lines, w words, and c characters.

;; Testing Area
(define line1 (cons "one" (cons "is" (cons "a" (cons "number" '())))))
(define ln1 (cons "one" (cons "is" (cons "the" (cons "one" '())))))
(define ln2 (cons "two" (cons "is" (cons "an" (cons "two" '())))))
(define ln3 (cons "three" (cons "is" (cons "a" (cons "three" '())))))
(define ln4 (cons "four" (cons "is" (cons "just" (cons "four" '())))))

(define lls1 '())
(define lls2 (cons ln1 (cons ln2 '())))
(define lls3 (cons ln2 (cons ln3 '())))
(define lls4 (cons ln3 (cons ln4 '())))
(define lls5 (cons ln3 (cons ln4 (cons line1 '()))))

;; list-of-strings -> Number (1String)
;; 1String count in list-of-strings
(define (1string-count ln)
  (cond
    [(empty? ln) 0]
    [else (+ 1 ; space between words
             (string-length (first ln))
             (1string-count (rest ln)))]))

;; LLS -> Number (1String)
;; 1String count in LLS
(define (1string-in-lines lls)
  (cond
    [(empty? lls) 0]
    [else (+ (if (empty? (first lls)) 1 0) ; empty line
             (1string-count (first lls))
             (1string-in-lines (rest lls)))]))

;; list-of-strings -> Number (words)
;; count the number of words in a line
(define (word-count ln)
  (cond
    [(empty? ln) 0]
    [else (add1 (word-count (rest ln)))]))

;; LLS -> Number (words)
;; count the number of words in LLS
(define (words-in-lines lls)
  (cond
    [(empty? lls) 0]
    [else (+ (word-count (first lls))
             (words-in-lines (rest lls)))]))

;; LLS (File) -> Number (Lines)
;; counts No. of line in a LLS
(define (lines-in-file lls)
  (cond
    [(empty? lls) 0]
    [else (add1 (lines-in-file (rest lls)))]))

;; Main
;; File -> (Chars, Words, Lines)
(define (main f)
  (make-info
   (lines-in-file (read-words/line f))
   (words-in-lines (read-words/line f))
   (1string-in-lines (read-words/line f))))

; (main "poem.txt")
