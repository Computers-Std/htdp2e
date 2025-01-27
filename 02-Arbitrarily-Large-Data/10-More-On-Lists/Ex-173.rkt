#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-173) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)
(require racket/string)


;; (read-words/line "articles.txt")

(define PREPEND-NAME "no-articles-")
(define ARTICLES (cons "a" (cons "an" (cons "the" '()))))

; String -> String
; interpretation: returns if that string is not an article
(define (isArt? s)
  (cond
    [(or (string=? s "a")
         (string=? s "an")
         (string=? s "the")) #true]
    [else #false]))

;; ;; String -> String
;; (define (for-Art s)
;;   (cond
;;     [(or (string=? s "a")
;;          (string=? s "an")
;;          (string=? s "the")) ""]
;;     [else s]))

;; ; File -> List-of-strings -> List-of-strings
;; ; interpretation
;; (define (rm-art ls)
;;   (cond
;;     [(empty? ls) '()]
;;     [else (cons (if (isArt? (first ls))
;;                     ""
;;                     (first ls))
;;                    (rm-art (rest ls)))]))

;; (rm-art (cons "a" (cons "an" (cons "the" (cons "ana" (cons "hello" '()))))))

;; Testing Area
(define ln1 (cons "one" (cons "is" (cons "the" (cons "one" '())))))
(define ln2 (cons "two" (cons "is" (cons "an" (cons "two" '())))))
(define ln3 (cons "three" (cons "is" (cons "a" (cons "three" '())))))
(define ln4 (cons "four" (cons "is" (cons "just" (cons "four" '())))))

(define lls1 '())
(define lls2 (cons ln1 (cons ln2 '())))
(define lls3 (cons ln2 (cons ln3 '())))
(define lls4 (cons ln3 (cons ln4 '())))

;; -- Functions

;; String -> String
;; saves the file with the text from file 'n'
;; having all articles removed
(define (main n)
  (write-file
   (build-name n)
   (collapse (read-words/line n))))

;; String -> String
;; Forms a new file name
(define (build-name n)
  (string-append PREPEND-NAME n))

;; LLS -> String
;; converts a list of lines into a string
(define (collapse lls)
  (cond
    [(empty? lls) ""]
    [else (string-append (collapse-line (first lls))
                         (collapse (rest lls)))]))

;; List-of-string -> String
;; Collapses the words - except articles - on the list ln.
(define (collapse-line ln)
  (cond
    [(empty? ln) ""]
    [else (string-append
           (if (isArt? (first ln)) "" (first ln))
           (if (empty? (rest ln)) "\n" (if (isArt? (first ln)) "" " "))
           (collapse-line (rest ln)))]))
