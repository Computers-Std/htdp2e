#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-174) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

(require 2htdp/batch-io)

;; CONSTANTS
(define PREPEND-NAME "encoded-")

;; Testing Area
(define ln1 (cons "one" (cons "is" (cons "the" (cons "one" '())))))
(define ln2 (cons "two" (cons "is" (cons "an" (cons "two" '())))))
(define ln3 (cons "three" (cons "is" (cons "a" (cons "three" '())))))
(define ln4 (cons "four" (cons "is" (cons "just" (cons "four" '())))))

(define lls1 '())
(define lls2 (cons ln1 (cons ln2 '())))
(define lls3 (cons ln2 (cons ln3 '())))
(define lls4 (cons ln3 (cons ln4 '())))

;; String -> String
;; Forms a new file name
(define (build-name n)
  (string-append PREPEND-NAME n))

; File -> File
(define (main f)
  (write-file
   (build-name f)
   (encode (read-words/line f))))

;; LLS -> String
;; converts the LLS into numeric string
(define (encode lls)
  (cond
    [(empty? lls) ""]
    [else (string-append (encode-line (first lls))
                         (encode (rest lls)))]))

; List-of-Strings -> String
; converts the List-of-Strings to numeric string
(define (encode-line los)
  (cond
    [(empty? los) ""]
    [else (string-append (encode-word (explode (first los)))
                         (if (empty? (rest los)) "\n" " ")
                         (encode-line (rest los)))]))

; String -> String
; converts a given string to a numeric string
(check-expect (encode-word (explode "apple")) "097112112108101")
(define (encode-word s)
  (cond
    [(empty? s) ""]
    [else (string-append (encode-letter (first s))
                         (encode-word (rest s)))]))

;; Pre-defined Functions : Fig 69
; 1String -> String
; converts the given 1String to a 3-letter numeric String
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10) (string-append "00" (code1 s))]
    [(< (string->int s) 100) (string-append "0" (code1 s))]))

; 1String -> String
; converts the given 1String into a String
(define (code1 c)
  (number->string (string->int c)))
