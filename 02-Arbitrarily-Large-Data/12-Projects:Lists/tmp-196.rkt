#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tmp-196) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)

; A Dictionary is a List-of-strings.
(define LOC "/usr/share/dict/words")

(define AS-LIST (read-lines LOC))

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter-Counts
(define-struct lcount [letter count])
; (make-lcount l c)
; l -> 1String
; c -> Number
; interpretation: represents the letter and its count

; A List-of-LC is one of:
; - '()
; - (cons LC List-of-LC)
(check-expect (count-by-letter (list "sksjdf" "xcoix" "sdfj" "poiop"))
              (list (make-lcount "p" 1) (make-lcount "s" 2) (make-lcount "x" 1)))
; Dict -> List-of-LC
(define (count-by-letter dict)
  (cond
    [(empty? dict) '()]
    [else (count-letter (string-ith (first dict) 0)
                        (count-by-letter (rest dict)))]))

; Letter List-of-LC -> List-of-LC
(check-expect (count-letter "c" '()) (list (make-lcount "c" 1)))
(check-expect (count-letter "a" (list (make-lcount "c" 1)))
              (list (make-lcount "c" 1) (make-lcount "a" 1)))
(define (count-letter l lolc)
  (if (member? l LETTERS)
      (cond
        [(empty? lolc) (cons (make-lcount l 1) '())]
        [else (if (string=? l (lcount-letter (first lolc)))
                  (cons (make-lcount l (add1 (lcount-count (first lolc)))) (rest lolc))
                  (cons (first lolc) (count-letter l (rest lolc))))])
      lolc))

;; (count-by-letter AS-LIST)

;; This Exercise went over my head
;; I should do more practice
