#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-197) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)
;; Exercise 197. Design most-frequent. The function consumes a
;; Dictionary. It produces the Letter-Count for the letter that occurs
;; most often as the first one in the given Dictionary.

;; What is the most frequently used letter in your computer’s
;; dictionary and how often is it used?

;; Note on Design Choices This exercise calls for the composition of
;; the solution to the preceding exercise with a function that picks
;; the correct pairing from a list of Letter-Counts. There are two
;; ways to design this latter function:

;; Design a function that picks the pair with the maximum count.
;; Design a function that selects the first from a sorted list of
;; pairs. Consider designing both. Which one do you prefer? Why?

(define LOC "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
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


;; Previous Exercise

; Dict -> List-of-LC
(check-expect (count-by-letter (list "sksjdf" "xcoix" "sdfj" "poiop"))
              (list (make-lcount "p" 1) (make-lcount "s" 2) (make-lcount "x" 1)))
(define (count-by-letter dict)
  (cond
    [(empty? dict) '()]
    [else (count-letter (string-ith (first dict) 0)
                        (count-by-letter (rest dict)))]))

; Letter List-of-LC -> List-of-LC
; add(if >0) or insert(if =0) the count of a letter to the list
(check-expect (count-letter "a" (list (make-lcount "a" 3) (make-lcount "b" 3)))
              (list (make-lcount "a" 4) (make-lcount "b" 3)))
(check-expect (count-letter "a" '())
              (cons (make-lcount "a" 1) '()))
(define (count-letter l lolc)
  (if (member? l LETTERS)
      (cond
        [(empty? lolc) (cons (make-lcount l 1) '())]
        [else (if (string=? l (lcount-letter (first lolc)))
                  (cons (make-lcount l (add1 (lcount-count (first lolc)))) (rest lolc))
                  (cons (first lolc) (count-letter l (rest lolc))))])
      lolc))

; Sort

;; 1. function that picks the pair with the maximum count.
;; List-of-LC -> Letter-count
(check-expect (pick-max-1 (list (make-lcount "a" 1) (make-lcount "b" 3) (make-lcount "c" 2)))
              (make-lcount "b" 3))
(define (pick-max-1 lolc)
  (cond
    [(empty? (rest lolc)) (first lolc)]
    [else (select-max (first lolc) (pick-max-1 (rest lolc)))]))

(define (select-max lc1 lc2)
  (if (>= (lcount-count lc1) (lcount-count lc2))
      lc1 lc2))

;; -----------------

; 2. function that selects the first from a sorted list of pairs.
; List-of-LC -> Letter-count
(check-expect (pick-max-2 (list (make-lcount "a" 1) (make-lcount "b" 3) (make-lcount "c" 2)))
              (make-lcount "b" 3))
(define (pick-max-2 lolc)
  (first (sort> lolc)))

; List-of-LC -> List-of-LC
(check-expect (sort> (list (make-lcount "b" 4) (make-lcount "c" 3) (make-lcount "a" 2)))
              (list (make-lcount "b" 4) (make-lcount "c" 3) (make-lcount "a" 2)))
(define (sort> lolc)
  (cond
    [(empty? lolc) '()]
    [else (insert (first lolc) (sort> (rest lolc)))]))

; LC List-of-LC -> List-of-LC
; inserts LC into the sorted List-of-LC
(check-expect (insert (make-lcount "a" 2) (list (make-lcount "b" 4) (make-lcount "c" 3)))
              (list (make-lcount "b" 4) (make-lcount "c" 3) (make-lcount "a" 2)))
(define (insert lc lolc)
  (cond
    [(empty? lolc) (cons lc '())]
    [else (if (>= (lcount-count lc) (lcount-count (first lolc)))
              (cons lc lolc)
              (cons (first lolc) (insert lc (rest lolc))))]))

; Dictionary -> Letter-count
;  produces letter that occurs most often as the first one in the given Dictionary.

;; 1.
(check-expect (most-freq-1 (list "sksjdf" "xcoix" "sdfj" "poiop"))
              (make-lcount "s" 2))
(define (most-freq-1 dict)
  (pick-max-1 (count-by-letter dict)))

;; 2.
(check-expect (most-freq-2 (list "sksjdf" "xcoix" "sdfj" "poiop"))
              (make-lcount "s" 2))
(define (most-freq-2 dict)
  (pick-max-2 (count-by-letter dict)))

;; (most-freq-2 AS-LIST) -> (make-lcount "s" 11857)
