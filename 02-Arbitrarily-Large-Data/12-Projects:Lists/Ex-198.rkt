#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-198) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)

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

; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

; A LLS is one of:
; - '()
; - (cons List-of-strings LLS)

; Letter-Counts
(define-struct lcount [letter count])
; (make-lcount l c)
; l -> 1String
; c -> Number
; interpretation: represents the letter and its count


; Dictionary -> List-of-dictionaries
; list-of-strings -> LLS
; consumes a Dictionary and produces a list of Dictionarys, one per
; Letter.
(check-expect (words-by-first-letter (list "apple" "ant" "bat" "ball" "cat" "cow" "dog" "duck"))
              (list (list "dog" "duck") (list "cat" "cow") (list "bat" "ball") (list "apple" "ant")))
(define (words-by-first-letter lls)
  (cond
    [(empty? lls) '()]
    [else (word-to-lls (first lls) (words-by-first-letter (rest lls)))]))


; String LLS -> LLS
; add String to the List-of-List-of-Strings (LLS)
(check-expect (word-to-lls "apple" (list (list "ant") (list "ball" "bat") (list "cat" "cow")))
              (list (list "apple" "ant") (list "ball" "bat") (list "cat" "cow")))
(check-expect (word-to-lls "dog" (list (list "ant") (list "ball" "bat") (list "cat" "cow")))
              (list (list "ant") (list "ball" "bat") (list "cat" "cow") (list "dog")))
(check-expect (word-to-lls "@" (list (list "ant") (list "ball" "bat") (list "cat" "cow") (list "dog")))
              (list (list "ant") (list "ball" "bat") (list "cat" "cow") (list "dog")))
(check-expect (word-to-lls "cat" '()) (list (list "cat")))
(define (word-to-lls str lls)
  (if (member? (string-0 str) LETTERS)
      (cond
        [(or
          (empty? lls)                 ; '()
          (empty? (first lls)))        ; (list (list '()))
         (list (list str))]
        [else (if (string=? (string-0 str) (string-0 (first (first lls))))
                  (cons (cons str (first lls)) (rest lls))
                  (cons (first lls) (word-to-lls str (rest lls))))])
      lls))

; String -> Letter
(define (string-0 str)
  (cond
    [(string=? "" str) (error "given empty string")]
    [else (string-ith str 0)]))

; Part-2

;; Redesign most-frequent from exercise 197 using this new function.
;; Call the new function most-frequent.v2. Once you have completed the
;; design, ensure that the two functions compute the same result on
;; your computer’s dictionary:

;; (check-expect
;;  (most-frequent AS-LIST)
;;  (most-frequent.v2 AS-LIST))

; Ex-197
;; -------------------------------
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
;; -------------------------------
; 2. function that selects the first from a sorted list of pairs.
; List-of-LC -> Letter-count
(check-expect (pick-max (list (make-lcount "a" 1) (make-lcount "b" 3) (make-lcount "c" 2)))
              (make-lcount "b" 3))
(define (pick-max lolc)
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
;; -------------------------------
;; 2.
; list-of-strings -> LC
(check-expect (most-freq (list "sksjdf" "xcoix" "sdfj" "poiop"))
              (make-lcount "s" 2))
(define (most-freq dict)
  (pick-max (count-by-letter dict)))

;; most-freq.v2

; List-of-strings -> LC

;; (words-by-first-letter (list "apple" "ant" "bat" "ball" "cat" "cow" "dog" "duck"))
;; (list (list "dog" "duck") (list "cat" "cow") (list "bat" "ball") (list "apple" "ant"))

(check-expect (most-freq.v2 (list "apple" "ant" "bat" "ball" "cat" "cow" "dog" "duck" "donkey"))
              (make-lcount "d" 3))
(define (most-freq.v2 los)
  (make-lcount (string-0 (first (bigL (words-by-first-letter los))))
               (length (bigL (words-by-first-letter los)))))

; List-of-LS -> LS
(check-expect (bigL
               (list (list "dog" "duck") (list "cat" "cow") (list "bat" "ball") (list "apple" "ant" "animal")))
              (list "apple" "ant" "animal"))
(define (bigL lls)
  (cond
    [(empty? (rest lls)) (first lls)]
    [else (max-of (first lls) (bigL (rest lls)))]))

; List-of-LS List-of-LS -> List-of-LS
; Biggest of both LS's
(check-expect (max-of (list "a" "b" "c" "d") (list 1 2 3 4 5)) (list 1 2 3 4 5))
(define (max-of ls1 ls2)
  (if (> (length ls1) (length ls2))
      ls1 ls2))

(define dict1 (list "apple" "ant" "animal" "bat" "ball" "cat" "cow" "dog" "duck"))
(check-expect (most-freq dict1) (most-freq.v2 dict1))

;; (check-expect (most-freq AS-LIST) (most-freq.v2 AS-LIST))
