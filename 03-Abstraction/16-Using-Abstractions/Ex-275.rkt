#lang htdp/isl
(require 2htdp/batch-io)

;; Exercise 275. Real-World Data: Dictionaries deals with relatively
;; simple tasks relating to English dictionaries. The design of two of
;; them just call out for the use of existing abstractions:

;; Design most-frequent. The function consumes a Dictionary and
;; produces the Letter-Count for the letter that is most frequently
;; used as the first one in the words of the given Dictionary.

;; Design words-by-first-letter. The function consumes a Dictionary
;; and produces a list of Dictionarys, one per Letter. Do not include
;; '() if there are no words for some letter; ignore the empty
;; grouping instead.

(define LOC "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOC))

; Letters a-z
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter-Counts
(define-struct lcount [letter count])
; (make-lcount l c)
; l -> 1String
; c -> Number
; interpretation: represents the letter and its count

(define (string-0 str)
  (if (string=? "" str)
      (error "given empty string")
      (string-ith str 0)))

(define (word-by-first-letter lls)
  (local ((define (word->lls str lols)
            (local ((define word+lols
                      (local ((define (to-map n)
                                (cond
                                  [(string=? (string-0 str) (string-0 (first n))) (cons str n)]
                                  [else n])))
                        (map to-map lols)))
                    ; String LOLS -> Boolean
                    (define ifFamPresent?
                      (local ((define (same-fam nl)
                                (string=? (string-0 str) (string-0 (first nl)))))
                        (ormap same-fam lols))))
              (if (member? (string-0 str) LETTERS)
                  (if ifFamPresent?
                      word+lols
                      (append lols (list (list str))))
                  lols))))
    (foldr word->lls '() lls)))

(check-expect (most-freq (list "apple" "ant" "bat" "ball" "cat" "cow" "dog" "duck" "donkey"))
              (make-lcount "d" 3))
(define (most-freq lls)
  (local ((define (bigL lols)
            (foldr max-of '() lols))
          (define (max-of l1 l2)
            (if (> (length l1) (length l2)) l1 l2))
          (define big-list (bigL (word-by-first-letter lls))))
    (make-lcount (string-0 (first big-list)) (length big-list))))

(check-expect (most-f (list "apple" "ant" "bat" "ball" "cat" "cow" "dog" "duck" "donkey"))
              (make-lcount "d" 3))
(define (most-f lls)
  (local ((define (dict->lcount lls)
            (make-lc
             ount (string-0 (first lls)) (length lls))))
    (argmax lcount-count (map dict->lcount (word-by-first-letter lls)))))

; Better Alternative
; Copied

(define (word-by-first-letter.v2 ls)
  (local (; String -> Boolean
          (define (in-letters? i)
            (member? (string-0 i) LETTERS))
          ; String [List-of String] -> [List-of Dictionary]
          (define (group x y)
            (cond
              [(empty? y) (list (list x))]
              [else (if (string=? (string-0 x) (string-0 (first (first y))))
                        (cons (cons x (first y)) (rest y))
                        (cons (cons x '()) y))])))
    (foldr group '() (filter in-letters? ls))))

(check-expect (most-freq.v2 (list "apple" "ant" "bat" "ball" "cat" "cow" "dog" "duck" "donkey"))
              (make-lcount "d" 3))
(define (most-freq.v2 d)
  (local (;; Dictionary -> LC
          (define (dict->lc d)
            (make-lcount (string-0 (first d)) (length d))))
    (argmax lcount-count (map dict->lc (word-by-first-letter.v2 d)))))


(define dict1 (list "apple" "ant" "animal" "bat" "ball" "cat" "cow" "dog" "duck"))
(most-freq dict1)
;; (most-freq AS-LIST) => (make-lcount "s" 11857)
