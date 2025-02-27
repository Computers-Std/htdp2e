#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-214) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)

;; Variables
(define LOC "/usr/share/dict/words")
(define DICTIONARY (read-lines LOC))

;; Data Definitions

; A Word is one of:
; '()
; (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A Low is one of:
; '()
; (cons Word List-of-words)


;; Functions

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(check-expect (in-dictionary (list "hello" "cat" "tac")) (list "hello" "cat"))
(check-expect (in-dictionary (list "world" "")) (list "world"))
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else (if (member? (first los) DICTIONARY)
              (cons (first los) (in-dictionary (rest los)))
              (in-dictionary (rest los)))]))


; 1String Word Word -> List-of-words
; interpretation: Letter + (Pre Post (Word))
; in-ew -> insert-everywhere
(check-expect (in-ew "d" '() (list "e" "r"))
              (list (list "d" "e" "r") (list "e" "d" "r") (list "e" "r" "d")))
(define (in-ew l pre post)
  (cond
    [(empty? post) (list (append pre (list l)))]
    [else
     (append (list (append pre (list l) post))
             (in-ew l (append pre (list (first post))) (rest post)))]))

; List-of-words -> List-of-words
; like in-ew for a List-of-words
; in-ew-iaw -> insert-everywhere-in-all-words
(check-expect (in-ew-iaw "c" (list (list "t" "a") (list "a" "t")))
              (list (list "c" "t" "a")
                    (list "t" "c" "a")
                    (list "t" "a" "c")
                    (list "c" "a" "t")
                    (list "a" "c" "t")
                    (list "a" "t" "c")))
(define (in-ew-iaw l low)
  (cond
    [(empty? low) '()]
    [else (append (in-ew l '() (first low)) (in-ew-iaw l (rest low)))]))

;; ----- Auxiliary Funtions (from Ex-179)
; Lo1S -> Lo1S
; produces a reverse version of the given list
(check-expect (rev (cons "a" (cons "b" (cons "c" '())))) (cons "c" (cons "b" (cons "a" '()))))
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-to-end (rev (rest l)) (first l))]))

; Lo1S 1String -> Lo1S
; create a new list by adding s to the end of l
(check-expect (add-to-end (cons "c" (cons "b" '())) "a") (cons "c" (cons "b" (cons "a" '()))))

(define (add-to-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l) (add-to-end (rest l) s))]))

;; Only 3 Letter Word Arrangements supported
(check-expect (arrangements (list "c" "a" "t"))
              (list (list "c" "a" "t")
                    (list "a" "c" "t")
                    (list "a" "t" "c")
                    (list "c" "t" "a")
                    (list "t" "c" "a")
                    (list "t" "a" "c")))
(define (arrangements word)
  (in-ew-iaw (first word) (list (rest word) (rev (rest word)))))

; Word -> String
; converts w to a string
(check-expect (word->string (list "h" "e" "l" "l" "o")) "hello")
(define (word->string w)
  (cond
    [(empty? w) ""]
    [else (string-append (first w) (word->string (rest w)))]))

; List-of-words -> List-of-strings
; turns all Words in low into Strings
(check-expect (words->strings
               (list (explode "hello") (explode "world")))
              (list "hello" "world"))
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low))
                (words->strings (rest low)))]))

; String -> Word
(check-expect (string->word "hello") (list "h" "e" "l" "l" "o"))
(define (string->word s)
  (explode s))

;; Application
;; Given a word, find all words that are made up from the same letters
;; (check-satisfied (alt-words "dear") all-words-from-dear?)
(check-satisfied (alt-words "rat") all-words-from-rat?)
(check-satisfied (alt-words "cat") all-words-from-cat?)
(define (alt-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

; Test Functions
; List-of-Strings -> Boolean
(define (all-words-from-dear? w)
  (and
   (member? "read" w)
   (member? "dear" w)
   (member? "dare" w)))

; List-of-Strings -> Boolean
(define (all-words-from-rat? w)
  (and
   (member? "rat" w)
   (member? "art" w)
   (member? "tar" w)))
; List-of-Strings -> Boolean
(define (all-words-from-cat? w)
  (and
   (member? "cat" w)
   (member? "act" w)))
