#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-213) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 213. Design insert-everywhere/in-all-words. It consumes a
;; 1String and a list of words. The result is a list of words like its
;; second argument, but with the first argument inserted at the
;; beginning, between all letters, and at the end of all words of the
;; given list.

;; Start with a complete wish-list entry. Supplement it with tests for
;; empty lists, a list with a one-letter word, and another list with a
;; two-letter word, and the like. Before you continue, study the
;; following three hints carefully.

; A Word is one of:
; '()
; (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; 1String Word Word -> List-of-words
; interpretation: Letter + (Pre Post (Word))
; in-ew -> insert-everywhere
(check-expect (in-ew "d" '() (list "e" "r"))
              (list (list "d" "e" "r") (list "e" "d" "r") (list "e" "r" "d")))
(check-expect (in-ew "c" '() (list "a" "t"))
              (list (list "c" "a" "t") (list "a" "c" "t") (list "a" "t" "c")))
(define (in-ew l pre post)
  (cond
    [(empty? post) (list (append pre (list l)))]
    [else
     (append (list (append pre (list l) post))
             (in-ew l (append pre (list (first post))) (rest post)))]))

; List-of-words -> List-of-words
; like in-ew for a List-of-words
; in-ew-iaw -> insert-everywhere-in-all-words
(check-expect (in-ew-iaw "d" (cons (list "e" "r") (cons (list "r" "e") '())))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")
                    (list "d" "r" "e")
                    (list "r" "d" "e")
                    (list "r" "e" "d")))
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
