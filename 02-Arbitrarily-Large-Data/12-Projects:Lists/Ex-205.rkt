#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-205) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/itunes)

; An LLists is one of:
; - '()
; - (cons LAssoc LLists)

; An LAssoc is one of:
; - '()
; - (cons Association LAssoc)

; An Association is a list of two items:
; (cons Srting (cons BSDN '()))

; A BSDN is one of:
; - Boolean
; - Number
; - String
; - Date

; String -> LLists
; creates a list of lists representation for all tracks in file-name,
; which must be an XML export from iTunes
;; (define (read-itunes-as-lists file-name)
;; ...)


;; Examples of
; Date
(define DATE1 (create-date 2020 2 20 12 0 0))
(define DATE2 (create-date 2020 5 15 23 15 30))
(define DATE3 (create-date 2019 12 5 1 20 44))
(define DATE4 (create-date 2020 6 2 5 15 4))
(define DATE5 (create-date 2020 6 7 8 8 3))

; LAssoc
(define LASSOC1 '())
(define LASSOC2 (list (list
                       (list "Track ID" 1)
                       (list "Name" "Joyful Sonata")
                       (list "Artist" "T. Nikolayeva")
                       (list "Album" "The Best of Piano")
                       (list "Total Time" 301641)
                       (list "Track Number" 44)
                       (list "Date Added" DATE1)
                       (list "Play Count" 77)
                       (list "Date Modified" DATE4))))

(define LASSOC3 (list (list "Track ID" 2)
                      (list "Name" "Believer")
                      (list "Artist" "Imagine Dragons")
                      (list "Album" "Evolve")
                      (list "Total Time" 204000)
                      (list "Track Number" 3)
                      (list "Date Added" DATE3)
                      (list "Play Count" 82)
                      (list "Date Modified" DATE2)))

(define LASSOC4 (list (list "Track ID" 3)
                      (list "Name" "Whatever It Takes")
                      (list "Artist" "Imagine Dragons")
                      (list "Album" "Evolve")
                      (list "Total Time" 201000)
                      (list "Track Number" 2)
                      (list "Date Added" DATE3)
                      (list "Play Count" 80)
                      (list "Date Modified" DATE5)))

; LLists
(define LLIST1 '())
(define LLIST2 (list LASSOC1))
(define LLIST3 (list LASSOC1 LASSOC2))
(define LLIST4 (list LASSOC1 LASSOC2 LASSOC3))
(define LLIST5 (list LASSOC1 LASSOC2 LASSOC3 LASSOC4))
