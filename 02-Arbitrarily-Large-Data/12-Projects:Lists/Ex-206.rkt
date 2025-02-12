#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-206) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/itunes)
(require 2htdp/batch-io)

;; Exercise 206. Design the function find-association. It consumes
;; three arguments: a String called key, an LAssoc, and an element of
;; Any called default. It produces the first Association whose first
;; item is equal to key, or default if there is no such Association.

;; Note Read up on assoc after you have designed this function.

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

(define LASSOC5 (list (list "Track ID" 444)
                      (list "Name" "Only Time")
                      (list "Artist" "Enya")
                      (list "Album" "A Day Without Rain")
                      (list "Genre" "New Age")
                      (list "Kind" "MPEG audio file")
                      (list "Size" 4364035)
                      (list "Total Time" 218096)
                      (list "Track Number" 3)
                      (list "Track Count" 11)
                      (list "Year" 2000)
                      (list "Date Modified" (create-date 2002 7 17 0 0 21))
                      (list "Date Added" (create-date 2002 7 17 3 55 42))
                      (list "Bit Rate" 160)
                      (list "Sample Rate" 44100)
                      (list "Play Count" 18)
                      (list "Play Date" 3388484327)
                      (list "Play Date UTC" (create-date 2011 5 17 17 38 47))
                      (list "Sort Album" "Day Without Rain")
                      (list "Persistent ID" "EBBE9171392FA34A")
                      (list "Track Type" "File")
                      (list "Location" "file")
                      (list "File Folder Count" 4)
                      (list "Library Folder Count" 1)))

(define LASSOC6 (list (list "Track ID" 442)
                      (list "Name" "Wild Child")
                      (list "Artist" "Enya")
                      (list "Album" "A Day Without Rain")
                      (list "Genre" "New Age")
                      (list "Kind" "MPEG audio file")
                      (list "Size" 4562044)
                      (list "Total Time" 227996)
                      (list "Track Number" 2)
                      (list "Track Count" 11)
                      (list "Year" 2000)
                      (list "Date Modified" (create-date 2002 7 17 0 0 11))
                      (list "Date Added" (create-date 2002 7 17 3 55 14))
                      (list "Bit Rate" 160)
                      (list "Sample Rate" 44100)
                      (list "Play Count" 20)
                      (list "Play Date" 3388484113)
                      (list "Play Date UTC" (create-date 2011 5 17 17 35 13))
                      (list "Sort Album" "Day Without Rain")
                      (list "Persistent ID" "EBBE9171392FA348")
                      (list "Track Type" "File")
                      (list "Location" "file")
                      (list "File Folder Count" 4)
                      (list "Library Folder Count" 1)))

; LLists
(define LLIST1 '())
(define LLIST2 (list LASSOC1))
(define LLIST3 (list LASSOC1 LASSOC2))
(define LLIST4 (list LASSOC1 LASSOC2 LASSOC3))
(define LLIST5 (list LASSOC1 LASSOC2 LASSOC3 LASSOC4))

; String LAssoc Any -> Association
(check-expect (find-association "Artist" LASSOC4 "Not Found")
              (list "Artist" "Imagine Dragons"))
(check-expect (find-association "Track ID" LASSOC6 "Not Found")
              (list "Track ID" 442))
(check-expect (find-association "Name" LASSOC5 "Not Found")
              (list "Name" "Only Time"))
(check-expect (find-association "Runtime" LASSOC5 #false)
              #false)
(define (find-association key lassoc default)
  (cond
    [(empty? lassoc) default]
    [else
     (if (string=? key (first (first lassoc)))
         (first lassoc)
         (find-association key (rest lassoc) default))]))

(define ITUNES-LOCATION "files/itunes_small.xml")

; LLists
(define list-tracks (read-itunes-as-lists ITUNES-LOCATION))

;; (list (list (list "Track ID" 442)
;;             (list "Name" "Wild Child")
;;             (list "Artist" "Enya")
;;             (list "Album" "A Day Without Rain")
;;             (list "Genre" "New Age")
;;             (list "Kind" "MPEG audio file")
;;             (list "Size" 4562044)
;;             (list "Total Time" 227996)
;;             (list "Track Number" 2)
;;             (list "Track Count" 11)
;;             (list "Year" 2000)
;;             (list "Date Modified" (create-date 2002 7 17 0 0 11))
;;             (list "Date Added" (create-date 2002 7 17 3 55 14))
;;             (list "Bit Rate" 160)
;;             (list "Sample Rate" 44100)
;;             (list "Play Count" 20)
;;             (list "Play Date" 3388484113)
;;             (list "Play Date UTC" (create-date 2011 5 17 17 35 13))
;;             (list "Sort Album" "Day Without Rain")
;;             (list "Persistent ID" "EBBE9171392FA348")
;;             (list "Track Type" "File")
;;             (list "Location" "file")
;;             (list "File Folder Count" 4)
;;             (list "Library Folder Count" 1))
;;       (list (list "Track ID" 444)
;;             (list "Name" "Only Time")
;;             (list "Artist" "Enya")
;;             (list "Album" "A Day Without Rain")
;;             (list "Genre" "New Age")
;;             (list "Kind" "MPEG audio file")
;;             (list "Size" 4364035)
;;             (list "Total Time" 218096)
;;             (list "Track Number" 3)
;;             (list "Track Count" 11)
;;             (list "Year" 2000)
;;             (list "Date Modified" (create-date 2002 7 17 0 0 21))
;;             (list "Date Added" (create-date 2002 7 17 3 55 42))
;;             (list "Bit Rate" 160)
;;             (list "Sample Rate" 44100)
;;             (list "Play Count" 18)
;;             (list "Play Date" 3388484327)
;;             (list "Play Date UTC" (create-date 2011 5 17 17 38 47))
;;             (list "Sort Album" "Day Without Rain")
;;             (list "Persistent ID" "EBBE9171392FA34A")
;;             (list "Track Type" "File")
;;             (list "Location" "file")
;;             (list "File Folder Count" 4)
;;             (list "Library Folder Count" 1)))
