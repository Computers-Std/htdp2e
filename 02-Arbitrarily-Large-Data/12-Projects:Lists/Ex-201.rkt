#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-201) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/itunes)

;; Exercise 201. Design select-all-album-titles. The function consumes
;; an LTracks and produces the list of album titles as a
;; List-of-strings.

; LTracks -> List-of-String
;; interpretation: consumes an LTracks and produces the list of album
;; titles as a List-of-strings.
(define (select-all-album-titles lt)
  (cond
    [(empty? lt) '()]
    [else (cons (track-album (first lt))
                (select-all-album-titles (rest lt)))]))

; List-of-Strings -> List-of-Strings
(check-expect (create-set (list "one" "two" "one" "three" "two" "three"))
              (list "one" "two" "three"))
(define (create-set los)
  (cond
    [(empty? los) '()]
    [else (if (member? (first los) (rest los))
              (create-set (rest los))
              (cons (first los) (create-set (rest los))))]))

; LTracks -> List-of-Strings
(check-expect (select-album-titles/unique LT3) (list "A Day Without Rain"))
(define (select-album-titles/unique lt)
  (create-set (select-all-album-titles lt)))


;; Testing Area
; Example Dates
(define DATE1 (create-date 2002 7 17 3 55 14))
(define DATE2 (create-date 2011 5 17 17 35 13))
(define DATE3 (create-date 2002 7 17 3 55 42))
(define DATE4 (create-date 2011 5 17 17 38 47))

; Example Tracks
(define TRACK1
  (create-track "Wild Child" "Enya" "A Day Without Rain" 227996 2 DATE1 20 DATE2))
(define TRACK2
  (create-track "Only Time" "Enya" "A Day Without Rain" 218096 3 DATE3 18 DATE4))

; An LTracks is one of:
; – '()
; – (cons Track LTracks)

; Example LTracks
(define LT1 '())
(define LT2 (cons TRACK2 '()))
(define LT3 (list TRACK1 TRACK2))
