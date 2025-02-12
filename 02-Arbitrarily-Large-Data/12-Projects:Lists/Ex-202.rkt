#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-202) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/itunes)

; Exercise 202. Design select-album. The function consumes the title
; of an album and an LTracks. It extracts from the latter the list of
; tracks that belong to the given album.

; An LTracks is one of:
; – '()
; – (cons Track LTracks)

; String LTracks -> List-of-Strings
; interpretation: produce the list of songs of album (title) from the
; LTracks
(check-expect (select-album "A Day Without Rain" LT3)
              (list "Wild Child" "Only Time"))
(define (select-album al lt)
  (cond
    [(empty? lt) '()]
    [else (if (string=? al (track-album (first lt)))
              (cons (track-name (first lt)) (select-album al (rest lt)))
              (select-album al (rest lt)))]))


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
