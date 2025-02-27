
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-203) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/itunes)

; Exercise 203. Design select-album-date. The function consumes the
; title of an album, a date, and an LTracks. It extracts from the
; latter the list of tracks that belong to the given album and have
; been played after the given date. Hint You must design a function
; that consumes two Dates and determines whether the first occurs
; before the second

; String Number LTracks -> List-of-Strings
;; (check-expect (select-album-date "Wild Child" DATE3 LT2))
(check-expect (select-album-date "A Day Without Rain" DATE3 LT3)
              (list "Wild Child" "Only Time"))
(define (select-album-date al dt lt)
  (cond
    [(empty? lt) '()]
    [else (if (and (string=? al (track-album (first lt)))
                   (isBefore? dt (track-played (first lt))))
              (cons (track-name (first lt)) (select-album-date al dt (rest lt)))
              (select-album-date al dt (rest lt)))]))

; Number Number -> Boolean
; Date Date -> Boolean
; consumes two Dates and determines whether the first occurs before
; the second
(check-expect
 (isBefore? (create-date 2012 1 23 4 30 2) (create-date 2011 1 22 6 07 22)) #false)
(check-expect
 (isBefore? (create-date 2011 1 22 6 07 22) (create-date 2012 1 23 4 30 2)) #true)
(define (isBefore? d1 d2)
  (cond
    [(< (date-year d1) (date-year d2)) #true]
    [(= (date-year d1) (date-year d2))
     (cond
       [(< (date-month d1) (date-month d2)) #true]
       [(= (date-month d1) (date-month d2))
        (cond
          [(< (date-day d1) (date-day d2)) #true]
          [(= (date-day d1) (date-day d2))
           (cond
             [(< (time2sec d1) (time2sec d2)) #true]
             [else #false])]
          [else #false])]
       [else #false])]
    [else #false]))

; Number -> Number
; Time -> Seconds
; Hour Minute Seconds -> Seconds
(check-expect (time2sec (create-date 2012 1 23 4 30 2)) 16202)
(check-expect (time2sec (create-date 2011 1 22 6 07 22)) 22042)
(define (time2sec t)
  (+ (* 60 (* 60 (date-hour t)))
     (* 60 (date-minute t))
     (date-second t)))

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
(define TRACK3
  (create-track "The Soul of Appanna" "S. Thaman" "Game Changer" 11560 1
                (create-date 2025 1 10 4 50 44) 10 (create-date 2025 2 12 01 49 00)))
; An LTracks is one of:
; – '()
; – (cons Track LTracks)

; Example LTracks
(define LT1 '())
(define LT2 (cons TRACK2 '()))
(define LT3 (list TRACK1 TRACK2))
