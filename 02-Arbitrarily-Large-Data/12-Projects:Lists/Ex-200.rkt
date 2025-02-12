#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-200) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/itunes)

;; Exercise 200. Design the function total-time, which consumes an
;; element of LTracks and produces the total amount of play time. Once
;; the program is done, compute the total play time of your iTunes
;; collection.

; An LTracks is one of:
; – '()
; – (cons Track LTracks)

; (create-track "Wild Child" "Enya" "A Day Without Rain" 227996 2 DATE1 20 DATE2)
; LTracks -> Time (* (track-time lt) (track-play# lt))
(define (total-time lt)
  (cond
    [(empty? lt) 0]
    [else (+ (* (track-time (first lt)) (track-play# (first lt)))
             (total-time (rest lt)))]))

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
