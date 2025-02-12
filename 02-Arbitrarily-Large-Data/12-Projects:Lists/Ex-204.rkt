#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-204) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/itunes)

;; Exercise 204. Design select-albums. The function consumes an
;; element of LTracks. It produces a list of LTracks, one per album.
;; Each album is uniquely identified by its title and shows up in the
;; result only once. Hints (1) You want to use some of the solutions
;; of the preceding exercises. (2) The function that groups consumes
;; two lists: the list of album titles and the list of tracks; it
;; considers the latter as atomic until it is handed over to an
;; auxiliary function. See exercise 196.

(check-expect
 (select-albums LT4)
 (list (create-track "Only Time" "Enya" "A Day Without Rain" 218096 3
                     (create-date 2002 7 17 3 55 42) 18 (create-date 2011 5 17 17 38 47))
       (create-track "The Soul of Appanna" "S. Thaman" "Game Changer" 11560
                     1 (create-date 2025 1 10 4 50 44) 10 (create-date 2025 2 9 9 36 0))))
(check-expect
 (select-albums LT2)
 (list (create-track "Only Time" "Enya" "A Day Without Rain" 218096 3
                     (create-date 2002 7 17 3 55 42) 18 (create-date 2011 5 17 17 38 47))))
; LTracks -> List-of-LTracks
(define (select-albums lt)
  (cond
    [(empty? lt) '()]
    [else (if (album-repeat? (first lt) (rest lt))
              (select-albums (rest lt))
              (cons (first lt) (select-albums (rest lt))))]))

; Track LTracks -> Boolean
; If Track-album is repeated again in the LTracks list
(check-expect (album-repeat? TRACK1 LT4) #true)
(check-expect (album-repeat? TRACK3 LT2) #false)
(define (album-repeat? t lt)
  (cond
    [(empty? lt) #false]
    [else (if (album=? t (first lt))
              #true
              (album-repeat? t (rest lt)))]))

; Track Track -> Boolean
; Are the tracks belong to same album
(check-expect (album=? TRACK1 TRACK2) #true)
(check-expect (album=? TRACK1 TRACK3) #false)
(define (album=? t1 t2)
  (string=? (track-album t1) (track-album t2)))


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
                (create-date 2025 1 10 4 50 44) 10 (create-date 2025 2 09 09 36 00)))
; An LTracks is one of:
; – '()
; – (cons Track LTracks)

; Example LTracks
(define LT1 '())
(define LT2 (cons TRACK2 '()))
(define LT3 (list TRACK1 TRACK2))
(define LT4 (list TRACK1 TRACK2 TRACK3))
