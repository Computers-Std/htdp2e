#lang htdp/isl
(require 2htdp/itunes)

;; Exercise 276. Real-World Data: iTunes explains how to analyze the
;; information in an iTunes XML library.

;; Design select-album-date. The function consumes the title of an
;; album, a date, and an LTracks. It extracts from the latter the list
;; of tracks from the given album that have been played after the
;; date.

;; Design select-albums. The function consumes an LTracks. It produces
;; a list of LTracks, one per album. Each album is uniquely identified
;; by its title and shows up in the result only once.

;; See figure 77 for the services provided by the 2htdp/itunes
;; teachpack.

; ----- select-album-date
;; NOTE: I misread the question and implemented with extracting the
;; titles of relevant-tracks instead of producing the list as it is.

; String Number LTracks -> [List-of String]
; Album Number LTracks -> [List-of String]
; extracts the list of tracks that belong to the given album and have
; been played after the given date
(check-expect (select-album-date "A Day Without Rain" DATE3 LT3) (list "Wild Child" "Only Time"))
(define (select-album-date al dt lt)
  (local ;; Time -> Number
      ((define (time->sec t)
         (+ (* 3600 (date-hour t)) (* 60 (date-minute t)) (date-second t)))
       ; Date Date -> Boolean
       (define (isBefore? d1 d2)
         (cond
           [(< (date-year d1) (date-year d2)) #true]
           [(= (date-year d1) (date-year d2))
            (cond
              [(< (date-month d1) (date-month d2)) #true]
              [(= (date-month d1) (date-month d2))
               (cond
                 [(< (date-day d1) (date-day d2)) #true]
                 [(= (date-day d1) (date-day d2)) (< (time->sec d1) (time->sec d2))]
                 [else #false])]
              [else #false])]
           [else #false]))
       ; LTracks -> LTracks
       (define relevant-tracks
         (local ((define (isTrackof? tr)
                   (and (string=? al (track-album tr)) (isBefore? dt (track-played tr)))))
           (filter isTrackof? lt)))
       ; LTracks -> [List-of String]
       (define (track-tiles lt)
         (local ((define (title-of tr lt)
                   (cons (track-name tr) lt)))
           (foldr title-of '() lt))))
    (track-tiles relevant-tracks)))

; ----- select-albums

(check-expect (select-albums LT4)
              (list (create-track "Only Time"
                                  "Enya"
                                  "A Day Without Rain"
                                  218096
                                  3
                                  (create-date 2002 7 17 3 55 42)
                                  18
                                  (create-date 2011 5 17 17 38 47))
                    (create-track "The Soul of Appanna"
                                  "S. Thaman"
                                  "Game Changer"
                                  11560
                                  1
                                  (create-date 2025 1 10 4 50 44)
                                  10
                                  (create-date 2025 2 9 9 36 0))))
(check-expect (select-albums LT2)
              (list (create-track "Only Time"
                                  "Enya"
                                  "A Day Without Rain"
                                  218096
                                  3
                                  (create-date 2002 7 17 3 55 42)
                                  18
                                  (create-date 2011 5 17 17 38 47))))
(define (select-albums lotr)
  (local ((define (album-member? tr lotr)
            (local ((define (list-albums lotr)
                      (local ((define (extract tr lotr)
                                (cons (track-album tr) lotr)))
                        (foldr extract '() lotr))))
              (member? (track-album tr) (list-albums lotr))))
          (define (extract-uniq tr lotr)
            (cond
              [(album-member? tr lotr) lotr]
              [else (cons tr lotr)])))
    (foldr extract-uniq '() lotr)))

;; Better Alternative (Copied)
(check-expect (select-albums.v2 LT4)
              (list (create-track "Only Time"
                                  "Enya"
                                  "A Day Without Rain"
                                  218096
                                  3
                                  (create-date 2002 7 17 3 55 42)
                                  18
                                  (create-date 2011 5 17 17 38 47))
                    (create-track "The Soul of Appanna"
                                  "S. Thaman"
                                  "Game Changer"
                                  11560
                                  1
                                  (create-date 2025 1 10 4 50 44)
                                  10
                                  (create-date 2025 2 9 9 36 0))))
(check-expect (select-albums.v2 LT2)
              (list (create-track "Only Time"
                                  "Enya"
                                  "A Day Without Rain"
                                  218096
                                  3
                                  (create-date 2002 7 17 3 55 42)
                                  18
                                  (create-date 2011 5 17 17 38 47))))
(define (select-albums.v2 tracks)
  (local ; Track LTracks -> LTracks
      ((define (traverse tr lotr)
         (local ; Track -> Boolean
             ((define (album-member? t)
                (string=? (track-album t) (track-album tr))))
           (if (ormap album-member? lotr)
               lotr
               (cons tr lotr)))))
    (foldr traverse '() tracks)))

;; -------- Extract only Unique Albums --------
;; (define (select-albums lt)
;;   (local ((define (list-albums lotr)
;;             (local ((define (extract tr lotr)
;;                       (cons (track-album tr) lotr)))
;;               (foldr extract '() lotr)))
;;           (define (uniq-albums loal)
;;             (local ((define (isPresent? al loal)
;;                       (member? al loal))
;;                     (define (extract al ls)
;;                       (if (isPresent? al ls) ls
;;                           (cons al ls))))
;;               (foldr extract '() loal))))
;;     (uniq-albums (list-albums lt))))

;; Testing Area
; Example Dates
(define DATE1 (create-date 2002 7 17 3 55 14))
(define DATE2 (create-date 2011 5 17 17 35 13))
(define DATE3 (create-date 2002 7 17 3 55 42))
(define DATE4 (create-date 2011 5 17 17 38 47))

; Example Tracks
(define TRACK1 (create-track "Wild Child" "Enya" "A Day Without Rain" 227996 2 DATE1 20 DATE2))
(define TRACK2 (create-track "Only Time" "Enya" "A Day Without Rain" 218096 3 DATE3 18 DATE4))
(define TRACK3
  (create-track "The Soul of Appanna"
                "S. Thaman"
                "Game Changer"
                11560
                1
                (create-date 2025 1 10 4 50 44)
                10
                (create-date 2025 2 09 09 36 00)))
; An LTracks is one of:
; – '()
; – (cons Track LTracks)

; Example LTracks
(define LT1 '())
(define LT2 (cons TRACK2 '()))
(define LT3 (list TRACK1 TRACK2))
(define LT4 (list TRACK1 TRACK2 TRACK3))
