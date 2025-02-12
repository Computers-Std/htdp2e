#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname track-as-struct) (read-case-sensitive #t)
                           (teachpacks ())
                           (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/itunes)

;; Note A list-based representation is a bit less organized than a
;; structure-based one. The word semi-structured is occasionally used
;; in this context. Such list-representations accommodate properties
;; that show up rarely and thus don’t fit the structure type. People
;; often use such representations to explore unknown information and
;; later introduce structures when the format is well-known. Design a
;; function track-as-struct, which converts an LAssoc to a Track when
;; possible. End

;; Examples of
; Date
(define DATE1 (create-date 2020 2 20 12 0 0))
(define DATE2 (create-date 2020 5 15 23 15 30))
(define DATE3 (create-date 2019 12 5 1 20 44))
(define DATE4 (create-date 2020 6 2 5 15 4))
(define DATE5 (create-date 2020 6 7 8 8 3))

(define LASSOC1 '())
(define LASSOC2
  (list (list "Track ID" 1)
        (list "Name" "Joyful Sonata")
        (list "Artist" "T. Nikolayeva")
        (list "Album" "The Best of Piano")
        (list "Total Time" 301641)
        (list "Track Number" 44)
        (list "Date Added" DATE1)
        (list "Play Count" 77)
        (list "Date Modified" DATE4)))

(define LASSOC3
  (list (list "Track ID" 2)
        (list "Name" "Believer")
        (list "Artist" "Imagine Dragons")
        (list "Album" "Evolve")
        (list "Total Time" 204000)
        (list "Track Number" 3)
        (list "Date Added" DATE3)
        (list "Play Count" 82)
        (list "Date Modified" DATE2)))

(define LASSOC4
  (list (list "Track ID" 3)
        (list "Name" "Whatever It Takes")
        (list "Artist" "Imagine Dragons")
        (list "Album" "Evolve")
        (list "Total Time" 201000)
        (list "Track Number" 2)
        (list "Date Added" DATE3)
        (list "Play Count" 80)
        (list "Date Modified" DATE5)))

; String LAssoc Any -> Association
(check-expect (find-association "Downloaded" LASSOC1 "Not Found") (list "Downloaded" "Not Found"))
(check-expect (find-association "Artist" LASSOC2 "Not Found") (list "Artist" "T. Nikolayeva"))
(check-expect (find-association "Track ID" LASSOC3 "Not Found") (list "Track ID" 2))
(define (find-association key lassoc default)
  (cond
    [(empty? lassoc) (list key default)]
    [(string=? key (first (first lassoc))) (first lassoc)]
    [else (find-association key (rest lassoc) default)]))

;; (create-track name artist album time track# added play# played)
;; -----------------s s s n n d n d

;; ; LAssoc -> Track
(check-expect (track-as-struct LASSOC2)
              (create-track "Joyful Sonata"
                            "T. Nikolayeva"
                            "The Best of Piano"
                            301641
                            1
                            (create-date 2020 2 20 12 0 0)
                            77
                            (create-date 2020 6 2 5 15 4)))
;; (check-expect (track-as-struct LASSOC1) (error "Given LAssoc cannot be a Track")) --- ?
(define (track-as-struct la)
  (cond
    [(empty? la) (error "Given LAssoc cannot be a Track")]
    [else
     (create-track (second (find-association "Name" la "Null"))
                   (second (find-association "Artist" la "Null"))
                   (second (find-association "Album" la "Null"))
                   (second (find-association "Total Time" la 0))
                   (second (find-association "Track ID" la 0))
                   (second (find-association "Date Added" la "no date"))
                   (second (find-association "Play Count" la 0))
                   (second (find-association "Date Modified" la "no date")))]))
