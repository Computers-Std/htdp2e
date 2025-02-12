#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname itunes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)
(require 2htdp/itunes)


(define ITUNES-LOCATION "files/itunes2.xml")
;; (define ITUNES-LOCATION "files/itunes.xml")

; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))
