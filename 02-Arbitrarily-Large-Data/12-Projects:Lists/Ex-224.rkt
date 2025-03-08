#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-224) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)
(require 2htdp/universe)
;; Constants
(define WIDTH 250)
(define HEIGHT 250)                     ; 500
(define BACKGROUND (empty-scene WIDTH HEIGHT "white"))

(define UFO-HEIGHT 20)
(define UFO-WIDTH (* 2 UFO-HEIGHT))
(define UFO-IMG
  (underlay/align/offset "center"
                         "top"
                         (wedge (/ UFO-HEIGHT 2) 180 "solid" "gray")
                         0
                         9
                         (ellipse UFO-WIDTH (/ UFO-HEIGHT 2) "solid" "olivedrab")))
(define UFO-START-X (/ WIDTH 2))
(define UFO-MIN-X (/ UFO-WIDTH 2))
(define UFO-MAX-X (- WIDTH (/ UFO-WIDTH 2)))
(define UFO-START-Y (/ UFO-HEIGHT 2))
(define UFO-LAND-Y (- HEIGHT (/ UFO-HEIGHT 2)))
(define UFO-SPEED 2)

(define TANK-IMG
  (overlay/align/offset "center"
                        "top"
                        (rectangle 4 14 "solid" "black")
                        0
                        7
                        (overlay/align "center"
                                       "center"
                                       (circle 7 "solid" "black")
                                       (rectangle 24 25 "solid" "yellowgreen"))))
(define TANK-HEIGHT (image-height TANK-IMG))
(define TANK-WIDTH (image-width TANK-IMG))
(define TANK-START-X (/ TANK-WIDTH 2))
(define TANK-MIN-X (/ TANK-WIDTH 2))
(define TANK-MAX-X (- WIDTH (/ TANK-WIDTH 2)))
(define TANK-Y (- HEIGHT (/ TANK-HEIGHT 2)))
(define TANK-SPEED 3)

(define TANK-MISSILE (ellipse 5 10 "solid" "black"))
(define TANK-MISSILE-START-Y (- TANK-Y (/ TANK-HEIGHT 2)))
(define TANK-MISSILES-NUM 5) ; quantity of the charges the tank has.

(define MISSILE-SPEED (* UFO-SPEED 2))

(define UFO-MISSILE (ellipse 5 10 "solid" "red"))
(define UFO-MISSILE-ADD 20) ; each pixels

;; Data Definitions

; A Missiles is one of:
; - '()
; - (cons Posn Missiles)

(define-struct ufo [x y missiles])
; A UFO is a structure
; (make-ufo Number Number Missiles)
; (make-ufo x y m) is a UFO
; x and y are coordinates
; m is list of missiles launched by UFO

(define-struct tank [x speed missiles])
; A Tank is structure
; (make-tank Number Number Missiles)
; (make-tank x dx m)
; x => coordinate
; dx => pixel/tick (speed)
; m => list of missiles

(define-struct game [ufo tank])
; A GameState is a Structure
; (make-game ufo tank)

(define INIT-STATE
  (make-game
   (make-ufo UFO-START-X UFO-START-Y '())
   (make-tank TANK-START-X TANK-SPEED '())))

(define TEST-AIM
  (make-game
   (make-ufo 10 20 '())
   (make-tank 28 (- 0 TANK-SPEED) '())))

(define UFO-LAUNCHED (make-ufo 20 100 (list (make-posn 40 120))))

(define TANK-LAUNCHED
  (make-tank 100 TANK-SPEED (list (make-posn 100 TANK-MISSILE-START-Y) (make-posn 20 150))))

(define TEST-LAUNCHED (make-game UFO-LAUNCHED TANK-LAUNCHED))

(define TEST-HIT
  (make-game
   (make-ufo 20 100 '())
   (make-tank 100 TANK-SPEED (list (make-posn 20 103)))))

(define TEST-LANDED
  (make-game
   (make-ufo 70 UFO-LAND-Y '())
   (make-tank 28 (- 0 TANK-SPEED) (list (make-posn 32 32)))))


(define (tank-render tank scene)
  (place-images (make-list (length (tank-missiles tank)) TANK-MISSILE)
                (tank-missiles tank)
                (place-image TANK-IMG (tank-x tank) TANK-Y scene)))

(define (ufo-render ufo scene)
  (place-images (make-list (length (ufo-missiles ufo)) UFO-MISSILE)
                (ufo-missiles ufo)
                (place-image UFO-IMG (ufo-x ufo) (ufo-y ufo) scene)))

; GameState -> Image
; Tank + UFO + Missiles + BACKGROUND
(define (game-render gs)
  (tank-render (game-tank gs)
               (ufo-render (game-ufo gs)
                           BACKGROUND)))

; Number -> Number
; Random x-coordinate for ufo
;; (define (random-ufo-x x)
;;   (+ x (if (= 1 (random 2))
;;            (random UFO-JUMP-MAX)
;;            (- 0 (random )))))
;; TODO: complete this exercise at some time
