#lang htdp/isl

(require 2htdp/universe)
(require 2htdp/image)

;; Data Definitions

; A Missiles is one of:
; - '()
; - (cons Posn Missiles)

(define-struct ufo [x y missiles])
; A UFO is a structure
; (make-ufo Number Number Number)
; (make-ufo x y m) is the UFO's
; location x and y coordinates
; & the list of misslies launched by UFO

(define-struct tank [x vel missiles])
; A Tank is a structure:
; (make-tank Number Number Missiles)
; (make-tank x dx missiles) specifies the x position,
; the tank's speed: dx pixels/tick,
; and the list of the missiles launched by the tank.

(define-struct game [ufo tank])
; A GameState is a structure:
; (make-game UFO Tank)

; A KeyEventMove is one of:
; - left
; - right
; Represents a pressed key that sets
; a movement direction of the tank.

; A KeyEvent is one of:
; - KeyEventMove
; - " "
; Represents a pressed key that triggers
; a game state change.

;; Constants
(define WIDTH 300)
(define HEIGHT 600)
(define BACKGROUND (empty-scene WIDTH HEIGHT "deepskyblue"))

(define UFO-HEIGHT 20)
(define UFO-WIDTH (* 2 UFO-HEIGHT))
(define UFO-IMAGE
  (overlay (circle (/ UFO-HEIGHT 2) "solid" "palegreen")
           (ellipse UFO-WIDTH (/ UFO-HEIGHT 2) "solid" "green")))
(define UFO-X-START (/ WIDTH 2))
(define UFO-X-MIN (/ UFO-WIDTH 2))
(define UFO-X-MAX (- WIDTH (/ UFO-WIDTH 2)))
(define UFO-Y-START (/ UFO-HEIGHT 2))
(define UFO-Y-LANDED (- HEIGHT (/ UFO-HEIGHT 2)))
(define UFO-SPEED 2)
(define UFO-JUMP-MAX 22)

(define TANK-HEIGHT 10)
(define TANK-WIDTH (* 2 TANK-HEIGHT))
(define TANK-X-START (/ TANK-WIDTH 2))
(define TANK-X-MIN (/ TANK-WIDTH 2))
(define TANK-X-MAX (- WIDTH (/ TANK-WIDTH 2)))
(define TANK-Y (- HEIGHT (/ TANK-HEIGHT 2)))
(define TANK-IMAGE (rectangle TANK-WIDTH TANK-HEIGHT "solid" "midnightblue"))
(define TANK-SPEED 3)

(define TANK-MISSILE (triangle 5 "solid" "black"))
(define TANK-MISSILE-Y-START (- TANK-Y (/ TANK-HEIGHT 2)))
(define TANK-MISSILES-NUM 5) ; quantity of the charges the tank has.

(define MISSILE-SPEED (* UFO-SPEED 2))

(define UFO-MISSILE (isosceles-triangle 6 300 "solid" "red"))
(define UFO-MISSILE-ADD 20) ; each pixels

(define HIT-DISTANCE 15) ; pixels between UFO or Tank and a missile centers.

(define INIT-STATE
  (make-game (make-ufo UFO-X-START UFO-Y-START '()) (make-tank TANK-X-START TANK-SPEED '())))

(define TEST-AIM (make-game (make-ufo 10 20 '()) (make-tank 28 (- 0 TANK-SPEED) '())))

(define UFO-LAUNCHED (make-ufo 20 100 (list (make-posn 40 120))))

(define TANK-LAUNCHED
  (make-tank 100 TANK-SPEED (list (make-posn 100 TANK-MISSILE-Y-START) (make-posn 20 150))))

(define TEST-LAUNCHED (make-game UFO-LAUNCHED TANK-LAUNCHED))

(define TEST-HIT
  (make-game (make-ufo 20 100 '()) (make-tank 100 TANK-SPEED (list (make-posn 20 103)))))

(define TEST-LANDED
  (make-game (make-ufo 70 UFO-Y-LANDED '()) (make-tank 28 (- 0 TANK-SPEED) (list (make-posn 32 32)))))

;; Functions

;; GameState -> GameState
;; Usage: (si-main INIT-STATE)
;; Each click on the space key launches a tank missile.
;; By default, the tank has only 5 charges.
;; Move the tank with the left and right arrows.
(define (si-main gs)
  (big-bang gs
            [to-draw si-render]
            [on-tick si-move]
            [on-key si-control]
            [check-with game-state?]
            [stop-when si-game-over? si-render-final]))

; GameState -> Image
; Adds Tank, UFO and Missiles to the
; BACKGROUND Scene.
(define (si-render gs)
  (tank-render (game-tank gs) (ufo-render (game-ufo gs) BACKGROUND)))

;; Tank Image -> Image
;; Adds tank to the given image.
(define (tank-render tank scene)
  (place-images (make-list (length (tank-missiles tank)) TANK-MISSILE)
                (tank-missiles tank)
                (place-image TANK-IMAGE (tank-x tank) TANK-Y scene)))

;; UFO Image -> Image
;; Adds ufo to the given image.
(define (ufo-render ufo scene)
  (place-images (make-list (length (ufo-missiles ufo)) UFO-MISSILE)
                (ufo-missiles ufo)
                (place-image UFO-IMAGE (ufo-x ufo) (ufo-y ufo) scene)))

; GameState -> GameState
; Moves the game objects on each clock tick.
(define (si-move gs)
  (make-game (move-ufo (game-ufo gs)) (move-tank)))

; UFO -> UFO
; Caclculate the next position of the UFO
(define (move-ufo ufo)
  (make-ufo (random-ufo-x (safe-ufo-x (ufo-x ufo)))
            (+ (ufo-y ufo) UFO-SPEED)
            (move-ufo-missiles (if (= 0 (modulo (ufo-y ufo) UFO-MISSILE-ADD))
                                   (cons (make-posn (ufo-x ufo) (+ (/ UFO-HEIGHT 2) (ufo-y ufo)))
                                         (ufo-missiles ufo))
                                   (ufo-missiles ufo)))))

; Number -> Number
; Randomly selects new x position of the UFO
(define (random-ufo-x x)
  (+ x
     (if (= 1 (random 2))
         (random UFO-JUMP-MAX)
         (- 0 (random UFO-JUMP-MAX)))))

; Number -> Number
; Limits UFO's x position to prevent jumping over the scene edges
(define (safe-ufo-x x)
  (cond
    [(>= (+ x UFO-JUMP-MAX) UFO-X-MAX) (- x UFO-JUMP-MAX)]
    [(<= (- x UFO-JUMP-MAX) UFO-X-MIN) (+ x UFO-JUMP-MAX)]
    [else x]))

; Missiles -> Missiles
; Returns the next positions of the UFO missiles.
(define (move-ufo-missiles missiles)
  (local ; Posn -> Posn
      ((define (move-ufo-missile m)
         (make-posn (posn-x m) (+ MISSILE-SPEED (posn-y m)))))
    (map move-ufo-missile missiles)))

; Missile -> Missile
; Returns the next positions of teh tank missiles.
(define (move-tank-missiles missiles)
  (local ((define (move-tank-missile m)
            (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED))))
    (map move-tank-missile missiles)))

; Tank -> Tank
; Returns the next position of the Tank
(define (move-tank tank)
  (cond
    [(and (< (tank-vel tank) 0) (<= (tank-x tank) TANK-X-MIN))
     (make-tank TANK-X-MIN TANK-SPEED (move-tank-missiles (tank-missiles tank)))]
    [(and (> (tank-vel tank) 0) (>= (tank-x tank) TANK-X-MAX))
     (make-tank TANK-X-MAX (- 0 TANK-SPEED) (move-tank-missiles (tank-missiles tank)))]
    [else
     (make-tank (+ (tank-x tank) (tank-vel tank))
                (tank-vel tank)
                (move-tank-missiles (tank-missiles tank)))]))

; GameState KeyEvent -> GameState
; Produces a new game state
; when one of these keys is pressed:
; - left [ensures the tank moves left]
; - right [ensures the tank moves right]
; - space [launches a missile]
(define (si-control gs ke)
  (cond
    [(or (string=? ke "left") (string=? ke "right"))
     (make-game (game-ufo gs)
                (make-tank (tank-x (game-tank gs)) (tank-speed ke) (tank-missiles (game-tank gs))))]
    [(and (string=? ke " ") (<= (length (tank-missiles (game-tank gs))) TANK-MISSILES-NUM))
     (make-game (game-ufo gs)
                (make-tank (tank-x (game-tank gs))
                           (tank-vel (game-tank gs))
                           (cons (make-posn (tank-x (game-tank gs)) TANK-MISSILE-Y-START)
                                 (tank-missiles (game-tank gs)))))]
    [else gs]))

; KeyEventMove -> Number
; Returns tank speed.
(define (tank-speed ke)
  (cond
    [(string=? ke "left") (- 0 TANK-SPEED)]
    [(string=? ke "right") TANK-SPEED]
    [else (error "Not supported key event")]))

; GameState -> Boolean
; Identifies if the game is over due to one of the following
; - the UFO landed
; - Missile hit the UFO
(define (si-game-over? gs)
  (local ; UFO -> Boolean
      ((define (ufo-landed? ufo)
         (>= (ufo-y ufo) UFO-Y-LANDED)))
    (or (ufo-hit? (game-ufo gs) (tank-missiles (game-tank gs)))
        (tank-hit? (game-tank gs) (ufo-missiles (game-ufo gs)))
        (ufo-landed? (game-ufo gs)))))

; UFO Missiles -> Boolean
;; Checks if any of Missiles hit the UFO.
(define (ufo-hit? ufo missiles)
  (local ((define (hit-check m)
            (hit? (make-posn (ufo-x ufo) (ufo-y ufo)) m)))
    (ormap hit-check missiles)))

;; UFO Missiles-> Boolean
;; Checks if any of Missiles hit the Tank.
(define (tank-hit? tank missiles)
  (local ((define (hit-check m)
            (hit? (make-posn (tank-x tank) TANK-Y) m)))
    (ormap hit-check missiles)))

; Posn Posn -> Boolean
(define (hit? p m)
  (local ; Posn Posn -> PositiveNumber
      ; Calculates distance b/w the two points
      ((define (distance p1 p2)
         (integer-sqrt (+ (expt (- (posn-x p1) (posn-x p2)) 2)
                          (expt (- (posn-y p1) (posn-y p2)) 2)))))
    (<= (distance p m) HIT-DISTANCE)))

;; Any -> Boolean
;; Checks that gs is an element of the GameState collection.
(define (game-state? gs)
  (game? gs))

;; GameState -> Image
;; Renders the last scene of the game.
(define (si-render-final gs)
  (overlay (text (if (ufo-hit? (game-ufo gs) (tank-missiles (game-tank gs))) "YOU WON" "Game Over")
                 26
                 "yellow")
           BACKGROUND))

;(si-main INIT-STATE)
