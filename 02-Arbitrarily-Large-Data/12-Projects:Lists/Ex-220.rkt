#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-220) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)

(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT 10)
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))

(define BLOCK ; red squares with black rims
  (overlay (square (- SIZE 1) "solid" "red") (square SIZE "outline" "black")))
(define SCENE (empty-scene SCENE-SIZE SCENE-SIZE))

(define-struct tetris [block landscape])
(define-struct block [x y])

; A Tetris is a structure:
;   (make-tetris Block Landscape)

; A Landscape is one of:
; – '()
; – (cons Block Landscape)

; A Block is a structure:
;   (make-block N N)

; interpretations: (make-block x y) depicts a block whose left corner
; is (* x SIZE) pixels from the left and (* y SIZE) pixels from the
; top
; (make-tetris b0 (list b1 b2 ...)) means b0 is the dropping
; block, while b1, b2, and ... are resting

; Data Collections (examples)
(define landscape0 (cons (make-block 2 (- HEIGHT 1)) '()))
(define landscape1
  (list (make-block 2 (- HEIGHT 1)) (make-block 4 (- HEIGHT 1)) (make-block 5 (- HEIGHT 4))))
(define block-dropping (make-block 3 4))
(define tetris0
  (make-tetris (make-block 0 0)
               (list (make-block WIDTH (- HEIGHT 1)) (make-block (- WIDTH 1) (- HEIGHT 1)))))
(define tetris0-drop (make-tetris (make-block 2 4) landscape0))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))

; tetris-render
; Tetris -> Image
; Block Landscape -> Image
(define (tetris-render t)
  (place-image BLOCK
               (con-block (block-x (tetris-block t)))
               (con-block (block-y (tetris-block t)))
               (render-landscape (tetris-landscape t) SCENE)))

; Landscape Image -> Image
(define (render-landscape ls bg)
  (cond
    [(empty? ls) bg]
    [else
     (place-image BLOCK
                  (con-block (block-x (first ls)))
                  (con-block (block-y (first ls)))
                  (render-landscape (rest ls) bg))]))

; Number -> Number
; converts block-x, block-y to the respective coordinates
(check-expect (con-block 9) 90)
(define (con-block n)
  (* SIZE n))
