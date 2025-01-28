#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-180) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

;; Exercise 180. Design editor-text without using implode.

;; CONSTANTS
(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

;; DATA DEFINITIONS
(define-struct editor [pre post])
; An Editor is a Structure:
; (make-editor Lo1S lo1S)

; An Lo1S is one of:
; - '()
; - (cons 1String Lo1S)



; Lo1s -> Image
; renders a list of 1Strings as a text image
(check-expect (editor-text (cons "p" (cons "o" (cons "s" (cons "t" '())))))
              (text (implode (cons "p" (cons "o" (cons "s" (cons "t" '()))))
                             ) FONT-SIZE FONT-COLOR))
(define (editor-text s)
  (text (editor-implode s)
        FONT-SIZE FONT-COLOR))

; Lo1s -> String
; produces a String from given list of 1Strings
(check-expect (implode (cons "p" (cons "o" (cons "s" (cons "t" '())))))
              (editor-implode (cons "p" (cons "o" (cons "s" (cons "t" '()))))))
(define (editor-implode s)
  (cond
    [(empty? s) ""]
    [else (string-append (first s)
                         (editor-implode (rest s)))]))

(define (editor-render ed)
  (place-image/align
   (beside (editor-text (reverse (editor-pre ed)))
           CURSOR
           (editor-text (editor-post ed)))
   1 1
   "left" "top"
   MT))
