#lang htdp/isl+

; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else (cons (first front)
                (replace-eol-with (rest front) end))]))
