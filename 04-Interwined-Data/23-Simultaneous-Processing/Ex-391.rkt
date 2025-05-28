#lang htdp/isl+

; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end

(check-expect (replace-eol-with-1 '() '(a b)) '(a b))
(check-expect (replace-eol-with-1 (cons 1 '()) '(a)) (cons 1 '(a)))
(check-expect (replace-eol-with-1 (cons 2 (cons 1 '())) '(a)) (cons 2 (cons 1 '(a))))
(define (replace-eol-with-1 front end)
  (cond
    [(and (empty? front) (empty? end)) '()]
    [(and (empty? front) (cons? end)) end]
    [(and (cons? front) (empty? end)) front]
    [(and (cons? front) (cons? end))
     (cons (first front) (replace-eol-with-1 (rest front) end))]))

(check-expect (replace-eol-with-2 '() '(a b)) '(a b))
(check-expect (replace-eol-with-2 (cons 1 '()) '(a)) (cons 1 '(a)))
(check-expect (replace-eol-with-2 (cons 2 (cons 1 '())) '(a)) (cons 2 (cons 1 '(a))))
(define (replace-eol-with-2 front end)
  (cond
    [(empty? front) end]
    [else (cons (first front)
                (replace-eol-with-2 (rest front) end))]))
