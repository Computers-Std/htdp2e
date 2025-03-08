#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname webpage) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/web-io)
;; (require 2htdp/image)

; String String -> ... deeply nested list ...
; produces a web page with given author and title
(define (my-fist-web-page author title)
  `(html
    (head
     (title ,title)
     (meta ((http-equiv "content-type")
            (content "text-html"))))
    (body
     (h1 ,title)
     (p "I am, " ,author ", made this page."))))

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l)) (make-row (rest l)))]))

; Number -> ... nested list ...
; creates a cell for an HTML table from a number
(define (make-cell n)
  `(td ,(number->string n)))


(define my-first-table `(html (body (table ((border "1"))
                                           (tr ((width "200")) ,@(make-row '(1 2)))
                                           (tr ((width "200")) ,@(make-row '(99 65)))))))

;; (show-in-browser (my-fist-web-page "Ushakiran" "Hello World!"))
(show-in-browser my-first-table)
