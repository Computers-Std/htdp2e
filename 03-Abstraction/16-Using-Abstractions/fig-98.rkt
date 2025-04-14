#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fig-98) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

(define-struct address [first-name last-name street])
; An Addr is a structure:
; (make-address String String String)
; interpretation associates an address with a person's name

; [List-of Addr] -> String
; creates a string of first names,
; sorted in alphabetical order,
; seperated and surrounded by blank spaces
(define (listinf.v2 l)
  (local (; 1. extract names
          (define names (map address-first-name l))
          ; 2. sort the names
          (define sorted (sort names string<?))
          ; 3. append them, add spaces
          ; String String -> String
          ; append two strings, prefix with " "
          (define (helper s t)
            (string-append " " s t))
          (define concat+spaces
            (foldr helper " " sorted)))
    concat+spaces))

; Figure 99: Organizing interconnected function definitions with local

; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; produces a version of alon0, sorted according to cmp
(define (sort-cmp alon0 cmp)
  (local (; [List-of Number] -> [List-of Number]
          ; produces the sorted version of alon
          (define (isort alon)
            (cond
              [(empty? alon) '()]
              [else
               (insert (first alon) (isort (rest alon)))]))
          ; Number [List-of Number] -> [List-of Number]
          ; inserts n into the sorted list of numbers alon
          (define (insert n alon)
            (cond
              [(empty? alon) (cons n '())]
              [else (if (cmp n (first alon))
                        (cons n alon)
                        (cons (first alon) (insert n (rest alon))))])))
    (isort alon0)))
