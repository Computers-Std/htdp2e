#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-208) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/itunes)
(require 2htdp/batch-io)

;; Exercise 208. Design boolean-attributes. The function consumes an
;; LLists and produces the Strings that are associated with a Boolean
;; attribute. Hint Use create-set from exercise 201.

; An LLists is one of:
; - '()
; - (cons LAssoc LLists)

; An LAssoc is one of:
; - '()
; - (cons Association LAssoc)

; An Association is a list of two items:
; (cons Srting (cons BSDN '()))

; A BSDN is one of:
; - Boolean
; - Number
; - String
; - Date

; Ex-201
; List-of-Strings -> List-of-Strings
(check-expect (create-set (list "one" "two" "one" "three" "two" "three")) (list "one" "two" "three"))
(define (create-set los)
  (cond
    [(empty? los) '()]
    [else
     (if (member? (first los) (rest los))
         (create-set (rest los))
         (cons (first los) (create-set (rest los))))]))

; String LAssoc Any -> Association
(define (find-association key lassoc default)
  (cond
    [(empty? lassoc) (list key default)]
    [(string=? key (first (first lassoc))) (first lassoc)]
    [else (find-association key (rest lassoc) default)]))

(define LASSOC1
  (list (list "Logged In" #true)
        (list "Title" "Kilimanjaro")
        (list "Downloaded" #false)
        (list "Completed" #true)))
(define LASSOC2
  (list (list "Logged In" #false)
        (list "Registered" #true)
        (list "Downloaded" #true)
        (list "Date Added" (create-date 2024 12 20 12 30 19))))

(define LLIST1 '())
(define LLIST2 (list LASSOC1 LASSOC2))

;; LLists -> List-of-Strings
; produces the Unique Strings that are associated with a Boolean
; attribute
(check-expect (boolean-attributes LLIST2) (list "Completed" "Logged In" "Registered" "Downloaded"))
(define (boolean-attributes llist)
  (create-set (bool-attr-all llist)))

;; LLists -> List-of-strings (Not Unique)
;; Procudes the list of unique strings
;; that are associated with Boolean attributes.
(check-expect (bool-attr-all LLIST1) '())
(check-expect (bool-attr-all LLIST2)
              (list "Logged In" "Downloaded" "Completed" "Logged In" "Registered" "Downloaded"))
(define (bool-attr-all llist)
  (cond
    [(empty? llist) '()]
    [else (append (bool-attr-lassoc (first llist)) (bool-attr-all (rest llist)))]))

; LAssoc -> List-of-Strings
;; Produces the list of strings
;; that are associated with Boolean attributes.
(check-expect (bool-attr-lassoc LASSOC1) (list "Logged In" "Downloaded" "Completed"))
(check-expect (bool-attr-lassoc LASSOC2) (list "Logged In" "Registered" "Downloaded"))
(define (bool-attr-lassoc la)
  (cond
    [(empty? la) '()]
    [else
     (if (boolean? (second (first la)))
         (cons (first (first la)) (bool-attr-lassoc (rest la)))
         (bool-attr-lassoc (rest la)))]))
