#lang htdp/isl+
(require 2htdp/abstraction)
;; Exercise 376. Design a program that counts all "hello"s in an
;; instance of XEnum.v2

;; Data Definitions

; An XWord is '(word ((text String)))

; An XItem.v2 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons [List-of Attribute] (cons XWord '())))
; - (cons 'li (cons XEnum.v2 '()))
; - (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))

; An XEnum.v2 is one of:
; - (cons 'ul [List-of XItem.v2])
; - (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define a0 '((a "apple") (b "ball")))
(define w1 '(word ((text "hello"))))
(define w2 '(word ((text "hola"))))
(define w3 '(word ((text "namaste"))))
(define i1 `(li ,w1))
(define i2 `(li ,a0 ,w2))
(define e0 `(ul ,i1 ,i2))
(define i3 `(li ,e0))
(define e1 `(ul ,a0 ,i1 ,i2 ,i3))

; Xexpr -> [List-of Xexpr]
; retrieves the list-of content of xe
(define (xexpr-content xe)
  (local ((define opt-loa+content (rest xe))
          (define (list-of-attributes? x)
            (cond
              [(empty? x) #true]
              [else (local ((define possible-attribute (first x)))
                      (cons? possible-attribute))]))
          (define loa-or-content (first opt-loa+content))
          (define possible-content (rest opt-loa+content)))
    (cond
      [(empty? opt-loa+content) '()]
      [else (if (list-of-attributes? loa-or-content)
                possible-content opt-loa+content)])))

;; Any -> Boolean
;; Determines whether v is an XWord.
(define (word? v)
  (match v
    [(list 'word (list (list 'text (? string?)))) #true]
    [else #false]))

; XWord -> String
; extracts the value of XWord
(define (word-text xw)
  (match xw
    [(list 'word (list (list 'text str))) str]))

; XItem -> Number
; Checks and Counts in given XItem
(check-expect (count-item i3) 1)
(check-expect (count-item i1) 1)
(define (count-item xi)
  (local ((define content (first (xexpr-content xi))))
    (cond
      [(word? content)
       (if (string=? (word-text content) "hello") 1 0)]
      [else (count-enum content)])))

; XEnum -> Number
(check-expect (count-enum e0) 1)
(check-expect (count-enum e1) 2)
(define (count-enum xe)
  (local ((define content (xexpr-content xe))
          (define (traverse item total)
            (+ (count-item item) total)))
    (foldr traverse 0 content)))
