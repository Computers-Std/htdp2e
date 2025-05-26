#lang htdp/isl+
(require 2htdp/abstraction)
(require 2htdp/image)
;; Data Definitions

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; A Body is a [List-of Xexpr].

; An Xexpr is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))

; An XWord is '(word ((text String))).

;; An XEnum.v1 is one of:
; - (cons 'ul [List-of XItem.v1])
; - (cons 'ul (cons [List-of Attribute] [List-of XItem.v1]))

;; An XItem.v1 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons [List-of Attribute] (cons XWord '())))

; Constants
(define BT (beside (circle 3 "solid" "black") (text " " 11 "white")))
(define a0 '((len "10") (wid "5")))
(define a1 '((color "purple") (smell "fresh")))
(define w0 '(word ((text "plant"))))
(define w1 '(word ((text "pole"))))
(define e0 '(ul (li (word ((text "one")))) (li (word ((text "two"))))))
(define e1 `(ul ,a0 (li ,w0) (li ,w1)))
(define e2 `(ul (li ,a1 ,w0) (li ,a1 ,w1)))
(define i0 `(li ,a1 ,w0))
(define i1 `(li ,a1 ,w1))

; Xexpr -> [List-of Xexpr]
; retrieves the list-of content of xe

; NOTE: the reason we stick with the name xexpr-content, is that it
; deals with two different but similar structures of data, they are
; XEnum plus Attributes and XItem plus Attributes, even though they
; are different Structures, under the hood they are X-Expressions of
; similar form.

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

; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet
(check-expect (render-item1 (first (xexpr-content e0)))
              (beside/align 'center BT (text "one" 12 'black)))
(check-expect (render-item1 (second (xexpr-content e1)))
              (beside/align 'center BT (text "pole" 12 'black)))
(check-expect (render-item1 (first (xexpr-content e2)))
              (beside/align 'center BT (text "plant" 12 'black)))
(check-expect (render-item1 (second (xexpr-content e2)))
              (beside/align 'center BT (text "pole" 12 'black)))
(check-expect (render-item1 i1)
              (beside/align 'center BT (text "pole" 12 'black)))
(define (render-item1 i)
  (local (; remove the attrs in XItem
          (define content (xexpr-content i))
          (define (word-text xw)
            (match xw
              [(list 'word (list (list 'text str))) str]))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

; This function takes a XItem and renders it. If the XItem is in form
; of second-clause(see e2), then function treats it as XEnum and retrives the
; content by removing Attributes with "xexpr-content", finally renders
; it.
