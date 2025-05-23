#lang htdp/isl+
(require 2htdp/abstraction)
(require 2htdp/image)
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


;; Constants
(define SIZE 12)
(define COLOR "blue")
;; Functions
(define BT (beside (circle 3 'solid COLOR)
                   (text " " SIZE COLOR)))
(define a0 '((len "10") (wid "5")))
(define a1 '((color "purple") (smell "fresh")))
(define a2 '((afor "apple") (bfor "ball")))
(define w0 '(word ((text "plant"))))
(define w1 '(word ((text "pole"))))
(define w2 '(word ((text "veggie"))))
(define w3 '(word ((text "fruit"))))
(define e0 '(ul (li (word ((text "one")))) (li (word ((text "two"))))))
(define e1 `(ul ,a0 (li ,w0) (li ,w1)))
(define e2 `(ul (li ,a1 ,w2) (li ,a1 ,w3)))
(define i0 `(li ,a1 ,w0))
(define i1 `(li ,a1 ,w1))
; --

(define I0 '(li (word ((text "mango")))))
(define I1 `(li ,a2 (word ((text "one")))
                (word ((text "two"))) (word ((text "three")))))
(define I2 `(li ,e1))
(define I3 `(li ,a1 ,e2))

(define E0 `(ul ,I0 ,I1))
(define E1 `(ul ,a2 ,I2 ,I3))


; Image -> Image
; marks item with bullet
(define (bulletize item)
  (beside/align 'center BT item))

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

; XItem Image -> Image
; renders one XItem as an image
(check-expect (render-xitem '(li (word ((text "one")))))
              (beside/align 'center BT (text "one" SIZE COLOR)))
(check-expect (render-xitem `(li ,a1 ,w0))
              (beside/align 'center BT (text "plant" SIZE COLOR)))
(define (render-xitem xi)
  (local ((define content (first (xexpr-content xi))))
    (cond
      [(word? content)
       (bulletize (text (word-text content) SIZE COLOR))]
      [else (bulletize (render-xenum content))])))

; XEnum -> Image
(check-expect (render-xenum e0)
              (above/align 'left (beside/align 'center BT (text "one" SIZE COLOR))
                           (beside/align 'center BT (text "two" SIZE COLOR))))
(check-expect (render-xenum e1)
              (above/align 'left (beside/align 'center BT (text "plant" SIZE COLOR))
                           (beside/align 'center BT (text "pole" SIZE COLOR))))
(check-expect (render-xenum e2)
              (above/align 'left (beside/align 'center BT (text "veggie" SIZE COLOR))
                           (beside/align 'center BT (text "fruit" SIZE COLOR))))
(define (render-xenum xe)
  (local ((define content (xexpr-content xe))
          (define (render-one-item an-xi so-far)
            (above/align 'left (render-xitem an-xi) so-far)))
    (foldr render-one-item empty-image content)))

; This should work because, in either case bulletize function will add
; bullet besides whatever image given. For the same reason, I think
; the previous version (wrapping cond) is quite suitable
