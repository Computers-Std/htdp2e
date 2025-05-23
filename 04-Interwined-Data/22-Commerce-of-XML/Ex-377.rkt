#lang htdp/isl+
(require 2htdp/abstraction)

(define a0 '((a "apple") (b "ball")))
(define w1 '(word ((text "hello"))))
(define w1-bye '(word ((text "bye"))))
(define w2 '(word ((text "hola"))))
(define w3 '(word ((text "namaste"))))
(define i1 `(li ,w1))
(define i1-bye `(li ,w1-bye))
(define i2 `(li ,a0 ,w2))
(define e0 `(ul ,i1 ,i2))
(define e0-bye `(ul ,i1-bye ,i2))
(define i3 `(li ,e0))
(define i3-bye `(li ,e0-bye))
(define e1 `(ul ,a0 ,i1 ,i2 ,i3))
(define e1-bye `(ul ,a0 ,i1-bye ,i2 ,i3-bye))

; Xexpr -> Symbol
(define (xexpr-name xe)
  (first xe))

(define (xexpr-content xe)
  (local
      ((define opt-loa+content (rest xe))
       (define possible-content (rest opt-loa+content)))
    (cond
      [(empty? opt-loa+content) '()]
      [else (if (list-of-attributes? (first opt-loa+content))
                possible-content opt-loa+content)])))

; Attrs-or-Xexpr -> Boolean
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local ((define possble-attribute (first x)))
            (cons? possble-attribute))]))

; Xexpr -> [List-of Attribute]
(define (xexpr-attr xe)
  (local ((define opt-loa+content (rest xe)))
    (cond
      [(empty? opt-loa+content) '()]
      [else
       (local ((define loa-or-x (first opt-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x '()))])))

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

; XItem String String -> XItem
(define (replace-item xi old new)
  (local ((define content (first (xexpr-content xi)))
          (define old-str (symbol->string old))
          (define new-str (symbol->string new))
          ; Symbol [List-of Attribute] Body -> Xexpr
          (define (build-xexpr name attrs body)
            (if (empty? attrs)
                (list name body)
                (list name attrs body))))
    (build-xexpr (xexpr-name xi)
                 (xexpr-attr xi)
                 (cond
                   [(word? content)
                    (if (string=? (word-text content) old-str)
                        `(word ((text ,new-str)))
                        content)]
                   [else (replace-enum content old new)]))))

; XEnum String String -> XEnum
(define (replace-enum xe old new)
  (local ((define replace-content-~
            (for/list ([i (xexpr-content xe)])
              (replace-item i old new)))
          (define replace-content
            (foldr (lambda (it ls) (cons (replace-item it old new) ls))
                   '() (xexpr-content xe)))
          (define attrs (xexpr-attr xe))
          (define body
            (if (empty? attrs)
                replace-content
                (cons attrs replace-content))))
    (cons (xexpr-name xe) body)))

(check-expect (replace-item i1 'hello 'bye) i1-bye)
(check-expect (replace-item i3 'hello 'bye) i3-bye)
(check-expect (replace-enum e0 'hello 'bye) e0-bye)
(check-expect (replace-enum e1 'hello 'bye) e1-bye)
