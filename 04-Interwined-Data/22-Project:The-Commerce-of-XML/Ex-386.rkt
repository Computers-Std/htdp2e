#lang htdp/isl+
(require 2htdp/abstraction)

; Xexpr is one of:
; - Symbol
; - String
; - Number
; - (cons Symbol (cons [List-of Attribute] [List-of Xexpr]))
; - (cons Symbol [List-of Xexpr])
;
; An Attribute is:
; (list Symbol String)

(define ERROR "not found")

; X -> Boolean
(define (atom? x)
  (or (symbol? x) (string? x) (number? x)))

; Xexpr -> Symbol
(define (xexpr-name xe)
  (first xe))

; [Maybe [List-of Attribute]] -> Boolean
(define (list-of-attrs? x)
  (cond
    [(empty? x) #true]
    [else (local ((define possible-attrs (first x)))
            (cons? possible-attrs))]))

; Xexpr -> [List-of Attribute]
(define (xexpr-attr xe)
  (local ((define opt-loa+content (rest xe)))
    (cond
      [(empty? opt-loa+content) '()]
      [else (local ((define loa-or-x (first opt-loa+content)))
              (if (list-of-attrs? loa-or-x)
                  loa-or-x '()))])))

; Xexpr -> [List-of Xexpr]
(define (xexpr-content xe)
  (local ((define opt-loa+content (rest xe)))
    (cond
      [(empty? opt-loa+content) '()]
      [else (local ((define loa-or-x (first opt-loa+content))
                    (define possible-content (rest opt-loa+content)))
              (if (list-of-attrs? loa-or-x)
                  possible-content opt-loa+content))])))

; [List-of Attribute] Symbol -> String
; retrieves the value of symbol in loa
(define (find-attr-val loa sym)
  (local ((define found (assoc sym loa)))
    (if (false? found)
        #false (second found))))

; Xexpr String -> String
; retrieves the value of the "content" attribute from a 'meta element
; that has attribute "itemprop" with value s
(check-expect
 (get '(meta ((content "+1") (itemprop "F"))) "F") "+1")
(check-error
 (get '(meta ((content "+1") (itemprop "F"))) "T") ERROR)
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result) result
        (error ERROR))))

; Xexpr String -> [Maybe String]
(define (get-xexpr xe itemp-val)
  (if (atom? xe)
      #false
      (local ((define xname (xexpr-name xe))
              (define xattrs (xexpr-attr xe))
              (define xcontent (xexpr-content xe))
              (define is-meta? (symbol=? xname 'meta))
              (define have-itemp?
                (string=? (find-attr-val xattrs 'itemprop) itemp-val))
              (define meta-content-val
            (if have-itemp?
                (find-attr-val xattrs 'content) #false)))
    (if (and is-meta? have-itemp? (not (false? meta-content-val)))
        meta-content-val
        (for/or ([sub-xe xcontent])
          (get-xexpr sub-xe itemp-val))))))
