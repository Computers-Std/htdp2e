#lang htdp/isl+
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

;; (define PREFIX "Https://www.google.com/finance?q=")
(define BASE-URL "https://www.marketwatch.com/investing/index/")
(define SIZE 22) ; font size

(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)

; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append BASE-URL co))
          ; [StockWorld -> StockWorld]
          ; takes no locally defined argument
          (define (retrieve-stock-data __w)
            (local (; URL -> Xexpr
                    ; produces the first XML element as an Xexpr
                    (define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image
          ; retrieves data-price and data-delta from the StockWord and
          ; makes an Image
          (define (render-stock-data sw)
            (local (; [StockWorld String -> String] -> Image
                    ; takes a Struct-Value (data-price or data-delta)
                    ; out of sw (StockWorld) and color (Srting) to
                    ; make an Image
                    (define (word sel col)
                      (text (sel sw) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
              [on-tick retrieve-stock-data 15]
              [to-draw render-stock-data])))

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute from a 'meta element
; that has "itemprop" with value s
(define (get xe s)
  ...)
