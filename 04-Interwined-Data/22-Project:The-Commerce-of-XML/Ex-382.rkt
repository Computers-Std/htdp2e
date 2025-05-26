#lang htdp/isl+
;; Exercise 382. Formulate an XML configuration for the BW machine,
;; which switches from white to black and back for every key event.
;; Translate the XML configuration into an XMachine representation.
;; See exercise 227 for an implementation of the machine as a program

;; <machine inital="white">
;;          <action state="white" next="black" />
;;          <action state="black" next="white" />
;; </machine>

(define bwm
  '(machine ((initial "white"))
            (action ((state "white") (next "black")))
            (action ((state "black") (next "white")))))
