#lang htdp/isl+

'(server ((name "example.org")))
; <server name="example.org" />

'(carcas (board (grass)) (player ((name "sam"))))
; <carcas>
;     <board><grass /></board>
;     <player name="sam" />
; </carcas>

'(start)
; <start />

; 'start is an element of Xexpr.v0
; 'server is an element of Xexpr.v1
; 'carcas is an element of Xexpr.v2
