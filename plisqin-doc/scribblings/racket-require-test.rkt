#lang racket

; The point of "racket.rkt" is to provide all the imports
; that don't clash with (require plisqin).
; This file simply tests that this does not error:
(require (for-label plisqin
                    plisqin-doc/scribblings/racket))
