#lang racket

; The point of "racket.rkt" is to provide all the imports
; that don't clash with plisqin/private/lang/default-require.
; This file simply tests that this does not error:
(require (for-label plisqin/private/lang/default-require
                    plisqin/scribblings/racket))