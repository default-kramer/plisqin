#lang info
(define collection "plisqin")
(define deps '("base" "db-lib" "plisqin-lib"))
(define implies '("plisqin-lib"))
(define build-deps '("plisqin-lib" "scribble-lib" "racket-doc" "rackunit-lib" "sandbox-lib"))
;(define scribblings '(("scribblings/plisqin.scrbl" (multi-page))))
(define pkg-desc "SQL Generator")
(define version "0.3")
(define pkg-authors '(|Ryan Kramer|))
