#lang info
(define collection "plisqin")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/plisqin.scrbl" (multi-page))))
(define pkg-desc "SQL Generator")
(define version "0.2")
(define pkg-authors '(Ryan Kramer))
