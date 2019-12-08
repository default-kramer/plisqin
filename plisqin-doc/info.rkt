#lang info
(define collection "plisqin-doc")
(define deps '("base" "db-lib"))
(define build-deps '("plisqin-lib" "scribble-lib" "racket-doc" "rackunit-lib" "sandbox-lib"))
(define scribblings '(("scribblings/tokens.scrbl" (multi-page))))
(define pkg-desc "documentation for Plisqin")
(define pkg-authors '(|Ryan Kramer|))
