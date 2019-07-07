#lang scribble/manual
@(require (for-label plisqin-lib
                     "racket.rkt"
                     plisqin-lib/examples/video-rental-schema))
@(require scribble/eval
          plisqin
          rackunit
          "helpers.rkt"
          "racket.rkt")
@(define-syntax-rule (interact forms ...)
   (interaction
    #:eval my-eval
    forms ...))

@title{Plisqin}

First time here? Go read the @seclink["intro"]{Introduction} instead.

Code is available @hyperlink["https://github.com/default-kramer/plisqin"]{on GitHub}.

@(local-table-of-contents)

@(include-section "intro.scrbl")
@(include-section "getting-started.scrbl")
@(include-section "walkthrough.scrbl")
@(include-section "reference.scrbl")
@(include-section "rental-doc.scrbl")
@(include-section "for-developers.scrbl")
@(include-section "intro-0.2.scrbl")