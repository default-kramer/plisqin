#lang scribble/manual
@(require (for-label plisqin-lib
                     "racket.rkt"))
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

@(include-section "intro-define-schema.scrbl")
@(include-section "tokens.scrbl")
