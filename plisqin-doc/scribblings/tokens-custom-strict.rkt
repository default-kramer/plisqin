#lang scribble/manual

@(begin
   (provide custom-strict-docs)

   (require (for-label plisqin
                       "racket.rkt"))

   (define (custom-strict-docs)
     @defform[(val x)]{
 Identical to @(racket (%val x)).
}
     ))
