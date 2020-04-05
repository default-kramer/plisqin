#lang scribble/manual

@(begin
   (provide custom-unsafe-docs)

   (require racket/require)
   (require (for-label plisqin-lib/unsafe
                       (subtract-in plisqin plisqin-lib/unsafe)
                       "racket.rkt"))

   (define (custom-unsafe-docs)
     @defproc*[([(val [x string?]) String]
                [(val [x number?]) Number])]{
 Converts a Racket value into a @(racket Token).
 The token's @tech{nullability} is always @(racket no).
}
     ))
