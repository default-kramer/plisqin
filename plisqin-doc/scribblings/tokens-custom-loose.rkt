#lang scribble/manual

@(begin
   (provide custom-loose-docs)

   (require scribble/example
            "helpers.rkt")
   (require (for-label plisqin-lib/types
                       plisqin-lib/loose
                       (prefix-in %% plisqin-lib/unsafe)
                       (prefix-in % plisqin-lib/loose)
                       "racket.rkt"))

   (define (custom-loose-docs)
     @defform[(val x)]{
 A variant of @(racket (%%val x)).
 This variant protects against SQL injection by requiring @(racket x) to be a
 literal string or literal number.
 @(examples #:eval my-eval
            (code:comment "this will work:")
            (%val "a string literal")
            (code:comment "this will produce an error:")
            (eval:error (let ([not-a-string-literal "hi"])
                          (%val not-a-string-literal))))
}
     ))
