#lang scribble/manual

@(begin
   (provide custom-loose-docs)

   (require scribble/example
            "helpers.rkt")
   (require (for-label plisqin-lib/types
                       plisqin-lib
                       plisqin-lib/loose
                       (prefix-in %% plisqin-lib/unsafe)
                       (prefix-in % plisqin-lib/loose)
                       "racket.rkt"))

   (define (custom-loose-docs)
     (nested (doc-val)
             (doc-??)))

   (define (doc-val)
     @defform[(val x)]{
 A variant of @(racket (%%val x)).
 This variant protects against SQL injection by requiring @(racket x) to be a
 literal string or literal number.
 @(examples #:eval my-eval
            (%val "a string literal")
            (eval:error (let ([not-a-string-literal "hi"])
                          (%val not-a-string-literal))))
 })
   (define (doc-??)
     @defform[#:kind "procedure" (?? token-1 [token-N ...+] [/fallback])
              #:contracts ([token-1 Token]
                           [token-N Token]
                           [/fallback fallback?])]{
 This procedure is typically used to attach a @tech{fallback}, but it can also be
 used as a synonym for @(racket %coalesce) if no fallback is given.
 More precisely, this procedure has 3 cases:
 @(itemlist
   #:style 'ordered
   @item{When given two arguments and the last argument is a @(racket fallback?),
  it simply attaches the fallback to the first argument:
  @(racketblock
    (?? token-1 /fallback)
    (code:comment "is equivalent to")
    (>> token-1 #:fallback /fallback))}
   @item{When given more than two arguments and the last argument is a @(racket fallback?),
  it attaches the fallback to the result of @(racket %coalesce):
  @(racketblock
    (?? token-1 token-N ...+ /fallback)
    (code:comment "is equivalent to")
    (>> (%coalesce token-1 token-N ...+) #:fallback /fallback))}
   @item{When the last argument is not a @(racket fallback?), it is simply a
  synonym for @(racket %coalesce):
  @(racketblock
    (?? token-1 token-N ...+)
    (code:comment "is equivalent to")
    (%coalesce token-1 token-N ...+))})

 You may need to check the documentation on @(racket %coalesce) to know the return
 type of cases 2 and 3.
 })
   )
