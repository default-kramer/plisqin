#lang scribble/manual

@(begin
   (provide custom-strict-docs)

   (require (for-label plisqin
                       "racket.rkt"))

   (define (custom-strict-docs)
     (nested (doc-val)
             (doc-??)))

   (define (doc-val)
     @defform[(val x)]{
 Identical to @(racket (%val x)).
}
     )
   (define (doc-??)
     @defthing[#:kind "procedure" ?? procedure?]{
 This procedure is typically used to attach a @tech{fallback}, but it can also be
 used as a synonym for @(racket coalesce) if no fallback is given.
 The general form of this procedure is:
 @(racketblock
   (?? token [maybe-more-tokens ...] [maybe-fallback]))
 When @(racket maybe-more-tokens) are given, @(racket coalesce) is used.
 When @(racket maybe-fallback) is given, it is attached.
 To be more specific, this procedure has 3 cases:
 @(itemlist
   #:style 'ordered
   @item{When given two arguments and the last argument is a @(racket fallback?),
  it simply attaches the fallback to the first argument:
  @(racketblock
    (?? token /fallback)
    (code:comment "is equivalent to")
    (>> token #:fallback /fallback))}
   @item{When given more than two arguments and the last argument is a @(racket fallback?),
  it attaches the fallback to the result of @(racket coalesce):
  @(racketblock
    (?? token-1 .. token-N /fallback)
    (code:comment "is equivalent to")
    (>> (coalesce token-1 .. token-N) #:fallback /fallback))}
   @item{When the last argument is not a @(racket fallback?), it is simply a
  synonym for @(racket coalesce):
  @(racketblock
    (?? token-1 .. token-N)
    (code:comment "is equivalent to")
    (coalesce token-1 .. token-N))})

 You may need to check the documentation on @(racket coalesce) to know the return
 value of cases 2 and 3.
 })
   )
