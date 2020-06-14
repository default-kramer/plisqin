#lang scribble/manual

@(begin
   (provide custom-unsafe-docs)

   (require racket/require
            "tokens-helpers.rkt"
            "helpers.rkt"
            )
   (require (for-label plisqin-lib/unsafe
                       (prefix-in %% plisqin-lib/unsafe)
                       plisqin-lib
                       (subtract-in plisqin plisqin-lib/unsafe)
                       "racket.rkt"))

   (define (custom-unsafe-docs)
     (parameterize ([type-lookup unsafe-table])
       (nested (doc-aggregate)
               (doc-scalar)
               (doc-sql)
               (doc-val)
               (doc-??))))

   (define (doc-aggregate)
     @def-token[aggregate]{
 A primitive building block with no @tech{strict} counterpart.
 Constructs an arbitrary fragment of SQL that represents an aggregate operation.
 This fragment will recognize grouped joins like the built-in aggregates, such
 as @(racket %%count) or @(racket %%max).
 Let's show the maximum UnitPriceDiscount we've ever given for each Product,
 and add 42 just for fun:
 @(repl
   (define (%%max-plus-42 . tokens)
     (%%aggregate "max(" tokens ") + 42")))
 @(repl-query
   (aw:show-table
    (from p Product
          (join detailsG SalesOrderDetail
                (group-by (ProductID detailsG))
                (join-on (.= (ProductID detailsG)
                             (ProductID p))))
          (select (ProductName p))
          (select (%%max-plus-42 (UnitPriceDiscount detailsG)))
          (limit 3))))
 })

   (define (doc-scalar)
     @def-token[scalar]{
 A primitive building block with no @tech{strict} counterpart.
 Constructs an arbitrary fragment of SQL that represents a scalar.
 This fragment will be injected into a grouped join if needed.
 In the example on @(racket %%aggregate), notice that
 @(racket (ProductID detailsG)) occurs in a join-on clause of a grouped join.
 This is why it appears as "__INJECT0" in the generated SQL.
 This behavior is enabled because @(racket (ProductID detailsG)) is returning
 a @(racket %%scalar).
 Although every @(racket %%scalar) @italic{should} be a @(racket Scalar?),
 the type @(racket Scalar?) is irrelevant to this behavior.})

   (define (doc-sql)
     @def-token[sql]{
 A primitive building block with no @tech{strict} counterpart.
 Constructs an arbitrary fragment of SQL with no special behavior.})

   (define (doc-val)
     @defproc[(val [x (or/c number? string? boolean?)]
                   [type (or/c #f type?) #f])
              Token?]{
 Converts a Racket value into a @(racket Token?).
 The token's @tech{nullability} is always @(racket no).
 If @(racket type) is @(racket #f), it is inferred as follows:
 @(repl
   (eval:check (Number? (%%val 42)) #t)
   (eval:check (String? (%%val "hello world")) #t)
   (eval:check (Bool? (%%val #t)) #t))

 If @(racket type) is not @(racket #f), the return value will be cast to it:
 @(repl
   (eval:check (Datetime? (%%val "1999-12-31" Datetime?)) #t))
 })
   (define (doc-??)
     @defform[#:kind "procedure" (?? token-1 [token-N ...+] [/fallback])
              #:contracts ([token-1 Token?]
                           [token-N Token?]
                           [/fallback fallback?])]{
 This procedure is typically used to attach a @tech{fallback}, but it can also be
 used as a synonym for @(racket %%coalesce) if no fallback is given.
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
  it attaches the fallback to the result of @(racket %%coalesce):
  @(racketblock
    (?? token-1 token-N ...+ /fallback)
    (code:comment "is equivalent to")
    (>> (%%coalesce token-1 token-N ...+) #:fallback /fallback))}
   @item{When the last argument is not a @(racket fallback?), it is simply a
  synonym for @(racket %%coalesce):
  @(racketblock
    (?? token-1 token-N ...+)
    (code:comment "is equivalent to")
    (%%coalesce token-1 token-N ...+))})

 You may need to check the documentation on @(racket %%coalesce) to know the return
 type of cases 2 and 3.
 })
   )
