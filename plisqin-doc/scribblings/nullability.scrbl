#lang scribble/manual

@(begin
   (require plisqin
            (for-label "standard-label.rkt")
            "helpers.rkt"
            "racket.rkt"
            (for-syntax (only-in (submod plisqin-lib/private2/sql/truth for-scribble)
                                 truth-table-as-stx))
            scribble/example
            racket/sandbox))

@title{Nullability}
@section{Introduction to Nullability}
Every token in Plisqin has a property called @deftech{nullability},
which may be one of three values:
@(itemlist
  @item{@(racket yes) - Plisqin knows that the token is nullable}
  @item{@(racket no) - Plisqin knows that the token is not nullable}
  @item{@(racket maybe) - Plisqin doesn't know whether the token is nullable})

The most common way to set a nullability is to use the @(racket #:null) option
of @(racket define-schema), as in the following example:
@(repl
  (define-schema my-test-schema
    (table Album
           #:column
           [AlbumID #:type Number? #:null no]
           [ReleaseYear #:type Number? #:null yes]))
  (eval:check (nullability (AlbumID Album))
              no)
  (eval:check (nullability (ReleaseYear Album))
              yes))

When you build a larger token out of smaller ones, Plisqin usually determines the
correct nullability, as in the following example:
@(repl
  (code:comment "Neither operand is nullable, so the result is not nullable")
  (eval:check (nullability (.+ (AlbumID Album)
                               (val 1)))
              no)
  (code:comment "(ReleaseYear Album) is nullable, so the result is nullable")
  (eval:check (nullability (.+ (ReleaseYear Album)
                               (val 1)))
              yes))

If Plisqin does not or cannot determine the correct nullability, you can
use @(racket >>) to override it.

@section{Avoiding Three-Valued Logic}
In SQL, a boolean expression may have three values: true, false and unknown.
This makes it very easy for a programmer to make a mistake.
The most common mistakes occur with the comparison operators.
A programmer might write "x <= y" but fail to consider what should happen
if x is null or y is null or both are null.

@(load-checkpoint! "final")
The @tech{strict} variant of Plisqin prevents these kinds of mistakes.
It does this in two ways:
@(itemlist
  @item{It uses the @(racket 2bool?) contract to ensure that the unknown
 boolean value will not be present in "decision making positions."
 For example, @(racket where) requires a @(racket 2bool?) because it is deciding
 which rows to filter from the result set and will not tolerate any ambiguity.
 On the other hand, @(racket select) isn't making a decision, so it
 will accept a nullable @(racket Boolish?) and just propogate any dbnulls that
 may be present.}
  @item{
 @margin-note{The return value of all the comparison operators (such as
  @(racket .<=)) could be better documented as
  @(racket (and/c Bool? 2bool?)). This is a limitation of Plisqin's
  documentation mechanism that could be improved.}
 Anywhere that it returns a @(racket Bool?), it promises that the return
 value will also be a @(racket 2bool?).
 In order to fulfill this promise, all the comparison operators (such as
 @(racket .<=)) require the caller to disambiguate what should happen when
 dbnull is encountered.
 This disambiguation is accomplished using fallbacks, which the rest of this
 section will explain.})

@subsection{An Example}
This section uses the Adventure Works example database:
@(racketblock
  (require plisqin-examples/adventure-works
           plisqin-examples/adventure-works/schema))

Now let's imagine that we have been tasked with producing a list of Products
that have sold less than 20 units all-time.
The following example might be a first attempt at such a query.
It produces an error message that we can learn from:
@(repl
  (eval:error (from p Product
                    (where (.<= (TotalQtySold p)
                                (val 20))))))

This error message is saying that @(racket .<=) wants non-nullable tokens.
In fact, all the strict comparison operators want non-nullable tokens.
This is because comparing dbnull to anything produces the unknown boolean value,
which the strict variant of Plisqin has promised to eradicate.
We can confirm that the nullability of the first argument is @(racket yes):
@(repl
  (eval:check (nullability (TotalQtySold Product))
              yes))

So in order to perform this comparison, we will use a @deftech{fallback}.
A fallback is a value that can be attached to any token.
The fallback tells the comparison operators how dbnull should be handled.
In the following example, we attach the @(racket /minval) fallback using @(racket ??):
@(racketblock
  (from p Product
        (where (.<= (?? (TotalQtySold p) /minval)
                    (val 20)))))

Using the fallback makes the comparison unambigous.
The programmer has explicitly told Plisqin that whenever @(racket (TotalQtySold p))
is dbnull, it should fall back to @(racket /minval), an artificial value that
is less than every value your database can hold.
Since @(racket /minval) is obviously less than or equal to @(racket (val 20)), the
comparison will be true whenever @(racket (TotalQtySold p)) is dbnull.

Let's peek at the result set for completeness:
@(repl-query
  (aw:show-table
   (from p Product
         (limit 5)
         (select (ProductName p))
         (select (ProductNumber p))
         (select (TotalQtySold p))
         (where (.<= (?? (TotalQtySold p) /minval)
                     (val 20))))))

@subsubsub*section{Recap}
At first we tried the following comparison:
@(racketblock
  (.<= (TotalQtySold p)
       (val 20)))
But the strict variant of Plisqin gave us an error, because it refuses to produce
boolean expressions that might contain the unknown value.
We used the @(racket /minval) fallback to make our intention explicit.
This avoided the mistake of accidentally filtering out records where TotalQtySold is null.

@subsection[#:tag "fallback-meanings"]{Fallback Meanings}
The meaning of each fallback is as follows:
@(itemlist
  @item{@(racket /minval) represents a set containing exactly one value.
 This value is an artificial value that is less than every value
 that your database can hold. It is equal only to itself.}
  @item{@(racket /maxval) represents a set containing exactly one value.
 This value an artificial value that is greater than every value
 that your database can hold. It is equal only to itself.}
  @item{@(racket /void) represents an empty set of values.
 Comparing @(racket /void) against anything produces false.}
  @item{@(racket /any) represents a set of values that includes every value
 your database can hold, plus @(racket /minval) and @(racket /maxval).})

Notice that each fallback represents a "set of values."
When a fallback is considered, the comparison will be true if any value from
this "set of values" makes it true (sort of like a boolean "or" over a
Cartesian product of comparisons).
Because @(racket /void) represents an empty set, comparing it against anything
always produces false.
The comparison says "We've tried nothing and we're all out of ideas."
This is true even when comparing @(racket /void) against @(racket /any).

The comparison behavior of @(racket /minval) and @(racket /maxval) is pretty
straightforward:
@(itemlist
  @item{@(racket /minval) is considered less than 40 by definition.}
  @item{@(racket /minval) is not considered greater than 40 because it is less than 40.}
  @item{@(racket /maxval) is considered greater than 40 by definition.}
  @item{@(racket /maxval) is not considered less than 40 because it is greater than 40.}
  @item{@(racket /maxval) is considered greater than infinity by definition,
 assuming that "infinity" is a value your database can hold.}
  @item{@(racket /minval) is not considered less than @(racket /void) because a
 set of one value compared against a set of zero values produces zero comparisons.
 Trivially, none of these comparisons are true.})

The comparison behavior of @(racket /any) is a little more tricky.
Because @(racket /any) represents such a large set, comparing it against
something other than @(racket /void) often (but not always) produces true.
Some examples:
@(itemlist
  @item{@(racket /any) is considered equal to 40 because it contains 40.}
  @item{@(racket /any) is considered not-equal to 40 because it contains
 many values which are not-equal to 40, such as 42.}
  @item{@(racket /any) is considered less than "aardvark" because it contains
 many values which are less than "aardvark", such as "aaa" and @(racket /minval).}
  @item{@(racket /any) is considered equal to @(racket /minval) because it contains
 @(racket /minval).}
  @item{@(racket /any) is not considered less than @(racket /minval)
 because there are no values which are less than @(racket /minval).}
  @item{@(racket /any) is not considered less than @(racket /void)
 because an infinite set of values compared against a set of zero values
 produces zero comparisons. Trivially, none of these comparisons are true.})

Remember that fallbacks are only considered when the primary value is dbnull.
The following example demonstrates a useless fallback.
Even though @(racket /minval) is less than zero, @(racket (val 42)) is never
dbnull so the fallback is never considered and the comparison always checks
whether 42 is less than zero.
@(repl-query
  (aw:show-table
   (from cat ProductCategory
         (where (.< (?? (val 42) /minval)
                    (val 0))))))

@subsection{Truth Table}
@margin-note{There is no significance to writing @(racket #true) instead of
 @(racket #t) here. It is just for the visual effect.}
@(define-syntax (make-truth-table stx)
   #`(racketblock #,truth-table-as-stx))
@(make-truth-table)
