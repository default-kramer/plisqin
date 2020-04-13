#lang scribble/manual

@(begin
   (require plisqin
            (for-label plisqin
                       "racket.rkt")
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
           [AlbumID #:type Number #:null no]
           [ReleaseYear #:type Number #:null yes]))
  (nullability (AlbumID Album))
  (nullability (ReleaseYear Album)))

When you build a larger token out of smaller ones, Plisqin usually determines the
correct nullability, as in the following example:
@(repl
  (code:comment "Neither operand is nullable, so the result is also `no`")
  (nullability (.+ (AlbumID Album)
                   (val 1)))
  (code:comment "(ReleaseYear Album) is nullable, so the result is `yes`")
  (nullability (.+ (ReleaseYear Album)
                   (val 1))))

TODO document the nullcheck behavior of each token constructor.
Teach people how to read it here.
Also mention using >> to override the nullability.
Or maybe do that after the 3VL stuff.

@section{Avoiding Three-Valued Logic}
In SQL, a boolean expression may have three values: true, false and unknown.
This makes it very easy for a programmer to make a mistake.
The most common mistakes occur with the comparison operators.
A programmer might write "x <= y" but fail to consider what should happen
if x is null or y is null or both are null.

@(load-checkpoint! "final")
@(void "TODO deftech{strict boolean} might help here")
The @tech{strict} version of Plisqin prevents these kinds of mistakes.
It does this in two ways:
@(itemlist
  @item{Anywhere that it accepts a @(racket Bool), it demands that its nullability
 is @(racket no), meaning that it cannot contain the unknown value.}
  @item{Anywhere that it returns a @(racket Bool), it guarantees that its nullability
 is @(racket no), meaning that it cannot contain the unknown value.})

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
@margin-note{TODO maybe put a footnote explaining why TotalQtySold is nullable.
 It is nullable because it is derived from the Product's group of SalesOrderDetails,
 and that group might have no records. Meaning it is a left join that might fail.
 We could make it non-nullable by coalescing it with zero, but it seems better to defer
 that to the caller. Some callers might want to know the difference between "some records
 that summed to zero" and "no records".}
@(repl
  (nullability (TotalQtySold Product)))

So in order to perform this comparison, we will use a @deftech{fallback}.
A fallback is a value that can be attached to any token.
The fallback tells Plisqin how dbnull should be handled.
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
  (show-table
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

@subsection{More Fallbacks}
TODO explain the other fallbacks.

@subsection{Truth Table}
@(define-syntax (make-truth-table stx)
   #`(racketblock #,truth-table-as-stx))
@(make-truth-table)
