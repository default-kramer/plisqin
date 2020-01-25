#lang scribble/manual
@(require (for-label plisqin-lib
                     "racket.rkt"))
@(require scribble/eval
          plisqin-lib
          rackunit
          "helpers.rkt"
          "racket.rkt")

@title{Design Notes}

@section{Cardinality Tracking}
This is something I've always wanted from SQL.
@(racketblock
  (def/append! (Foo x)
    (join f Foo
          (join-on f.FooId = x.FooId)
          (assert-cardinality x))))
In the above example @(racket (assert-cardinality x)) is a new clause that says
"I (the programmer) think the cardinality of this join is x, give an error if you can't prove me right."
Plisqin would have to know that @(racket FooId) is unique in order to prove this.
There would also be something like @(racket trust-cardinality) which would say
"I (the programmer) am telling you the cardinality of this join is x, just take my word for it."

But... it might be difficult to make it smart enough to solve actual problems.

@subsection{Naming Convention?}
Another take on this is to simply use a naming convention.
Just as Racket uses the exclamation point (e.g. @(racket set!)) to indicate mutation,
I think Plisqin could use a naming convention to indicate cardinality.
How about this:
@(tabular
  #:sep (hspace 2)
  (list (map bold (list "Name" "Meaning" "May Duplicate?" "May Eliminate?"))
        (list (racket (foo* x)) "zero or more" "yes" "yes")
        (list (racket (foo+ x)) "one or more" "yes" "no")
        (list (racket (foo- x)) "one or fewer" "no" "yes")
        (list (racket (foo x)) "exactly one" "no" "no")))

Let's assume that we are only dealing with inner joins.
"May Duplicate?" means that including the expression in the query may increase the
cardinality of the result set.
"May Eliminate?" means that including the expression in the query may filter the
result set.

If we use a left join instead of an inner join, then "May Eliminate?" means
"May Introduce Null?" instead.

So I think we have 3 separate concepts: duplication, elimination, and nullability.
I think they are orthogonal?
It is possible to have an expression that does all three.
For example, assume that @(racket (orders* product)) is an inner join
representing the "zero or more" orders that a product has.
This join may duplicate some products and eliminate others.
Now if we have
@(racketblock
  (define (blah___ product)
    (code:comment "assume shipment-date is a nullable column of the order table")
    (shipment-date/? (orders* product))))
What do we want @(racket blah___) to be named?
It may duplicate some products, eliminate other products, and finally introduce
nulls on any rows that remain.

Perhaps I am not thinking precisely enough about nullability.
A failed left join really represents "potential nullability" - a null will not appear
until I try to access a column.
Perhaps it is a mistake to conflate the "potential nullability" of join with the
"actual nullability" of a scalar?

Ah ha, I forgot that my @(racket blah___) example from earlier is not allowed by my
personal convention: inline joins may not be duplicating.
So perhaps it doesn't matter.

Just to help me think, how ridiculous could I get with this?
Let's say we encode all 3 concepts into the naming convention.
@(tabular
  #:sep (hspace 2)
  (list (map bold (list "Name" "Duplicating?" "Eliminating?" "Nullable?"))
        (list (racket foo+-?) "yes" "yes" "yes")
        (list (racket foo+-) "yes" "yes" "no")
        (list (racket foo+?) "yes" "no" "yes")
        (list (racket foo+) "yes" "no" "no")
        (list (racket foo-?) "no" "yes" "yes")
        (list (racket foo-) "no" "yes" "no")
        (list (racket foo?) "no" "no" "yes")
        (list (racket foo) "no" "no" "no")))
Well, that's the answer I guess.
All eight kinds of expressions are possible.
But what an ugly naming convention that would be.
Not to mention that the question mark is reserved for predicates.

Does this look any better?
@(tabular
  #:sep (hspace 2)
  (list (map bold (list "Name" "Duplicating?" "Eliminating?" "Nullable?"))
        (list (racket /foo*) "yes" "yes" "yes")
        (list (racket foo*) "yes" "yes" "no")
        (list (racket /foo+) "yes" "no" "yes")
        (list (racket foo+) "yes" "no" "no")
        (list (racket /foo-) "no" "yes" "yes")
        (list (racket foo-) "no" "yes" "no")
        (list (racket /foo) "no" "no" "yes")
        (list (racket foo) "no" "no" "no")))

Maybe, but still too cumbersome to be practical I'd bet.

@section{The Reader - Lessons Learned}
This has tripped me up twice now, so let me write down my thoughts while they are fresh.
It seems that Scribble works using the output of @(racket read-syntax).
If you want things to get labeled properly, you need to make sure the syntax you produce
has an obvious correspondence to the original source code.
(It seems that @(racket srcloc) and @(racket prop) are important to this correspondence.)

Specifically, one thing I tried was having Racket's built-in @(racket read-syntax) return
and then wrapping dotted identifiers in something like @(racket (#%DOTS foo.bar foo bar))
thinking "this will be perfect, the label phase can see all 3 pieces of syntax I might want
to use. Then I can decide later which one I actually want." This approach doesn't work.
I'm guessing it is because Scribble doesn't know which one(s) I will end up using.

Also, customizing the readtable for @(racket #\.) wasn't the best approach either.
It requires a terminating macro which breaks other things, most notably @(racket 3.14) no longer
reads as a number. Also, it wasn't obvious how to know whether the most recent paren-shape was
braces or not. This meant that @(racket a.b) would read as @(racket a .b) everywhere, and I didn't
really want that outside of braces.

So now to handle dotted identifiers I just let Racket read everything as normal.
Then, before returning from @(racket read-syntax) I find dotted identifiers inside braces and split them
apart, for example @(racket {a.b}) becomes @(racket {a |.| b}) (but with correct srclocs).
This makes it easy for Scribble to figure out the correspondence.
Then later in module begin, I can rewrite @(racket {a |.| b}) to something like
@(racket {(#%do-dot a b)}).
(Scribble can probably handle this kind of rewrite, but best to keep things simple.)
