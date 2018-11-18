#lang scribble/manual
@(require (for-label plisqin
                     "racket.rkt"
                     plisqin/examples/video-rental-schema))
@(require scribble/eval
          plisqin
          rackunit
          "helpers.rkt"
          "racket.rkt")

@title[#:tag "layer0"]{Layer 0 - The Core}
The core of Plisqin is pretty small.
It consists of
@itemlist[@item{Fragments (such as @(racket where), @(racket order-by), @(racket scalar), etc...)}
          @item{Queries, created by @(racket from)}
          @item{Joins, created by @(racket join)}
          @item{Injections, which are automatically inferred}]

@(define-syntax-rule (interact forms ...)
   (interaction
    #:eval my-eval
    forms ...))

@section{Fragments}
The smallest building blocks in Plisqin are fragments.
@(racketblock
  (from x "X"
        (where (bool (scalar x".Bar")" = 3"))
        (select (scalar x".Foo"))))

The above example uses 4 different kinds of fragments:
@(racket where), @(racket select), @(racket bool), and @(racket scalar).
Note that @(racket where) and @(racket select) are special - they belong
in "root position" of the query, and are called "clauses".
(All clauses are fragments; not all fragments are clauses.)

Note that non-clause fragments (like @(racket scalar) and @(racket bool))
are usually not written directly into a query like the above example.
More typically, non-clause fragments are used as the return value of procedures:
@(racketblock
  (define (Bar x)
    (scalar x".Bar"))
  (define (Foo x)
    (scalar x".Foo"))
  (define (my= a b)
    (bool "("a" = "b")"))
  (from x "X"
        (where (my= (Bar x) 3))
        (select (Foo x))))

The reason for using non-clause fragments like @(racket scalar) won't
become clear until later sections.
For now, just know that fragments exist, and some of them are clauses.

@section{From}
The @(racket from) macro is used to create queries.
The simplest possible query is:
@(racketblock (from x "Y"))

First we have @(racket x) which must be an identifier.
It is analogous to an alias in standard SQL.
Next we have @(racket "Y") which is the name of the table.
Let's just see how this query renders to SQL:
@(interact
  (display (to-sql (from x "Y"))))

We can see in the generated SQL that the table name is "Y", with an alias of "x".
We can also see that, since there are no clauses, Plisqin generated
@(racketoutput "select x.*") to make a valid query.

@subsection{Appending Queries}
The value representing the table does not have to be a string like @(racket "Y").
One of the most interesting things it can be is another query:
@(interact
  (define (query-1)
    (from x "X"
          (select x".one")
          (select x".two")))
  (define (query-2)
    (from y (query-1)
          (select y".three")
          (select y".four")))
  (displayln (to-sql (query-2))))

In the example above, @(racket query-2) appends to @(racket query-1).
You can see in the generated SQL that all 4 @(racket select) clauses are present in @(racket query-2).
You can also see that the original alias from @(racket query-1) (x) is used, but this behavior is not guaranteed.

@subsection{Always Use the Alias!}
In Plisqin, it is highly recommended to always use the alias instead of naked column names:
@(racketblock
  (from r "Rental"
        (code:comment "bad:")
        (select "CopyId")
        (code:comment "better:")
        (select r".CopyId")))

This is because queries in Plisqin are highly composable, so even if a query is unambiguous locally,
it might become ambiguous if another query appends to it.
If someone appends to the above example and adds a join from @(racket Rental) to @(racket Copy),
the naked @(racket "CopyId") becomes ambiguous, and the generated SQL is invalid.

@subsection{Query Contents}
We have already seen clauses, which are the most common contents of a query.
But we can actually use any expression that evaluates to a clause, or a list of clauses:
@(interact
  (define (my-query)
    (from x "X"
          (if #t
              (select x".truth")
              (error "true is false?!"))
          (if #t
              (list
               (where "1=1")
               (where "2=2"))
              (error "true is false?!"))))
  (display (to-sql (my-query))))

There are also some forms that get special handling.
One is used to create joins, which we will look at very soon.
The other is define, which is a limited version of racket's built-in @(racket define).
@(interact
  (define (my-query)
    (from x "X"
          (code:comment "define a value:")
          (define rating
            (scalar x".Rating"))
          (code:comment "define a procedure:")
          (define (count-when predicate)
            (scalar "sum(case when "predicate" then 1 else 0 end)"))
          (code:comment "the definitions are available until the (from ...) is closed")
          (select (count-when (bool rating" >= 7"))
                  " as NumGoodRatings")
          (select (count-when (bool rating" >= 9"))
                  " as NumGreatRatings")))
  (display (to-sql (my-query))))

@section{Join}
Joins are much like queries.
Everything that you can do inside @(racket from), you can also do inside @(racket join).
But joins can be a little more confusing, because there are two variations of
what @(racket join) actually means, depending on its context.
Here one way that joins can be used:
@(racketblock
  (define (Foo-of x)
    (join f "Foo"
          (join-on f".FooId = "x".FooId")))
  (from b "Bar"
        (select (Foo-of b)".Baz")))

Another way is this:
@(racketblock
  (define (Foo-of x)
    (join f "Foo"
          (join-on f".FooId = "x".FooId")))
  (from b "Bar"
        (code:comment "The colon in b:foo means nothing. It could be any identifier.")
        (join b:foo (Foo-of b))
        (select b:foo".Baz")))

Yet another way is this:
@(racketblock
  (from b "Bar"
        (join f "Foo"
              (join-on f".FooId = "x".FooId"))
        (select f".Baz")))

These 3 variations will all generate the same SQL in this particular example,
but there is a subtle difference that the next section will explain.

@subsection{Attached vs Detached Joins}
Look at these two variations.
The only difference is that the first uses @(racket define) where the second uses @(racket join).
@(interact
  (define (variation-one)
    (from x "X"
          (define y (join y "Y"
                          (join-on y".YID = "x".YID")))
          (where (exists (from z "Z"
                               (where z".Foo = "y".Foo"))))))
  (define (variation-two)
    (from x "X"
          (join y (join y "Y"
                        (join-on y".YID = "x".YID")))
          (where (exists (from z "Z"
                               (where z".Foo = "y".Foo")))))))

But the generated SQL will be different,
specifically with regard to where the @(racket y) join gets rendered:
@(interact
  (display (to-sql (variation-one)))
  (display (to-sql (variation-two))))

In @(racket variation-one), @(racket y) is a "detached" join.
This join is located inside the subquery @(racket z) so that is where it gets rendered in SQL.
You might think that @(racket y) has some special relationship to the main query @(racket x)
because @(racket y) mentions @(racket x) in its @(racket join-on) clause.
But that is not the case.
Plisqin does not try to guess where a join belongs based on the contents of
its @(racket join-on) clauses; doing so would be even more confusing!
(Especially when you remember that the presence or absence of a join can affect
the number of rows returned even if it is not used in any clauses.)

In @(racket variation-two), @(racket y) is an "attached" join.
It is attached to its enclosing query @(racket x).
The pedagogical way to create an attached join is @(racket join-attach).
Consider this example:
@(racketblock
  (define (make-join a)
    (join x "X"
          (join-attach y "Y"
                       (join-on y".XID = "x".XID"))
          (join-on y".AID = "a".AID"))))

I used @(racket join-attach) to make it more clear
that it is a special form recognized by its enclosing join.
The outer join "X" sees @(racket join-attach) and attaches that join to itself.
But @(racket join-attach) is verbose, so it has an alias @racketfont{join}.
Don't let the syntax highlighting in the following example fool you; it is exactly
the same as the previous example:
@(racketblock
  (define (make-join a)
    (join x "X"
          (code:comment "the first word following this comment means join-attach")
          (join y "Y"
                (join-on y".XID = "x".XID"))
          (join-on y".AID = "a".AID"))))

@subsection{Joined Subqueries}
If all of a join's clauses are @(racket join-on) clauses, then I call it a "simple join".
This is probably the most common kind of join.
But if a join contains any clauses other than @(racket join-on), the rules
of SQL require that it gets rendered as a joined subquery:
@(interact
  (define (Foo-of x)
    (join f "Foo"
          (select f".*")
          (where "1=1")
          (join-on f".FooId = "x".FooId")))
  (define (my-query)
    (from b "Bar"
          (select b".*")
          (select (Foo-of b)".blah")))
  (display (to-sql (my-query))))

This highlights a scoping issue of non-simple joins.
If you want your Racket code to match the generated SQL more closely,
you could write the above example like this instead:
@(racketblock
  (join f (from f "Foo"
                (select f".*")
                (where "1=1"))
        (join-on f".FooId = "x".FooId")))

This makes it more clear that the @(racket select) and @(racket where) clauses live
inside the subquery, while the @(racket join-on) clause lives outside of it.
There is a clarity/verbosity tradeoff here; you can decide your own preference.
But I prefer the first, less verbose version.

@subsection[#:tag "join-query-conversion"]{Join-to-Query Conversion}
We have already seen queries that append to existing queries.
Queries can also append to joins, after the join is automatically converted to a query.
In practice, this means that the @(racket join-on) clauses are changed to @(racket where) clauses.
This is most useful for subqueries:
@(interact
  (define (Copies-for-Item i)
    (join c "Copy"
          (join-on c".ItemId = "i".ItemId")))
  (define (my-query)
    (from i "Item"
          (where (exists (from copy (Copies-for-Item i)
                               (code:comment "append a clause just for fun:")
                               (where "1=1"))))))
  (display (to-sql (my-query))))
@(check-sql
  my-eval (my-query)
  #<<HEREDOC
select i.*
from Item i
where exists (
    select c.*
    from Copy c
    where c.ItemId = i.ItemId
    and 1=1)
HEREDOC
  )

You can see that @(racketoutput "c.ItemId = i.ItemId") was a @(racket join-on)
clause that got converted to a @(racket where) clause.

@section[#:tag "injections"]{Injections}
Injections are used in the context of a grouped join.
A grouped join is a join that contains a @(racket group-by) clause.
For example, this procedure encodes the fact that every @(racket Copy)
has a group of @(racket Rental)s:
@(interact
  (define (Rentals-by-Copy copy)
    (join r "Rental"
          (group-by r".CopyId")
          (join-on r".CopyId = "copy".CopyId"))))

This seems reasonable, right?
But this won't work when we render it to SQL:
@(interact
  (define (my-query)
    (from c "Copy"
          (join r (Rentals-by-Copy c))))
  (display (to-sql (my-query))))

SQL Server tells me "Column 'Rental.RentalId' is invalid in the select list because
it is not contained in either an aggregate function or the GROUP BY clause."
One approach I could use is to manually manage which columns get @(racket select)ed:
@(interact
  (define (Rentals-by-Copy copy)
    (join r "Rental"
          (group-by r".CopyId")
          (code:comment "manually select the CopyId column here:")
          (select r".CopyId")
          (join-on r".CopyId = "copy".CopyId")))
  (display (to-sql (my-query))))

Manually managing the select list is not ideal.
If I use a @(racket scalar) expression instead, Plisqin can automatically add it to
the select list inside the grouped join:
@(interact
  (define (Rentals-by-Copy copy)
    (join r "Rental"
          (group-by (scalar r".CopyId"))
          (join-on (scalar r".CopyId")" = "(scalar copy".CopyId"))))
  (display (to-sql (my-query))))

The above example demonstrates the first type of injection, called @bold{scalar injection}.
Plisqin could recognize that the @(racket (scalar r".CopyId")) expression which occurs inside
the @(racket join-on) clause must be rewritten.
The scalar gets "injected into" the grouped join as a @(racket select) clause with
the synthesized name @(racketoutput "__INJECT1").
Then, outside of the grouped join, it is accessed by that synthesized name.

All of this isn't very useful, until we pair it with the other type of injection which
is @bold{aggregate injection}. Here is a quick example:
@(interact
  (define (my-query)
    (from c "Copy"
          (join r (Rentals-by-Copy c))
          (select (count r)" as NumRentals")
          (select (avg r".PricePaid")" as AveragePricePaid")
          (select c".*")))
  (display (to-sql (my-query))))
@(check-sql
  my-eval (my-query)
  #<<HEREDOC
select
  r.__INJECT2 as NumRentals
  , r.__INJECT3 as AveragePricePaid
  , c.*
from Copy c
inner join (
    select
      r.CopyId as __INJECT1
      , count(*) as __INJECT2
      , avg(r.PricePaid) as __INJECT3
    from Rental r
    group by r.CopyId) r
on r.__INJECT1 = c.CopyId
HEREDOC
  )

In the above example, both @(racket (count r)) and @(racket (avg r".PricePaid")) get injected
into the grouped join in a similar way to the scalar injection we already saw.
I don't have to manually manage the select list to include these aggregates.
This is very useful: I have the ability to perform aggregate operations on a grouped join
from outside that grouped join, without changing its definition, or even parameterizing it!

@subsection{Aggregate Injection in Depth}
The @(racket aggregate) fragment is what is important for aggregate injection.
Plisqin provides some built-in aggregates such as @(racket count) and @(racket avg),
but these are not essential; they work because they return @(racket aggregate)s:
@(racketblock
  (avg "something")
  (code:comment "is roughly equivavlent to:")
  (aggregate "avg(" "something" ")"))


The @bold{target} of an aggregate is the grouped join that it will be injected into.
An aggregate may not have multiple targets, but it may have none.
For example, the following aggregate does not have a target so no injection occurs:
@(interact
  (display (to-sql (from x "X"
                         (select (count x))))))

The following aggregate has two potential targets, so an error is immediately
reported and the aggregate is not constructed:
@(interact
  (attach-callstacks)
  (define (make-grouped-join x)
    (join gj "GJ"
          (group-by (scalar gj".GroupKey"))
          (join-on (scalar gj".GroupKey")" = "x)))
  (aggregate
   (make-grouped-join 1)
   (make-grouped-join 2)))

To recap, if an aggregate has no target then no injection occurs.
An aggregate is not allowed to have more than 1 target.
So the rest of this section is only interested in aggregates that have 1 target.

Finding an aggregate and its target is the key to understanding injections.
@(interact
  (define (my-query)
    (from x "X"
          (join j (make-grouped-join (scalar x".Id")))
          (select (avg j".foo")" as AverageFoo")
          (select (max j".bar")" as MaxBar"))))

The above example has two aggregates.
The target of both is @(racket j), the grouped join constructed by @(racket make-grouped-join).
The aggregate expressions get injected into their target (scalar injection also makes an appearance here):
@(interact
  (display (to-sql (my-query))))

@subsection{Nested Aggregates}
The only caveat left to consider is nesting.
I would recommend avoiding nesting if possible, because it can be confusing.
I'm going to use the Video Rental Example Schema here.
An @(racket Item) has many @(racket Copy)s, and a @(racket Copy) has many @(racket Rental)s.

I also need to introduce @tech{of/g} which is pronounced "of" but carries extra meaning.
The @(racket /g) signifies "grouping", so "the Copies @tech{of/g} the Item" means
"the (group of) Copies of the Item."
And per this naming convention, @(racket (Copies-of/g item)) returns a grouped join.

If I wanted to know how many times an Item has been rented, I could do either of these:
@(racketblock
  (count (Rentals-of/g item))
  (code:comment "or")
  (sum (count (Rentals-of/g (Copies-of/g item)))))

The first is a non-nested case that we have already seen:
we are just counting a grouped join of Rentals grouped by Item.
The second involves nesting, which is what we are interested in.
Here is my best attempt at translating the code to its almost-English meaning:
@(define props '(right bottom-border))
@(define of/g @tech{of/g})
@tabular[
 #:sep @hspace[2]
 #:row-properties `(() ,props () ,props () ,props () ,props () right)
 (list
  (list @(racket item))
  (list @elem{the Item})
  (list @(racket (Copies-of/g item)))
  (list @elem{the Copies @of/g the Item})
  (list @(racket (Rentals-of/g (Copies-of/g item))))
  (list @elem{the Rentals @of/g the Copies @of/g the Item})
  (list @(racket (count (Rentals-of/g (Copies-of/g item)))))
  (list @elem{(the count of the Rentals @of/g) the Copies @of/g the Item})
  (list @(racket (sum (count (Rentals-of/g (Copies-of/g item))))))
  (list @elem{(the sum of (the count of the Rentals @of/g) the Copies @of/g) the Item}))]

I added the parentheses to show how aggregates resolve grouped joins.
Specifically, when the @(racket count) expression appears, it finds
that "the Rentals @of/g" is unresolved, and resolves it.
When the @(racket sum) expression appears, it finds that "the Copies @of/g"
is unresolved, and resolves it.
Importantly, when we reach the final step, all the grouped joins are resolved.
(Visually, every @of/g is enclosed in an aggregate.)
If the result had had an unresolved grouped join, it would not be a valid scalar.
Plisqin does not currently detect this error; it will just generate SQL that will produce
an error when it is run.

To wrap things up, look at the SQL generated by both the nested and non-nested variants.
First, the complicated nested variant:
@(interact
  (define (my-query)
    (from item Item
          (select (sum (count (Rentals-of/g (Copies-of/g item))))
                  " as NumRentals")))
  (display (to-sql (my-query))))
@(check-sql
  my-eval (my-query)
  #<<HEREDOC
select
  _copy.__INJECT2 as NumRentals
from Item item
inner join (
    select
      _copy.ItemId as __INJECT1
      , sum(_rental.__INJECT2) as __INJECT2
    from Copy _copy
    inner join (
        select
          _rental.CopyId as __INJECT1
          , count(*) as __INJECT2
        from Rental _rental
        group by _rental.CopyId) _rental
    on _rental.__INJECT1 = _copy.CopyId
    group by _copy.ItemId) _copy
on _copy.__INJECT1 = item.ItemId
HEREDOC
  )

And here is the SQL of the simpler non-nested variant:
@(interact
  (define (my-query)
    (from item Item
          (select (count (Rentals-of/g item))
                  " as NumRentals")))
  (display (to-sql (my-query))))
@(check-sql
  my-eval (my-query)
  #<<HEREDOC
select
  _rental.__INJECT2 as NumRentals
from Item item
inner join (
    select
      _copy.ItemId as __INJECT1
      , count(*) as __INJECT2
    from Rental _rental
    inner join Copy _copy
    on _copy.CopyId = _rental.CopyId
    group by _copy.ItemId) _rental
on _rental.__INJECT1 = item.ItemId
HEREDOC
  )