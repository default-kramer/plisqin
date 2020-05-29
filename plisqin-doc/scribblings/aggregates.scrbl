#lang scribble/manual

@(begin
   (require (for-label "standard-label.rkt"))
   (require "helpers.rkt")
   )

@title{Aggregates}
@section{Grouping}
To understand aggregates, you must first understand how @(racket group-by)
changes the shape of the result set.
First, consider the following query.
It will return every row in the @(racket Product) table.
@(racketblock
  (from p Product))

If we filter out some rows using @(racket where) clauses, every row in
the result set still corresponds to exactly one Product.
@(racketblock
  (from p Product
        (where (.> (ListPrice p)
                   (val 100)))))

When we add a @(racket group-by) clause, the shape of the result set is very
different.
In the following query, each row of the result set no longer corresponds to a
single Product, but to a group of Products sharing the same Color.
@(repl-query
  (aw:show-table
   (from p Product
         (group-by (Color p))
         (select (Color p)))))

@subsubsub*section{An Illegal Select}
What would happen if you were to add @(racket (select (ListPrice p))) to the
previous example?
I call this an @deftech{illegal select} because it doesn't make sense
-- there might be 10 yellow Products that all have different List Prices.
PostgreSQL and MSSQL will raise an error if you try this.
Unfortunately for my demonstration, SQLite tolerates illegal selects.
In this example, it would choose a single ListPrice at random from the group.
But I recommend that you avoid illegal selects.

Note that @(racket (select (Color p))) was not a @tech{illegal select}.
We grouped by Color, so each group will have exactly one Color by definition.

@section{Aggregating}
It seems that @(racket group-by) is mostly useless on its own.
Once we add aggregates, things start to make sense.
Plisqin provides the following aggregate operations:
@(racket count), @(racket avg), @(racket min), @(racket max), @(racket sum).

If we have Products grouped by Color, it does not make sense to ask
"What is the ListPrice of each group of Products?"
But we can ask "What is the @italic{maximum} ListPrice of each group of Products?"
@(repl-query
  (aw:show-table
   (from p Product
         (group-by (Color p))
         (select (Color p))
         (select (count p))
         (select (min (ListPrice p)))
         (select (max (ListPrice p))))))

@subsubsub*section{Implicit Group Caveat}
You can use aggregates on an ungrouped query.
Doing so implicitly puts every item into a single group.
The following example is aggregating over the group of all Products.
@(repl-query
  (aw:show-table
   (from p Product
         (select (count p))
         (select (min (ListPrice p)))
         (select (max (ListPrice p))))))

Attempting to add @(racket (select (ListPrice p))) to the previous example
would be another @tech{illegal select}.
The grouping is implicit, but the reasoning is the same -- the group of all
Products does not have a single List Price.

@section{Aggregating Grouped Joins}
TODO continue writing.

Note that "Products grouped by Category" is a good candidate for grouped join
aggregation because you can start (from cat ProductCategory).
On the other hand, "Products grouped by Color" requires traditional
aggregation because there is no Color table.
