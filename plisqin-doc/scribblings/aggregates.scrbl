#lang scribble/manual

@(begin
   (require (for-label "standard-label.rkt"))
   (require "helpers.rkt")
   )

@title{Aggregates}
If you are going to follow along, be sure you
@(repl (require plisqin-examples/adventure-works/schema))
@(void
  ; This isn't just for the reader's benefit.
  ; We need to make sure the right bindings are in scope and not something
  ; left over from a `load-checkpoint!` call. Mutability burns me again!
  )

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

@section{Traditional Aggregation}
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

The previous query is an example of @deftech{traditional aggregation}.
Soon we will contrast traditional aggregation with another style.

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

@section{Illegal vs Quasi-Legal Selects}
We previously saw the following example of @tech{traditional aggregation}:
@(racketblock
  (from p Product
        (group-by (Color p))
        (select (Color p))
        (select (count p))
        (select (min (ListPrice p)))
        (select (max (ListPrice p)))))

What if we wanted to modify this query to group by ProductCategory
instead of by Color?
Sticking with @tech{traditional aggregation}, we can write the following query:
@(repl-query
  (aw:show-table
   (from p Product
         (group-by (ProductCategoryID p))
         (select (ProductCategoryID p))
         (select (CategoryName p))
         (select (count p))
         (select (min (ListPrice p)))
         (select (max (ListPrice p))))))

In the previous query, @(racket (select (CategoryName p))) might look like an
@tech{illegal select}, but I am going to say that it is a
@deftech{quasi-legal select} instead, and here is why:
@(itemlist
  @item{We have already grouped by ProductCategoryID.}
  @item{ProductCategoryID is a unique key of the Category table.}
  @item{Therefore, for every column C of the Category table, each group contains
 exactly one value of C.}
  @item{CategoryName is a column of the Category table.}
  @item{Therefore, each group contains exactly one CategoryName.}
  @item{Therefore, "What is the CategoryName of each group?" has an unambiguous
 answer, and it is not an @tech{illegal select}.})

@margin-note{
 I think I read that PostgreSQL now accepts some quasi-legal selects, but
 only when it can prove that they are not illegal, which is probably impossible
 to do with 100% accuracy.}
None of this would be worth mentioning if every database permitted these
quasi-legal selects, but they don't.
MSSQL rejects all quasi-legal selects, so we would have to modify our query.
Adding @(racket (group-by (CategoryName p))) would be acceptable to MSSQL.
But we are using SQLite which accepts all quasi-legal selects (and illegal
selects too), so the query works and produces a reasonable result.

@section{Grouped Join Aggregation}
We have seen a few examples of @tech{traditional aggregation}, but now it is
time to introduce @deftech{grouped join aggregation}.
First, we will define a @deftech{grouped join} to mean "any join which contains
a @(racket group-by) clause."
The following procedure returns a grouped join:
@(repl
  (define/contract (Products-by-Category cat)
    (-> (instanceof ProductCategory) (instanceof Product))
    (join p Product #:to cat
          (join-type 'left)
          (group-by (ProductCategoryID p))
          (join-on (.= (?? (ProductCategoryID p) /void)
                       (ProductCategoryID cat))))))

When a grouped join is passed into an aggregate, the aggregate recognizes the
grouped join and performs the aggregation over that group.
This is called @tech{grouped join aggregation}; here is an example:
@margin-note{
 According to my normal naming convention, @(racket group-of-products) would
 be named @(racket productsG) for brevity. The longer name is for clarity.}
@(repl-query
  (aw:show-table
   (from cat ProductCategory
         (select (ProductCategoryID cat))
         (select (CategoryName cat))
         (define group-of-products
           (Products-by-Category cat))
         (select (count group-of-products))
         (select (min (ListPrice group-of-products)))
         (select (max (ListPrice group-of-products))))))

Notice that @tech{grouped join aggregation} gives us better composability than
@tech{traditional aggregation} thanks to separation of concerns.
Specifically, the "grouping" and "aggregating" concerns are now separated.
The @(racket Products-by-Category) procedure says nothing about aggregation;
it only encodes the relationship "a ProductCategory has a group of Products."
This means we can reuse it in other queries with other aggregations.
And each aggregate (such as @(racket max)) says nothing about grouping,
but it will accept any grouped join you give it.

Also notice that we no longer have the @tech{quasi-legal select} that we did
in the traditional aggregation version of this query.
Each @(racket select) is fully legal here.
This means that grouped join aggregation is usually easier to use if you
are using a database that does not allow quasi-legal selects.

Finally, notice how @tech{grouped join aggregation} allows us to define an
aggregate expression as a function of a single argument.
The caller of this function sees @(racket MinListPrice) as just another
property of the @(racket ProductCategory) table:
@(racketblock
  (define/contract (MinListPrice cat)
    (-> (instanceof ProductCategory) Number?)
    (min (ListPrice (Products-by-Category cat)))))

@section{Summary}
@tech{Grouped join aggregation} is more composable than
@tech{traditional aggregation}, but it is also more limited.
That is, grouped join aggregation can always be converted to traditional
aggregation, but the reverse it not true.
Let's look at the first example of traditional aggregation again:
@(racketblock
  (code:comment "Traditional aggregation, this works:")
  (from p Product
        (group-by (Color p))
        (select (Color p))
        (select (count p))
        (select (min (ListPrice p)))
        (select (max (ListPrice p))))
  (code:comment "Grouped join aggregation, this does not work")
  (code:comment "because there is no Color table:")
  (from c Color
        (define productsG (Products-by-Color c))
        (select (ColorName c))
        (select (count p))
        (select (min (ListPrice p)))
        (select (max (ListPrice p)))))

In the previous example, grouped join aggregation is impossible because the
Color table was hypothetical; it does not exist.
But we do have a ProductCategory table, which is why both styles are
possible in the following example:
@(racketblock
  (code:comment "traditional aggregation:")
  (from p Product
        (group-by (ProductCategoryID p))
        (select (ProductCategoryID p))
        (code:comment "A quasi-legal select; this won't work in some databases:")
        (select (CategoryName p))
        (select (count p))
        (select (min (ListPrice p)))
        (select (max (ListPrice p))))
  (code:comment "grouped join aggregation:")
  (from cat ProductCategory
        (code:comment "All selects are fully legal here:")
        (select (ProductCategoryID cat))
        (select (CategoryName cat))
        (define group-of-products
          (Products-by-Category cat))
        (select (count group-of-products))
        (select (min (ListPrice group-of-products)))
        (select (max (ListPrice group-of-products)))))

In the previous example, the two styles will produce different result sets.
The traditional aggregation will include one more row representing the group
of Products that don't belong to any ProductCategory.
(If every Product belonged to a Category, there would be no difference.)
In this situation, I recommend that you choose grouped join aggregation unless
you need the following "null Category" to appear in your result set:
@(repl-query
  (aw:show-table
   (from p Product
         (group-by (ProductCategoryID p))
         (select (ProductCategoryID p))
         (select (CategoryName p))
         (select (count p))
         (select (min (ListPrice p)))
         (select (max (ListPrice p))))))
