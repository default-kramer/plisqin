#lang scribble/manual
@(require (for-label plisqin
                     "racket.rkt"
                     plisqin/examples/video-rental-schema))
@(require scribble/eval
          plisqin
          rackunit
          "helpers.rkt"
          "racket.rkt")
@(define-syntax-rule (interact forms ...)
   (interaction
    #:eval my-eval
    forms ...))

@(define-syntax-rule (heading stuff)
   (section stuff))

@title[#:tag "intro"]{Introduction}

I love relational databases, but SQL usually leaves me disappointed.
Plisqin is an SQL generator written in Racket.
It is intended to demonstrate some shortcomings of SQL
and explore how a better database query language could be designed.

Code is available @hyperlink["https://github.com/default-kramer/plisqin"]{on GitHub}.

@subsubsub*section{@larger{Reading Racket Code}}
This document will contain a lot of Racket code:
@(interaction
  #:eval my-eval
  (if (integer? 12.3) "an int" "not an int"))
Notice how @(racket if) and @(racket integer?) are hyperlinked.
Even if you have no Racket/Scheme/Lisp experience, you might be able to follow along
by using these links when you are not sure what some code is doing.

@subsubsub*section{@larger{The Video Rental Example Schema}}
Here is a query in Plisqin:
@(racketblock
  (from r "Rental"
        (where r".RentalId > 100")))

Here is an (almost) equivalent query:
@(racketblock
  (from r Rental
        (where (RentalId r)" > 100")))

When you see upper-case identifiers such as @(racket Rental) and @(racket RentalId),
they almost certainly refer to part of the @secref{Video_Rental_Example_Schema}.
This schema models a brick-and-mortar video rental store (think pre-Netflix).
Keep @hyperlink["https://github.com/default-kramer/plisqin/raw/master/examples/video-rental-diagram.PNG"]{this
 database diagram} handy while reading this document.

@heading{Appendable Queries}
The @(racket from) macro is used to create queries.
Here @(racket new-releases-1) is defined as a procedure that creates and returns a query.
Then we render it to SQL:
@(interact
  (define (new-releases-1)
    (from i "Item"
          (where i".ReleaseDate > '2018-01-01'")
          (select i".ItemName")))
  (display (to-sql (new-releases-1))))

This query has 2 clauses: one @(racket where) clause and one @(racket select) clause.
It is easy to add more clauses to an existing query.
Here, we create @(racket new-releases-2) which appends 3 more clauses to @(racket new-releases-1):
@(interact
  (define (new-releases-2)
    (from i (new-releases-1)
          (select i".ReleaseDate")
          (where i".ItemTypeId = 1")
          (order-by i".ReleaseDate asc")))
  (display (to-sql (new-releases-2))))

This isn't very exciting alone, but combined with the rest of Plisqin and the full
power of Racket, we can get great composability that SQL lacks.

@heading{Reusable Joins}
Joins can be defined as Racket procedures and reused.
For example, I know that every @(racket Rental) has exactly one @(racket Item).
I have defined this join in the @(racket Item-of) procedure:

@(interact
  (define (recent-rentals)
    (from rental "Rental"
          (join item (Item-of rental))
          (where item".ReleaseDate >= '2018-11-22'")))
  (display (to-sql (recent-rentals))))

You can see in the generated SQL that I glossed over some details.
I said that every @(racket Rental) has exactly one @(racket Item).
To be more precise, I could have said that
every @(racket Rental) has exactly one @(racket Copy) (joined using CopyId),
and every @(racket Copy) has exactly one @(racket Item) (joined using ItemId).
But those are details I don't want to think about at this level.
I would much rather just say @(racket (Item-of rental)).

@heading{Inline Joins}
Joins are allowed to appear pretty much anywhere inside a query.
This has some powerful results.
For starters, I could have defined the previous example like this instead:
@(racketblock
  (define (recent-rentals)
    (from rental "Rental"
          (where (Item-of rental)".ReleaseDate >= '2018-11-22'"))))

The join is "inline" in the @(racket where) clause.
But we can do even better.
The following example also contains an inline join, can you guess where it is?

@(interact
  (define (recent-rentals)
    (from rental "Rental"
          (where (ReleaseDate rental)" >= '2018-11-22'")))
  (display (to-sql (recent-rentals))))

The inline join comes from @(racket (ReleaseDate rental)), which returns
@(racketblock (scalar (Item-of rental)".ReleaseDate"))

Putting joins inside scalars can be very useful.
It is nice to be able to say "the ReleaseDate of a Rental" without repeating the details
of any join(s) that might be involved.

@heading{Aggregates}
This concept is a little more complex, so let's start with some hand-written SQL.
Let's imagine that I need to write a query that returns @(racket Customer)s with
1) their number of @(racket Rental)s and
2) their number of movie @(racket Rental)s.
(Our store rents things other than movies, such as video games.)
This is the SQL I might write:
@(show-sql #<<HEREDOC
select customer.*
  , rentalSummary.MovieRentalCount
  , rentalSummary.TotalRentalCount
from Customer customer
inner join (
    select checkout.CustomerId
      , sum(case when item.ItemTypeId = 1 then 1
                 else 0 end) as MovieRentalCount
      , count(*) as TotalRentalCount
    from Rental rental
    inner join Copy copy on copy.CopyId = rental.CopyId
    inner join Item item on item.ItemId = copy.ItemId
    inner join Checkout checkout on checkout.CheckoutId = rental.CheckoutId
    group by checkout.CustomerId
) rentalSummary
on rentalSummary.CustomerId = customer.CustomerId
HEREDOC
           )

This is a pretty common pattern (for me at least):
@itemlist[@item{Group a query @italic{(group Rentals by CustomerId)}}
          @item{Join it to the main query @italic{(join on rentalSummary.CustomerId ...)}}
          @item{Select the aggregates @italic{(select MovieRentalCount and TotalRentalCount)}}
          #:style 'ordered]

But this hand-written SQL suffers from major composability problems.
Notice how the aggregate expressions (MovieRentalCount
and TotalRentalCount) are "sealed inside" the grouped join.
Ideally I want to be able to define the grouping and joining (parts 1 and 2)
independently of the aggregate operations (part 3).
Plisqin allows me to do this:
@(interact
  (define (my-query)
    (from cust Customer
          (join rentals (grouped-Rentals-of cust))
          (select cust".*")
          (select (count rentals)
                  " as TotalRentalCount")
          (select (sum "case when "(ItemTypeId rentals)" = 1 then 1 else 0 end")
                  " as MovieRentalCount")))
  (display (to-sql (my-query))))
@(check-sql
  my-eval (my-query)
  #<<HEREDOC
select
  cust.*
  , _rental.__INJECT2 as TotalRentalCount
  , _rental.__INJECT3 as MovieRentalCount
from Customer cust
inner join (
    select
      _checkout.CustomerId as __INJECT1
      , count(*) as __INJECT2
      , sum(case when _item.ItemTypeId = 1 then 1 else 0 end) as __INJECT3
    from Rental _rental
    inner join Checkout _checkout
    on _checkout.CheckoutId = _rental.CheckoutId
    inner join Copy _copy
    on _copy.CopyId = _rental.CopyId
    inner join Item _item
    on _item.ItemId = _copy.ItemId
    group by _checkout.CustomerId) _rental
on _rental.__INJECT1 = cust.CustomerId
HEREDOC
  )

In the above example, @(racket (grouped-Rentals-of cust)) returns a grouped join.
This join has no select clauses of its own,
but @(racket (count rentals)) gets "injected into" this grouped join and exposed via
the synthetic name __INJECT2.
Similarly, the larger @(racket sum) expression also gets "injected into" the grouped join as __INJECT3.
(Don't worry about __INJECT1 for now.)

Notice how the aggregate expressions live outside of the grouped join.
In the following example two more aggregate expressions are added and new injections
appear, while @(racket (grouped-Rentals-of cust)) does not change.
@(interact
  (define (my-query)
    (from cust Customer
          (join rentals (grouped-Rentals-of cust))
          (code:comment @#,elem{Local definitions can help readability:})
          (define (is-movie rental)
            (bool (ItemTypeId rental)" = 1"))
          (define (is-recent rental)
            (bool (CheckoutTime rental)" > '2018-01-01'"))
          (define (count-when predicate)
            (sum "case when "predicate" then 1 else 0 end"))
          (select cust".*")
          (select (count rentals)
                  " as TotalRentalCount")
          (select (count-when (is-movie rentals))
                  " as MovieRentalCount")
          (select (count-when (is-recent rentals))
                  " as RecentRentalCount")
          (select (count-when (bool (is-movie rentals)
                                    " and "
                                    (is-recent rentals)))
                  " as RecentMovieRentalCount")))
  (display (to-sql (my-query))))

@heading{Putting it All Together}
Let's imagine that we want a report of Frequently-Rented @(racket Item)s.
But we are going to want many variations on this report, such as

@itemlist[@item{Frequently rented at certain stores}
          @item{Frequently rented in a certain month}
          @item{Frequently rented action movies}
          @item{Frequently rented by customers over a certain age}]

We might use a table-valued function in standard SQL, but this will have at least
three problems. First, every variation we introduce might require additional parameters.
For example, to handle "customers older than X" we would need to add a "customerAgeMinimum"
parameter and then we would have to update all the call sites. Second, we might end up
paying a performance cost for joins that don't get used. For example, when we add that
"customerAgeMinimum" parameter, we are going to have to join the @(racket Customer) table
even if the caller indicates "no minimum" by passing null or zero. Third, passing a list
(such as a list of StoreIds) is painful at best.

In Plisqin, we can handle all these variations with only one parameter.

@(interaction
  #:eval my-eval
  (define (frequently-rented-items [rental-query Rental])
    (from i Item
          (join rentals rental-query
                (group-by (ItemId rentals))
                (join-on (ItemId rentals)" = "(ItemId i)))
          (select i".*")
          (select (count rentals)" as NumRentals")
          (order-by (count rentals)" desc"))))

The @(racket rental-query) parameter is optional, and it defaults to @(racket Rental)
which will just create a new, unfiltered query of the Rental table. But since queries
append by default, we can pass in an arbitrarily-filtered Rental query and our grouped
join will append to it. This will allow us to handle all the variations we listed.
First, let's do the "Frequently rented at certain stores" variation:

@(interaction
  #:eval my-eval
  (define (at-certain-stores)
    (from r Rental
          (where (StoreId r)" in (42, 43, 44)")))
  (display (to-sql (frequently-rented-items (at-certain-stores)))))

Next, frequently rented in a certain month:
@(interaction
  #:eval my-eval
  (define (this-month)
    (from r Rental
          (where (CheckoutTime r)" >= '2018-11-01'")
          (where (CheckoutTime r)" < '2018-12-01'")))
  (display (to-sql (frequently-rented-items (this-month)))))

OK, we see where this is headed. Let's just combine them all and declare victory:

@(interaction
  #:eval my-eval
  (define (combined-query)
    (frequently-rented-items
     (from r Rental
           (code:comment "at certain stores:")
           (where (StoreId r)" in (42, 43, 44)")
           (code:comment "in a certain month:")
           (where (CheckoutTime r)" >= '2018-11-01'")
           (where (CheckoutTime r)" < '2018-12-01'")
           (code:comment "having an 'Action' genre:")
           (join g (Genres-of r)
                 (join-on g".GenreName = 'Action'"))
           (code:comment "by customers over a certain age:")
           (where (CustomerBirthDate r)" < '1988-11-01'"))))
  (display (to-sql (combined-query))))

This concludes the sales pitch.
Hopefully you can see that Plisqin allows you to do things that are impossible in SQL.
Continue reading at @secref["getting-started"].