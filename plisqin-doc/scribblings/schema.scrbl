#lang scribble/manual
@(require (for-label plisqin-lib
                     "racket.rkt"))
@(require scribble/eval
          (prefix-in sc: scribble/core)
          (prefix-in sh: scribble/html-properties)
          plisqin-lib
          rackunit
          ; Use only-in to avoid accidentally using "my-eval" which contains
          ; the definitions that we are trying to re-implement:
          (only-in "helpers.rkt" show-sql check-sql check-sql2)
          "racket.rkt")

@(define (scribble-path filename)
   ; If we just run the "Scribble HTML" button in DrRacket, the current directory will be
   ; the same as this file. But if we are running from raco, it will be the project root.
   ; This is a workaround so that both work.
   (let* ([dir (current-directory)]
          [dir (path->string dir)]
          [local? (string-contains? dir "scribblings")])
     (if local?
         filename
         (string-append "scribblings/" filename))))

@(define labelled-code
   (sc:style "labelledCode"
             (list (sh:css-addition (scribble-path "plisqin-custom.css"))
                   (sh:js-addition (scribble-path "plisqin-custom.js")))))

@(define my-eval (make-base-eval))

@(define-syntax-rule (HIDDEN-CODE args ...)
   ; Runs the given code using "my-eval" and includes it into the "code' submodule
   (begin
     (interaction-eval #:eval my-eval
                       args ...)
     (module+ code
       args ...)))
@(define-syntax-rule (CODE args ...)
   ; Does everything HIDDEN-CODE does plus shows the code as a racketblock with
   ; a filename label on it.
   (begin
     (HIDDEN-CODE args ...)
     @elem[#:style labelled-code]{video-rental-schema.rkt}
     (racketblock args ...)))

@(module+ code
   (provide (all-defined-out)))

@(define-syntax-rule (INTERACT args ...)
   (interaction
    #:eval my-eval
    args ...))

@title[#:tag "schema-as-procs"]{Layer 1 - Schema as Procedures}

We've already seen two ways to deal with our DB schema.
One is to use strings, like this:

@(racketblock
  (RS from r "Rental"
      (where r".RentalId > 100")))

The other is to use procedures, like this:

@(racketblock
  (RS from r Rental
      (where (RentalId r)" > 100")))

But we haven't really looked at how these procedures are defined.
This section will walk you through how to recreate the definitions of the @secref{Video_Rental_Example_Schema}.
(Procedures like @(racket RentalId) are not hyperlinked in this section because we are
not using those definitions; we are recreating them.)
The code we write will be heavily influenced by my personal convention for
naming and organizing the procedures that represent the database schema.

We will be constructing a file called "video-rental-schema.rkt".
If you are following along, start a new file now.
The first line of this file should be @(tt "#lang racket").
Next, add this line to your file:

@(CODE
  (require plisqin))

Whenever you see a code block formatted like the above, you should add that code to your file.

This format, on the other hand, shows a REPL interaction:

@(INTERACT
  (to-sql (RS scalar "hello world")))

You can try that code in your REPL and verify that the result matches this guide.
If you get errors, revisit @(secref "environment-setup").

@section{Tables}

First we use @(racket table) to define our tables. We can use the information
schema to generate this code. For example, running this query in SQL Server:

@(show-sql #<<HEREDOC
select '(table '+TABLE_NAME+')'
from INFORMATION_SCHEMA.TABLES
where TABLE_TYPE='BASE TABLE'
order by TABLE_NAME
HEREDOC
           )

gives us the following Racket code:

@margin-note{
 I like to use CamelCase for tables and fields.
 This helps avoid name clashes and enhances readability for me.
}
@(CODE
  (table Account)
  (table Checkout)
  (table Copy)
  (table Customer)
  (table District)
  (table Employee)
  (table Genre)
  (table Item)
  (table ItemGenre)
  (table ItemType)
  (table Rental)
  (table Store))

These table definitions can be used with @(racket from) and @(racket join) like this:
@(INTERACT
  (displayln
   (to-sql
    (RS from r (Rental)
        (join c (Copy)
              (join-on c".CopyId = "r".RentalId"))))))

For every table procedure (like @(racket Rental)) there is also a tester procedure (like @(racket Rental?)).
The tester procedure tests whether it single argument is a query or join of the given table:
@(INTERACT
  (and (Rental? (Rental))
       (Rental? (from r (Rental)))
       (Rental? (join r (Rental))))
  (Rental? (Copy)))

@section{Columns}
To define the columns, we can use the @(racket def-fields-of) macro.
And the information schema can help again.
This query works for SQL Server:

@(show-sql #<<HEREDOC
select '(def-fields-of '+TABLE_NAME+' '+COLUMN_NAME+')'
from INFORMATION_SCHEMA.COLUMNS
order by TABLE_NAME, COLUMN_NAME
HEREDOC
           )

And produces the following code:

@(CODE
  (def-fields-of Account AccountId)
  (def-fields-of Account Address)
  (def-fields-of Account PhoneNumber)
  (def-fields-of Checkout CheckoutId)
  (def-fields-of Checkout CheckoutTime)
  (def-fields-of Checkout CustomerId)
  (def-fields-of Checkout EmployeeId)
  (def-fields-of Checkout StoreId)
  (def-fields-of Copy Barcode)
  (def-fields-of Copy CopyId)
  (def-fields-of Copy ItemId)
  (def-fields-of Customer AccountId)
  (def-fields-of Customer CustomerBirthDate)
  (def-fields-of Customer CustomerId)
  (def-fields-of Customer CustomerName)
  (def-fields-of District DistrictId)
  (def-fields-of District DistrictName)
  (def-fields-of Employee EmployeeId)
  (def-fields-of Employee EmployeeName)
  (def-fields-of Employee PrimaryStoreId)
  (def-fields-of Genre GenreId)
  (def-fields-of Genre GenreName)
  (def-fields-of Item ItemId)
  (def-fields-of Item ItemName)
  (def-fields-of Item ItemTypeId)
  (def-fields-of Item ReleaseDate)
  (def-fields-of ItemGenre GenreId)
  (def-fields-of ItemGenre ItemId)
  (def-fields-of ItemType ItemTypeId)
  (def-fields-of ItemType ItemTypeName)
  (def-fields-of Rental CheckoutId)
  (def-fields-of Rental CopyId)
  (def-fields-of Rental PricePaid)
  (def-fields-of Rental RentalId)
  (def-fields-of Store Address)
  (def-fields-of Store DistrictId)
  (def-fields-of Store StoreId))

@section{Joins}
I classify joins into 3 different types.
@itemlist[
 @item{A @bold{singular join} is a join that will not increase the number of rows
  in the result set. It gets a name like @(racket SingularNoun-of/s).
  This means that @(racket (SingularNoun-of/s x)) should join at most 1
  matching SingularNoun for each x.}
 @item{A @bold{plural join} is a join that is allowed to increase the number of rows
  in the result set. It gets a name like @(racket PluralNouns-of/p).
  This means that @(racket (PluralNouns-of/p x)) can join any number of
  matching PluralNouns for each x.}
 @item{A @bold{grouped join} is a join that contains at least one @(racket group-by) clause.
  It gets a name like @(racket PluralNouns-of/g) but it is actually a special case of singular join.
  For example, @(racket (Rentals-of/g item)) means "group Rentals by Item and join those groups to Item".}]

Note that this is just my personal convention.
(See also: @tech{of/s}, @tech{of/p}, and @tech{of/g}.)
Plisqin does not know or care whether a join is singular or plural.

It might be possible have the information schema help us here, but I just write joins by hand.

@subsection{Singular Join Examples}
Let's define @(racket Copy-of/s) which should return a valid value for everything that has at
most 1 Copy:

@(CODE
  (field-cases (Copy-of/s x)
               [(Rental? x)
                (RS join c (Copy)
                    (join-on (CopyId c)" = "(CopyId x)))]
               [(Copy? x) x]))

Using @(racket field-cases) is kind of like Racket's built-in @(racket cond), but one difference
is that the conditions are checked in reverse order (bottom to top). So our definition of
@(racket (Copy-of/s x)) means "If x is already a Copy, just return it. If x is a Rental return
a join using CopyId. Otherwise, none of the conditions match and it is an error."

My convention says that singular joins are allowed to contain other singular joins, but not plural joins.
To demonstrate, let's do one more singular join:

@(CODE
  (field-cases (Item-of/s x)
               [(Copy? x)
                (RS join i (Item)
                    (join-on (ItemId i)" = "(ItemId x)))]
               [(Rental? x)
                (Item-of/s (Copy-of/s x))]
               [(Item? x) x]))

The use of recursion in the @(racket Rental?) condition is a common pattern.
We have @(racket (Item-of/s copy)) working already.
We have previously defined @(racket (Copy-of/s rental)).
So we can use these two definitions to define @(racket (Item-of/s rental)) as
@(racket (Item-of/s (Copy-of/s rental))).

@subsection{Plural Join Examples}
Plural joins work pretty much the same way as singular joins.
Let's look at Genres. The Item:Genre relationship is Many:Many so we
have the ItemGenre mapping table between them. That means Genre:ItemGenre is One:Many,
and Item:ItemGenre is also One:Many.
First we define @(racket ItemGenres-of/p)

@(CODE
  (field-cases (ItemGenres-of/p x)
               [(Item? x)
                (RS join ig (ItemGenre)
                    (join-on (ItemId ig)" = "(ItemId x)))]
               [(Genre? x)
                (RS join ig (ItemGenre)
                    (join-on (GenreId ig)" = "(GenreId x)))]
               [(or (Copy? x)
                    (Rental? x))
                (ItemGenres-of/p (Item-of/s x))]))
If the argument is an Item, we join its ItemGenres on ItemId.
If the argument is a Genre, we join its ItemGenres on GenreId.
If the argument is a Copy or a Rental, we recurse using its Item.

But in most of our queries, we won't care about the ItemGenre table.
We will only be thinking about Items and Genres. For example, join all the
Genres for a given Item/Copy/Rental. We can define @(racket Genres-of/p) like this:

@(CODE
  (field-cases (Genres-of/p x)
               [(Item? x)
                (RS join g (Genre)
                    (join-on (GenreId g)" = "(GenreId (ItemGenres-of/p x))))]
               [(or (Copy? x)
                    (Rental? x))
                (Genres-of/p (Item-of/s x))]))
If the argument is an Item, we use its ItemGenres to get its Genres.
If the argument is a Copy or a Rental, we recurse usings its Item.

Let's also define @(racket Copies-of/p). There isn't really much to explain here,
but we will need these definitions later.

@(CODE
  (field-cases (Copies-of/p x)
               [(Item? x)
                (RS join c (Copy)
                    (join-on (ItemId c)" = "(ItemId x)))]))

@subsection{Grouped Join Examples}
A grouped join is a join that contains a group-by clause.
The definition of @(racket (Rentals-of/g x)) should return the correct grouped
join for any @(racket x) that has a group of @(racket Rental)s:

@(CODE
  (field-cases (Rentals-of/g x)
               [(Copy? x)
                (RS join r (Rental)
                    (join-on (CopyId r)" = "(CopyId x))
                    (group-by (CopyId r)))]
               [(Item? x)
                (RS join r (Rental)
                    (join c (Copy-of/s r))
                    (join-on (ItemId c)" = "(ItemId x))
                    (group-by (ItemId c)))]))

Having just defined a polymorphic @(racket Rentals-of/g), let's take a quick detour
to appreciate its power. The following @(racket rental-summary) defines a query
that can show you the most-rented @(racket Item)s or @(racket Copy)s.
This is pretty much impossible in SQL:
@(INTERACT
  (define (rental-summary item-or-copy)
    (RS from x item-or-copy
        (join r (Rentals-of/g x))
        (select (count r)" as NumRentals")
        (select x".*")
        (order-by (count r)" desc")))
  (define copy-sql (to-sql (rental-summary (Copy))))
  (define item-sql (to-sql (rental-summary (Item))))
  (displayln copy-sql)
  (displayln item-sql))
@(check-sql2
  (my-eval 'copy-sql)
  #<<HEREDOC
select
  _rental.__INJECT2 as NumRentals
  , _copy.*
from Copy _copy
inner join (
    select
      _rental.CopyId as __INJECT1
      , count(*) as __INJECT2
    from Rental _rental
    group by _rental.CopyId) _rental
on _rental.__INJECT1 = _copy.CopyId
order by _rental.__INJECT2 desc
HEREDOC
  )
@(check-sql2
  (my-eval 'item-sql)
  #<<HEREDOC
select
  _rental.__INJECT2 as NumRentals
  , _item.*
from Item _item
inner join (
    select
      _copy.ItemId as __INJECT1
      , count(*) as __INJECT2
    from Rental _rental
    inner join Copy _copy
    on _copy.CopyId = _rental.CopyId
    group by _copy.ItemId) _rental
on _rental.__INJECT1 = _item.ItemId
order by _rental.__INJECT2 desc
HEREDOC
  )

If you don't understand the occurrences of __INJECT, revisit the @(secref "injections") section.

@section{Derived Scalars}
By "derived scalar" I mean any scalar expression that is not a direct access of some database column.
For example, an SQL expression like @(racket (lineItem.UnitPrice * lineItem.Quantity)) would be a derived scalar.
My convention dictates that scalars must be singular, meaning that they do not contain plural joins.
It would be very confusing if accessing a scalar could increase the number of rows in the result set.

This is a very common type of derived scalar.
It says that the ItemName of a Copy or Rental is the ItemName of its (singular) Item:

@(CODE
  (field-cases (ItemName x)
               [(or (Copy? x)
                    (Rental? x))
                (ItemName (Item-of/s x))]))

But didn't we already define @(racket ItemName) earlier, in the @secref{Columns} section?
Yes, but @(racket field-cases) allows you to append to previously existing definitions, even across files:

@(INTERACT
  (field-cases (twice x)
               [(integer? x) (* x 2)])
  (field-cases (twice x)
               [(string? x) (format "~a ~a" x x)])
  (twice 3)
  (twice "pizza"))

The conditions are evaluated in reverse order. (This allows you to save generated code
in one file and manual overrides in another file, which can be handy.)
To prove this, let's override the @(racket integer?) case
of @(racket twice), and notice that the @(racket string?) case still works the same:

@(INTERACT
  (field-cases (twice x)
               [(integer? x)
                (format "Overridden: ~a" (* x 2))])
  (twice 3)
  (twice "pizza"))

Now let's do one more derived scalar. This one will involved a grouped join.
Remember that grouped joins are singular, so they are allowed in scalars.

@(CODE
  (field-cases (NumRentals x)
               [(or (Item? x)
                    (Copy? x))
                (count (Rentals-of/g x))]))

Now we can get the number of Rentals for a given Item or Copy.
Let's test both @(racket NumRentals) and the extended definition of @(racket ItemName).

@(INTERACT
  (RS define (my-query)
      (from c (Copy)
            (select (NumRentals c))
            (select (ItemName c))
            (select c".*")))
  (displayln (to-sql (my-query))))

It works as expected.

Imagine that as our business grows, we notice that queries including @(racket (NumRentals item))
are performing poorly, even with proper indexing. So we decide to denormalize, and we add a "NumRentals"
column to the Item table, knowing that it might be a few hours out of date. We can update our definition
of @(racket NumRentals) to use this new column, and none of the calling code has to change.
