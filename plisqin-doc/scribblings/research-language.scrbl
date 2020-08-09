#lang scribble/manual

@(begin
   (require (for-label "standard-label.rkt"))
   (require scribble/example
            "helpers.rkt")
   )

@title[#:tag "research-language"]{Plisqin as a Research Language}

@section{My Ideal Query Language}
To me, the perfect query language for relational databases must satisfy
at least the following rules.

(There are other qualities that my perfect query language must have,
but I cannot distill them into simple rules.
The generalized @(racket sales-report) created in the
@(secref "using-define-schema") walkthrough is a good example.)

@subsection{Rule of Power}
@emph{If the database knows a fact about some table @(racket T), then it
 must be possible to encode that fact into a function that takes a single
 argument representing a row in the table @(racket T).}

For example, assume that "total sales since 2004" is a fact that our database
knows about the @(racket Product) table.
Plisqin allows you to encode that fact into a procedure as follows:
@(racketblock
  (define/contract (total-sales-since-2004 product)
    (-> (instanceof Product) Number?)
    (error "to be implemented")))

Note that the choice of implementation is not important here.
What is important is the contract (or "type signature", if you prefer).

Also, it would be silly not to make "2004" an argument in real code.
The "single argument" mandate is a litmus test for language designers, not a
recommendation for end users of the language.

@subsection{Rule of Uniformity}
@emph{There must be a uniform call-site syntax for every kind of fact,
 regardless of how that fact is implemented.}

This rule is intended to insulate calling code from changes in implementation.
For example, let's imagine that "total sales" is a fact about the Product table.
In Plisqin, the call-site syntax is always @(racket (total-sales product)).
The implementation might be any of the following
@(itemlist
  @item{A simple access of the Product.TotalSales column}
  @item{An access of the ProductStats.TotalSales column,
 assuming that Product:ProductStats is a 1:1 relationship}
  @item{A subquery of the SalesOrderDetail table,
 assuming that Product:SalesOrderDetail is a 1:many relationship}
  @item{A @tech{grouped join aggregation} of the SalesOrderDetail table,
 assuming that Product:SalesOrderDetail is a 1:many relationship}
  @item{A stored procedure call})

... or anything else. The implementation does not matter.
What matters is being able to change the implementation without breaking the
call sites.

This rule does not prohibit other non-uniform syntaxes from being supported.
For example, your uniform syntax might be @(racket product..total_sales)
but you could also support @(racket product.total_sales)
which would raise an error if @(racket total_sales) is not a direct column
access of the table identified by @(racket product).

@subsection{Rule of Polymorphism}
@emph{If the database knows the same fact about multiple tables
 @(racket T1 .. TN), then it must be possible to encode that fact into a
 function that accepts a single argument representing a row in any of the
 tables @(racket T1 .. TN).}

For example, assume that CategoryName is a fact about the Category table,
and CategoryName is also a fact about the Subcategory table.
This rule states that it must be possible to create a @(racket CategoryName)
function that will work whether it is passed a Category or a Subcategory.

This rule is the main motivation behind @(racket define-schema).
But users of Plisqin could also write polymorphic functions by hand, as follows:
@(racketblock
  (define/contract (CategoryName x)
    (-> (or/c (instanceof Category)
              (instanceof Subcategory))
        Scalar?)
    (cond
      [((instanceof Category) x)
       (%%scalar x".CategoryName")]
      [((instanceof Subcategory) x)
       (code:comment "this will recurse with an (instanceof Category)")
       (CategoryName (join cat Category #:to x
                           (join-on (.= (CategoryID cat)
                                        (CategoryID x)))))])))

@section{Plisqin Desugared}
The following example shows how the @(secref "Teaser") from the define-schema
walkthrough can be recreated using the smallest possible subset of Plisqin.
Here we will use the @tech{unsafe} variant because it will be cumbersome to
avoid typecheck and nullcheck errors without using @(racket define-schema).
@(examples
  #:eval (make-base-eval)
  #:label #f
  #:no-prompt
  (require (only-in plisqin
                    instanceof to-sql
                    from join limit
                    %%scalar %%aggregate
                    %%select %%join-on %%group-by %%order-by)
           (prefix-in aw: plisqin-examples/adventure-works))

  (define (undefined name arg)
    (error (format "~a is not defined for:"
                   name)
           arg))

  (define (ProductName x)
    (cond
      [((instanceof 'Product) x)
       (%%scalar x".Name")]
      [else
       (undefined 'ProductName x)]))

  (define (ProductID x)
    (cond
      [((instanceof 'Product) x)
       (%%scalar x".ProductID")]
      [((instanceof 'SalesOrderDetail) x)
       (%%scalar x".ProductID")]
      [else
       (undefined 'ProductID x)]))

  (define (CategoryName x)
    (cond
      [((instanceof 'ProductCategory) x)
       (%%scalar x".Name")]
      [else
       (CategoryName (ProductCategory x))]))

  (define (ProductCategory x)
    (cond
      [((instanceof 'ProductSubcategory) x)
       (join pc 'ProductCategory #:to x
             (%%join-on pc".ProductCategoryID = "x".ProductCategoryID"))]
      [else
       (ProductCategory (ProductSubcategory x))]))

  (define (ProductSubcategory x)
    (cond
      [((instanceof 'Product) x)
       (join subcat 'ProductSubcategory #:to x
             (%%join-on subcat".ProductSubcategoryID = "x".ProductSubcategoryID"))]
      [else
       (undefined 'ProductSubcategory x)]))

  (define (DetailsG x)
    (cond
      [((instanceof 'Product) x)
       (join dtls 'SalesOrderDetail #:to x
             (%%group-by (ProductID dtls))
             (%%join-on (ProductID dtls)" = "(ProductID x)))]
      [else
       (undefined 'DetailsG x)]))

  (define (TotalSales x)
    (cond
      [((instanceof 'Product) x)
       (let* ([dtls (DetailsG x)]
              [line-total (%%scalar dtls".LineTotal")])
         (%%aggregate "sum("line-total")"))]
      [else
       (undefined 'TotalSales x)]))

  (define the-query
    (from p 'Product
          (%%select (ProductName p)" as ProductName")
          (%%select (CategoryName p)" as CategoryName")
          (%%select (TotalSales p)" as TotalSales")
          (%%order-by 'desc (TotalSales p))))

  (aw:show-table
   (from x the-query
         (limit 3))))

@section{Open Questions and Shortcomings}
Plisqin is closer to my ideal query language than anything else I've tried.
But there are still some rough edges that could be improved.

@subsection{The Limit/Offset Problem}
See @hyperlink["https://github.com/default-kramer/plisqin/issues/12"]{this issue}.

This problem highlights the fact that a scalar is more than just a data type
(such as @(racket Number?)) and a nullability (such as @(racket no)).
There is at least one more aspect, which I called "static" in the Github issue,
which is relevant here.
But can we always know whether an expression is "static", or does it depend
on where it is being used...?

An easy way around this is to add @tech{unsafe} versions of @(racket limit)
and @(racket offset), but that is not interesting from a research perspective.

@subsection{Nominal vs Structural Typing}
Plisqin currently satisfies the Rule of Polymorphism using nominal typing.
Specifically, it uses @(racket instanceof) to dispatch:
@(racketblock
  (define (CategoryName x)
    (cond
      [((instanceof ProductCategory) x)
       (%%scalar x".Name")]
      [((instanceof ProductSubcategory) x)
       (code:comment "presumably (ProductCategory x) is returning a join here")
       (CategoryName (ProductCategory x))])))

As long as you have an instance of @(racket ProductCategory) or
@(racket ProductSubcategory), the previous function works fine.
But the following example will not work:
@(racketblock
  (from x (subquery (from pc ProductCategory
                          (%%select "42 as Blah")))
        (select (CategoryName x))))

Should @(racket x) be considered an @(racket (instanceof ProductCategory))?
Absolutely not!
It has none of the columns that we expect an instance of ProductCategory to have.
That was a silly example, so consider another one:
@(racketblock
  (from x (subquery (from pc ProductCategory
                          (select (Name pc))))
        (select (CategoryName x))))

This time, the previous example would actually work if we consider @(racket x)
to be an @(racket (instanceof ProductCategory)).
This highlights the problem.
A subquery might have some but not all of the columns that a normal instance
of the same table would have.

This turns out not to be a big problem because the query composition techniques
that Plisqin encourages make subqueries less common.
Still, my ideal query language probably needs an answer to this problem.

@subsubsection{Basic Structural Typing is not Good Enough}
In theory, a structural type system could notice that the subquery and the
ProductCategory table are structurally identical.
That is, they have the same column names and types.
But a purely structural approach makes dispatch awkward.
How would we redefine @(racket CategoryName) using structural typing?
@(racketblock
  (define (CategoryName x)
    (cond
      [(looks-like-a-category? x)
       (%%scalar x".Name")]
      [else
       (CategoryName (ProductCategory x))])))

I omitted the definition of @(racket looks-like-a-category?) because I don't
think there is a good answer.
We could check that it has a numeric ProductCategoryID column and a string
Name column, but that is probably not accurate.
After all, the ProductSubcategory table might also have those two columns, but
its Name is not the same as a ProductCategory's Name.
(This problem could be avoided by renaming the columns to CategoryName and
SubcategoryName, but this places constraints on the design of the database
schema, which Plisqin does not want to do.)

@subsubsection{A Hybrid Approach?}
One possibility is to allow something like the following
@(racketblock
  (%%subquery #:instanceof ProductCategory
              "select *, 'hello world' as Greeting from ProductCategory"))

This would mean that when that subquery is used as a queryable, the instance
would be an @(racket (instanceof ProductCategory)).
And based on what we know about SQL, this would work just fine (assuming that
ProductCategory table does not already have a Greeting column).

Now consider the following:
@(racketblock
  (subquery #:instanceof ProductCategory
            (from pc ProductCategory
                  (select (ProductCategoryID pc))
                  (select (Name pc)))))

The previous subquery can pass for an @(racket (instanceof ProductCategory)) as
long as we only ever attempt to access its ProductCategoryID and Name columns.
If we attempt to access any other column, it will be an error.
This might be reasonable behavior and possible to make well-defined.

Basically, we use @(racket #:instanceof) to guide the dispatcher, but we
accept that we might still encounter a structural type error later if we
dispatch into some code that needs a column that is missing from the instance.

@subsection{Lateral Joins}
Currently Plisqin has no support for lateral joins
(aka "cross apply" / "outer apply" in SQL Server).
Adding support won't be hard, but there is a decision that needs to be made.

The following join should be a lateral join.
This is because the @(racket %%where) clause contains a "foreign instance"
(namely @(racket x)) that comes from an outer scope.
@margin-note{
 There is nothing special about @(racket %%where) here.
 It is more accurate to say that @(racket join-on) is special in that it
 does not exhibit this behavior.
}
@(repl
  (define lateral-example
    (from x 'X
          (join y 'Y
                (join-type 'left)
                (%%where y".foo = "x".foo"))))
  (code:comment "This SQL is invalid because Plisqin has no support for lateral joins:")
  (displayln (to-sql lateral-example)))

Plisqin could easily detect the "foreign instance" in the previous example,
and automatically generate a lateral join.
Should it?
I think...
@(itemlist
  @item{If the user explicitly says "lateral join", then generate a lateral join.}
  @item{If the user explicitly says "non-lateral join", then generate a normal
 join. Raise an error if a foreign instance is detected.}
  @item{If the user does not say anything, generate a lateral join when
 necessary and a normal join otherwise.})

A user who doesn't like that decision should be able to easily redefine
@(racket join) to be non-lateral by default.

@subsection{Nullability and @(racket exists)}
From this section: @secref["Task_3__Products_with_Non-Zero_Sales"]

We define the equivalent of this procedure
@(racketblock
  (define/contract (HasSales? prd)
    (-> (instanceof Product) Boolish?)
    (exists (from dtl SalesOrderDetail
                  (where (.= (ProductID dtl)
                             (ProductID prd)))))))

But the problem is, what if the given @(racket prd) is a left join?
We will fail with a nullability error.
So we could add a fallback, but even that doesn't feel right.
This feels like one of the rare times when we want to preserve the unknown
boolean value.

I think we want the call-site pattern to be as follows:
@(racketblock
  (where (coalesce (HasSales? product)
                   (val #f))))

The caller could also choose not to coalesce at all if it knows that that
the instance it passes in is not a left join.

But how to implement this?
There are a couple of problems.
First, if we switch to @(racket %%=) the nullability of the where clause will be
@(racket yes), but the queries do not have a notion of nullability.
If they did, what does it mean for a query to be nullable?
Does it only affect @(racket exists)?
Is a query nullable when any of its clauses are nullable?

Second, even if we have a well-defined way to make the @(racket (exists ....))
token nullable, that doesn't seem to help because in SQL "exists" never returns
the unknown boolean value.
This would make SQL generation very difficult or maybe impossible.
Reconsider the following call site:
@(racketblock
  (where (coalesce (HasSales? product)
                   (val #f))))

What SQL should be generated?
Do you have an answer that generalizes?
If so, please let me know, because I'm stumped.

@section{Other Random Thoughts}
@subsection{Cardinality}
For example, I would like to be able to assert "this query should return at
most one row per ProductCategory" and have Plisqin error if it can prove that I
wrong or warn if it cannot prove that I am correct.

A very unsophisticated solution works similar to nullability, where we need the
user to give us hints like @(racket #:has-one) so we know that a join is singular.
I wonder if such an unsophisticated soluation would be useful at all, or if it
just means that the user makes the same mistake in a different place.

A sophisticated solution requires deep knowledge of the database schema.
Which columns are unique keys of each table?
Which comparison expressions are guaranteed to match exactly one row?
This gets super tricky when you think about collation.

@subsection{Separating Specification from Implementation}
Barring tremendous leaps in query optimizers (which are already amazing IMO),
I think many projects would benefit from having two database languages.
A "specification language", something like Plisqin, would be used by application
developers to define what result set they need from the DB.
The "implementation language" would be used by DBAs to tell the database how a
certain query from the specification language should be implemented.
Things like query hints obviously belong in the implementation language,
but so do decisions like choosing between a negative join or "not exists".

SQL (and therefore Plisqin) combine specification and implementation.
In other words, from a certain level SQL is not actually declarative.
I don't think this is a huge flaw of SQL.
Forcing users to write a specification and an implementation for every
single query would not be a good language design.
A good language would be able to support both workflows.

@link["http://cosette.cs.washington.edu/"]{Cosette} looks promising.
For example, an application developer uses Plisqin as the specification language.
Plisqin generates SQL which might be very inefficient.
A DBA hand-crafts SQL that is acceptably efficient.
Cosette ensures that the hand-written SQL is equivalent to Plisqin's SQL.
