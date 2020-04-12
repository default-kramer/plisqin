#lang scribble/manual
@(require scribble/example
          "helpers.rkt"
          plisqin-examples/adventure-works
          racket/exn
          racket/string
          (prefix-in db: db)
          (for-syntax racket/base)
          (for-label plisqin
                     "racket.rkt"))

@(define-syntax-rule (bossquote stuff ...)
   (tabular #:row-properties '(left right)
            (list (list (italic stuff ...))
                  (list @nested{-- your boss}))))


@title{Using define-schema}
@section{Motivation}
@(load-checkpoint! "final.rkt")
The benefits of @(racket define-schema) are most evident when you are working with
a mature schema. This section demonstrates the power of a mature schema.
Later sections will walk through how to build this schema from the ground up.
For now, just imagine this scenario...

I have been working with the AdventureWorks database for some time and
building up my schema definition along the way.
My boss just asked to see our top 3 best-selling Products of all-time,
including the Product Name, Category Name, and Total Sales.
So I need to write a query.
Thanks to my schema definition, I nail it on the first try:
@(repl-query
  (show-table
   (from p Product
         (limit 3)
         (select (ProductName p))
         (select (CategoryName p))
         (select (TotalSales p))
         (order-by 'desc (TotalSales p)))))

@margin-note{Seeing the generated SQL might make you worry about performance.
 I found that in a real project, Plisqin generated queries that were equivalent
 to my hand-written queries most of the time.}
Click on "Show SQL" and you might be surprised!
The generated SQL is much larger than the Plisqin query.
This demonstrates the reason to use @(racket define-schema) -- it acts as a repository of
reusable properties. I have previously defined @(racket (ProductName Product)),
@(racket (CategoryName Product)), and @(racket (TotalSales Product)).
Now I can reuse them whenever I am working with the @(racket Product) table.
Furthermore, these property definitions are:
@(itemlist
  @item{@bold{Powerful} - If the database contains a fact about some table,
 then it is possible to encode that fact as a property of that table.
 A property may contain anything, including joins, aggregations and subqueries.}
  @item{@bold{Uniform} - All properties have the same call-site syntax.
 No matter how simple or complex, the caller always writes @(racket (TheProperty x)).
 This allows the property's implementation to change without breaking any call sites.}
  @item{@bold{Reusable} - If a property is defined for some table, it can be used
 anywhere we have an instance of that table.
 
 TODO I like that phrase "an instance of that table" - perhaps @(racket (instanceof Foo))
 means @(racket (or/c (tupleof Foo) (joinof Foo)))...}
  @item{@bold{Polymorphic} - The definition of @(racket (CategoryName x)) can mean something
 different depending on what @(racket x) is. For example, if @(racket x) is a Category
 then it probably just accesses a column of the Category table, but if @(racket x) is a
 Product then it probably includes a join from Product to Category.})

@subsection{My Personal Motivation}
During my software development career, many of my tasks could be described
as "fetch data from an SQL database and display that data."
Some tasks have very little display logic, such as a report that simply
shows an SQL result set verbatim.
Other tasks have more complicated display logic, such as a web page that shows
a musical artist, their albums, and the track listings for each album.
But regardless of how complex the display logic might be, I found that my favorite
strategy for these tasks is usually
@(itemlist
  #:style 'ordered
  @item{Imagine the ideal result set(s) for the task.}
  @item{Use SQL to produce those result sets.}
  @item{Use something else to display the results.})

It is step 2 that Plisqin aims to improve.
SQL and all the SQL alternatives I have tried are lacking in some way.
I always end up duplicating fragments of queries and joins all over the place.
Using Plisqin allows me to reduce duplication to what I suspect is the theoretical minimum.

@section{Building a Schema}
Now I will show how we can grow our schema from the ground up.
In this scenario, we are working with the AdventureWorks example database.
TODO show how to connect and execute raw queries.

TODO definitely need to make sure people understand that `from` and `join`
introduce identifiers and what they mean.
Do I want to say that @(racket (from p Product ....)) binds @(racket p)
as "an instance of @(racket Product)"?

TODO show how to generate the starting file.
Save it as "aw-schema.rkt" or something like that.
Whenever I ask you to @deftech{perform} a refactoring recipe, you will have to update
this file.

TODO is this the place to mention the @(racket (.= a b)) operator naming convention?

TODO explain how each task is going to work.
1) Prerequisites
2) Create the query
3) Refactor to make stuff reusable

@section{The Tasks}
@(define task subsection)

@(load-checkpoint! "1.rkt")
@task{Task 1: Subcategories & Categories}
@bossquote{I want to see a list of Subcategories with the Category that they belong to.}

We need to write a query.
The first task, as always, is to determine which table we need to query.
(TODO link to a recipe here?)
The @(racket ProductSubcategory) table looks promising.
Let's see what it contains:
@(repl-query
  (show-table
   (from subcat ProductSubcategory
         (limit 5))))

This is a good start.
Our boss didn't say exactly what columns he wants to see, but he did say
to include "the Category". It looks like ProductCategoryID is a foreign key.
Based on the name, we conclude that it points to the @(racket ProductCategory) table.
It makes sense that a Subcategory would belong to a single Category.
We need to add a join to our query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (join cat ProductCategory
              (join-on (.= (ProductCategoryID cat)
                           (ProductCategoryID subcat))))))

You can run that query, but it will produce the same result set.
Even though we have added a join, this query does not contain any @(racket select)
clauses, so it just shows the primary table by default.
(The primary table is @(racket ProductSubcategory) here.)
Let's add some @(racket select) clauses to control which columns get displayed:
@(repl-query
  (show-table
   (from subcat ProductSubcategory
         (limit 5)
         (join cat ProductCategory
               (join-on (.= (ProductCategoryID cat)
                            (ProductCategoryID subcat))))
         (select (Name subcat))
         (select (Name cat)))))

That's better, but we can see that @(racket Name) is not a very good name.
@tech{Perform} refactoring recipe TODO so that the following query works:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (join cat ProductCategory
              (join-on (.= (ProductCategoryID cat)
                           (ProductCategoryID subcat))))
        (select (SubcategoryName subcat))
        (select (CategoryName cat))))

That looks good. But we are not done refactoring.
@tech{Perform} refactoring recipe TODO so that the following query works:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (join cat (ProductCategory subcat))
        (select (SubcategoryName subcat))
        (select (CategoryName cat))))

But we are still not done refactoring.
Use refactoring recipe to produce this equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (select (SubcategoryName subcat))
        (select (CategoryName (ProductCategory subcat)))))

But we are still not done refactoring.
@tech{Perform} refactoring recipe TODO so that the following query works:
@(load-checkpoint! "2.rkt")
@(repl-query
  (show-table
   (from subcat ProductSubcategory
         (limit 5)
         (select (SubcategoryName subcat))
         (select (CategoryName subcat)))))

And now we are done!

@subsubsub*section{Refactoring Recap}
While refactoring our query, we made the following enhancements to our schema.
@(racketblock
  (SubcategoryName ProductSubcategory)
  (CategoryName ProductCategory)
  (ProductCategory ProductSubcategory)
  (CategoryName ProductSubcategory))

@task{Task 2: Products & Subcategories & Categories}
@bossquote{Show me a list of Products with Subcategory and Category names.}

The first task, as always, is to determine which table we need to query.
The @(racket Product) table looks promising.
Let's see what it contains:
@(repl-query
  (show-table
   (from prd Product
         (limit 5))))

OK, it looks like there is a join from Product to ProductSubcategory using the
ProductSubcategoryID column. But at least some of the records have a null value.
So this should be a left join to avoid eliminating Products from the result set:
@(racketblock
  (from prd Product
        (limit 5)
        (join subcat ProductSubcategory
              (join-type 'left)
              (join-on (.= (ProductSubcategoryID subcat)
                           (ProductSubcategoryID prd))))))

And now let's add some select clauses:
@margin-note{TODO this is the first introduction to nullchecking and fallbacks, right?}
@(repl-query
  (show-table
   (from prd Product
         (limit 5)
         (join subcat ProductSubcategory
               (join-type 'left)
               (join-on (.= (ProductSubcategoryID subcat)
                            (?? (ProductSubcategoryID prd) /void))))
         (select (Name prd))
         (select (ProductNumber prd))
         (select (SubcategoryName subcat)))))

It looks like we are on the right track.
The top 5 rows have a null SubcategoryName, but this seems to be correct.
If you remove the @(racket limit) clause you will see some rows with non-null Subcategories.
Now we just need to include the name of the Category.
Hang on, we've worked with @(racket CategoryName) in the past, haven't we?
Let's see what it is defined for:
@(repl (adventure-works-schema '(CategoryName _)))

Sweet!
We can see that @(racket CategoryName) is defined for @(racket ProductSubcategory).
We did this as part of the previous task's refactoring.
That investment pays off now, because our current query has an instance of
@(racket ProductSubcategory), so we can pass it into @(racket CategoryName):
@margin-note{The careful reader will notice that both joins in the generated SQL
 are left joins. TODO write up how @(racket (join-type 'infer)) works and link to it?
 Or is that just a distraction at this point?}
@(repl-query
  (show-table
   (from prd Product
         (limit 5)
         (join subcat ProductSubcategory
               (join-type 'left)
               (join-on (.= (ProductSubcategoryID subcat)
                            (?? (ProductSubcategoryID prd) /void))))
         (select (Name prd))
         (select (ProductNumber prd))
         (select (SubcategoryName subcat))
         (select (CategoryName subcat)))))

This query looks good!
That means it's refactoring time.
@tech{Perform} refactoring recipe TODO to make the join reusable,
so that the following query works:
@(racketblock
  (from prd Product
        (limit 5)
        (join subcat (ProductSubcategory prd))
        (select (Name prd))
        (select (ProductNumber prd))
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))))

@tech{Perform} refactoring recipe TODO to generate this equivalent query:
@(racketblock
  (from prd Product
        (limit 5)
        (select (Name prd))
        (select (ProductNumber prd))
        (select (SubcategoryName prd))
        (select (CategoryName prd))))

@tech{Perform} refactoring recipe TODO to rename @(racket (Name prd)):
@(load-checkpoint! "3.rkt")
@(repl-query
  (show-table
   (from prd Product
         (limit 5)
         (select (ProductName prd))
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (select (CategoryName prd)))))

And now we are done!

@subsubsub*section{Refactoring Recap}
While refactoring our query, we made the following enhancements to our schema.
@(racketblock
  (ProductSubcategory Product)
  (SubcategoryName Product)
  (CategoryName Product)
  (ProductName Product))

@task{Task 3: Products with Non-Zero Sales}
@bossquote{Show me a list of Products that have non-zero sales,
 with Subcategory and Category names.}
This just adds the "non-zero sales" criteria to the previous task.
Our boss explains that our Product catalog could use some culling, but for now
"has sales?" is an acceptable way to filter out obsolete Products.

TODO we need to determine how to know whether a Product has been sold.

Anyway, we eventually land on this:
@(repl-query
  (show-table
   (from prd Product
         (limit 5)
         (select (ProductName prd))
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (select (CategoryName prd))
         (where (exists (from dtl SalesOrderDetail
                              (where (.= (ProductID dtl)
                                         (ProductID prd)))))))))

This query looks good!
It's time to refactor.
@tech{Perform} refactoring recipe TODO to make @(racket (HasSales? Product))
return the @(racket (exists ....)) expression.
The following query should now work:
@(load-checkpoint! "4.rkt")
@(repl-query
  (show-table
   (from prd Product
         (limit 5)
         (select (ProductName prd))
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (select (CategoryName prd))
         (where (HasSales? prd)))))

And now we are done!

@subsubsub*section{Refactoring Recap}
While refactoring our query, we made the following enhancements to our schema.
@(racketblock
  (HasSales? Product))

Notice the encapsulation that @(racket (HasSales? Product)) provides.
Today it is implemented using @(racket exists), but in the future we might
decide to denormalize and add a HasSales column to the Product table.
The important point is that we can choose a different implementation
without breaking any calling code - the calling code will always remain
@(racket (HasSales? product)).
This is not true in SQL - switching from an "exists" implementation to a
simple column access would require updating all the call sites.

@task{Task 4: Sales by Product}
@(define task4-quote
   @bossquote{Show me a list of the best-selling Products of all time.
 Sort by total revenue. Include total quantity sold and subcategory.})
@task4-quote

TODO write prose from here to the end.
Initial query:
@(repl-query
  (show-table
   (from prd Product
         (limit 5)
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (join detailsG SalesOrderDetail
               (group-by (ProductID detailsG))
               (join-on (.= (ProductID detailsG)
                            (ProductID prd))))
         (select (round (sum (LineTotal detailsG)) 2))
         (select (sum (OrderQty detailsG)))
         (order-by 'desc (sum (LineTotal detailsG))))))

@tech{Perform} refactoring recipe TODO such that @(racket (DetailsG Product))
returns our @(racket (join detailsG ....)) expression.
The following query should now work:
@(load-checkpoint! "5.rkt")
@(repl-query
  (show-table
   (from prd Product
         (limit 5)
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (join detailsG (DetailsG prd))
         (select (>> (round (sum (LineTotal detailsG)) 2)
                     #:as 'TotalSales))
         (select (>> (sum (OrderQty detailsG))
                     #:as 'TotalQty))
         (order-by 'desc (sum (LineTotal detailsG))))))

You might want to go further with the refactoring and define properties like
@(racket (TotalSales Product)) and @(racket (TotalQty Product)).
This might be a good idea, and you are welcome to do so.
But I am going to stop here for now.

@subsubsub*section{Refactoring Recap}
While refactoring our query, we made the following enhancements to our schema.
@(racketblock
  (DetailsG Product))

@task{Task 5: Sales by Subcategory}
@(define task5-quote
   @bossquote{Show me a list of the best-selling Subcategories of all time.
 Sort by total revenue. Include total quantity sold and category name.})
@task5-quote

Initial revision
@(repl-query
  (show-table
   (from subcat ProductSubcategory
         (limit 5)
         (select (SubcategoryName subcat))
         (select (CategoryName subcat))
         (join detailsG SalesOrderDetail
               (join prd Product
                     (join-on (.= (ProductID prd)
                                  (ProductID detailsG))))
               (group-by (ProductSubcategoryID prd))
               ; TODO all grouped joins should be left joins.
               (join-on (.= (?? (ProductSubcategoryID prd) /void)
                            (ProductSubcategoryID subcat))))
         (select (round (sum (LineTotal detailsG)) 2))
         (select (sum (OrderQty detailsG)))
         (order-by 'desc (sum (LineTotal detailsG))))))

@tech{Perform} refactoring recipe TODO so that this query works:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))
        (join detailsG SalesOrderDetail
              (code:hilite (join prd (Product detailsG)))
              (group-by (ProductSubcategoryID prd))
              (join-on (.= (ProductSubcategoryID prd)
                           (ProductSubcategoryID subcat))))
        (select (round (sum (LineTotal detailsG)) 2))
        (select (sum (OrderQty detailsG)))
        (order-by 'desc (sum (LineTotal detailsG)))))

@tech{Perform} refactoring recipe TODO so that this query works:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))
        (join detailsG SalesOrderDetail
              (group-by (code:hilite (ProductSubcategoryID detailsG)))
              (join-on (.= (code:hilite (ProductSubcategoryID detailsG))
                           (ProductSubcategoryID subcat))))
        (select (round (sum (LineTotal detailsG)) 2))
        (select (sum (OrderQty detailsG)))
        (order-by 'desc (sum (LineTotal detailsG)))))

@tech{Perform} refactoring recipe TODO so that this query works:
@(load-checkpoint! "6.rkt")
@(repl-query
  (show-table
   (from subcat ProductSubcategory
         (limit 5)
         (select (SubcategoryName subcat))
         (select (CategoryName subcat))
         (join detailsG (DetailsG subcat))
         (select (>> (round (sum (LineTotal detailsG)) 2)
                     #:as 'TotalSales))
         (select (>> (sum (OrderQty detailsG))
                     #:as 'TotalQty))
         (order-by 'desc (sum (LineTotal detailsG))))))

Again, You might want to go further with the refactoring and define properties like
@(racket (TotalSales Product)) and @(racket (TotalQty Product)).
This might be a good idea, and you are welcome to do so. But I am going to stop here for now.

@subsubsub*section{Refactoring Recap}
While refactoring our query, we made the following enhancements to our schema.
@(racketblock
  (Product SalesOrderDetail)
  (ProductSubcategoryID SalesOrderDetail)
  (DetailsG ProductSubcategory))

@task{Task 6: Sales by Anything}
Let's look at the previous two tasks.
@task4-quote
@task5-quote

We can generalize this to
@italic{Show me a list of the best-selling THINGS of all time.
 Sort by total revenue. Include total quantity sold and SOME_OTHER_STUFF.}

Let's look at where we left the previous two tasks:
@(racketblock
  (code:comment "Sales by Product:")
  (from prd Product
        (limit 5)
        (select (ProductNumber prd))
        (select (SubcategoryName prd))
        (join detailsG (DetailsG prd))
        (select (>> (round (sum (LineTotal detailsG)) 2)
                    #:as 'TotalSales))
        (select (>> (sum (OrderQty detailsG))
                    #:as 'TotalQty))
        (order-by 'desc (sum (LineTotal detailsG))))
  (code:comment "Sales by Subcategory:")
  (from subcat ProductSubcategory
        (limit 5)
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))
        (join detailsG (DetailsG subcat))
        (select (>> (round (sum (LineTotal detailsG)) 2)
                    #:as 'TotalSales))
        (select (>> (sum (OrderQty detailsG))
                    #:as 'TotalQty))
        (order-by 'desc (sum (LineTotal detailsG)))))

Notice that the last few clauses of both queries are identical.
We could easily remove this duplication using a macro, but we can also use a regular
procedure if we recognize that queries are appendable [TODO link here?].
We will make a procedure @(racket sales-report) which accepts a query and appends some
more clauses to the end of it:
@(examples
  #:eval my-eval
  #:no-result
  (define (sales-report some-query)
    (from x some-query
          (limit 5)
          (join detailsG (DetailsG x))
          (select (>> (round (sum (LineTotal detailsG)) 2)
                      #:as 'TotalSales))
          (select (>> (sum (OrderQty detailsG))
                      #:as 'TotalQty))
          (order-by 'desc (sum (LineTotal detailsG))))))

Now we can use this procedure to reimplement the Sales by Product report:
@(repl-query
  (show-table
   (sales-report (from prd Product
                       (select (ProductNumber prd))
                       (select (SubcategoryName prd))))))

We can do the same thing for the Sales by Subcategory report:
@(repl-query
  (show-table
   (sales-report (from subcat ProductSubcategory
                       (select (SubcategoryName subcat))
                       (select (CategoryName subcat))))))

Interesting! The @(racket sales-report) procedure accepts a query of any table
for which @(racket DetailsG) is defined!
It appends some more clauses and returns the result.

@subsubsection[#:tag "ec1"]{Extra Credit}
Extra Credit: Extend the definition of DetailsG so that it is defined for Category, SalesPerson, and Territory.
Try plugging those tables into the generalized sales report.

@subsubsection[#:tag "ec2"]{Extra Credit}
Extra Credit: Instead of using appendable queries, implement the generalized sales report
as a procedure that returns a list of the relevant clauses.

@task{Task 7: Sales by Anything with Date Range}
What else could we do with the @(racket sales-report)?
Right now it is showing all-time sales.
We could modify it to accept a time window of sales to consider as follows:
@(examples
  #:eval my-eval
  #:no-result
  (define (sales-report some-query
                        #:start-date [start-date #f]
                        #:end-date [end-date #f])
    (from x some-query
          (limit 5)
          (join detailsG (DetailsG x)
                (join soh SalesOrderHeader
                      (join-on (.= (SalesOrderID soh)
                                   (SalesOrderID detailsG))))
                (when start-date
                  (where (.>= (OrderDate soh)
                              start-date)))
                (when end-date
                  (where (.< (OrderDate soh)
                             end-date))))
          (select (>> (round (sum (LineTotal detailsG)) 2)
                      #:as 'TotalSales))
          (select (>> (sum (OrderQty detailsG))
                      #:as 'TotalQty))
          (order-by 'desc (sum (LineTotal detailsG))))))

@(repl-query
  ; TODO should have a %%datetime proc available here
  (show-table
   (sales-report
    #:start-date (>> (%%sql "'2012-01-01'") #:cast Datetime #:null no)
    #:end-date (>> (%%sql "'2013-01-01'") #:cast Datetime #:null no)
    (from subcat ProductSubcategory
          (select (SubcategoryName subcat))
          (select (CategoryName subcat))))))

@subsubsection[#:tag "ec3"]{Extra Credit}
Apply the appropriate refactoring recipies so that @(racket sales-report)
can be rewritten as follows:
@(racketblock
  (define (sales-report some-query
                        #:start-date [start-date #f]
                        #:end-date [end-date #f])
    (from x some-query
          (limit 5)
          (join detailsG (DetailsG x)
                (when start-date
                  (where (.>= (OrderDate detailsG)
                              start-date)))
                (when end-date
                  (where (.< (OrderDate detailsG)
                             end-date))))
          (select (>> (round (sum (LineTotal detailsG)) 2)
                      #:as 'TotalSales))
          (select (>> (sum (OrderQty detailsG))
                      #:as 'TotalQty))
          (order-by 'desc (sum (LineTotal detailsG))))))

Hint: First use recipe TODO to define
@(racketblock
  (SalesOrderHeader SalesOrderDetail))
Then use recipe TODO to define
@(racketblock
  (OrderDate SalesOrderDetail))
