#lang scribble/manual
@(require scribble/example
          "helpers.rkt"
          plisqin-examples/adventure-works
          racket/exn
          racket/string
          (prefix-in db: db)
          (for-syntax racket/base)
          (for-label "standard-label.rkt"))

@(define-syntax-rule (bossquote stuff ...)
   (tabular #:row-properties '(left right)
            (list (list (italic stuff ...))
                  (list @nested{-- your boss}))))


@title[#:tag "using-define-schema"]{Using define-schema}
@section{Teaser}
@margin-note{
 TODO: need some link or quick explanation of the Adventure Works
 sample database.}
We are about to walk through how to use @(racket define-schema) to partially
recreate the definitions exported by the
@(racket plisqin-examples/adventure-works/schema) module.
But first, here is a preview of how the finished definitions can be used.
Imagine that I need to write a query to find the top 3 best-selling Products
of all-time, showing Product Name, Category Name, and Total Sales.
Because the schema already contains the definitions I need, it is very easy:
@(repl-query
  (aw:show-table
   (from p Product
         (limit 3)
         (select (ProductName p))
         (select (CategoryName p))
         (select (TotalSales p))
         (order-by 'desc (TotalSales p)))))

Click on "Show SQL" and you might be surprised!
The generated SQL is much larger than the Plisqin query.
This may seem like "too much magic" right now, but once you learn how it works
it is actually pretty formulaic.

@section{Getting Started}
@(define aw-schema @tech{aw-schema.rkt})
Start a new file using #lang racket.
Save it as @deftech{aw-schema.rkt}, short for "Adventure Works schema".
Add the the following code to this file:
@(racketblock
  (require plisqin
           (prefix-in aw: plisqin-examples/adventure-works)))

You should now have access to the @(racket aw:show-table) procedure from your
REPL. This allows you to execute a query against the SQLite database.
Try executing the following raw SQL:
@(repl-query
  (aw:show-table
   "select name from sqlite_master where name like 'Business%'"))
@(repl-query
  (aw:show-table
   "pragma table_info(BusinessEntity)"))

@(define initial-url
   "https://github.com/default-kramer/plisqin/blob/morselize/plisqin-doc/scribblings/adventure-works-checkpoints/1.rkt#L8")
@(load-checkpoint! "1.rkt")

You might imagine that we could use "sqlite_master" and "pragma table_info"
to generate definitions for all the tables and columns in the database.
You would need to do that in a real project, but that is not very interesting.
We are much more interested the new definitions we will be adding.
So we will take a shortcut -- copy and paste the @(racket (define-schema ....))
from @link[initial-url]{this file} into your @aw-schema file.
This creates definitions for all the tables and columns.
The following query should now work on your REPL
@(repl-query
  (aw:show-table
   (from pc ProductCategory
         (select (Name pc)))))

@margin-note{
 TODO I suspect I can speed up define-schema significantly.}
Now that @tech{aw-schema.rkt} is somewhat large, DrRacket may
take a long time to run it. For this reason, I recommend that you add
@(racket (provide (all-defined-out))) to your @aw-schema file and
@(racket require) it from another file to avoid this delay whenever possible.
You may also want to disable debugging in DrRacket.

There are some other REPL tricks you should learn before proceeding.
The first identifier we passed into @(racket define-schema) was
@(racket adventure-works-schema). This procedure is designed to be used at
the REPL as a manual alternative to autocomplete.
For example, if you want to know what procedures exist that accept a
ProductCategory, you can ask using the REPL:
@(repl
  (adventure-works-schema '(_ ProductCategory)))

You can also ask what types of argument the ProductCategoryID procedure accepts.
@(repl
  (adventure-works-schema '(ProductCategoryID _)))

(The previous REPL interaction is very useful for discovering relationships
between tables in this database. This is one reason that I prefer primary
key columns to be named like "ProductCategoryID" rather than "ID".)

There is one final REPL trick which lists all the tables.
The output of this will be long, so here is the command only:
@(racketblock
  (adventure-works-schema 'tables))

@section{The Tasks}
@(define task subsection)
In this situation, you are a new employee at Adventure Works.
They may have bits of SQL here and there, but it can be hard to find and most
of it is not reusable at all.
Your boss is going to ask you for a series of reports.
You will explore the database, create a query, and capture certain facts about
this database into @(racket define-schema) so that it is easy for you and other
programmer to find and reuse them.

Each subsection will correspond to one task, a request from your boss. You will
@(itemlist
  #:style 'ordered
  @item{Create a query.}
  @item{Repeatedly refactor that query.
 In this part, you will add definitions to @tech{aw-schema.rkt}.}
  @item{Recap and verify that your refactorings were correct.}
  @item{Proceed to the next task.})

During refactoring, I will link you to refactoring recipes that you will follow.
These recipes are meant to be very thorough.
As you build confidence with the recipes, you might start taking shortcuts.
This is fine. You can use the Recap to make sure you are in sync.

@(load-checkpoint! "1.rkt")
@task{Task 1: Subcategories & Categories}
@bossquote{I want to see a list of Subcategories with the Category that they belong to.}

We need to write a query.
The first step, as always, is to determine which table we need to query.
The @(racket ProductSubcategory) table looks promising.
Let's see what it contains:
@(repl-query
  (aw:show-table
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
  (aw:show-table
   (from subcat ProductSubcategory
         (limit 5)
         (join cat ProductCategory
               (join-on (.= (ProductCategoryID cat)
                            (ProductCategoryID subcat))))
         (select (Name subcat))
         (select (Name cat)))))

That's better, but we can see that @(racket Name) is not a very good name.
Use the @(secref "ds:rename") recipe twice to create
the following equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (join cat ProductCategory
              (join-on (.= (ProductCategoryID cat)
                           (ProductCategoryID subcat))))
        (select (code:hilite (SubcategoryName subcat)))
        (select (code:hilite (CategoryName cat)))))

That looks good. But we are not done refactoring.
Use the @(secref "join1->schema") recipe to create the following equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (code:hilite (join cat (ProductCategory subcat)))
        (select (SubcategoryName subcat))
        (select (CategoryName cat))))

But we are still not done refactoring.
Use the @(secref "join->inline") recipe to create the following equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (code:hilite (code:comment "the join has been moved inline"))
        (select (SubcategoryName subcat))
        (select (code:hilite (CategoryName (ProductCategory subcat))))))

But we are still not done refactoring.
Use the @(secref "scalar->schema") recipe to create the following equivalent query:
@(load-checkpoint! "2.rkt")
@(repl-query
  (aw:show-table
   (from subcat ProductSubcategory
         (limit 5)
         (select (SubcategoryName subcat))
         (select (code:hilite (CategoryName subcat))))))

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
  (aw:show-table
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
@margin-note{The code @(racket (?? expr /void)) says that "if expr is null, it
 should not be considered equal to anything."
 You can read more about this at @(secref "Nullability").}
@(repl-query
  (aw:show-table
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
@(void
  ; The careful reader will notice that both joins in the generated SQL are
  ; left joins. We could write up how @(racket (join-type 'infer)) works
  ; and link to it here, but I think that is just a distraction...
  )
@(repl-query
  (aw:show-table
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
Use the @(secref "join1->schema") recipe to create the following equivalent query:
@(racketblock
  (from prd Product
        (limit 5)
        (code:hilite (join subcat (ProductSubcategory prd)))
        (select (Name prd))
        (select (ProductNumber prd))
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))))

Use the @(secref "join->inline") recipe to create the following equivalent query:
@(racketblock
  (from prd Product
        (limit 5)
        (code:hilite (code:comment "the join has been moved inline"))
        (select (Name prd))
        (select (ProductNumber prd))
        (select (code:hilite (SubcategoryName (ProductSubcategory prd))))
        (select (code:hilite (CategoryName (ProductSubcategory prd))))))

Use the @(secref "scalar->schema") recipe twice to create the following equivalent query:
@(racketblock
  (from prd Product
        (limit 5)
        (select (Name prd))
        (select (ProductNumber prd))
        (select (code:hilite (SubcategoryName prd)))
        (select (code:hilite (CategoryName prd)))))

Use the @(secref "ds:rename") recipe to create the following equivalent query:
@(load-checkpoint! "3.rkt")
@(repl-query
  (aw:show-table
   (from prd Product
         (limit 5)
         (select (code:hilite (ProductName prd)))
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (select (CategoryName prd)))))

And now we are done!

@subsubsub*section{Refactoring Recap}
While refactoring our query, we made the following enhancements to our schema.
You can try the following on your REPL and verify that no error is raised.
@(repl
  (void (list (ProductSubcategory Product)
              (SubcategoryName Product)
              (CategoryName Product)
              (ProductName Product))))

@task{Task 3: Products with Non-Zero Sales}
@bossquote{Show me a list of Products that have non-zero sales,
 with Subcategory and Category names.}
This just adds the "non-zero sales" criteria to the previous task.
Your boss explains that the Product catalog could use some culling, but for now
"has sales?" is an acceptable way to filter out obsolete Products.

How do we know whether a Product has been sold?
Remember our REPL tricks. We might want to see the list of tables again:
@(racketblock
  (adventure-works-schema 'tables))
It looks like the SalesOrderHeader and SalesOrderDetail tables are the main
suspects. Let's see what is defined for both of these tables:
@(racketblock
  (adventure-works-schema '(_ SalesOrderHeader))
  (adventure-works-schema '(_ SalesOrderDetail)))

Aha, the SalesOrderDetail table has a ProductID column.
This seems to be the relationship we need to consider.
The presence of at least one SalesOrderDetail means that a Product has been sold.
With that in mind, we can write this query:
@(repl-query
  (aw:show-table
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
Use the @(secref "scalar->schema") recipe to create the following equivalent query:
@(load-checkpoint! "4.rkt")
@(repl-query
  (aw:show-table
   (from prd Product
         (limit 5)
         (select (ProductName prd))
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (select (CategoryName prd))
         (where (code:hilite (HasSales? prd))))))

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

This query will be of the Product table.
But in order to get "total revenue" and "total quantity sold", we will need
to use some aggregations.
We recall from the previous task that the SalesOrderDetail table points to
the Product table.
From the other direction, we can say that each Product has a group of
SalesOrderDetail records -- the records whose ProductID matches the Product.
We build the @(racket detailsG) grouped join and perform aggregations over it
to solve this task.

@margin-note{
 TODO: Grouped joins and aggregates deserve their own section. Link to it here.}
@(void
  ; Missing Feature: The as-name of (LineTotal detailsG)) is 'LineTotal.
  ; We could infer the as-name of (sum (LineTotal detailsG))) to be 'sum_LineTotal
  ; We could say that the as-name of (round x) is the as-name of x.
  ; By these rules, the inferred as-name would be 'sum_LineTotal.
  )

@(repl-query
  (aw:show-table
   (from prd Product
         (limit 5)
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (join detailsG SalesOrderDetail
               (join-type 'left)
               (group-by (ProductID detailsG))
               (join-on (.= (ProductID detailsG)
                            (ProductID prd))))
         (select (>> (round (sum (LineTotal detailsG)) 2)
                     #:as 'TotalSales))
         (select (>> (sum (OrderQty detailsG))
                     #:as 'TotalQtySold))
         (order-by 'desc (sum (LineTotal detailsG))))))

Now it is time to refactor.
Use the @(secref "joinG->schema") recipe to create the following equivalent query:
@(load-checkpoint! "5.rkt")
@(repl-query
  (aw:show-table
   (from prd Product
         (limit 5)
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (join detailsG (code:hilite (DetailsG prd)))
         (select (>> (round (sum (LineTotal detailsG)) 2)
                     #:as 'TotalSales))
         (select (>> (sum (OrderQty detailsG))
                     #:as 'TotalQtySold))
         (order-by 'desc (sum (LineTotal detailsG))))))

You might want to go further with the refactoring and use the
@(secref "scalar->schema") recipe to define @(racket (TotalSales Product))
and @(racket (TotalQtySold Product)).
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
  (aw:show-table
   (from subcat ProductSubcategory
         (limit 5)
         (select (SubcategoryName subcat))
         (select (CategoryName subcat))
         (join detailsG SalesOrderDetail
               (join-type 'left)
               (join prd Product
                     (join-on (.= (ProductID prd)
                                  (ProductID detailsG))))
               (group-by (ProductSubcategoryID prd))
               (join-on (.= (?? (ProductSubcategoryID prd) /void)
                            (ProductSubcategoryID subcat))))
         (select (>> (round (sum (LineTotal detailsG)) 2)
                     #:as 'TotalSales))
         (select (>> (sum (OrderQty detailsG))
                     #:as 'TotalQtySold))
         (order-by 'desc (sum (LineTotal detailsG))))))

Use the @(secref "join1->schema") recipe to create the following equivalent query:
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
        (select (>> (round (sum (LineTotal detailsG)) 2)
                    #:as 'TotalSales))
        (select (>> (sum (OrderQty detailsG))
                    #:as 'TotalQtySold))
        (order-by 'desc (sum (LineTotal detailsG)))))

Use the @(secref "join->inline") recipe to create the following equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))
        (join detailsG SalesOrderDetail
              (code:hilite (code:comment "the join is moved inline"))
              (group-by (code:hilite (ProductSubcategoryID (Product detailsG))))
              (join-on (.= (code:hilite (ProductSubcategoryID (Product detailsG)))
                           (ProductSubcategoryID subcat))))
        (select (>> (round (sum (LineTotal detailsG)) 2)
                    #:as 'TotalSales))
        (select (>> (sum (OrderQty detailsG))
                    #:as 'TotalQtySold))
        (order-by 'desc (sum (LineTotal detailsG)))))

Use the @(secref "scalar->schema") recipe to create the following equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))
        (join detailsG SalesOrderDetail
              (group-by (code:hilite (ProductSubcategoryID detailsG)))
              (join-on (.= (code:hilite (ProductSubcategoryID detailsG))
                           (ProductSubcategoryID subcat))))
        (select (>> (round (sum (LineTotal detailsG)) 2)
                    #:as 'TotalSales))
        (select (>> (sum (OrderQty detailsG))
                    #:as 'TotalQtySold))
        (order-by 'desc (sum (LineTotal detailsG)))))

Use the @(secref "joinG->schema") recipe to create the following equivalent query:
@(load-checkpoint! "6.rkt")
@(repl-query
  (aw:show-table
   (from subcat ProductSubcategory
         (limit 5)
         (select (SubcategoryName subcat))
         (select (CategoryName subcat))
         (join detailsG (code:hilite (DetailsG subcat)))
         (select (>> (round (sum (LineTotal detailsG)) 2)
                     #:as 'TotalSales))
         (select (>> (sum (OrderQty detailsG))
                     #:as 'TotalQtySold))
         (order-by 'desc (sum (LineTotal detailsG))))))

Again, you might want to go further with the refactoring and define properties
like @(racket (TotalSales ProductSubcategory)) and
@(racket (TotalQtySold ProductSubcategory)).
This might be a good idea, and you are welcome to do so.
But I am going to stop here for now.

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
                    #:as 'TotalQtySold))
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
                    #:as 'TotalQtySold))
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
                      #:as 'TotalQtySold))
          (order-by 'desc (sum (LineTotal detailsG))))))

Now we can use this procedure to reimplement the Sales by Product report:
@(repl-query
  (aw:show-table
   (sales-report (from prd Product
                       (select (ProductNumber prd))
                       (select (SubcategoryName prd))))))

We can do the same thing for the Sales by Subcategory report:
@(repl-query
  (aw:show-table
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
                      #:as 'TotalQtySold))
          (order-by 'desc (sum (LineTotal detailsG))))))

@(repl-query
  ; TODO should have a %%datetime proc available here
  (aw:show-table
   (sales-report
    #:start-date (>> (%%sql "'2012-01-01'") #:cast Datetime? #:null no)
    #:end-date (>> (%%sql "'2013-01-01'") #:cast Datetime? #:null no)
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
                      #:as 'TotalQtySold))
          (order-by 'desc (sum (LineTotal detailsG))))))

Hint: First use the @(secref "join1->schema") recipe to define
@(racketblock
  (SalesOrderHeader SalesOrderDetail))

Next use the @(secref "join->inline") recipe, and finally use the
@(secref "scalar->schema") recipe to define
@(racketblock
  (OrderDate SalesOrderDetail))
