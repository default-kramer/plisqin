#lang scribble/manual
@(require scribble/example
          "helpers.rkt"
          plisqin-examples/adventure-works
          racket/exn
          racket/string
          (prefix-in db: db)
          (for-syntax racket/base)
          (for-label "standard-label.rkt"))

@(begin
   (define-syntax-rule (bossquote stuff ...)
     (tabular #:row-properties '(left right)
              (list (list (italic stuff ...))
                    (list @nested{-- your boss}))))

   (def-green-ids repl-query
     [CATEGORYNAME           CategoryName]
     [DETAILSG               DetailsG]
     [HASSALES?              HasSales?]
     [PRODUCT                Product]
     [PRODUCTCATEGORY        ProductCategory]
     [PRODUCTNAME            ProductName]
     [PRODUCTSUBCATEGORY     ProductSubcategory]
     [PRODUCTSUBCATEGORYID   ProductSubcategoryID]
     [SUBCATEGORYNAME        SubcategoryName])

   (define-syntax-rule (recap x ...)
     @nested{
 @subsubsub*section{Refactoring Recap}
 While refactoring our query, we made the following enhancements to our schema.
 You can try the following on your REPL and verify that no error is raised.
 @(repl #:no-result x ...)
 })
   )


@title[#:tag "using-define-schema"]{Using define-schema}
For this walkthrough, you are a new employee at AdventureWorks.
AdventureWorks is a fictional company that sells bicycles and related products.
The company has a mature database, but the SQL that has been written so far
is scattered all over the place, making it difficult to find and reuse.

Your boss is going to ask you for a series of reports.
As you produce these reports, you will capture certain facts about this
database using @(racket define-schema), making it easier for yourself and
other programmers to find and reuse them.
You will also learn how Plisqin offers opportunities for code reuse that
are simply impossible using plain SQL.

@section{Teaser}
Here is a preview of what you will be able to do by the end of this walkthrough.
Imagine that your boss asks to find the top 3 best-selling Products
of all-time, showing Product Name, Category Name, and Total Sales.
Thanks to the reusable definitions you have already captured using
@(racket define-schema), it is this easy:
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
Start a new file using @racketplainfont{#lang racket}.
Save it as @deftech{aw-schema.rkt}, short for "AdventureWorks schema".
Add the the following code to this file:
@(racketblock
  (require plisqin
           (prefix-in aw: plisqin-examples/adventure-works)))

You should now have access to the @(racket aw:show-table) procedure from your
REPL. This allows you to execute a query against the SQLite database.
To make sure everything is set up correctly, try asking SQLite what time it is:
@(repl-query
  (aw:show-table "select datetime('now')"))

@(define initial-url
   "https://github.com/default-kramer/plisqin/blob/morselize/plisqin-doc/scribblings/adventure-works-checkpoints/1.rkt#L8")

@margin-note{
 If you really want to know how to automatically generate this, see
 @(secref "schema-generation").}
OK, the first thing we need to add to our schema definition is the
table and column information.
There are ways to automatically inspect the database and generate the initial
schema definition, but that is not what we are interested in right now.
You can just copy and paste the @(racket (define-schema ....))
from @link[initial-url]{this file} into your @aw-schema file.

@(load-checkpoint! "1.rkt")
Save, Run, and the following query should now work on your REPL:
@(repl-query
  (aw:show-table
   (from pc ProductCategory
         (select (Name pc)))))

Now that @tech{aw-schema.rkt} is somewhat large, DrRacket may
take a long time to run it. For this reason, I recommend that you add
@(racket (provide (all-defined-out))) to your @aw-schema file and
@(racket require) it from another file to avoid this delay whenever possible.
Disabling debugging in DrRacket will also speed things up a lot.

There are a few REPL tricks you should learn before proceeding.
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
Your boss is going to assign you some tasks.
For each task, you will
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

@margin-note{Remember you can use @(racket (adventure-works-schema 'tables))
 to list all the tables.}
We need to create a query.
The first step is to determine which table we need to query.
The @(racket ProductSubcategory) table looks promising.
Let's see what it contains:
@(repl-query
  (aw:show-table
   (from subcat ProductSubcategory
         (limit 5))))

This is a good start.
Your boss didn't say exactly which columns he wants to see, but he did say
to include "the Category".
It looks like ProductCategoryID is a foreign key.
(Any column that ends in "ID" is probably a primary key or foreign key.)
We can check if any other tables have a ProductCategoryID as follows:
@(repl
  (adventure-works-schema '(ProductCategoryID _)))

Based on the names, we conclude that ProductCategoryID is the primary key
of the ProductCategory table.
And we conclude that each ProductSubcategory record points to a single
ProductCategory record via the ProductCategoryID column.
We need to add a join to our query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (join cat ProductCategory
              (join-on (.= (ProductCategoryID cat)
                           (ProductCategoryID subcat))))))

You can run that query, but it will produce the same result set.
Even though we added a join, the query does not contain any @(racket select)
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

Now the query seems to be returning the correct data.
We saw the "Mountain Bikes" subcategory earlier, but now we also see that it
belongs to the "Bikes" category.
One obvious problem is that both columns are shown as "Name".
We will immediately fix that during refactoring.

@subsubsub*section{Refactoring}
Use the @(secref "ds:rename") recipe twice to create
the following equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (join cat ProductCategory
              (join-on (.= (ProductCategoryID cat)
                           (ProductCategoryID subcat))))
        (select (SUBCATEGORYNAME subcat))
        (select (CATEGORYNAME cat))))

That looks good. But we are not done refactoring.
Use the @(secref "join1->schema") recipe to create the following equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (join cat (PRODUCTCATEGORY subcat))
        (select (SubcategoryName subcat))
        (select (CategoryName cat))))

But we are still not done refactoring.
Use the @(secref "join->inline") recipe to create the following equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        #,(code:strike (join cat (code:hilite (ProductCategory subcat))))
        (select (SubcategoryName subcat))
        (select (CategoryName (code:hilite (ProductCategory subcat))))))

But we are still not done refactoring.
Use the @(secref "scalar-flattening") recipe to create the following equivalent query:
@(load-checkpoint! "2.rkt")
@(repl-query
  (aw:show-table
   (from subcat ProductSubcategory
         (limit 5)
         (select (SubcategoryName subcat))
         (select (CATEGORYNAME subcat)))))

And now we are done!

@(recap (SubcategoryName ProductSubcategory)
        (CategoryName ProductCategory)
        (ProductCategory ProductSubcategory)
        (CategoryName ProductSubcategory))

@task{Task 2: Products & Subcategories & Categories}
@bossquote{Show me a list of Products with Subcategory and Category names.}

We need to create a query.
The first step is to determine the table we need to query.
The @(racket Product) table looks promising.
Let's see what it contains:
@(repl-query
  (aw:show-table
   (from prd Product
         (limit 5))))

It looks like ProductSubcategoryID is a foreign key to the ProductSubcategory table.
But at least some of the records have dbnull for their ProductSubcategoryID.
This means that not every Product belongs to a ProductSubcategory.
We create a join and use @(racket (join-type 'left)) to avoid eliminating
these Products from the result set:
@(racketblock
  (from prd Product
        (limit 5)
        (join subcat ProductSubcategory
              (join-type 'left)
              (join-on (.= (ProductSubcategoryID subcat)
                           (ProductSubcategoryID prd))))))

Now we need to add some select clauses:
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

@subsubsub*section{Refactoring}
Use the @(secref "join1->schema") recipe to create the following equivalent query:
@(racketblock
  (from prd Product
        (limit 5)
        (join subcat (PRODUCTSUBCATEGORY prd))
        (select (Name prd))
        (select (ProductNumber prd))
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))))

Use the @(secref "join->inline") recipe to create the following equivalent query:
@(racketblock
  (from prd Product
        (limit 5)
        #,(code:strike (join subcat (code:hilite (ProductSubcategory prd))))
        (select (Name prd))
        (select (ProductNumber prd))
        (select (SubcategoryName (code:hilite (ProductSubcategory prd))))
        (select (CategoryName (code:hilite (ProductSubcategory prd))))))

Use the @(secref "scalar-flattening") recipe twice to create the following
equivalent query:
@(racketblock
  (from prd Product
        (limit 5)
        (select (Name prd))
        (select (ProductNumber prd))
        (select (SUBCATEGORYNAME prd))
        (select (CATEGORYNAME prd))))

Use the @(secref "ds:rename") recipe to create the following equivalent query:
@(load-checkpoint! "3.rkt")
@(repl-query
  (aw:show-table
   (from prd Product
         (limit 5)
         (select (PRODUCTNAME prd))
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (select (CategoryName prd)))))

And now we are done!

@(recap (ProductSubcategory Product)
        (SubcategoryName Product)
        (CategoryName Product)
        (ProductName Product))

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

@subsubsub*section{Refactoring}
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
         (where (HASSALES? prd)))))

And now we are done!

@(recap (HasSales? Product))

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

So far, each query prior to refactoring has been similar to how you would
write the query using plain SQL.
This time, the initial query will use @tech{grouped join aggregation},
a technique that is unlike anything in SQL.
You don't need to understand this right now in order to perform the refactoring,
but if you are curious, the @(secref "Aggregates") section should be next on
your reading list.

We need to create a query.
The first step is to determine the table we need to query.
This query will be of the Product table.
But in order to get "total revenue" and "total quantity sold", we will need
to use some aggregations.
We recall from the previous task that the SalesOrderDetail table points to
the Product table.
From the other direction, we can say that each Product has a group of
SalesOrderDetail records -- the records whose ProductID matches the Product.
We build the @(racket detailsG) @tech{grouped join} and perform aggregations
over it to solve this task.
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

This query looks good!

@subsubsub*section{Refactoring}
Use the @(secref "joinG->schema") recipe to create the following equivalent query:
@(load-checkpoint! "5.rkt")
@(repl-query
  (aw:show-table
   (from prd Product
         (limit 5)
         (select (ProductNumber prd))
         (select (SubcategoryName prd))
         (join detailsG (DETAILSG prd))
         (select (>> (round (sum (LineTotal detailsG)) 2)
                     #:as 'TotalSales))
         (select (>> (sum (OrderQty detailsG))
                     #:as 'TotalQtySold))
         (order-by 'desc (sum (LineTotal detailsG))))))

You might want to go further with the refactoring by using the
@(secref "join->inline") recipe to move @(racket detailsG) inline, then using the
@(secref "scalar->schema") recipe to define @(racket (TotalSales Product))
and @(racket (TotalQtySold Product)).
This might be a good idea, and you are welcome to do so.
But I am going to stop here for now.

@(recap (DetailsG Product))

@task{Task 5: Sales by Subcategory}
@(define task5-quote
   @bossquote{Show me a list of the best-selling Subcategories of all time.
 Sort by total revenue. Include total quantity sold and category name.})
@task5-quote

This task is very similar to the previous task.
We need to create a query.
This query will be of the ProductSubcategory table.
In order to get "total revenue" and "total quantity sold", we will use
@tech{grouped join aggregation} over the SalesOrderDetail table just as we
did in the previous task.
Again, you don't need to fully understand grouped join aggregation at this time.
You can take the initial query on faith; just make sure that the refactoring
makes sense to you.
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

This query looks good!

@subsubsub*section{Refactoring}
Use the @(secref "join1->schema") recipe to create the following equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))
        (join detailsG SalesOrderDetail
              (join-type 'left)
              (join prd (PRODUCT detailsG))
              (group-by (ProductSubcategoryID prd))
              (join-on (.= (?? (ProductSubcategoryID prd) /void)
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
              (join-type 'left)
              #,(code:strike (join prd (code:hilite (Product detailsG))))
              (group-by (ProductSubcategoryID (code:hilite (Product detailsG))))
              (join-on (.= (?? (ProductSubcategoryID (code:hilite (Product detailsG))) /void)
                           (ProductSubcategoryID subcat))))
        (select (>> (round (sum (LineTotal detailsG)) 2)
                    #:as 'TotalSales))
        (select (>> (sum (OrderQty detailsG))
                    #:as 'TotalQtySold))
        (order-by 'desc (sum (LineTotal detailsG)))))

Use the @(secref "scalar-flattening") recipe to create the following equivalent query:
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))
        (join detailsG SalesOrderDetail
              (join-type 'left)
              (group-by (PRODUCTSUBCATEGORYID detailsG))
              (join-on (.= (?? (PRODUCTSUBCATEGORYID detailsG) /void)
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
         (join detailsG (DETAILSG subcat))
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

@(recap (Product SalesOrderDetail)
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
procedure if we recognize that queries are appendable.
By "appendable" I mean that a query has a list of clauses, and you can easily
add more clauses to an existing query.
The following example demonstrates this.
@(repl
  (define original
    (from p Product
          (select (ProductNumber p))
          (select (ProductName p))
          (select (SubcategoryName p))
          (select (CategoryName p))))
  (define first-half
    (from p Product
          (select (ProductNumber p))
          (select (ProductName p))))
  (define both-halves
    (code:comment "This appends two more clauses to `first-half`")
    (from x first-half
          (select (SubcategoryName x))
          (select (CategoryName x))))
  (eval:check (equal? original both-halves) #t))

With this pattern in mind, we can make a procedure @(racket sales-report) which
accepts a query and appends some clauses to it.
We will append the clauses that were common to the two previous tasks:
@(examples
  #:eval my-eval
  #:no-result
  (define (sales-report first-half)
    (from x first-half
          (limit 5)
          (join detailsG (DetailsG x))
          (select (>> (round (sum (LineTotal detailsG)) 2)
                      #:as 'TotalSales))
          (select (>> (sum (OrderQty detailsG))
                      #:as 'TotalQtySold))
          (order-by 'desc (sum (LineTotal detailsG))))))

Now we can use this procedure to reimplement the Sales by Product report.
We just have to pass in the @(racket first-half) as follows:
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
It appends some more clauses to create a larger query.

@subsubsection[#:tag "ec1"]{Extra Credit}
Extra Credit: Extend the definition of DetailsG so that it is defined for Category, SalesPerson, and Territory.
Try using @(racket sales-report) with these tables.

@task{Task 7: Sales by Anything with Date Range}
What else could we do with the @(racket sales-report)?
Right now it is showing all-time sales.
We could modify it to accept a time window of sales to consider as follows:
@(examples
  #:eval my-eval
  #:no-result
  (define (sales-report first-half
                        #:start-date [start-date #f]
                        #:end-date [end-date #f])
    (from x first-half
          (limit 5)
          (join detailsG (DetailsG x)
                (code:comment "Like queries, joins are also appendable.")
                (code:comment "The following clauses are appended to the join")
                (code:comment "that (DetailsG x) returned:")
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
  (aw:show-table
   (sales-report
    #:start-date (>> (val "2012-01-01") #:cast Datetime?)
    #:end-date (>> (val "2013-01-01") #:cast Datetime?)
    (from subcat ProductSubcategory
          (select (SubcategoryName subcat))
          (select (CategoryName subcat))))))

@subsubsection[#:tag "ec3"]{Extra Credit}
Apply the appropriate refactoring recipies so that @(racket OrderDate) is
defined for @(racket SalesOrderDetail).
This allows you to refactor @(racket sales-report) as follows:
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
@(secref "scalar-flattening") recipe to define
@(racketblock
  (OrderDate SalesOrderDetail))

@section[#:tag "schema-generation"]{
 Appendix A: Generating the Initial Schema Definition}
Extracting table and column information for use with @(racket define-schema)
is currently an ad-hoc process.
Plisqin may make this easier in the future.
But it is already pretty easy if you are using SQL Server or PostgreSQL.

@(define gen-schema-url
   (string-append "https://github.com/default-kramer/plisqin/blob/master/"
                  "plisqin-doc/scribblings/adventure-works-checkpoints/gen-schema.sql"))
The AdventureWorks database is originally for SQL Server;
Plisqin provides a port of this database to SQLite.
So to generate the initial AdventureWorks schema, I ran
@hyperlink[gen-schema-url]{this query} against the SQL Server original.
This produces Racket code that just needs some simple text massaging.

If you have no choice but to use SQLite, you would probably have to write
a small program with some loops that uses "sqlite_master" and "pragma table_info".
But the following queries show that the data is available:
@(repl-query
  (aw:show-table
   "select type, name from sqlite_master where name like 'Business%'"))
@(repl-query
  (aw:show-table
   "pragma table_info(BusinessEntity)"))
