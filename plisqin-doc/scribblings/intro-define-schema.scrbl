#lang scribble/manual
@(require scribble/example
          "helpers.rkt"
          plisqin-examples/adventure-works
          racket/exn
          (prefix-in db: db)
          (for-syntax racket/base)
          (for-label plisqin
                     "racket.rkt"))

@(define (load-checkpoint! checkpoint)
   (define cp-path (string-append "./adventure-works-checkpoints/" checkpoint))
   (reset-eval!)
   (my-eval `(begin
               (require plisqin-examples/adventure-works)
               (require ,cp-path)
               (current-connection (connect-adventure-works)))))
@(load-checkpoint! "1.rkt")

@(define (show-results query-datum)
   ; Use `my-eval` to get the sql from the query-datum.
   ; But we need to get the db:rows-result in our own evaluation context.
   ; This is OK, we can trust that (db:query adventure-works-conn sql)
   ; will work exactly the same in both evaluation contexts.
   (define sql
     (with-handlers ([exn? (λ(ex)
                             (error 'show-results "eval failed\n~a\n~a"
                                    query-datum (exn->string ex)))])
       (my-eval `(to-sql ,query-datum))))
   (define result
     (let* ([conn (connect-adventure-works)]
            [result (db:query conn sql)])
       (db:disconnect conn)
       result))
   (to-table result sql))

@(define-syntax-rule (repl form ...)
   (examples #:eval my-eval
             #:label #f
             form ...))

@(define-syntax (repl-query stx)
   (syntax-case stx ()
     [(_ show-table-form)
      (let* ([query (syntax-case #'show-table-form ()
                      [(_ q) #'q])]
             [query-datum (syntax->datum query)])
        #`(begin
            (racketinput show-table-form)
            (show-results '#,query-datum)))]))

@title{Using define-schema}
This section is opinionated:
it attempts to show you a somewhat rigid way to realize the benefits of Plisqin.
But using @(racket define-schema) is optional; Plisqin's core is schemaless and flexible.

This section is "top-down":
it does not attempt to explain everything completely.

You might prefer to learn "bottom-up", in which case you could start [TODO link].
Then you could return to this section, or you might decide that @(racket define-schema)
is not a good fit for your project.

@section{Initial Setup}
This page uses the AdventureWorks example database.
We will use @(racket define-schema) to encode facts about our database as Racket procedures.
Start with this file [TODO link to starting file].
If you are using DrRacket, you need to Run (ctrl + R) the file first.

Now let's try some basic queries on the REPL:
@(repl-query
  (show-table
   (from cat ProductCategory)))
@(repl-query
  (show-table
   (from cat ProductCategory
         (where (.> (ProductCategoryID cat) 1))
         (select (Name cat))
         (select (ModifiedDate cat)))))

Notice: In this document, all of Plisqin's operators are prefixed with a dot.
When you see a procedure that begins with a dot (such as @(racket .>)),
it refers to an operator from @(racket plisqin-lib/operators).
This agrees with @(racket (require plisqin)), which imports the operators prefixed with a dot.

@(define-syntax-rule (bossquote stuff ...)
   (tabular #:row-properties '(left right)
            (list (list (italic stuff ...))
                  (list @nested{-- your boss}))))

@section{Task 1}
@bossquote{I want to see a list of Subcategories with the Category that they belong to.}

Let's start by looking at the Subcategory table.
@(repl-query
  (show-table
   (from subcat ProductSubcategory
         (limit 5))))

OK, this is a start but we need to join the Category table to satisfy the request.
@(examples
  #:eval my-eval
  #:no-result
  #:no-prompt
  (define (task1/revision1)
    (from subcat ProductSubcategory
          (join cat ProductCategory
                (join-on (.= (ProductCategoryID cat)
                             (ProductCategoryID subcat))))
          (select (Name subcat) #:as 'SubcategoryName)
          (select (Name cat) #:as 'CategoryName))))

The definition of @(racket task1/revision1) is an acceptable solution.
If you want to see all the results, you can do
@(racketinput (show-table (task1/revision1)))

To avoid cluttering this documentation, I'll add a limit clause:
@(repl-query
  (show-table
   (from subcat (task1/revision1)
         (limit 5))))

@(define truncated (italic "truncated ..."))
@(define illustrative "For illustrative purposes only. Do not add this code to your file.")

@subsection{Making the Join Reusable}
But we can do better.
We've just determined that every ProductCategory has exactly 1 ProductCategory.
This is a join that we will probably want to reuse often.
So let's modify our @(racket define-schema) form by adding the hilighted code:
@margin-note{The highlighted change can also be seen in this diff [TODO link it!]}
@margin-note{This change could also be written
 @(racket #:has-one [ProductCategory #:using ProductCategoryID]).
 See the documentation of @(racket define-schema) for more shortcuts.}
@(racketblock
  (define-schema adventure-works-schema
    #,truncated
    (table ProductSubcategory
           #,truncated
           (code:hilite #:has-one)
           (code:hilite [ProductCategory
                         (join cat ProductCategory
                               (join-on (.= (ProductCategoryID cat)
                                            (ProductCategoryID this))))]))
    #,truncated))
@(load-checkpoint! "3.rkt")

In English, this says that a ProductSubcategory has one ProductCategory.
In Racket, it contributes to the definition of @(racket ProductCategory) by
generating something like the following code:
@(racketblock
  (code:comment #,illustrative)
  (define ((code:hilite ProductCategory) . args)
    (match args
      [(list subcat)
       #:when (ProductSubcategory? subcat)
       (let ([this subcat])
         (code:hilite (join cat ProductCategory
                            (join-on (.= (ProductCategoryID cat)
                                         (ProductCategoryID this))))))]
      #,(italic "maybe-more-match-clauses ..."))))

Now we can use that join:
@(racketblock
  (define (task1/revision2)
    (from subcat ProductSubcategory
          (code:hilite (join cat (ProductCategory subcat)))
          (select (Name subcat) #:as 'SubcategoryName)
          (select (Name cat) #:as 'CategoryName))))

Note that @(racket (task1/revision2)) is equivalent to @(racket (task1/revision1)), meaning they will
generate the same SQL. (They might not be @(racket equal?) though.)

For just one query, this revision might not seem like a big improvement.
But having the @(racket (ProductCategory subcat)) join at our fingertips will
surely pay dividends as we write more queries.
Also, moving that join into @(racket define-schema) serves as documentation of the fact
that ProductSubcategory has one ProductCategory.

@subsection{Making the Property Reusable}
But we can do better.
In Plisqin, joins are values and they can appear almost anywhere inside a query.
We can immediately write a third revision:
@(racketblock
  (define (task1/revision3)
    (from subcat ProductSubcategory
          (code:hilite (code:comment "The join is no longer here!"))
          (select (Name subcat) #:as 'SubcategoryName)
          (code:hilite (select (Name (ProductCategory subcat)) #:as 'CategoryName)))))

This is another equivalent revision.
And now we might notice that the expression @(racket (Name (ProductCategory subcat)))
could be returned from a function.
We can modify our @(racket define-schema) again to add this as a @tech{property}.
@margin-note{The highlighted change can also be seen in this diff [TODO link it!]}
@(racketblock
  (define-schema adventure-works-schema
    #,truncated
    (table ProductSubcategory
           #,truncated
           (code:hilite #:property)
           (code:hilite [CategoryName
                         (Name (ProductCategory this))]))
    #,truncated))
@(load-checkpoint! "4.rkt")

In English, this says that CategoryName is a property of ProductSubcategory.
In Racket, it contributes to the definition of @(racket CategoryName) by generating
something like the following code:
@(racketblock
  (code:comment #,illustrative)
  (define ((code:hilite CategoryName) . args)
    (match args
      [(list subcat)
       #:when (ProductSubcategory? subcat)
       (let ([this subcat])
         (code:hilite (Name (ProductCategory this))))]
      #,(italic "maybe-more-match-clauses ..."))))

This allows us to be even more concise:
@(racketblock
  (define (task1/revision4)
    (from subcat ProductSubcategory
          (select (Name subcat) #:as 'SubcategoryName)
          (code:hilite (select (CategoryName subcat) #:as 'CategoryName)))))
@(repl-query
  (show-table
   (from x (task1/revision4)
         (limit 5))))

This is the final revision, and it is ideal.
We determined that CategoryName is a property of ProductSubcategory,
so now we can use throughout our project without having to think about which
joins might be involved.

Again, this might not seem like a big improvement at first, but as we write many
more queries and build up our definitions of joins and properties, it will save us
a lot of typing and thinking.

@section{Task 2}
@bossquote{Show me a list of Products with Subcategory and Category names.}

Let's start by looking at the Product table
@(repl-query
  (show-table
   (from prd Product
         (limit 5))))

OK, it looks like there is a join from Product to ProductSubcategory using the
ProductSubcategoryID column. But at least some of the records have a null value.
So this is a @(racket #:has-one) relationship, but we will define it as a left join.
Add the following hilighted code to your file:
@(racketblock
  (define-schema adventure-works-schema
    #,truncated
    (table Product
           #,truncated
           (code:hilite #:has-one)
           (code:hilite [ProductSubcategory
                         (join subcat ProductSubcategory
                               'left-join
                               (join-on (.= (ProductSubcategoryID subcat)
                                            (ProductSubcategoryID this))))]))
    #,truncated))

In English, this says that a Product has one optional ProductSubcategory.
In Racket, it contributes to the definition of ProductSubcategory by generating
something like the following code:
@(racketblock
  (code:comment #,illustrative)
  (define ((code:hilite ProductSubcategory) . args)
    (match args
      [(list prd)
       #:when (Product? prd)
       (let ([this prd])
         (code:hilite (join subcat ProductSubcategory
                            'left-join
                            (join-on (.= (ProductSubcategoryID subcat)
                                         (ProductSubcategoryID this))))))]
      #,(italic "maybe-more-match-clauses ..."))))

Now we can write our first revision of this task.
@(racketblock
  (define (task2/revision1)
    (from prd Product
          (join subcat (ProductSubcategory prd))
          (select (Name prd) #:as 'ProductName)
          (select (ProductNumber prd))
          (select (Name subcat) #:as 'Subcategory)
          (select (CategoryName subcat) #:as 'Category))))
@(load-checkpoint! "5.rkt")

Let's try it out
@(repl-query
  (show-table
   (from prd (task2/revision1)
         (limit 5))))

The first 5 rows still have nulls.
This is correct -- their ProductCategoryID is null so the join fails,
and the null propogates to any values based on that join.
Just for demonstration purposes, I will show some records that won't have nulls:
@(repl-query
  (show-table
   (from prd (task2/revision1)
         (where (.is-not (ProductSubcategoryID prd)
                         'null))
         (limit 5))))

Perhaps your boss will ask for an @(racket order-by) clause later, but
@(racket task2/revision1) is an acceptable solution.

@subsection{Adding Joins and Properties}
Again, we have discovered some facts about our database schema and we should
encode these facts as procedures using @(racket define-schema).
Specifically, we learned that
@itemlist[@item{Product has one ProductSubcategory}
          @item{Product has one ProductCategory}
          @item{Product has a property SubcategoryName}
          @item{Product has a property CategoryName}]

We already defined @(racket ProductSubcategory) in the previous section,
so let's do the other 3 items now. Add the highlighted code to your file:
@(racketblock
  (define-schema adventure-works-schema
    #,truncated
    (table Product
           #,truncated
           (code:hilite #:has-one)
           (code:hilite [ProductCategory
                         (ProductCategory (ProductSubcategory this))])
           (code:hilite #:property)
           (code:hilite [SubcategoryName
                         (Name (ProductSubcategory this))])
           (code:hilite [CategoryName
                         (Name (ProductCategory this))]))
    #,truncated))
@(load-checkpoint! "6.rkt")

The properties should be self-explanatory.
But this definition of @(racket ProductCategory) is interesting.
We define @(racket ProductCategory) @tech{given} @(racket Product) as
@(racketblock
  (ProductCategory (ProductSubcategory this)))

This works because we previously defined @(racket ProductSubcategory) @tech{given}
@(racket Product) as well as @(racket ProductCategory) @tech{given} @(racket ProductSubcategory).
We simply compose them, passing the result of the first into the second.
The return value is the join from Product to ProductSubcategory to ProductCategory.
We can verify that this join works with a quick one-off query:
@margin-note{The careful reader will notice that both joins in the generated SQL
 are left joins. TODO write up how @(racket 'infer-join-type) works and link to it?
 Or is that just a distraction at this point?}
@(repl-query
  (show-table
   (from p Product
         (join cat (ProductCategory p))
         (select (Name p) #:as 'ProductName)
         (select (Name cat) #:as 'CategoryName)
         (limit 3))))

Getting back on task, we can now rewrite our answer as:
@(racketblock
  (define (task2/revision2)
    (from prd Product
          (select (Name prd) #:as 'ProductName)
          (select (ProductNumber prd))
          (select (SubcategoryName prd) #:as 'Subcategory)
          (select (CategoryName prd) #:as 'Category))))

And let's just make sure it still works:
@(repl-query
  (show-table
   (from prd (task2/revision2)
         (where (.is-not (ProductSubcategoryID prd)
                         'null))
         (limit 5))))

OK, we still have the same query but we have beefed up our @(racket define-schema)
with a new join and two new properties that will come in handy in future queries.

@subsection{A new kind of Property}
Now imagine that the boss only wants to see Products that have non-zero sales.
He explains that our Product catalog needs culling, but for now "has sales?" is
the easy way to filter out obsolete Products.
You get the impression that "has sales?" is an important property of Product,
and will very likely be a part of future tasks.
You create a purely synthetic property HasSales? so that you can write
@(racketblock
  (from prd Product
        (select (Name prd))
        (select (ProductNumber prd))
        (select (SubcategoryName prd))
        (select (CategoryName prd))
        (where (HasSales? prd))))

Explain the value of insulation here.
Today it is a subquery, but tomorrow it may be denormalized (like SalesOrderHeader.SubTotal).