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
               #;(require (only-in plisqin-lib/private/lang/default-require
                                   show-table current-connection))
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

@subsection{Making the Join Reusable}
But we can do better.
We've just determined that every ProductCategory has exactly 1 ProductCategory.
This is a join that we will probably want to reuse often.
So let's modify our @(racket define-schema) form by adding the hilighted code:
@margin-note{The highlighted change can also be seen in this diff [TODO link it!]}
@(racketblock
  (define-schema adventure-works-schema
    ...
    (table ProductSubcategory
           ...
           (code:hilite #:has-one)
           (code:hilite [ProductCategory
                         (join cat ProductCategory
                               (join-on (.= (ProductCategoryID cat)
                                            (ProductCategoryID this))))]))
    ...))
@(load-checkpoint! "3.rkt")

In English, this says that a ProductSubcategory has one ProductCategory.
In more Rackety terms, this says that @itemlist[
 @item{the procedure @(racket ProductCategory)}
 @item{when given a single argument (bound to @(racket this))}
 @item{such that @(racket (ProductSubcategory? this)) is truthy}
 @item{will return the highlighted @(racket join) expression.}]

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
In Plisqin, joins are values, meaning they can appear almost anywhere inside a query.
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
    ...
    (table ProductSubcategory
           ...
           (code:hilite #:property)
           (code:hilite [CategoryName
                         (Name (ProductCategory this))]))
    ...))
@(load-checkpoint! "4.rkt")

In English, this says that CategoryName is a property of ProductSubcategory.
In more Rackety terms, this says that @itemlist[
 @item{the procedure @(racket CategoryName)}
 @item{when given a single argument (bound to @(racket this))}
 @item{such that @(racket (ProductSubcategory? this)) is truthy}
 @item{will return @(racket (Name (ProductCategory this))).}]

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

@section{A Closer Look}
TODO now seems to be the time to explain what define-schema is really doing.
Maybe enhance @(racket CategoryName) such that the resulting definition is equivalent to
@(racketblock
  (define (CategoryName this)
    (cond
      [(ProductCategory? this)
       (Name this)]
      [(ProductSubcategory? this)
       (Name (ProductCategory this))]
      [else
       (error "CategoryName is undefined for" this)])))
