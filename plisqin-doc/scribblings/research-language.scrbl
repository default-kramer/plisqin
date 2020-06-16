#lang scribble/manual

@(begin
   (require (for-label "standard-label.rkt"))
   (require scribble/example)
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
