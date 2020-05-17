#lang racket

; Perhaps this is the "for language/library designers" section.
; We rebuild the Teaser from the define-schema tutorial using the smallest
; subset of Plisqin that we can.
; The goal should be to demonstrate 1) joins are values, 2) aggregates are
; self-contained, and 3) queries are appendable.

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

;(displayln (to-sql the-query))

(aw:show-table
 (from x the-query
       (limit 3)))
