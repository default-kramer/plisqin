#lang at-exp racket

@(begin
   (provide (for-syntax get-strict-content))
   (require (for-label "standard-label.rkt")
            scribble/manual
            "tokens-helpers.rkt"
            "helpers.rkt"
            ))

@(def-content-provider (get-strict-content id)
   [(count)
    @nested{
     When given an @(racket instance?), counts the number of rows in that instance.
     The following query shows how many Products are red:
     @(repl-query
       (aw:show-table
        (from p Product
              (where (.= (?? (Color p) /void)
                         (val "Red")))
              (select (count p)))))

     The given @(racket instance?) may be a grouped join.
     The following query shows how many SalesOrderDetail records exist
     for each Product:
     @(repl-query
       (aw:show-table
        (from p Product
              (join detailsG SalesOrderDetail
                    (group-by (ProductID detailsG))
                    (join-on (.= (ProductID detailsG)
                                 (ProductID p))))
              (select (ProductName p))
              (select (>> (count detailsG) #:as 'TimesSold))
              (order-by 'desc (count detailsG))
              (limit 5))))

     When given a @(racket Scalar?), counts how many non-null values are present.
     Adding the @(racket 'distinct) option counts how many unique
     non-null values are present.
     @margin-note{
      In this example, @(racket (count (ProductID p))) is equivalent to
      @(racket (count p)) because the ProductID is non-nullable.}
     @(repl-query
       (aw:show-table
        (from p Product
              (select (>> (count (ProductID p))
                          #:as 'NumProducts))
              (select (>> (count (ProductSubcategoryID p))
                          #:as 'NumProductsWithSubcategories))
              (select (>> (count 'distinct (ProductSubcategoryID p))
                          #:as 'NumUniqueSubcategories)))))
     }]
   [(select where)
    @nested{This is just a demonstration of how I could write custom content
     in a different file and hook it into the skeleton.

     This is the documentation for @(racket id).}]
   [else "~~ TODO ~~"])
