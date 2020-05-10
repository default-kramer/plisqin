#lang at-exp racket

@(begin
   (provide (for-syntax get-strict-content))
   (require (for-label "standard-label.rkt")
            scribble/manual
            "tokens-helpers.rkt"
            "helpers.rkt"
            ))

@(def-content-provider (get-strict-content id)
   [(select)
    @nested{
     If a query or join has no @(racket select) clauses, then all the columns
     of its queryable are returned.
     Otherwise, each @(racket select) clause adds a column to the result set
     @(repl-query
       (aw:show-table
        (from pc ProductCategory
              (select (ProductCategoryID pc))
              (select (Name pc))
              (select (val "hello world")))))
     }]
   [(where)
    @nested{
     Eliminates rows from the result set that do not satisfy the given condition.
     @(repl-query
       (aw:show-table
        (from pc ProductCategory
              (where (.= (Name pc)
                         (val "Bikes"))))))
     }]
   [(group-by)
    @nested{
     TODO link to a "groups and aggregates" section, to be written.

     Adds a grouping key to the result set.
     See example on @(racket having).
     }]
   [(having)
    @nested{
     TODO link to a "groups and aggregates" section, to be written.

     Can only be used when @(racket group-by) clauses are also present.
     Eliminates rows from the result set that do not satisfy the given condition.
     @(repl-query
       (aw:show-table
        (from subcat ProductSubcategory
              (group-by (ProductCategoryID subcat))
              (having (.> (count subcat)
                          (val 10)))
              (select (ProductCategoryID subcat))
              (select (>> (count subcat) #:as 'NumSubcategories)))))

     Note that @(racket where) is applied to individual rows, while
     @(racket having) is applied to groups.
     You can imagine that your database engine first applies any @(racket where)
     clauses, then applies any @(racket group-by) clauses, then applies any
     @(racket having) clauses.
     }]
   [(order-by)
    @nested{
     Specifies how the result set should be ordered.
     If any previous @(racket order-by) clauses exist, this one will be used to
     break any ties.
     If the first argument is @(racket 'asc) or @(racket 'desc), it specifies
     ascending or descending; otherwise the default is ascending.
     @(repl-query
       (aw:show-table
        (from p Product
              (limit 5)
              (order-by 'desc (Color p))
              (order-by 'desc (ProductID p)))))
     }]
   [(join-on)
    @nested{
     This clause can only be used inside @(racket join); not @(racket from).
     Specifies how rows from this join should be matched to the parent query.
     @(repl-query
       (aw:show-table
        (from subcat ProductSubcategory
              (join cat ProductCategory
                    (join-on (.= (ProductCategoryID cat)
                                 (ProductCategoryID subcat))))
              (%%select subcat".Name as SubcategoryName")
              (%%select cat".Name as CategoryName")
              (limit 5))))
     }]
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
                          #:as 'Num_Products))
              (select (>> (count (ProductSubcategoryID p))
                          #:as 'Num_Products_With_Subcategories))
              (select (>> (count 'distinct (ProductSubcategoryID p))
                          #:as 'Num_Unique_Subcategories)))))
     }]
   [else "~~ TODO ~~"])
