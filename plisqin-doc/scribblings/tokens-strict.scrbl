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
     Adds a grouping key to the result set.
     See example on @(racket having) or read the @(secref "Aggregates")
     section for more complete details.
     }]
   [(having)
    @nested{
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
   [(date+)
    @nested{
     Adds each interval to the given datetime.
     The following example shows that the order of the intervals matters:
     @(repl-query
       (aw:show-table
        (from x (%%subquery "select 1 as one")
              (define feb-01
                (>> (%%sql "'2019-02-01'") #:cast Datetime?))
              (select (date+ feb-01 (months 1) (days 29)))
              (select (date+ feb-01 (days 29) (months 1))))))

     For this reason, Plisqin provides no way to add intervals to each other.
     One possible implementation would preserve the sequence of intervals;
     another would eagerly combine them into one interval.

     Note also that intervals may be dynamic, as in the following query:
     @(repl-query
       (aw:show-table
        (from pc ProductCategory
              (select (ProductCategoryID pc))
              (select (ModifiedDate pc))
              (select (>> (date+ (ModifiedDate pc)
                                 (years (ProductCategoryID pc)))
                          #:as 'AddSomeYears)))))
     }]
   [(date-)
    @nested{
     Equivalent to calling @(racket date+) with each interval negated.
     }]
   [(years months days hours minutes seconds)
    @nested{
     Creates an @(racket interval?). See example on @(racket date+).
     }]
   [(and)
    @nested{
     Boolean "and" -- Produces true when all arguments are true.
     Usually prefixed @(racket .and).}]
   [(or)
    @nested{
     Boolean "or" -- Produces true when any argument is true.
     Usually prefixed @(racket .or).}]
   [(not)
    @nested{
     Boolean "not" -- Produces true when the argument is false.
     Usually prefixed @(racket .not).}]
   [(=)
    @nested{
     "Equals" -- Produces true when its arguments are equal.
     Usually prefixed @(racket .=).}]
   [(<>)
    @nested{
     "Does not Equal" -- Produces true when its arguments are not equal.
     Usually prefixed @(racket .<>).}]
   [(<)
    @nested{
     "Less Than" -- Produces true when the first argument is less than the second.
     Usually prefixed @(racket .<).}]
   [(<=)
    @nested{
     "Less Than or Equal To" --
     Produces true when the first argument is less than or equal to the second.
     Usually prefixed @(racket .<=).}]
   [(>)
    @nested{
     "Greater Than" -- Produces true when the first argument is greater than the second.
     Usually prefixed @(racket .>).}]
   [(>=)
    @nested{
     "Greater Than or Equal To" --
     Produces true when the first argument is greater than or equal to the second.
     Usually prefixed @(racket .>=).}]
   [(like)
    @nested{
     Usually prefixed @(racket .like).
     Returns true if the first argument matches the second.
     The use of @(racket %) to match zero or more characters is guaranteed
     to be supported.
     Other matching patterns may exist; see "like" in your database's
     documentation for more details.
     @(repl-query
       (aw:show-table
        (from p Product
              (where (.like (ProductNumber p)
                            (val "CS-%")))
              (select (ProductName p))
              (select (ProductNumber p)))))
     }]
   [(not-like)
    @nested{
     Inverse of @(racket .like).
     Returns true if the first argument does not match the second.
     Usually prefixed @(racket .not-like).}]
   [(is)
    @nested{
     An equality test in which dbnull is considered equal to dbnull.
     The value @(racket 'null) can be used as a constant representing dbnull.
     Assuming that @(racket foo) and @(racket bar) are both @(racket Scalar?)s,
     the truth table is:
     @(itemlist
       @item{@(racket (.is 'null 'null)) -- always true}
       @item{@(racket (.is foo 'null)) -- true when foo is dbnull}
       @item{@(racket (.is 'null bar)) -- true when bar is dbnull}
       @item{@(racket (.is foo bar)) -- true when foo equals bar, or when
      both foo and bar are dbnull})

     Unlike other comparisons, @(racket .is) ignores any @tech{fallbacks}
     because the comparison behavior of dbnull is already completely specified.
     @(repl-query
       (aw:show-table
        (from p Product
              (limit 3)
              (select (ProductName p))
              (select (ProductNumber p))
              (select (Color p))
              (where (.is (Color p) 'null)))))
     @(repl-query
       (aw:show-table
        (from p Product
              (limit 3)
              (select (ProductName p))
              (select (ProductNumber p))
              (select (Color p))
              (where (.is (Color p) (val "Silver"))))))
     }]
   [(is-not)
    @nested{
     The expression @(racket (.is-not a b)) is always equivalent to
     @(racket (.not (.is a b))).}]
   [(+)
    @nested{
     Numeric addition.
     Usually prefixed @(racket .+).}]
   [(-)
    @nested{
     Numeric subtraction.
     Usually prefixed @(racket .-).}]
   [(*)
    @nested{
     Numeric multiplication.
     Usually prefixed @(racket .*)}]
   [(/)
    @nested{
     Numeric division.
     Usually prefixed @(racket ./)
     @(repl-query
       (aw:show-table
        (from pc ProductCategory
              (define pcid
                (ProductCategoryID pc))
              (select pcid)
              (select (.+ pcid (val 1)))
              (select (.- pcid (val 1)))
              (select (.* pcid (val 10)))
              (select (./ pcid (val 2))))))
     }]
   [else "~~ TODO ~~"])
