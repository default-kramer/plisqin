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
     When given an @(racket instance?), counts how many rows are in each group.
     An example of this can be seen on @(racket avg).

     When given a @(racket Scalar?), counts how many non-dbnull values
     are in each group.
     When the scalar is preceded by @(racket 'distinct), counts how many
     unique non-dbnull values are in each group.

     @margin-note{
      In this example, @(racket (count (ProductID p))) is equivalent to
      @(racket (count p)) because the ProductID is non-nullable.}
     @(repl-query
       (aw:show-table
        (from p Product
              (select (>> (count p)
                          #:as 'Num_Products))
              (select (>> (count (ProductID p))
                          #:as 'Num_Product_IDs))
              (select (>> (count (ProductSubcategoryID p))
                          #:as 'Num_Products_With_Subcategories))
              (select (>> (count 'distinct (ProductSubcategoryID p))
                          #:as 'Num_Unique_Subcategories)))))
     }]
   [(avg)
    @nested{
     Produces the average of the given expression, relative to each group.
     The following example uses @tech{traditional aggregation}, but every
     aggregate in Plisqin also works with @tech{grouped join aggregation}.
     Read @(secref "Aggregates") for complete understanding.
     @(repl-query
       (aw:show-table
        (from p Product
              (group-by (Color p))
              (select (Color p))
              (select (count p))
              (select (avg (ListPrice p)))
              (select (min (ListPrice p)))
              (select (max (ListPrice p)))
              (select (sum (ListPrice p))))))
     }]
   [(min)
    @nested{
     Like @(racket avg), but produces the minimum value.
     See example on @(racket avg).
     }]
   [(max)
    @nested{
     Like @(racket avg), but produces the maximum value.
     See example on @(racket avg).
     }]
   [(sum)
    @nested{
     Like @(racket avg), but produces the sum.
     See example on @(racket avg).
     }]
   [(exists)
    @nested{
     Produces true when the given argument has any rows, false otherwise.
     The following where clause filters out Products which do not have any
     corresponding rows in the SalesOrderDetail table.
     @(repl-query
       (aw:show-table
        (from prd Product
              (limit 5)
              (select (ProductName prd))
              (select (ProductNumber prd))
              (where (exists (from dtl SalesOrderDetail
                                   (where (.= (ProductID dtl)
                                              (ProductID prd)))))))))
     }]
   [(subquery)
    @nested{
     Represents a subquery.

     This @tech{strict} variant is really only useful for blocking the
     appending behavior of @(racket from).
     That is, if @(racket q) is a @(racket query?), then
     @(racket (from x q ....)) will append more clauses to @(racket q).
     If you want to treat @(racket q) as a subquery instead, you need to wrap
     it: @(racket (from x (subquery q) ....))

     The @tech{unsafe} variant, @(racket %%subquery), is probably more useful.
     A common pattern is to create a subquery with exactly one row, as follows:
     @(repl-query
       (aw:show-table
        (from x (%%subquery "select 1 as one")
              (select (val "hello"))
              (select (val "world")))))
     }]
   [(coalesce)
    @nested{
     Produces the first non-dbnull value from the given arguments.
     If the @tech{nullability} of any argument is @(racket no), then the
     nullability of the returned token is also @(racket no).

     The following example uses @(racket coalesce) to define @(racket maxDiscount)
     as "the maximum DiscountPct from the group of Special Offers, or zero when
     there are no Special Offers (and the maximum is dbnull)."
     @(repl-query
       (aw:show-table
        (from p Product
              (%%where (ProductID p)" in (514, 680, 707, 725)")
              (select (ProductName p))
              (select (ListPrice p))
              (join offersG (SpecialOffersG p))
              (define maxDiscount
                (coalesce (max (DiscountPct offersG))
                          (val 0)))
              (select (>> maxDiscount #:as 'BestDiscount))
              (select (>> (.* (ListPrice p)
                              (.- (val 1) maxDiscount))
                          #:as 'BestPrice)))))

     If you do not use @(racket coalesce), dbnull will propogate through the
     expressions built using @(racket maxDiscount):
     @(repl-query
       (aw:show-table
        (from p Product
              (%%where (ProductID p)" in (514, 680, 707, 725)")
              (select (ProductName p))
              (select (ListPrice p))
              (join offersG (SpecialOffersG p))
              (define maxDiscount
                (max (DiscountPct offersG)))
              (select (>> maxDiscount #:as 'BestDiscount))
              (select (>> (.* (ListPrice p)
                              (.- (val 1) maxDiscount))
                          #:as 'BestPrice)))))
     }]
   [(round)
    @nested{
     The first argument is a @(racket Number?) to be rounded.
     The second argument specifies how many decimal digits should be retained;
     this defaults to zero.
     @(repl-query
       (aw:show-table
        (from x (%%subquery "select 1 as one")
              (select (round (val 12.3456)))
              (select (round (val 12.3456) 2)))))
     }]
   [(date+)
    @nested{
     Adds each interval to the given datetime.
     The following example shows that the order of the intervals matters:
     @(repl-query
       (aw:show-table
        (from x (%%subquery "select 1 as one")
              (define feb-01
                (val "2019-02-01" Datetime?))
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
   [else
    (error "get-strict-content has no answer for:" 'id)])
