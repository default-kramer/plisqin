#lang scribble/manual
@(require (for-label "standard-label.rkt"))

This is to be a log of things that surprised me while implementing and
then using null checking.

@section{Every "Instance" Might Be Null}
I had something in @(racket define-schema) like this:
@(racketblock
  (table ProductSubcategory
         ...
         #:has-one
         [ProductCategory
          (join cat ProductCategory
                (join-on (.= (ProductCategoryID cat)
                             (code:comment "`this` might be a left join!")
                             (ProductCategoryID this))))]
         ...))

And I got an error that @(racket (ProductCategoryID this)) was nullable.
This surprised me at first because ProductCategoryID is not a nullable column.
And as long as @(racket this) is a tuple, everything works fine.
But if @(racket this) is a left join, the expression is nullable and @(racket .=)
raises a contract error.
So I simply added a fallback as follows:
@(racketblock
  (table ProductSubcategory
         ...
         #:has-one
         [ProductCategory
          (join cat ProductCategory
                (join-on (.= (ProductCategoryID cat)
                             (?? (ProductCategoryID this) /void))))]
         ...))

Let's assume the following, even thought I haven't exposed it yet:
@(racketblock
  (define (instance-of x)
    (or/c (tuple-of x)
          (join-of x))))

The root of the surprise is that in SQL, an "instance" can only be a tuple.
But in Plisqin, an "instance of ProductCategory" can be a tuple or a join.
Therefore, "instances" in SQL are never null, but in Plisqin every instance might
be null because it could be a left join.
Perhaps null-checking will be even more important than I thought.
Or perhaps null-checking will just be obnoxious.
It remains to be seen...

TODO think about how this interacts (or doesn't) with join type inference.
The inference logic searches the join-on clauses for other joins and if it finds
any left join, the join type is inferred to be left.
This seems to work well enough, but I feel like there might be surprises hiding here.
The real problem is that bulletproof join type inference would need to know a lot more
about the schema, including which sets of columns are unique keys.

@section{Left Join Assistance}
This was a pleasant surprise, I think.
I had the following query
@(racketblock
  (from prd Product
        (limit 5)
        (join subcat ProductSubcategory
              (join-type 'left)
              (join-on (.= (ProductSubcategoryID subcat)
                           (ProductSubcategoryID prd))))
        (select (Name prd))
        (select (ProductNumber prd))
        (select (SubcategoryName subcat))))

And once I added nullchecking, I got an error that the 2nd argument to @(racket .=) was nullable.
This is true, Product.ProductSubcategoryID is indeed a nullable column.
So I needed to add a fallback (see below).
I already had @(racket (join-type 'left)) but if I hadn't, this would be a big clue
that I will be eliminating rows from the result set where @(racket (ProductSubcategoryID prd)) is null.
This might make me realize that I actually wanted a left join after all.

@(racketblock
  (from prd Product
        (limit 5)
        (join subcat ProductSubcategory
              (join-type 'left)
              (join-on (.= (ProductSubcategoryID subcat)
                           (?? (ProductSubcategoryID prd) /void))))
        (select (Name prd))
        (select (ProductNumber prd))
        (select (SubcategoryName subcat))))

Here is another example.
I had to add the fallback seen in the following query
@(racketblock
  (from subcat ProductSubcategory
        (limit 5)
        (select (SubcategoryName subcat))
        (select (CategoryName subcat))
        (join detailsG SalesOrderDetail
              (join prd Product
                    (join-on (.= (ProductID prd)
                                 (ProductID detailsG))))
              (group-by (ProductSubcategoryID prd))
              (code:comment "using ?? here tells me that detailsG should be a left join")
              (join-on (.= (?? (ProductSubcategoryID prd) /void)
                           (ProductSubcategoryID subcat))))
        (select (round (sum (LineTotal detailsG)) 2))
        (select (sum (OrderQty detailsG)))
        (order-by 'desc (sum (LineTotal detailsG)))))

Which made me realize that detailsG is supposed to be a left join.
This is true, but may be a bad example because almost every grouped join should
be a left join regardless.
(TODO maybe infer join type should choose 'left if there are any group by clauses?)
If you pretend it's not a grouped join, the insight is still valid.
SalesOrderDetail does not have a non-null path to ProductSubcategory.
We can go from SalesOrderDetail to Product just fine, but then from Product to
ProductSubcategory we encounter the fact that Product.ProductSubcategoryID is null again.

TODO is this going to make the recipes even more mechanical?!?
That would be awesome! I think I can say something like: @italic{
 If you are not sure, just assume the join you are writing will always find a match.
 If you get a null check error, you were wrong, no big deal -- just add the fallback and
 consider making it a left join.}
Hmm, not quite, because that is only true if you already know that a foreign key exists.
And this only works when you know you are working with a tuple (or inner join), otherwise
you will need a fallback simply because your instance might be a left join and not because
of anything inherent to the schema.
