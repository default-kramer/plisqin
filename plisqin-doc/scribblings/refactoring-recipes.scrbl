#lang scribble/manual

@(require (for-label plisqin))

@(define stuff (italic (racket stuff ...)))

@title{Refactoring Recipes}

@section[#:tag "rr:schema:join->proc"]{Singular Join -> Procedure}
This recipe extracts a join into a procedure,
assuming that you are using @(racket define-schema).
@(racketblock
  (code:comment "original version:")
  (from x TableX
        #,stuff
        (code:hilite (join y TableY
                           (join-on (.= (Foo y)
                                        (Foo x)))))
        #,stuff)
  (code:comment "refactored version:")
  (from x TableX
        #,stuff
        (code:hilite (join y (TableY x)))
        #,stuff))

Our goal is that @(racket (TableY x)) in the refactored version
returns the highlighted join from the original version.
That is, we are trying to define @(racket TableY) @tech{given} @(racket TableX).
Copy the join from the original version into your schema definition
as in the following example:
@(racketblock
  (define-schema schema-name
    #,stuff
    (table TableX
           #,stuff
           (code:hilite #:has-one)
           (code:hilite [TableY
                         (join y TableY
                               (join-on (.= (Foo y)
                                            (Foo x))))])
           #,stuff)
    #,stuff))

This code should produce an error about @(racket x) not being defined.
Using TODO we replace @(racket x) with @(racket this) as follows:
@(racketblock
  (define-schema schema-name
    #,stuff
    (table TableX
           #,stuff
           #:has-one
           [TableY
            (join y TableY
                  (join-on (.= (Foo y)
                               (Foo (code:hilite this)))))]
           #,stuff)
    #,stuff))

Now the code should work.
Try @(racket (TableY TableX)) on your REPL and make sure you do not get an error.
Then try out the refactored version and make sure it is equivalent to the original version.
@(racketblock
  (from x TableX
        #,stuff
        (code:hilite (join y (TableY x)))
        #,stuff))

@subsection{Left Joins}
TODO explain why you probably want to make sure that nothing in
@(racket define-schema) can eliminate rows from the result set.

@subsection{Alternate Name Modification}
Most of the time, the procedure will have the same name as the table being joined.
Notice the two occurrences of @(racket TableY) in the following example:
@(racketblock
  (define-schema schema-name
    #,stuff
    (table TableX
           #,stuff
           #:has-one
           [(code:hilite TableY)
            (join y (code:hilite TableY)
                  (join-on (.= (Foo y)
                               (Foo this))))]
           #,stuff)
    #,stuff))

But this does not have to be the case.
We can make the procedure name whatever we want, as in the following example:
@(racketblock
  (define-schema schema-name
    #,stuff
    (table TableX
           #,stuff
           #:has-one
           [(code:hilite Whyever)
            (join y TableY
                  (join-on (.= (Foo y)
                               (Foo this))))]
           #,stuff)
    #,stuff))

The above example defines @(racket (Whyever TableX)).
But that is obviously not a realistic example.
So when might you want to use a different name?
@(itemlist
  @item{If using the table name would be confusing or ambiguous.
 For example, if I see @(racket (Company invoice)), I am not sure what it means.
 But if I see @(racket (BuyingCompany invoice)) or @(racket (SellingCompany invoice)),
 I am more confident that I understand the code and the schema.}
  @item{If your table names are plural, you should probably still use singular names
 for singular joins. For example, @(racket (Category product)) could return a join
 of the @(racket Categories) table.})
