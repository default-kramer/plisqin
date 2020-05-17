#lang scribble/manual

@(begin
   (require (for-label "standard-label.rkt"))
   (define-syntax-rule (recipe stuff ...)
     (subsection stuff ...))
   )

@(define stuff (italic (racket stuff ...)))

@title{Refactoring Recipes}

@section{Elemental Recipes}
@recipe[#:tag "join->ex"]{Join -> Expression}
This recipe simply rewrites a join into a self-contained expression that can
be relocated in future refactorings.
@(racketblock
  (code:comment "original version:")
  (from x TableX
        ....
        (join y TableY
              ....)
        ....)
  (code:comment "refactored version:")
  (from x TableX
        ....
        (define y
          (join y TableY #:to x
                ....))
        ....))

The refactored code is bigger! Is this a step in the wrong direction?
If we stop refactoring here, you could argue that it is.
The point of this recipe is to set up future refactoring.
In the original version, there was an unwritten @(racket #:to x) that would
be lost if we relocated the code.
By making @(racket #:to x) explicit, we can now relocate the code using normal
Racket techniques.

@recipe[#:tag "ex->proc"]{Expression -> Procedure}
In this recipe, we extract any expression into a procedure.
Unless you are brand new to Racket, you have done this before.
@(racketblock
  (code:comment "original version:")
  (from p 'Player
        (select (ShotsTaken p))
        (select (ShotsMade p))
        (select (>> (code:hilite (./ (ShotsMade p)
                                     (ShotsTaken p)))
                    #:as 'ShootingPercentage))
        (order-by (code:hilite (./ (ShotsMade p)
                                   (ShotsTaken p)))))
  (code:comment "refactored version:")
  (define (ShootingPercentage p)
    (./ (ShotsMade p)
        (ShotsTaken p)))
  (from p 'Player
        (select (ShotsTaken p))
        (select (ShotsMade p))
        (select (>> (code:hilite (ShootingPercentage p))
                    #:as 'ShootingPercentage))
        (order-by (code:hilite (ShootingPercentage p)))))

Assuming that @(racket (ShotsTaken p)) and @(racket (ShotsMade p)) both
return @(racket Number?)s, we could add the following contract:
@(racketblock
  (define/contract (ShootingPercentage p)
    (-> (instanceof 'Player) Number?)
    (./ (ShotsMade p)
        (ShotsTaken p))))

Here is one more example. This time the expression is a join.
Remember that you might have to use the @(secref "join->ex") recipe first if
your join does not have the @(racket #:to) argument specified.
@(racketblock
  (code:comment "original version:")
  (from player 'Player
        ....
        (define team
          (join team 'Team #:to player
                (join-on (.= (TeamID team)
                             (TeamID player)))))
        ....)
  (code:comment "refactored version:")
  (define/contract (Team-of-Player p)
    (-> (instanceof 'Player) (instanceof 'Team))
    (join team 'Team #:to player
          (join-on (.= (TeamID team)
                       (TeamID p)))))
  (from player 'Player
        ....
        (define t (Team-of-Player player))
        ....))

@recipe[#:tag "proc->schema"]{Procedure -> define-schema}
If you are using @(racket define-schema) and you have a procedure like this
@(racketblock
  (define/contract (NAME-ME x)
    (-> (instanceof TableX) any/c)
    (any-expression x
                    ....)))

It is a candidate for being moved into @(racket define-schema) assuming that:
@(itemlist
  @item{It accepts one argument. (The argument is @(racket x) in this example.)}
  @item{That argument is an @(racket instanceof) a table from your schema definition.
 (The table is @(racket TableX) in this example.)})

First we need to decide which keyword is appropriate based on what this
procedure returns. Choose from:
@(itemlist
  @item{@(racket #:property) if the return value is a @(racket Scalar?)}
  @item{@(racket #:has-group) if the return value is a singular grouped join.
 "Singular" means that adding the join to a query of TableX will not increase
 the number of rows that will be returned in the result set.
 "Grouped" means that the join contains at least one @(racket group-by) clause.}
  @item{@(racket #:has-one) if the return value is a singular simple join.
 "Singular" means that adding the join to a query of TableX will not increase
 the number of rows that will be returned in the result set.
 "Simple" means that all of the join's clauses are @(racket join-on) clauses.})

If you were unable to choose a keyword, then @(racket NAME-ME) probably does not
belong inside @(racket define-schema), but you can keep it as a separate procedure.
Let's just pretend that @(racket (any-expression x ....)) is a @(racket Scalar?),
so we choose the @(racket #:property) keyword.
We add that expression into @(racket define-schema) as follows.
Note that we replace @(racket x), the single argument, with @(racket this).
@(racketblock
  (define-schema my-schema
    ....
    (table TableX
           ....
           #:property
           [NAME-ME
            (any-expression this
                            ....)]
           ....)
    ....))

Finally, if the expression was actually a join, you will have something like
the following code. You can omit the @(racket #:to this) if you want, because
@(racket define-schema) will automatically add it for you.
@(racketblock
  [NAME-ME
   (join y TableY #:to this
         clauses ....)])

@subsubsection{Left Joins}
TODO explain why you probably want to make sure that nothing in
@(racket define-schema) can eliminate rows from the result set.

@recipe[#:tag "join<->define"]{Join <-> Define}
This recipe allows you to convert a join to a definition and back.
This only works if @(racket (TableY x)) returns a @(racket join?):
@(racketblock
  (from x TableX
        ....
        (join y (TableY x))
        ....)
  (code:comment "is almost equivalent to")
  (from x TableX
        ....
        (define y (TableY x))
        ....))

The preceding examples are "almost equivalent" because there is a subtle
case in which they are not equivalent.
@(itemlist
  @item{When @(racket y) is @(racket join)ed, the join is immediately added to
 the query and is guaranteed to appear in the generated SQL.}
  @item{When @(racket y) is @(racket define)d, the join is not immediately added
 to the query. If @(racket y) is used as content in some clauses that follow,
 it will be added to the query at that time and both versions become equivalent.
 But if @(racket y) is an unused definition, it essentially does not exist
 and both versions are not equivalent.})

@section{Compound Recipes}
These recipes use one or more of the Elemental Recipes.

@recipe[#:tag "join1->schema"]{Singular Join -> Schema Definition}
This recipe moves a singular join into @(racket define-schema).
If we start with the following code
@(racketblock
  (from x TableX
        ....
        (code:hilite (join y TableY
                           ....))
        ....))

First we use the @(secref "join->ex") recipe to produce code like this:
@(racketblock
  (from x TableX
        ....
        (code:hilite [define y
                      (join y TableY #:to x
                            ....)])
        ....))

Next we use the @(secref "ex->proc") recipe to produce code like this:
@(racketblock
  (define/contract (NAME-ME x)
    (-> (instanceof TableX) (instanceof TableY))
    (join y TableY #:to x
          ....))
  (from x TableX
        ....
        (code:hilite (define y (NAME-ME x)))
        ....))

Next we use the @(secref "proc->schema") recipe to move @(racket NAME-ME)
into our schema definition. We also immediately rename it.
Note that @(racket GetTableY) is usually named @(racket TableY); the names
differ in this example for educational reasons only:
@(racketblock
  (define-schema
    ....
    (table TableX
           ....
           #:has-one
           [GetTableY
            (join y TableY
                  ....)]
           ....)
    ....)
  (from x TableX
        ....
        (code:hilite (define y (GetTableY x)))
        ....))

Finally we use the @(secref "join<->define") recipe to make sure we are
equivalent to our starting position:
@(racketblock
  (from x TableX
        ....
        (code:hilite (join y (GetTableY x)))
        ....))

@recipe[#:tag "joinG->schema"]{Grouped Join -> Schema Definition}
This recipe moves a grouped join into @(racket define-schema).
If we start with the following code
@(racketblock
  (from x TableX
        ....
        (code:hilite (join y TableY
                           ....))
        ....))

First we use the @(secref "join->ex") recipe to produce code like this:
@(racketblock
  (from x TableX
        ....
        (code:hilite [define y
                      (join y TableY #:to x
                            ....)])
        ....))

Next we use the @(secref "ex->proc") recipe to produce code like this:
@(racketblock
  (define/contract (NAME-ME x)
    (-> (instanceof TableX) (instanceof TableY))
    (join y TableY #:to x
          ....))
  (from x TableX
        ....
        (code:hilite (define y (NAME-ME x)))
        ....))

Next we use the @(secref "proc->schema") recipe to move @(racket NAME-ME)
into our schema definition. We also immediately rename it.
My personal convention is that the name of a grouped join ends with "G".
@(racketblock
  (define-schema
    ....
    (table TableX
           ....
           #:has-group
           [YTablesG
            (join y TableY
                  ....)]
           ....)
    ....)
  (from x TableX
        ....
        (code:hilite (define y (YTablesG x)))
        ....))

Finally we use the @(secref "join<->define") recipe to make sure we are
equivalent to our starting position:
@(racketblock
  (from x TableX
        ....
        (code:hilite (join y (YTablesG x)))
        ....))

@recipe[#:tag "scalar->schema"]{Scalar -> Schema Definition}
Start with the following code and assume that @(racket SomeScalar)
returns a @(racket Scalar?):
@(racketblock
  (from x TableX
        ....
        (select (code:hilite (SomeScalar .... x)))
        ....))

First use the @(secref "ex->proc") recipe as follows:
@(racketblock
  (define/contract (TEMP-NAME x)
    (-> (instanceof TableX) Scalar?)
    (SomeScalar .... x))
  (from x TableX
        ....
        (select (code:hilite (TEMP-NAME x)))
        ....))

Next use the @(secref "proc->schema") recipe to move @(racket TEMP-NAME)
into our schema definition. We also immediately rename it.
@(racketblock
  (define-schema
    ....
    (table TableX
           ....
           #:property
           [NewName
            (SomeScalar .... this)]
           ....)
    ....)
  (from x TableX
        ....
        (select (code:hilite (NewName x)))
        ....))

@recipe[#:tag "join->inline"]{Inline Join}
This recipe moves a join inline.
This is mostly used to set up further refactoring.
Starting with code like this:
@(racketblock
  (from x TableX
        ....
        (join y (TableY x))
        ....
        (some-expression y ....)
        ....))

We first use the @(secref "join<->define") recipe as follows:
@(racketblock
  (from x TableX
        ....
        (define y (TableY x))
        ....
        (some-expression y ....)
        ....))

Now we just use normal refactoring techniques to replace @(racket y) with its
definition as follows:
@(racketblock
  (from x TableX
        ....
        (some-expression (TableY x) ....)
        ....))

And this recipe is complete.
They key point is that if we proceed to use the @(secref "ex->proc") recipe
on @(racket (some-expression ....)), the resulting procedure will no longer
demand an @(racket (instanceof TableY)) and it will demand an
@(racket (instanceof TableX)).

@recipe[#:tag "ds:rename"]{Name Clarification}
In this recipe, we simply create a more descriptive name for a procedure.
This recipe assumes we are using @(racket define-schema).

Imagine we start with the following code:
@(racketblock
  (from p Product
        (select (code:hilite (Name p)))))

We want to create @(racket ProductName) as an alias for @(racket Name) when
the argument is an @(racket (instanceof Product)).
First we use the @(secref "ex->proc") recipe to get the following code:
@(racketblock
  (define/contract (NAME-ME p)
    (-> (instanceof Product) String?)
    (Name p))
  (from p Product
        (select (code:hilite (NAME-ME p)))))

Finally we use the @(secref "proc->schema") recipe to move @(racket NAME-ME) into
our schema definition. We also immediately rename it to @(racket ProductName).
@(racketblock
  (define-schema
    ....
    (table Product
           ....
           #:property
           [ProductName
            (Name this)]
           ....)
    ....)
  (from p Product
        (select (code:hilite (ProductName p)))))

And we are done.
Note that @(racket define-schema) automatically sets the @(racket #:as) name of
each @(racket #:property), as if you had written the following:
@(racketblock
  #:property
  [ProductName
   (>> (Name this)
       #:as 'ProductName)])

Be aware of this to avoid breaking any existing call sites that depend on the
original name appearing in the result set.
The examples in this documentation ignore this caveat because this recipe
is always used on a brand new query that has no call sites yet.
