#lang scribble/manual

@(begin
   (require (for-label "standard-label.rkt")
            "helpers.rkt")
   (define-syntax-rule (recipe stuff ...)
     (subsection stuff ...))
   (def-green-ids repl-query
     [TEAM Team]
     [CURRENTTEAM CurrentTeam]
     [TEAMNAME TeamName]
     [PLAYERSG PlayersG]
     [SHOOTINGPERCENTAGE ShootingPercentage])
   )

@(define stuff (italic (racket stuff ...)))

@title{Refactoring Recipes}
This section is not meant to be read straight through,
unless you are very studious.

A green identifier like @(racket TEAM) indicates something that is being
added to @(racket define-schema).
A red highlight like @(racket (code:hilite (foo (bar x)))) is used to help you
follow a piece of code as it gets relocated, possibly with small adjustments.

@section{Elemental Recipes}
@recipe[#:tag "join->ex"]{Join -> Expression}
This recipe simply rewrites a join into a self-contained expression that can
be relocated in future refactorings.
@(racketblock
  (code:comment "original version:")
  (from x TableX
        ....
        (code:hilite (join y TableY
                           ....))
        ....)
  (code:comment "refactored version:")
  (from x TableX
        ....
        (define y
          (code:hilite (join y TableY #:to x
                             ....)))
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
  (from x TableX
        ....
        (select (code:hilite (foo (bar x)
                                  (baz x))))
        ....)
  (code:comment "refactored version:")
  (define (NEW-PROC x)
    (code:hilite (foo (bar x)
                      (baz x))))
  (from x TableX
        ....
        (select (NEW-PROC x))
        ....))

If we assume that @(racket foo) returns a @(racket Number?), you can add
a contract to @(racket NEW-PROC) as follows:
@(racketblock
  (define/contract (NEW-PROC x)
    (-> (instanceof TableX) Number?)
    (code:hilite (foo (bar x)
                      (baz x)))))

Here is one more example. This time the expression is a join.
Remember that you might have to use the @(secref "join->ex") recipe first if
your join does not have the @(racket #:to) argument specified.
@(racketblock
  (code:comment "original version:")
  (from x TableX
        ....
        (define y
          (code:hilite (join y TableY #:to x
                             (join-on (.= (YID y)
                                          (YID x))))))
        ....)
  (code:comment "refactored version:")
  (define/contract (Y-given-X x)
    (-> (instanceof TableX) (instanceof TableY))
    (code:hilite (join y TableY #:to x
                       (join-on (.= (YID y)
                                    (YID x))))))
  (from x TableX
        ....
        (define y (Y-given-X x))
        ....))

@recipe[#:tag "proc->schema"]{Procedure -> define-schema}
If you are using @(racket define-schema) and you have a procedure like this
@(racketblock
  (define/contract (NEW-PROC x)
    (-> (instanceof TableX) any/c)
    (code:hilite (foo (bar x)
                      (baz x)))))

It is a candidate for being moved into @(racket define-schema) assuming that:
@(itemlist
  @item{It accepts one argument. (The argument is @(racket x) in this example.)}
  @item{That argument is an @(racket instanceof) a table from your schema definition.
 (The table is @(racket TableX) in this example.)})

First we need to decide which keyword is appropriate based on what this
procedure returns. Choose from:
@(itemlist
  @item{@(racket #:property) if the return value is a @(racket Scalar?)
 (or one of its subtypes)}
  @item{@(racket #:has-group) if the return value is a singular grouped join.
 "Singular" means that adding the join to a query of TableX will not increase
 the number of rows that will be returned in the result set.
 "Grouped" means that the join contains at least one @(racket group-by) clause.}
  @item{@(racket #:has-one) if the return value is a singular simple join.
 "Singular" means that adding the join to a query of TableX will not increase
 the number of rows that will be returned in the result set.
 "Simple" means that each of the join's clauses is either a
 @(racket join-on) or @(racket join-type) clause.})

If you were unable to choose a keyword, then @(racket NEW-PROC) probably does not
belong inside @(racket define-schema), but you can keep it as a separate procedure.
Let's just pretend that @(racket (code:hilite (foo ....))) returns a
@(racket Scalar?), so we choose the @(racket #:property) keyword.
We add that expression into @(racket define-schema) and replace the single
argument (which was @(racket x)) with @(racket this) as follows:
@(racketblock
  (define-schema my-schema
    ....
    (table TableX
           ....
           #:property
           [NEW-PROC
            (code:hilite (foo (bar this)
                              (baz this)))]
           ....)
    ....))

Warning: this recipe is not complete!
Continue reading the following subsections.

@subsubsub*section{On Strict Comparisons}
If you have a @tech{strict} comparison involving @(racket this), you should
add a @tech{fallback} if one is not already present.
@(racketblock
  (code:comment "Notice that `this` is used in a strict comparison...")
  (.= (foo bar)
      (foo this))
  (code:comment "... and surround it with a fallback:")
  (.= (foo bar)
      (?? (foo this) /void)))

For the purposes of the @(secref "using-define-schema") walkthrough,
you can just always add the @(racket /void) fallback as seen above and move on.
Or if you are not satisfied with this hand-waving, you should first read
@(secref "Nullability") and then @(secref "this-is-nullable").

Alternatively, you don't have to add the fallback now.
If your code worked without a fallback prior to applying this recipe, it will
still work without a fallback after applying this recipe.
But future callers of this procedure might get an error.

@subsubsub*section{On Joins}
If the definition of @(racket NEW-PROC) returns a join, you will
have something like the following code.
You can omit the @(racket #:to this) if you want, because
@(racket define-schema) will automatically add it for you.
@(racketblock
  [NEW-PROC
   (code:hilite (join y TableY #:to this
                      clauses ....))])

@subsubsub*section{On Left Joins}
I recommend that every join you add to @(racket define-schema) should never
remove rows from the result set.
For example, perhaps a Player @(racket #:has-one) Team, but this relationship
is optional (that is, a Player might have no current Team).
In this case, the join should have @(racket (join-type 'left)) so that callers
who use this join do not accidentally filter out Players who have no current Team.
If a caller really wants to convert a @(racket 'left) join into an
@(racket 'inner) join, they can do so as follows:
@(racketblock
  (from p Player
        (code:comment "(Team p) returns a 'left join ...")
        (join t (Team p)
              (code:comment "... but we can override that here:")
              (join-type 'inner))
        ....))

Note that almost every @(racket #:has-group) relationship should be a left
join, because a group containing zero members is considered a failed join
and unless it is a left join, rows will be filtered from the result.

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

@bold{Caution:} In this example, the single-argument procedure @(racket TEAM)
happens to share its name with the existing table @(racket (code:hilite Team)).
This name-sharing is very common with singular joins, but not required.

@(racketblock
  (code:comment "current code:")
  (from p Player
        ....
        (code:hilite (join t Team
                           (join-on (.= (TeamID t)
                                        (TeamID p)))))
        ....)
  (code:comment "desired code:")
  (from p Player
        ....
        (join t (TEAM p))
        ....))

First we use the @(secref "join->ex") recipe as follows:
@(racketblock
  (from p Player
        ....
        (define t
          (code:hilite (join t Team #:to p
                             (join-on (.= (TeamID t)
                                          (TeamID p))))))
        ....))

Next we use the @(secref "ex->proc") recipe as follows:
@(racketblock
  (define/contract (NEW-PROC p)
    (-> (instanceof Player) (instanceof Team))
    (code:hilite (join t Team #:to p
                       (join-on (.= (TeamID t)
                                    (TeamID p))))))
  (from p Player
        ....
        (define t (NEW-PROC p))
        ....))

Next we use the @(secref "proc->schema") recipe to move @(racket NEW-PROC)
into our schema definition. We also immediately rename it to @(racket TEAM).
@(racketblock
  (define-schema
    ....
    (table Player
           ....
           #:has-one
           [TEAM
            (code:hilite (join t Team
                               (join-on (.= (TeamID t)
                                            (?? (TeamID this) /void)))))]
           ....)
    ....)
  (from p Player
        ....
        (define t (TEAM p))
        ....))

Finally we use the @(secref "join<->define") recipe to make sure we are
equivalent to our starting position:
@(racketblock
  (from p Player
        ....
        (join t (TEAM p))
        ....))

@subsubsub*section{Singular Join Naming}
As mentioned above, the single-argument procedure @(racket TEAM) shares its
name with the table @(racket (code:hilite Team)).
But this does not have to be the case.
You could, for example, name the procedure @(racket CURRENTTEAM) instead.
Then the refactored code would look like this:
@(racketblock
  (define-schema
    ....
    (table Player
           ....
           #:has-one
           [CURRENTTEAM
            (code:hilite(join t Team
                              (join-on (.= (TeamID t)
                                           (?? (TeamID this) /void)))))]
           ....)
    ....)
  (from p Player
        ....
        (join t (CURRENTTEAM p))
        ....))

@recipe[#:tag "joinG->schema"]{Grouped Join -> Schema Definition}
This recipe moves a grouped join into @(racket define-schema).
@(racketblock
  (code:comment "current code:")
  (from t Team
        ....
        (code:hilite (join playersG Player
                           (group-by (TeamID playersG))
                           (join-on (.= (TeamID playersG)
                                        (TeamID t)))))
        ....)
  (code:comment "desired code:")
  (from t Team
        ....
        (join playersG (PLAYERSG t))
        ....))

First we use the @(secref "join->ex") recipe to produce code like this:
@(racketblock
  (from t Team
        ....
        (define playersG
          (code:hilite (join playersG Player #:to t
                             (group-by (TeamID playersG))
                             (join-on (.= (TeamID playersG)
                                          (TeamID t))))))
        ....))

Next we use the @(secref "ex->proc") recipe to produce code like this:
@(racketblock
  (define/contract (NEW-PROC t)
    (-> (instanceof Team) (instanceof Player))
    (code:hilite (join playersG Player #:to t
                       (group-by (TeamID playersG))
                       (join-on (.= (TeamID playersG)
                                    (TeamID t))))))
  (from t Team
        ....
        (define playersG (NEW-PROC t))
        ....))

Next we use the @(secref "proc->schema") recipe to move @(racket NEW-PROC)
into our schema definition. We also immediately rename it to @(racket PLAYERSG).
My personal convention is that the name of a grouped join ends with "G".
@(racketblock
  (define-schema
    ....
    (table Team
           ....
           #:has-group
           [PLAYERSG
            (code:hilite (join playersG Player
                               (group-by (TeamID playersG))
                               (join-on (.= (TeamID playersG)
                                            (?? (TeamID this) /void)))))]
           ....)
    ....)
  (from t Team
        ....
        (define playersG (PLAYERSG t))
        ....))

Finally we use the @(secref "join<->define") recipe to make sure we are
equivalent to our starting position:
@(racketblock
  (from t Team
        ....
        (join playersG (PLAYERSG t))
        ....))

And this recipe is complete.

@recipe[#:tag "scalar->schema"]{Scalar -> Schema Definition}
This recipe moves a scalar into @(racket define-schema).
@(racketblock
  (code:comment "current code:")
  (from p Player
        ....
        (select (code:hilite (./ (ShotsMade p)
                                 (ShotsTaken p))))
        ....)
  (code:comment "desired code:")
  (from p Player
        ....
        (select (SHOOTINGPERCENTAGE p))
        ....))

First we use the @(secref "ex->proc") recipe as follows:
@(racketblock
  (define/contract (NEW-PROC p)
    (-> (instanceof Player) Scalar?)
    (code:hilite (./ (ShotsMade p)
                     (ShotsTaken p))))
  (from p Player
        ....
        (select (NEW-PROC p))
        ....))

Finally we use the @(secref "proc->schema") recipe to move @(racket NEW-PROC)
into our schema definition.
We also immediately rename it to @(racket SHOOTINGPERCENTAGE).
@(racketblock
  (define-schema
    ....
    (table Player
           ....
           #:property
           [SHOOTINGPERCENTAGE
            (code:hilite (./ (ShotsMade this)
                             (ShotsTaken this)))]
           ....)
    ....)
  (from p Player
        ....
        (select (SHOOTINGPERCENTAGE p))
        ....))

And this recipe is complete.

@recipe[#:tag "scalar-flattening"]{Scalar Flattening}
This recipe is a special case of the @(secref "scalar->schema") recipe.
This recipe says that if @(racket (code:hilite (TeamName (Team p)))) is
already defined, we can easily add a new property @(racket (TEAMNAME p))
which will be equal to the original expression.
@(racketblock
  (code:comment "current code:")
  (from p Player
        ....
        (select (code:hilite (TeamName (Team p))))
        ....)
  (code:comment "desired code:")
  (from p Player
        ....
        (select (TEAMNAME p))
        ....))

First we use the @(secref "ex->proc") recipe as follows:
@(racketblock
  (define/contract (NEW-PROC p)
    (-> (instanceof Player) Scalar?)
    (code:hilite (TeamName (Team p))))
  (from p Player
        ....
        (select (NEW-PROC p))
        ....))

Finally we use the @(secref "proc->schema") recipe to move @(racket NEW-PROC)
into our schema definition.
We also immediately rename it to @(racket TEAMNAME).
@(racketblock
  (define-schema
    ....
    (table Player
           ....
           #:property
           [TEAMNAME
            (code:hilite (TeamName (Team this)))]
           ....)
    ....)
  (from p Player
        ....
        (select (TEAMNAME p))
        ....))

And this recipe is complete.

@recipe[#:tag "join->inline"]{Inline Join}
This recipe moves a join inline.
This is mostly used to set up further refactoring.
It does not add anything to @(racket define-schema).
@(racketblock
  (code:comment "current code:")
  (from p Player
        ....
        (join t (code:hilite (Team p)))
        ....
        (select (TeamName t))
        ....)
  (code:comment "desired code:")
  (from p Player
        ....
        (code:comment "this code gets removed:")
        #,(code:strike (join t (code:hilite (Team p))))
        ....
        (select (TeamName (code:hilite (Team p))))
        ....))

We first use the @(secref "join<->define") recipe as follows:
@(racketblock
  (from p Player
        ....
        (define t (code:hilite (Team p)))
        ....
        (select (TeamName t))
        ....))

Now we just use normal refactoring techniques to replace @(racket t) with its
definition as follows:
@(racketblock
  (from p Player
        ....
        (code:comment "this code is removed:")
        #,(code:strike (define t (code:hilite (Team p))))
        ....
        (select (TeamName (code:hilite (Team p))))
        ....))

And this recipe is complete.
They key point is that if we proceed to use the @(secref "ex->proc") recipe
on @(racket (TeamName (Team p))), the resulting procedure will now accept one
argument which is an @(racket (instanceof Player)).
In the original version, it would have wanted an @(racket (instanceof Team)).

@recipe[#:tag "ds:rename"]{Name Clarification}
This recipe creates a more descriptive name for a procedure.
This recipe assumes we are using @(racket define-schema).
@(racketblock
  (code:comment "current code:")
  (from t Team
        ....
        (select (code:hilite (Name t)))
        ....)
  (code:comment "desired code:")
  (from t Team
        ....
        (select (TEAMNAME t))
        ....))

We want to create @(racket TeamName) as an alias for @(racket Name) when
the argument is an @(racket (instanceof Team)).
First we use the @(secref "ex->proc") recipe to get the following code:
@(racketblock
  (define/contract (NEW-PROC t)
    (-> (instanceof Team) Scalar?)
    (code:hilite (Name t)))
  (from t Team
        ....
        (select (NEW-PROC t))
        ....))

Finally we use the @(secref "proc->schema") recipe to move @(racket NEW-PROC)
into our schema definition.
We also immediately rename it to @(racket TEAMNAME).
@(racketblock
  (define-schema
    ....
    (table Team
           ....
           #:property
           [TEAMNAME
            (code:hilite (Name this))]
           ....)
    ....)
  (from t Team
        ....
        (select (TEAMNAME t))
        ....))

And we are done.

Note that @(racket define-schema) automatically sets the @(racket #:as) name of
each @(racket #:property), as if you had written the following:
@(racketblock
  #:property
  [TEAMNAME
   (>> (code:hilite (Name this))
       #:as 'TEAMNAME)])

Be aware of this to avoid breaking any existing call sites that depend on the
original name appearing in the result set.
The examples in this documentation ignore this caveat because this recipe
is always used on a brand new query that has no call sites yet.
