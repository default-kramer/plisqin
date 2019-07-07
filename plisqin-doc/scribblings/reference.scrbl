#lang scribble/manual
@(require (for-label plisqin-lib
                     (except-in "racket.rkt" define)
                     plisqin-lib/examples/video-rental-schema))
@(require "helpers.rkt"
          plisqin-lib
          scribble/eval
          "racket.rkt"
          (prefix-in doc: (submod plisqin-lib/private/model all docs))
          (prefix-in doc: (submod plisqin-lib/private/core docs)))
@(module label-help racket
   (require scribble/manual/lang (for-label racket))
   (define (r:define) (racket define))
   (provide r:define))
@(require 'label-help)

@title{Reference}
@(defmodule plisqin-lib)

@(define-syntax-rule (deffrags ELLIPSIS [ctor tester] ...)
   (deftogether [
 (defproc (ctor [tokens token-list?] ELLIPSIS) tester)
 ...]))

@deftogether[[
 @defform[(from id queryable statement ...)]
 @defform[#:literals (define join join-attach)
          (join id queryable statement ...)
          #:grammar ((statement (define val-id val-expr)
                                (define (proc-id args ...) body ...)
                                (join-attach join-id rest-of-join ...)
                                (join        join-id rest-of-join ...)
                                statement-expr))
          #:contracts ([queryable queryable?]
                       [statement-expr statement-expr?])]
 ]]{
 The @(racket from) and @(racket join) macros are very similar in that they are both "query-building macros."
 Queries and joins are so similar that most of the time when I say "query" I really mean "query or join".
 You might say that "join is a subtype of query, except when it isn't."

 The @(racket id) argument is introduced as an identifier which refers to the query being constructed.
 Its value is a @(racket source?) and it is in scope for all the statements.
 It is analogous to an alias in SQL.

 The @(racket queryable) argument defines the initial value of the query,
 before any of the @(racket statement)s are applied.
 It must be a value that satisfies the @(racket queryable?) contract.
 See the documentation on @(racket queryable?) for further information.

 Next come zero or more @(racket statement)s.
 Each statement may be a special form (@(racket define) or @(racket join-attach)),
 or it may be an expression.

 The @(racket define) statement behaves similar to Racket's built-in @(r:define).
 The @(racket val-id) or @(racket proc-id) that is introduced remains
 in scope for the rest of the statements.

 The @(racket join) and @(racket join-attach) statements do exactly the same thing.
 In other words, when @(racket join) appears as a @(racket statement), it gets treated
 as if it were @(racket join-attach).
 (The only reason @(racket join-attach) exists is to make writing this documentation easier.)

 The @(racket join-attach) statement does three things.
 First, it creates a join as @(racket (join join-id rest-of-join ...)).
 Second, it attaches that join to the enclosing query.
 Third, it makes that attached join available via @(racket join-id) for the rest of the statements.
 Read more about the difference between attached and detached joins at @(secref "Attached_vs_Detached_Joins").

 If a statement does not match one of the special forms, it is
 a @(racket statement-expr) which must satisfy the @(racket statement-expr?) contract.
}

@defproc[(to-sql [token sql-token?]) string?]{
 Renders the token to an SQL string.
 @(interaction
   #:eval my-eval
   (displayln (to-sql (RS from foo "Foo"
                          (select foo".Bar")))))
}

@defform[(def-table constructor [table-name] [default-alias] [tester?])
         #:grammar ([constructor id]
                    [tester? id])
         #:contracts ([table-name (or/c #f string?)]
                      [default-alias (or/c #f string?)])]{
 Defines two procedures: @(racket constructor) and @(racket tester?).
 The @(racket constructor) takes an optional alias argument and constructs a @(racket source?):
 @(interaction
   #:eval my-eval
   (def-table Foo)
   (Foo)
   (Foo "custom-alias"))

 The @(racket constructor) is defined using @(racket def/append!), making it appendable.

 The @(racket tester?) tests whether its single argument is a @(racket query?) or
 @(racket join?) or @(racket source?) with the correct table name:
 @(interaction
   #:eval my-eval
   (def-table Foo)
   (and (Foo? (Foo))
        (Foo? (Foo "f"))
        (Foo? (from f Foo))
        (Foo? (join f Foo))
        (Foo? (join f "Foo")))
   (Foo? (join f Rental)))

 All arguments except the first are optional.
 If @(racket table-name) is @(racket #f), it will simply convert @(racket constructor) to a string.
 If @(racket default-alias) is @(racket #f), it will prepend an underscore to the table name, and
 maybe lowercase the first character. These declarations are all equivalent:
 @(racketblock
   (def-table MyTable)
   (def-table MyTable #f)
   (def-table MyTable #f #f)
   (def-table MyTable #f #f MyTable?)
   (def-table MyTable "MyTable" "_myTable" MyTable?))
}

@defform[(table constructor [table-name] [default-alias] [tester?])]{
 Depreacted. Alias of @(racket def-table).
}

@defform[(def-fields-of table field ...+)
         #:grammar ([field id])
         #:contracts ([table (or/c string? table?)])]{
 Uses @(racket def/append!) to define or append each @(racket field) as a scalar of the given @(racket table).
 @(interaction
   #:eval my-eval
   (def-table Foo)
   (def-fields-of Foo
     FooId FooName)
   (FooId (Foo)))
}

@defform[(def/append! (id arg ...)
           [test-expr then-body] ...)
         #:grammar ([id id]
                    [arg id])]{
 If @(racket id) is not yet defined, defines @(racket id) as a procedure.
 If @(racket id) has already been defined using @(racket def/append!), its definition mutated
 to append the new @(racket [test-expr then-body]) forms to the existing definition.
 The @(racket test-expr)s are evaluated in reverse order and as soon as one returns
 a truthy value, the corresponding @(racket then-body) is evaluated and returned.
 For example:
 @(interaction
   #:eval my-eval
   (def/append! (twice x)
     [(integer? x) (* x 2)])
   (code:comment "we can append more conditions to the existing definition of twice:")
   (def/append! (twice x)
     [(string? x) (format "~a ~a" x x)])
   (twice 3)
   (twice "pizza"))

 This appending works even across files.
 This is to support having one file containing generated code and another containing manual overrides.
 Here I will override the @(racket integer?) case while leaving the @(racket string?) case intact:
 @(interaction
   #:eval my-eval
   (def/append! (twice x)
     [(integer? x)
      (format "Two times ~a is ~a" x (* 2 x))])
   (twice 3)
   (twice "pizza"))
}
@defform[(field-cases (id arg)
                      [test-expr then-body] ...)]{
 Deprecated. Alias of @(racket def/append!).
}

@defform[(via! bridge #:link from ...+ #:to to ...+)
         #:grammar ([bridge id]
                    [from id]
                    [to id])]{
 Uses @(racket def/append!) to append to the definition of each @(racket to).
 Assumes that each @(racket to) is defined given @(racket bridge),
 and that @(racket bridge) is defined given each @(racket from).
 For example, consider
 @(racketblock
   (via! Country
         #:link City
         #:to CountryName CountryPopulation))
 We assume that @(racket CountryName) and @(racket CountryPopulation) are already defined
 when given a @(racket Country). We also assume that @(racket Country) is defined when given
 a @(racket City) (this would be the join from a City to its Country).
 The result is equivalent to saying
 @(racketblock
   (def/append! (CountryName x)
     [(same-table? City x)
      (CountryName (Country x))])
   (def/append! (CountryPopulation x)
     [(same-table? City x)
      (CountryPopulation (Country x))]))
}

@(reset-eval!)
@defform[(attach-callstacks)]{
 @margin-note{
  For Racketeers: this works by customizing the caller's @(racket #%app), so I think
  it is accurate to say that it remains in effect for the remainder of the module.
 }
 Turns callstack tracking on.
 When callstack tracking is not enabled, this error message shows the values
 of the problematic joins, which can be hard to read:
 @(interaction
   #:eval my-eval
   (aggregate
    (Rentals-of/g (Item))
    (Rentals-of/g (Copy))))

 The following example turns on callstack tracking, and raises the same error.
 I'll also add another layer to the callstack for illustrative purposes.
 Now instead of showing the value, the error message shows the callstack that
 produced the value:
 @(interaction
   #:eval my-eval
   (attach-callstacks)
   (define (foo) (Rentals-of/g (Copy)))
   (aggregate
    (Rentals-of/g (Item))
    (foo)))
}
@(reset-eval!)

@defform[(RS x arg ...)]{
 "RS" means "raw SQL".
 Wraps all string literals within the body of @(racket RS) into @(racket raw-sql).
 If at least 1 @(racket arg) is given, then the result is @(racket (x arg ...)).
 Otherwise the result is @(racket x).
 @(interaction
   #:eval my-eval
   (RS "some raw sql")
   (let ([str "hello"])
     (code:comment "notice that `str` is not a string literal")
     (RS (list 1 2 3 (list "a" "b" str)))))

 This is needed because the @(racket sql-token?) contract does not accept raw strings.
 This is to prevent SQL injection. The following example would be an error without @(racket RS).
 @(interaction
   #:eval my-eval
   (display (to-sql (RS from y "Y"
                        (where y".Foo = 'bar'")
                        (select y".*")))))
}

@defform[(val: x)]{
 Wraps @(racket x), which must be a string or numeric literal, into a trusted value
 which is accepted by @(racket sql-token?).
 It is rendered as a string or numeric value within SQL.
 @(interaction
   #:eval my-eval
   (to-sql (RS where "something like " (val: "foo'bar%")))
   (val: (list "this will fail")))
}

@section[#:tag "ref-fragments"]{Fragments}
A fragment is a list of tokens with a @(racket fragment-kind?) attached.
The fragment kind tells Plisqin what part of an SQL query it is.
@(deffrags ...
   [select select?]
   [where where?]
   [join-on join-on?]
   [group-by group-by?]
   [having having?]
   [scalar scalar?]
   [aggregate aggregate?]
   [bool bool?]
   [subquery subquery?]
   [sql sql?])
Constructs an SQL fragment.

@defproc[(order-by [maybe-dir (or/c 'asc 'desc sql-token? (listof sql-token?))]
                   [tokens token-list?]
                   ...)
         order-by?]{
 A clause that controls how rows are sorted in the result set.
 The first argument is allowed to be @(racket 'asc) or @(racket 'desc)
 meaning "ascending" or "descending" respectively.
 If the sort direction is not specified, is defaults to ascending.
}

@(define-syntax-rule (define-frag-testers tester ...)
   (deftogether [
 (defproc (tester [x any/c]) boolean?)
 ...]))
@(define-frag-testers select? where? join-on? group-by? order-by? having? scalar? aggregate? bool? subquery? sql? fragment?)
Tests whether the argument is a fragment of the correct kind.
If any of these returns true, then @(racket fragment?) will also return true.
(Fragment is the supertype.)

@(interaction
  #:eval my-eval
  (where? (RS where "1=1"))
  (group-by? (RS where "1=1"))
  (fragment? (RS where "1=1")))

@section[#:tag "ref-sql"]{SQL Functions}

@(define-syntax-rule (def-aggregates ELLIPSIS [agg-id ...] content ...)
   (deftogether [
 (defproc (agg-id [tokens token-list?] ELLIPSIS
                  [#:distinct? distinct? any/c #f])
   aggregate?)
 ...]
     content ...))

@def-aggregates[... [count avg max min sum]]{
 These are the aggregate functions that PostgreSQL and MS SQL Server share in common.
 When there are no grouped joins, they do pretty much what you would expect:
 @(interaction
   #:eval my-eval
   (display
    (to-sql
     (RS from c "information_schema.columns"
         (select (sum c".ordinal_position")))))
   (display
    (to-sql
     (RS from c "information_schema.columns"
         (select (sum c".ordinal_position" #:distinct? #t))))))

 When there is a grouped join involved, they can cause an automatic injection:
 @(interaction
   #:eval my-eval
   (define (columns-of/g t)
     (RS join c "information_schema.columns"
         (group-by (scalar c".table_name"))
         (join-on (scalar c".table_name")
                  " = "
                  (scalar t".table_name"))))
   (display
    (to-sql
     (RS from t "information_schema.tables"
         (join c (columns-of/g t))
         (select (max c".ordinal_position"))
         (select t".table_name"))))
   (display
    (to-sql
     (RS from t "information_schema.tables"
         (join c (columns-of/g t))
         (select (max c".ordinal_position" #:distinct? #t))
         (select t".table_name")))))

 The @(racket count) function gets some special treatment that the others don't.
 Specifically, @(racket count) is allowed to accept a single token that refers
 to a query or a join:
 @(interaction
   #:eval my-eval
   (display (to-sql (from t "information_schema.tables"
                          (select (count t))))))

 This special case is reported as an error for the other aggregate functions.
 This is because it is possible to count rows, but it is not possible to sum or average them.
 For example, @(racket (avg t)) does not have a reasonable meaning here:
 @(interaction
   #:eval my-eval
   (display (to-sql (from t "information_schema.tables"
                          (select (avg t))))))
}

@defproc[(exists [tokens token-list?] ...)
         bool?]{
 Represents the SQL "exists" function.
 @(interaction
   #:eval my-eval
   (display (to-sql (RS from x "X"
                        (where (exists "select 1 as ONE"))))))

 There is special handling for a single argument that is a query:
 @(interaction
   #:eval my-eval
   (display (to-sql (from x "X"
                          (where (exists (from y "Y")))))))

 There is also special handling a single argument that is a join.
 The join gets converted to a  query.
 The details of this conversion are documented at @(secref "join-query-conversion"),
 here is an example:
 @(interaction
   #:eval my-eval
   (display (to-sql (RS from x "X"
                        (where (exists (join y "Y"
                                             (join-on y".foo = "x".bar"))))))))
}

@defform[(case-when maybe-of terms ...+ maybe-else)
         #:grammar [(maybe-of (code:line)
                              (code:line #:of of-expr))
                    (terms [when-expr then-expr])
                    (maybe-else (code:line)
                                (code:line #:else else-expr))]]{
 @margin-note{
  Warning! When invoked with curly braces like @(racket {case-when args ...}),
  the grammar changes to that defined by @(racket case).
  This is true even in #lang racket.
 }

 Corresponds to the SQL case expression.
 If @(racket maybe-of) is specified, its value is compared to each @(racket when-expr)
 and the the result is the @(racket then-expr) of the first match found.
 If @(racket maybe-of) is omitted, each @(racket when-expr) should be a boolean
 expression, and the result is the @(racket then-expr) of the first one that was true.

 @(interaction
   #:eval my-eval
   (define simple-case
     (case-when #:of (RS scalar "foo")
                [10 100]
                [20 200]))
   (displayln (to-sql simple-case))
   (define searched-case
     (case-when
      [(RS bool "x < y") -1]
      [(RS bool "x > y") 1]
      #:else 0))
   (displayln (to-sql searched-case)))
}

@defform[#:literals (when then else)
         (case maybe-of terms ...+ maybe-else)
         #:grammar [(maybe-of (code:line)
                              of-expr)
                    (terms {when x then y})
                    (maybe-else (code:line)
                                {else else-expr})]]{
 TODO explain this is only available to #lang plisqin.
 It falls back to @(racket (racket:case)) if not used with braces.

 TODO need to import stuff to show a #lang plisqin code example
}

@section[#:tag "ref-data-model"]{Data Model}

@defthing[fragment-kind? contract?]
@(doc:fragment-kind?)

@defthing[join-type? contract?]
@(doc:join-type?)

@defthing[sql-token? contract?]
The smallest part of SQL that Plisqin is concerned with.
Tokens are typically collected into @(racket fragment)s.
@(doc:sql-token?)

@defthing[token-list? contract?]
A list @(racket x) such that @(racket (flatten x)) satisfies the contract
@(racket (listof sql-token?)).
When @(racket token-list?) is used with @(racket define/contract) it will automatically
perform the flattening.
This is most commonly used with rest arguments, like this:
@(interaction
  #:eval (make-eval)
  (require racket)
  (define/contract (autoflatten . tokens)
    (->* () () #:rest token-list? (listof sql-token?))
    (code:comment "'tokens' has already been flattened by its contract here:")
    tokens)
  (RS autoflatten "a" '(1 (2 3)) "z"))

Basically any procedure that accepts a list of tokens works the same way:
@(interaction
  #:eval my-eval
  (RS where "a" '(1 (2 (3 4))) "z")
  (RS order-by "a" '(7 8 (9)) "z"))

@defthing[queryable? contract]
This is a contract that defines how you are allowed to start a query or join.
Specifically, in @(racket (from a b)) or @(racket (join a b)),
the value of @(racket b) must satisfy this contract.

@(doc:queryable?)

A @(racket source?) is typically obtained by applying the "constructor procedure"
defined by @(racket table).
In these examples, @(racket Rental) is such a procedure:
@(racketblock
  (from r (Rental))
  (from r (Rental "my-custom-alias")))

You can also pass the constructor procedure directly instead of applying it.
In this case, the identifier that precedes it is used as the alias:
@(racketblock
  (from abc Rental)
  (code:comment "is equivalent to:")
  (from abc (Rental "abc")))

A @(racket string?) value represents the table name.
The identifier that precedes it is used as the alias.
So this example shows a query of "MyTable" with an alias of "jkl":
@(racketblock
  (from jkl "MyTable"))

When the value is a @(racket query?), the query or join appends to that value.
Some examples:
@(racketblock
  (RS from a (from b "B"
                   (select "1 as ONE"))
      (select "2 as TWO"))
  (code:comment "is equivalent to:")
  (RS from b "B"
      (select "1 as ONE")
      (select "2 as TWO")))
And
@(racketblock
  (RS join a (from b "B"
                   (select "1 as ONE"))
      (join-on "2=2"))
  (code:comment "is equivalent to:")
  (RS join b "B"
      (select "1 as ONE")
      (join-on "2=2")))

When the value is a @(racket join?), it is a special appending scenario.
A join appends to a join with no surprises.
But when a query appends to a join, it first converts the join to a query.
You can read more about this at @seclink["join-query-conversion"]{Join-Query Conversion},
but the main point is that @(racket join-on) clauses get converted to @(racket where) clauses:
@(racketblock
  (RS from a (join b "B"
                   (join-on "1=1"))
      (select a".foo"))
  (code:comment "is equivalent to:")
  (RS from b "B"
      (where "1=1")
      (select b".foo")))

Finally, if the value is a @(racket subquery?), it suppresses the appending behavior.
Showing the SQL is the best way to explain this:
@(interaction
  #:eval my-eval
  (define (my-sub)
    (RS from x "X"
        (select x".foo")))
  (code:comment "queries append by default:")
  (display (to-sql (from a (my-sub))))
  (code:comment "use a subquery to prevent appending:")
  (display (to-sql (from a (subquery (my-sub))))))

You can also use @(racket subquery) to make a "literal" subquery, which I sometimes use
when I want a result set that has exactly one row:
@(racketblock
  (RS from one-row (subquery "select 1 as ONE_ROW")
      (join a "A" 'LeftJoin
            (join-on a".ID = 3"))
      (join b "B" 'LeftJoin
            (join-on b".ID = 9"))
      (select (coalesce
               (scalar a".Name")
               (scalar b".Name")
               "neither A nor B was found"))))

@defthing[statement? contract?]
A statement is a value that can be applied to a query to produce a new query.
@(doc:statement?)

@defthing[statement-expr? contract?]
@(doc:statement-expr?)

@deftogether[[
 @defthing[attached-join? contract?]
 @defthing[binding? contract?]
 @defthing[join? contract?]
 @defthing[source? contract?]
 ]]{
 @itemlist[
 @item{An @(racket attached-join?) is a join that is attached to its enclosing query. (See @(racket from).)}
 @item{A @(racket binding?) is a deprecated synonym for @(racket attached-join?).}
 @item{A @(racket join?) is a detached join.}
 @item{A @(racket source?) is the value of the identifier introduced
   by an enclosing @(racket from) or @(racket join).}
 ]
 @(interaction
   #:eval my-eval
   (displayln
    (to-sql
     (from
      ignored-id
      (RS join x "X"
          (define y (join y "Y"))
          (join z "Z")
          (define (what-is it)
            (list
             (if (attached-join? it)
                 (select it".is-attached-join")
                 (select it".is-not-attached-join"))
             (if (join? it)
                 (select it".is-join")
                 (select it".is-not-join"))
             (if (source? it)
                 (select it".is-source")
                 (select it".is-not-source"))))
          (what-is x)
          (what-is y)
          (what-is z))))))
}

@defthing[query? contract?]{
 A query is the value constructed by @(racket from).
 @(interaction
   #:eval my-eval
   (query? (from a "A"))
   (query? (join a "A")))
}

@defthing[injection? contract?]{
 An injection is the value created by @(racket inject).
}

@section{Terminology}
I have created 3 prepositions for my own naming convention, as well as for this documentation.
They are all pronounced "of" but carry some extra meaning:
@itemlist[
 @item{"The Item @deftech{of/s} the Copy" means "the Item that belongs to the Copy,
  and take note that there is at most 1 such Item."
  The /s refers to the @italic{singular} nature of this relationship.}
 @item{"The Copies @deftech{of/g} the Item" means "the group of Copies that belongs to the Item,
  and take note that there is exactly 1 such group. (But the group may be empty.)"
  The /g refers to the @italic{grouped} nature of this relationship.}
 @item{"The Copies @deftech{of/p} the Item" means "the Copies that belong to the Item,
  and take note that each Copy will make an individual appearance in the result set."
  The /p refers to the @italic{plural} nature of this relationship.}]

@section{#lang plisqin}
TODO write this section.

** Dots are read differently: @(racket foo.bar) gets read as @(racket foo .bar).
You can use bars to suppress this: @(racket |foo.bar|).

** What gets required by default in #lang plisqin.

** The {curly-brace rewrite rules}.
Note that @(racket a+b) is not the same as @(racket a + b).
@(define pattern racketplainfont)

@(define (one-of-proc items)
   (define head
     (racketplainfont (format "~a" (car items))))
   (define next
     (match items
       [(list a) ""]
       [(list a rest ...)
        (elem ", " (one-of-proc rest))]))
   (elem head next))
@(define-syntax-rule (one-of stuff ...)
   (elem "one of " (one-of-proc (list 'stuff ...))))

@(tabular
  #:sep (hspace 1)
  #:style 'boxed
  #:row-properties '(bottom-border)
  (list (map bold (list "Pattern" "Result" "Notes"))
        (list @pattern{@italic{LITERAL}}
              @(racket (quote LITERAL))
              @elem{When @pattern{@italic{LITERAL}} is @(one-of null asc desc)})
        (list @(racket x .y)
              @(racket {y x})
              @elem{Also works for @(racket x.y) which is read as @(racket x .y)})
        (list @(racket x || y)
              @(racket {|| x y})
              "String concatenation. TODO think about this precedence...")
        (list @pattern{x @italic{OP} y}
              @(racket {OP x y})
              @elem{When @pattern{@italic{OP}} is @(one-of * /)})
        (list @pattern{x @italic{OP} y}
              @(racket {OP x y})
              @elem{When @pattern{@italic{OP}} is @(one-of + -)})
        (list @(racket x ?? y)
              @(racket {?? x y})
              "means \"coalesce(x, y)\" in SQL")
        (list @pattern{x @italic{CMP} y}
              @(racket {CMP x y})
              @elem{When @pattern{@italic{CMP}} is @(one-of = <> like not-like
                                                            is is-not < <= > >=)})
        ))

TODO put examples here, to make the motivation clear.
Such as @(racket {order-by desc x.Foo}) is rewritten to @(racket (order-by 'desc (Foo x)))
