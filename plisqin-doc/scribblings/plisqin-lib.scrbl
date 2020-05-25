#lang scribble/manual

@(require (for-label "standard-label.rkt")
          (for-syntax "racket.rkt"
                      plisqin-lib/types)
          "helpers.rkt")

@title{plisqin-lib}
@(defmodule plisqin-lib)

@defform[#:literals (define join)
         (from instance-id queryable-expr statement ...)
         #:grammar
         [(statement (define (proc-id proc-arg ...) proc-body ...)
                     (define val-id val-expr)
                     (join join-stuff ...)
                     clause-expr)]
         #:contracts ([queryable-expr (or/c symbol?
                                            query?
                                            Subquery?
                                            trusted-queryable?)]
                      [clause-expr (or/c void? QueryClause?
                                         (listof (or/c void? QueryClause?)))])]{
 TODO
}

@defform[#:literals (define join)
         (join instance-id queryable-expr maybe-to statement ...)
         #:grammar
         [(maybe-to (code:line)
                    (code:line #:to link-expr))
          (statement (define (proc-id proc-arg ...) proc-body ...)
                     (define val-id val-expr)
                     (join join-stuff ...)
                     clause-expr)]
         #:contracts ([queryable-expr (or/c symbol?
                                            join?
                                            Subquery?
                                            trusted-queryable?)]
                      [link-expr instance?]
                      [clause-expr (or/c void? JoinClause?
                                         (listof (or/c void? JoinClause?)))])]{
 Similar to @(racket from), but creates a join instead of a query.

 The @(racket #:to) option @bold{must} be omitted when this @(racket join)
 is a @(racket statement) of an enclosing @(racket from) or @(racket join).
 In that case, it is automatically linked to the enclosing query or join:
 @(racketblock
   (from a 'A
         (join b 'B)
         (%%where b".foo = "a".foo"))
   (code:comment "is nearly equivalent to")
   (from a 'A
         (define b
           (join b 'B #:to a))
         (%%where b".foo = "a".foo")))
 @(repl
   ; I can't think of a reason that this *must* be an error.
   ; This is mostly to make sure I update the docs if I relax that restriction.
   (eval:error (from a 'A
                     (join b 'B #:to a))))

 Otherwise, the @(racket #:to) option is usually required.
 The value of the @(racket link-expr) specifies which query this join belongs to.
 In most real code, you only have one reasonable choice.
 In the following hypothetical example, @(racket pet) is the only @(racket instance?)
 in scope; it is the obvious and only possible @(racket link-expr):
 @(racketblock
   (define/contract (Owner pet)
     (-> (instanceof Pet) (instanceof Person))
     (join p Person
           #:to pet
           (join-on (.= (PersonId p)
                        (OwnerId pet))))))
}

@defproc[(query? [x any/c]) any/c]{
 Predicate that recognizes queries.
 @(repl
   (eval:check (query? (from a 'A)) #t))
}

@defproc[(join? [x any/c]) any/c]{
 Predicate that recognizes joins.
 @(repl
   (define (B-given-A a)
     (join b 'B #:to a
           (%%join-on b".Foo = "a".Foo")))
   (from a 'A
         (if (join? (B-given-A a))
             (%%select "of course it is a join")
             (error "Inconceivable!"))))
}

@defproc[(instance? [x any/c]) any/c]{
 Predicate that recognizes @deftech{instances}.
 An instance can be thought of as a row of data.
 If you translate code to English, you might translate an instance as
 @italic{the [table-name]} or @italic{each [table-name]}.
 For example, the following where clause can be translated to English as
 "where the Age of @italic{the Person} is at least 21":
 @(racketblock
   (from p Person
         (where (>= (Age p)
                    (val 21)))))

 Additionally, every @(racket join?) is also an instance!
 TODO link to a refactoring recipe that demonstrates why this is awesome.
}

@defproc[(instanceof [queryable any/c]) procedure?]{
 Creates a predicate roughly equivalent to
 @(racketblock
   (lambda (x)
     (and (instance? x)
          (has-same-queryable? x queryable))))
}

@defproc[(limit [n (or/c #f nonnegative-integer?)]) Limit?]{
 Places an upper bound on the number of rows that the query or join will produce.
 @(repl-query
   (aw:show-table
    (from pc ProductCategory
          (limit 2))))

 The last @(racket limit) clause overrides all the previous ones.
 The following example uses @(racket #f) to clear the limit.
 @(repl-query
   (aw:show-table
    (from pc ProductCategory
          (limit 2)
          (limit #f))))
}

@defproc[(offset [n (or/c #f nonnegative-integer?)]) Offset?]{
 Skips past the first @(racket n) rows of the result set.
 If using MSSQL, you must also have at least one @(racket order-by) clause or the
 SQL that is produced will be invalid.
 Offset is frequently used with @(racket limit) to implement paging.
 @(repl-query
   (aw:show-table
    (from p Product
          (order-by (ProductID p))
          (offset 30)
          (limit 3))))

 The last @(racket offset) clause overrides all the previous ones.
 A value of @(racket #f) clears the offset.
}

@defproc[(distinct [distinct? any/c]) Distinct?]{
 Specifies whether duplicate rows are eliminated from the result set.
 The default behavior is @(racket #f) which allows duplicate rows.
 Any non-false value means that duplicate rows will be eliminated.
 The last @(racket distinct) clause overrides all previous ones.

 The following query would return hundreds of rows (one for each Product)
 if the distinct flag was @(racket #f) instead:
 @(repl-query
   (aw:show-table
    (from p Product
          (select (Color p))
          (distinct #t))))
}

@defproc[(join-type [type (or/c #f 'inner 'left)]) JoinType?]{
 TODO
}

@defform[#:literals(table)
         (define-schema schema-id table-def ...)
         #:grammar
         [(schema-id id
                     #f)
          (table-def (table table-id item-def ...))
          (item-def (code:line #:column [id column-opt ...] ...+)
                    (code:line #:has-one [id expr] ...+)
                    (code:line #:has-group [id expr] ...+)
                    (code:line #:property [id expr] ...+))
          (column-opt (code:line #:as as-name)
                      (code:line #:type type)
                      (code:line #:null nullability)
                      (code:line #:dbname dbname))]]{
 TODO write documentation.
 Also, do we already support raw column ids? Should we? Like:
 @(racketblock
   #:column
   FirstName
   LastName
   [UserId #:type Number?]
   AnotherColumn)
}

@defidform[this]{
 For use within @(racket define-schema).
 Any other use is a syntax error.
}

@defform[(>> token modification ...)
         #:grammar
         [(modification (code:line #:cast Type)
                        (code:line #:as as-name)
                        (code:line #:null nullability)
                        (code:line #:fallback /fallback))]
         #:contracts
         ([Type type?]
          [as-name (or/c symbol? string?)]
          [nullability nullability?]
          [/fallback fallback?])]{
 Returns a copy of the given @(racket token) with the specified modifications.
 TODO needs more detail.
}

@defform[(define-statement (id arg ...) body ...+)
         #:grammar
         [(arg plain-arg
               kw-arg)
          (plain-arg arg-id
                     [arg-id Type-expr]
                     [arg-id Type-expr default-expr])
          (kw-arg (code:line keyword plain-arg))]
         #:contracts ([Type-expr type?])]{
 Defines an @deftech{uncompiled statement}, which is a normal procedure with some
 extra metadata stashed away that @(racket compile-statements) can use.
 @(racketblock
   (define-statement (foo a
                          [b Number?]
                          [c String? "c-default"]
                          #:d d
                          #:e [e Number?]
                          #:f [f String? "f-default"])
     (list a b c d e f))
   (code:comment "produces code similar to")
   (define (foo a
                b
                [c (%%val "c-default")]
                #:d d
                #:e e
                #:f [f (%%val "f-default")])
     (list a b c d e f))
   (begin
     (code:comment "this part is illustrative, not real code:")
     (module+ metadata-for-compile-statements
       (define meta:foo
         (foo (param a Scalar?)
              (param b Number?)
              (param c String?)
              #:d (param d Scalar?)
              #:e (param e Number?)
              #:f (param f String?))))))

 In the code above, the hypothetical @(racket (param b Number?)) expression
 creates a placeholder token having type @(racket Number?) and representing
 an SQL parameter. The @(racket a) and @(racket d) parameters were assigned the
 @(racket Scalar?) type, which is the default when @(racket Type-expr) is absent.

 TODO what about nullability?
 We could attach @(racket /void) to each param, but that seems dangerous.
 As of now, it's fine to force the user to add the fallback at the site of the
 comparison.
}

@defform[(compile-statements #:module module-path
                             #:dialect dialect-expr
                             maybe-provide)
         #:grammar
         [(maybe-provide (code:line)
                         (code:line #:provide? #t)
                         (code:line #:provide? #f))]
         #:contracts ([dialect-expr dialect?])]{
 Produces an @deftech{unbound statement} for each @tech{uncompiled statement} in
 the given @(racket module-path).
 Each unbound statement is a procedure with the same arity as its corresponding
 uncompiled statement, but its arguments are parameter values that will get passed
 to the database when the statement is executed.
 The unbound statement returns a @(racket db:statement?) suitable for use by
 @(racket db:query-rows) and similar procedures.

 @(repl
   (code:comment "This module contains a `get-category` uncompiled statement:")
   (compile-statements #:module plisqin-examples/adventure-works/statements
                       #:dialect (sqlite)
                       #:provide? #f)
   (code:comment "Now `get-category` has been compiled to an unbound statement:")
   (displayln get-category)
   (code:comment "We just need to supply the parameter values and we can")
   (code:comment "execute it against a real database!")
   (define conn (aw:connect-adventure-works))
   (db:query conn (get-category #:name "Bikes"))
   (db:disconnect conn))

 The SQL is generated only once, using the given @(racket dialect-expr).

 Unless @(racket #:provide?) is @(racket #f), each unbound statement will be
 @(racket provide)d. This option is really only intended for this documentation;
 your code probably has no need to ever use this option.
}

@defproc[(to-sql [x any/c]) string?]{
 TODO need to tighten up this contract.
 @(repl
   (displayln
    (to-sql
     (from a 'Album
           (%%where a".ReleaseYear = 1973")))))
}

@defproc[(interval? [x any/c]) interval?]{
 Predicate that recognizes intervals.
 @(repl
   (eval:check (interval? (days 2)) #t))

 See also the date math examples at @(racket date+).
}

@section[#:tag "reference:nullability"]{Nullability}
@deftogether[(@defthing[/void fallback?]
               @defthing[/minval fallback?]
               @defthing[/maxval fallback?]
               @defthing[/any fallback?])]{
 A fallback can be attached to any token using @(racket ??).
 It represents how the token behaves in comparisons when it is null.
 For example, @(racket (?? foo /minval)) can be read as "foo, or the minimum
 value when foo is null."
 The following hypothetical example includes Products with a null Price.
 The where clause can be read "where Price (or the minimum
 value when Price is null) is less than 50":
 @(racketblock
   (from p Product
         (where (.< (?? (Price p) /minval)
                    (val 50)))))

 The meaning of each fallback is as follows.
 @(itemlist
   @item{@(racket /minval) represents an artificial value that is less than
  every value that your database can hold. It is equal only to itself.}
   @item{@(racket /maxval) represents an artificial value that is greater than
  every value that your database can hold. It is equal only to itself.}
   @item{@(racket /any) represents a set of values that includes @(racket /minval),
  @(racket /maxval), and every value that your database can hold.}
   @item{@(racket /void) represents an empty set of values.
  This guarantees that the comparison will be false when the fallback is used.})

 When a fallback represents "a set of values", the comparison will be true if any value
 from the set could make the comparison true.

 Because @(racket /void) represents an empty set, comparing it against anything
 always produces false.
 This is true even when comparing @(racket /void) against @(racket /any).
 The comparison says "We've tried nothing and we're all out of ideas."

 Because @(racket /any) represents such a large set, comparing it against something
 other than @(racket /void) often (but not always) produces true.
 Some examples:
 @(itemlist
   @item{@(racket /any) is considered equal to 40 because it contains 40.}
   @item{@(racket /any) is considered not equal to 40 because it contains
  many values which are not equal to 40, such as 42.}
   @item{@(racket /any) is considered less than "aardvark" because it contains
  many values which are less than "aardvark", such as "aaa" and @(racket /minval).}
   @item{@(racket /any) is considered equal to @(racket /minval) because it contains
  @(racket /minval).}
   @item{@(racket /any) is @bold{not} considered less than @(racket /minval) because
  there are no values which are less than @(racket /minval).})

 TODO link to the complete truth table.

 TODO mention that @(racket .not) doesn't know anything about fallbacks.
}

@defproc[(fallback? [x any/c]) any/c]{
 Predicate that recognizes fallbacks.
 @(repl (eval:check (fallback? /void) #t))
}

@defproc[(fallback [x any/c]) (or/c #f fallback?)]{
 Returns the fallback of @(racket x) or @(racket #f) if none exists.
 @(repl
   (define my-token (%%sql "foo"))
   (eval:check (fallback my-token) #f)
   (eval:check (fallback (?? my-token /minval)) /minval))
}

@deftogether[(@defthing[yes nullability?]
               @defthing[no nullability?]
               @defthing[maybe nullability?])]{
 See @tech{nullability}.
}

@defproc[(nullability? [x any/c]) any/c]{
 Predicate that recognizes the @tech{nullabilities}.
 @(repl
   (eval:check (nullability? yes) #t)
   (eval:check (nullability? no) #t)
   (eval:check (nullability? maybe) #t))
}

@defproc[(nullability [x any/c]) nullability?]{
 Returns the @tech{nullability} of the given item.
 Literal values are never null:
 @(repl
   (eval:check (nullability (val 3)) no)
   (eval:check (nullability (val "hi")) no))

 Racket numbers are never null, but Racket strings might be.
 This is because Racket strings usually represent an arbitrary piece of SQL
 that Plisqin knows nothing about:
 @(repl
   (eval:check (nullability 3) no)
   (eval:check (nullability "case when 1=1 then null else null end") maybe)
   (eval:check (nullability "case when 1=1 then 'foo' else 'bar' end") maybe))

 The token constructors infer their own nullability from their contents.
 Some (eg @(racket coalesce)) have special rules, but most just take the "worst"
 nullability that was found.
 From worst to best: @(racket yes), @(racket maybe), @(racket no).
 @(repl
   (define content (%%sql "foo = 1"))
   (eval:check (nullability (%%where (>> content #:null no))) no)
   (eval:check (nullability (%%where (>> content #:null yes))) yes))
}

@section{Token Types}
@(defmodule plisqin-lib/types)
Plisqin's types are plain Racket values.
They start with a capital letter.
@(repl
  (eval:check Subquery? Subquery?)
  (eval:check (type? Subquery?) #t))

Types can be used as predicates, therefore they can also be used as contracts.
@(repl
  (eval:check (Number? (val 20)) #t)
  (eval:check (Token? (%%where "x.foo = 'bar'")) #t))

Most types have at least one supertype.
For example, @(racket Scalar?) is a supertype of @(racket Number?).
@(repl
  (eval:check (type-supertypes Number?) (list Scalar?))
  (eval:check (Number? (val 20)) #t)
  (eval:check (Scalar? (val 20)) #t))

@defproc[(type? [x any/c]) any/c]{
 Predicate that recognizes types.
 @(repl
   (eval:check (type? Token?) #t)
   (eval:check (type? Datetime?) #t))
}

@defproc[(type-supertypes [t type?]) (listof type?)]{
 Returns the supertypes of the given type.
 @(repl
   (eval:check (type-supertypes Datetime?) (list Scalar?)))
}

@(begin-for-syntax
   ; Stuff to help us introspect on the type hierarchy
   (define all-types 'uninitialized) ; will be set to (listof syntax?)

   (define type-map/sym (make-hash)) ; symbol? -> (listof symbol?)

   (define (register-supertypes! t supers)
     (hash-set! type-map/sym t supers))

   (define (strip-srcloc stx)
     (let* ([content (syntax-e stx)]
            [content (if (list? content)
                         (map strip-srcloc content)
                         content)])
       (datum->syntax stx content #f stx)))

   (define (type->symbol t)
     (string->symbol (format "~a" t)))

   (define (get-supertypes t)
     ; returns a list of syntax objects with the correct label and no srclocs
     ; (assuming that they were registered with the correct label)
     (cond
       [(syntax? t)
        (get-supertypes (syntax-e t))]
       [(type? t)
        (get-supertypes (type->symbol t))]
       [(symbol? t)
        (let ([super-syms (hash-ref type-map/sym t)])
          (filter (λ(super-stx) (member (syntax-e super-stx) super-syms))
                  all-types))]
       [else
        (error "contract violation:" t)])))

@(define-syntax (register-types! stx)
   (syntax-case stx ()
     [(_ [Type ...])
      (begin
        (set! all-types (map strip-srcloc (syntax->list #'(Type ...))))
        (for/list ([T (syntax->list #'(Type ...))])
          (let ([supertypes (eval-syntax #`(type-supertypes #,T))])
            (register-supertypes! (syntax-e T)
                                  (map type->symbol supertypes))))
        #'(void))]))

@(register-types!
  [Token? Scalar?
   ; booleans
   Boolish? Bit? Bool?
   ; scalar values
   Datetime? Number? String?
   ; other expressions
   Subquery?
   ; clauses
   Clause? JoinClause? QueryClause?
   Select? Where? GroupBy? Having? OrderBy? JoinOn?
   Limit? Offset? Distinct? JoinType?])

@(define-syntax (show-supertypes stx)
   (syntax-case stx ()
     [(_ Type content ...)
      (with-syntax ([(supertype ...) (get-supertypes #'Type)])
        #'(racketblock #:supertypes [supertype ...]))]))

@(define-syntax-rule (deftype Type content ...)
   (defthing Type type?
     (show-supertypes Type)
     content ...))
@deftype[Token?]{
 The root of the type hierarchy.
}

@deftype[Scalar?]{
 In general, every data type that a database column can have should correspond
 to @(racket Scalar?) or one of its subtypes.
 To use some PostgreSQL examples, "varchar(50)" corresponds to @(racket String?),
 and "bigint" corresponds to @(racket Number?).
 Less common data types such as "cidr" correspond to @(racket Scalar?) because
 Plisqin has no more appropriate subtype for them.
}

@deftype[Boolish?]{
 The supertype of @(racket Bit?) and @(racket Bool?).
 Most procedures that accept a @(racket Boolish?) argument really want a
 @(racket Bool?) but will tolerate a @(racket Bit?).
 The following example shows @(racket where) converting a bit to a bool by
 comparing it against zero.
 @(repl
   (displayln
    (to-sql
     (from x 'X
           (where (>> (val 0) #:cast Bit?)))))
   (displayln
    (to-sql
     (from x 'X
           (where (>> (val 1) #:cast Bit?))))))
}

@deftype[Bit?]{
 MSSQL and SQLite do not have a boolean data type for columns.
 MSSQL databases typically use the "bit" data type while SQLite
 databases typically use a regular number.
 The @(racket Bit?) type is designed to be compatible with these patterns,
 and with any numeric field that desires the following semantics:
 @margin-note{
  See @(secref "Nullability") for an explanation of "the unknown boolean value"
  and how the @tech{strict} variant helps avoid strange three-valued logic.}
 @(itemlist
   @item{A null value represents the unknown boolean value.}
   @item{A zero value represents false.}
   @item{Any other value represents true.})

 See example on @(racket Boolish?).
}

@deftype[Bool?]{
 Represents a boolean expression.
 Corresponds to the "boolean" data type in PostgreSQL.
 In MSSQL and SQLite, the only way to obtain a @(racket Bool?) is as a return
 value (for example, of a comparison).
 Columns in MSSQL and SQLite should never be declared as @(racket Bool?);
 consider using @(racket Bit?) instead.

 @subsubsub*section{MSSQL and Bool -> Scalar Conversion}
 In MSSQL, a boolean expression cannot be used as a scalar.
 For example, @(racket "select 42=42") is an error in MSSQL.
 But @(racket Bool?) is a subtype of @(racket Scalar?) (indirectly).
 This tension is resolved by generating SQL that converts a bool to a bit
 when a scalar is needed, as in the following example:
 @(repl
   (define q
     (from x 'X
           (define a-bool (.= (val 42)
                              (val 42)))
           (where a-bool)
           (select a-bool)))
   (parameterize ([current-dialect (mssql)])
     (displayln (to-sql q)))
   (code:comment "non-MSSQL dialects are much easier:")
   (displayln (to-sql q)))
}
@deftype[Datetime?]{
 TODO
}
@deftype[Number?]{
 TODO
}
@deftype[String?]{
 TODO
}
@deftype[Subquery?]{
 TODO
}
@deftype[Clause?]{
 TODO
}
@deftype[JoinClause?]{
 TODO
}
@deftype[QueryClause?]{
 TODO
}
@deftype[Select?]{
 TODO
}
@deftype[Where?]{
 TODO
}
@deftype[GroupBy?]{
 TODO
}
@deftype[Having?]{
 TODO
}
@deftype[OrderBy?]{
 TODO
}
@deftype[JoinOn?]{
 TODO
}
@deftype[Limit?]{
 TODO
}
@deftype[Offset?]{
 TODO
}
@deftype[Distinct?]{
 TODO
}
@deftype[JoinType?]{
 TODO
}


@section{Dialects}
@(defmodule plisqin-lib/dialect)
@defparam[current-dialect dialect (or/c #f dialect?)]{
 This parameter controls which flavor of SQL to generate.
 @(repl
   (define q (from x 'X
                   (limit 3)))
   (parameterize ([current-dialect (postgres)])
     (displayln (to-sql q)))
   (parameterize ([current-dialect (mssql)])
     (displayln (to-sql q))))
}

@defproc[(dialect? [x any/c]) any/c]{
 Predicate that recognizes dialects.
 @(repl
   (eval:check (dialect? (postgres)) #t)
   (eval:check (dialect? (mssql)) #t)
   (eval:check (dialect? (sqlite)) #t))
}

@deftogether[(@defproc[(postgres) dialect?]
               @defproc[(mssql) dialect?]
               @defproc[(sqlite) dialect?])]{
 Creates a dialect representing PostgreSQL, Microsoft SQL Server, or SQLite.
}

@deftogether[(@defproc[(postgres? [x any/c]) any/c]
               @defproc[(mssql? [x any/c]) any/c]
               @defproc[(sqlite? [x any/c]) any/c])]{
 Tests whether @(racket x) is a @(racket dialect?) representing PostgreSQL,
 Microsoft SQL Server, or SQLite.
 @(repl
   (eval:check (postgres? (postgres)) #t)
   (eval:check (postgres? (mssql)) #f)
   (eval:check (mssql? (mssql)) #t))
}
