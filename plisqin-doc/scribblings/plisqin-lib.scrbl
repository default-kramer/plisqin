#lang scribble/manual

@(require (for-label plisqin "racket.rkt"
                     (prefix-in db: db))
          (for-syntax "racket.rkt"
                      plisqin-lib/types)
          "helpers.rkt")

@title{plisqin-lib}
TODO explain that most of the good stuff is in strict, loose, and unsafe.

@(defmodule plisqin-lib)

@defform[(from TODO)]{
 TODO
}

@defform[(join TODO)]{
 TODO
}

@defproc[(limit [n nonnegative-integer?]) Limit]{
 TODO does #f clear the limit?
}

@defproc[(offset [n nonnegative-integer?]) Offset]{
 TODO does #f clear the offset?
}

@defproc[(distinct [distinct? any/c]) Distinct]{
 TODO
}

@defproc[(join-type [type (or/c #f 'inner 'left)]) JoinType]{
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
   [UserId #:type Number]
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
                          [b Number]
                          [c String "c-default"]
                          #:d d
                          #:e [e Number]
                          #:f [f String "f-default"])
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
         (foo (param a Scalar)
              (param b Number)
              (param c String)
              #:d (param d Scalar)
              #:e (param e Number)
              #:f (param f String))))))

 In the code above, the hypothetical @(racket (param b Number)) expression
 creates a placeholder token having type @(racket Number) and representing
 an SQL parameter. The @(racket a) and @(racket d) parameters were assigned the
 @(racket Scalar) type, which is the default when @(racket Type-expr) is absent.

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
  (eval:check Subquery Subquery)
  (eval:check (type? Subquery) #t))

Types can be used as predicates, therefore they can also be used as contracts.
@(repl
  (eval:check (Number (val 20)) #t)
  (eval:check (Token (%%where "x.foo = 'bar'")) #t))

Most types have at least one supertype.
For example, @(racket Scalar) is a supertype of @(racket Number).
@(repl
  (eval:check (type-supertypes Number) (list Scalar))
  (eval:check (Number (val 20)) #t)
  (eval:check (Scalar (val 20)) #t))

@defproc[(type? [x any/c]) any/c]{
 Predicate that recognizes types.
 @(repl
   (eval:check (type? Token) #t)
   (eval:check (type? Datetime) #t))
}

@defproc[(type-supertypes [t type?]) (listof type?)]{
 Returns the supertypes of the given type.
 @(repl
   (eval:check (type-supertypes Datetime) (list Scalar)))
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
  [Token Scalar
   ; booleans
   Boolish Bit Bool
   ; scalar values
   Datetime Number String
   ; other expressions
   Subquery
   ; clauses
   Clause JoinClause QueryClause
   Select Where GroupBy Having OrderBy JoinOn
   Limit Offset Distinct JoinType])

@(define-syntax (show-supertypes stx)
   (syntax-case stx ()
     [(_ Type content ...)
      (with-syntax ([(supertype ...) (get-supertypes #'Type)])
        #'(racketblock #:supertypes [supertype ...]))]))

@(define-syntax-rule (deftype Type content ...)
   (defthing Type type?
     (show-supertypes Type)
     content ...))
@deftype[Token]{
 The root of the type hierarchy.
}

@deftype[Scalar]{
 In general, every data type that a database column can have should correspond
 to @(racket Scalar) or one of its subtypes.
 To use some PostgreSQL examples, "varchar(50)" corresponds to @(racket String),
 and "bigint" corresponds to @(racket Number).
 Less common data types such as "cidr" correspond to @(racket Scalar) because
 Plisqin has no more appropriate subtype for them.
}

@deftype[Boolish]{
 TODO
}
@deftype[Bit]{
 TODO
}
@deftype[Bool]{
 TODO
}
@deftype[Datetime]{
 TODO
}
@deftype[Number]{
 TODO
}
@deftype[String]{
 TODO
}
@deftype[Subquery]{
 TODO
}
@deftype[Clause]{
 TODO
}
@deftype[JoinClause]{
 TODO
}
@deftype[QueryClause]{
 TODO
}
@deftype[Select]{
 TODO
}
@deftype[Where]{
 TODO
}
@deftype[GroupBy]{
 TODO
}
@deftype[Having]{
 TODO
}
@deftype[OrderBy]{
 TODO
}
@deftype[JoinOn]{
 TODO
}
@deftype[Limit]{
 TODO
}
@deftype[Offset]{
 TODO
}
@deftype[Distinct]{
 TODO
}
@deftype[JoinType]{
 TODO
}


TODO: Query and Join are not truly types right now.
They are simply predicates.
We should probably make them types for consistency.
