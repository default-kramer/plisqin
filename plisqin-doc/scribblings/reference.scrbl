#lang scribble/manual

@(require (for-label "standard-label.rkt")
          "helpers.rkt"
          "tokens-skeleton.rkt"
          (except-in "tokens-custom-unsafe.rkt" doc)
          (except-in "tokens-custom-strict.rkt" doc)
          racket)

@title[#:style '(toc)]{Reference}

@subsubsub*section[#:tag "modules-and-prefixes"]{Modules and Prefixes}
@(begin
   (define (combine-lists list1 list2)
     ; given '(a b c) and '(1 2 3), return '(a 1 b 2 c 3)
     (if (pair? list1)
         (cons (car list1)
               (cons (car list2)
                     (combine-lists (cdr list1) (cdr list2))))
         (list)))
   (define (make-row-properties n)
     (if (= 0 n)
         (list)
         (list* 'top-border 'bottom-border (make-row-properties (sub1 n)))))

   (define-syntax (make-table stx)
     (syntax-case stx ()
       [(_ [module-id prefix-expr example-id content] ...)
        (with-syntax ([count (length (syntax->list #'(module-id ...)))])
          (syntax/loc stx
            (tabular
             #:row-properties (make-row-properties count)
             (combine-lists
              (list (list (bold (~a 'module-id))
                          (if (quote prefix-expr)
                              @nested{Prefix: @(racket prefix-expr)}
                              @nested{Prefix: none})
                          @nested{Example: @(racket example-id)})
                    ...)
              (list (list content 'cont 'cont) ...)))))]))
   )

When you run @(racketplainfont "raco pkg install plisqin"), you get the
following modules. Please note the conventional prefixes,
which are used throughout this documentation.
Also note that when you @(racket (require plisqin)), you get all the modules
listed below (prefixed) except for @(racket plisqin-examples/adventure-works).
@(make-table
  [plisqin-lib #f from @nested{
    Contains essential functionality.}]
  [plisqin-lib/types #f String? @nested{
    Contains data types, which are normal Racket values.
    You can use them as predicates and contracts.}]
  [plisqin-lib/dialect #f mssql @nested{
    Allows you to control which dialect of SQL gets generated.}]
  [plisqin-lib/strict #f where @nested{
    Contains the @tech{strict} variant of Plisqin.}]
  [plisqin-lib/strict/operators |.| .<= @nested{
    Contains the @tech{strict} operators (comparison and arithmetic).
    Note that these identifiers would conflict with frequently-used Racket
    identifiers, so they are prefixed with a period by convention.
    For example @(racket or) vs @(racket .or) and @(racket +) vs @(racket .+)}]
  [plisqin-lib/unsafe %% %%where @nested{
    Contains the @tech{unsafe} variant of Plisqin.}]
  [plisqin-lib/unsafe/operators %% %%<= @nested{
    Contains the @tech{unsafe} operators (comparison and arithmetic).}]
  [plisqin-examples/adventure-works aw: aw:show-table @nested{
    Allows you to access the AdventureWorks sample SQLite database.
    AdventureWorks is an imaginary company that sells bicycles and related products.}]
  )

@(local-table-of-contents)

@(include-section "plisqin-lib.scrbl")

@section{plisqin-lib/unsafe}
@(defmodule plisqin-lib/unsafe)
@(custom-unsafe-docs)
@(docgen/unsafe)

@section{plisqin-lib/loose}
This variant is on hold.
I think it should use the same signatures as the strict variant, except that
instead of requiring the programmer to prove the absence of errors, the loose
variant requires nothing and only gets in the way when it can prove the
presence of an error.

Specifically, that means that supertypes should pass for subtypes.
A proc that wants a Number should accept a Scalar, because a Scalar
might actually be a Number that just lost type information somwhere.
However, a String is definitely not a Number and the loose variant would
raise an error in that case.

For nullability the strict variant typically treats "maybe" as "yes".
The loose variant would treat "maybe" as "no".

@section{plisqin-lib/strict}
@(defmodule plisqin-lib/strict)
@(custom-strict-docs)
@(docgen/strict)

@(include-section "aw-reference.scrbl")
