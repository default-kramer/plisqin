#lang scribble/manual

@title{For Developers}
This documentation is not written for a user of Plisqin, but you
might find something useful anyway.

@section{Needed for 0.2}
@itemlist[
 @item{Remove number? from sql-token? - need param or val.}
 @item{raw-sql NEEDS to look different than a regular string in error messages}
 @item{and, or, not}
 @item{the literal rewrites (or syntax parameters) of 'asc 'desc 'null}
 @item{something for sql-null}
 @item{Lots of testing needed - do rebuild views / SPs}
 @item{Lots of missing documentation}
 @item{Document the 3 modes (offline SQL gen, compile-time gen, runtime-gen (eager or lazy)).}
 ]

@section{Nice to have for 0.2}
@itemlist[
 @item{Compile-time SQL generation for Racket programs}
 @item{as-name inference}
 @item{extend! and maybe def/append! could maybe detect infinite recursion and other errors?}
 @item{Support for update and delete}
 @item{Overhaul query printing / that to-list stuff}
 @item{rename binding? to attached-join?}
 @item{Split core lib from extras, like the examples}
 ]

@section{What Else}
@itemlist[
 @item{What type system should Plisqin have? Dynamic, sure, but what about...}
 @item{Can @(racket (scalar x".Foo")) be used in addition? (A: Maybe?)}
 @item{Can @(racket (scalar x".Foo" #:typed-as 'decimal)) be used in addition? (A: Always.)}
 @item{Can @(racket (scalar x".Foo" #:typed-as 'varchar)) be used in addition? (A: Never.)}
 @item{If I integrate with Postgres/SQLite, could it detect the type errors
  after I translate the query to its native format?}
 @item{Would I want it to? Probably not... I can't imagine the semantics being similar enough.}
 @item{How should Plisqin handle NULL?}
 @item{Some aggregate operations have reasonable answers for empty grouped joins
  (specifically @(racket sum) and @(racket count) which should return 0.)
  But what about other aggregates? What is the average of an empty set?
  Force the programmer to specify what they want?}
 @item{Can I satisfy people who have never seen (or hate) Lisp without sacrificing power?}
 @item{Can Plisqin provide a uniform API for different databases?}]

@(include-section "design-notes.scrbl")

@(include-section "tests.scrbl")
