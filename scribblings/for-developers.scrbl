#lang scribble/manual

@title{For Developers}
This documentation is not written for a user of Plisqin, but you
might find something useful anyway.

@section{What Next?}
There are a lot of things I could try to do next.
@itemlist[
 @item{Work on parameters (see notes below).}
 @item{Can Plisqin be designed in such a way that it can generate SQL or directly integrate with Postgres?}
 @item{Is the previous question important enough to care about right now?}
 @item{Should I try direct integration with SQLite before trying Postgres?}
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
 @item{What does #lang plisqin look like?}
 @item{Can I satisfy people who have never seen (or hate) Lisp without sacrificing power?}
 @item{Can Plisqin provide a uniform API for different databases?}]

@(include-section "design-notes.scrbl")

@(include-section "tests.scrbl")