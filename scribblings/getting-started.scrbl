#lang scribble/manual
@(require (for-label plisqin
                     "racket.rkt"
                     plisqin/examples/video-rental-schema))
@(require scribble/eval
          plisqin
          rackunit
          "helpers.rkt"
          "racket.rkt")

@title[#:tag "getting-started"]{Getting Started}

@section{Project Status}
Plisqin is highly experimental and may introduce breaking changes at any time.

Plisqin's ideal scope is Create, Read, Update, and Delete (CRUD).
Currently, only Read is supported, and even this support is incomplete.
Missing features include top/limit, distinct, union, and window functions.

Out of scope is what I call "DBA-level SQL" such as @italic{create index}
which has never really been a pain point for me.

In its hypothetical final form, Plisqin is an alternate query language
supported natively by the RDBMS, bypassing SQL entirely.

@section{The Example Database Schema}
This documentation uses an example database schema, the
@secref{Video_Rental_Example_Schema} which models a brick-and-mortar video rental
store (think pre-Netflix). When you see an upper-case identifier such as @(racket Rental)
it almost certainly links to the documentation of this example schema, which might
help you if you don't understand a query.

TODO link to ERD.
TODO link to scripts (and fiddles, if applicable).

@section{Strings vs Procedures}
Here is a simple query in Plisqin:
@(racketblock
  (from r "Rental"
        (where r".RentalId > 100")))
The details of our database schema are in strings, namely the @(racket "Rental") table
and the @(racket "RentalId") column. An alternative style is to encode our database
schema into Racket procedures. Here is an [almost] equivalent query:
@(racketblock
  (from r Rental
        (where (RentalId r)" > 100")))
The details of this 2nd style won't be explained until the @secref["schema-as-procs"] section,
but any section of this guide might use either style, depending on what is convenient.

@section[#:tag "environment-setup"]{Environment Setup}
TODO it should be a simple matter of
1) install Racket
2) raco pkg install plisqin
3) start DrRacket and paste in a query