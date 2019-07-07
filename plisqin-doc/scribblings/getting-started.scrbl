#lang scribble/manual
@(require (for-label plisqin-lib
                     "racket.rkt"
                     plisqin-lib/examples/video-rental-schema))
@(require scribble/eval
          plisqin-lib
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

@hyperlink["https://github.com/default-kramer/plisqin/raw/master/examples/video-rental-diagram.PNG"]{This database diagram}
might be useful to have while reading this document.

If you want to follow along using a local database, the following scripts will create the tables:
@itemlist[
 @item{@hyperlink["https://github.com/default-kramer/plisqin/blob/master/examples/video-rental-schema.pg.sql"]{Scripts for PostgreSQL}}
 @item{@hyperlink["https://github.com/default-kramer/plisqin/blob/master/examples/video-rental-schema.ms.sql"]{Scripts for Microsoft SQL Server}}
 ]

@section{Strings vs Procedures}
Here is a simple query in Plisqin:
@(racketblock
  (RS from r "Rental"
      (where r".RentalId > 100")))
The details of our database schema are in strings, namely the @(racket "Rental") table
and the @(racket "RentalId") column. An alternative style is to encode our database
schema into Racket procedures. Here is an [almost] equivalent query:
@(racketblock
  (RS from r Rental
      (where (RentalId r)" > 100")))
The details of this 2nd style won't be explained until the @secref["schema-as-procs"] section,
but any section of this guide might use either style, depending on what is convenient.

@section[#:tag "environment-setup"]{Environment Setup}
To try out Plisqin, you need Racket installed.
You can download Racket @hyperlink["https://download.racket-lang.org/"]{here}.

Once you have Racket installed, run @racketplainfont{raco pkg install plisqin} from your command line.
This should download and setup the plisqin package.

Now start DrRacket.
You should have a file that contains one line: @racketplainfont{#lang racket}.
This line should be at the top of every Racket file you write.
Now paste the following code below @racketplainfont{#lang racket}
@(racketblock
  (require plisqin)
  (display (to-sql (from f "Foo"))))
When you run the program (Ctrl+R), you should see an SQL query:
@racketresultfont{select f.* from Foo f}