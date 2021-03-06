#lang scribble/manual

@(require (for-label "standard-label.rkt")
          "helpers.rkt")

@title{Read Me First}

@section{What is Plisqin?}
@(itemlist
  @item{Plisqin is a Racket library that helps you rearrange fragments of
 SQL queries in a coherent way.}
  @item{Plisqin is a query language for relational databases that offers
 better semantics than SQL.})

Both of these statements are a valid mental model.
If you are very familiar with SQL, you might prefer the former;
if not, you will probably prefer the latter.
Eventually you will find both mental models useful.

Also: @(itemlist
        @item{Plisqin is a @seclink["research-language"]{research language.}})

As far as I know, my ideal query language has not yet been developed.
Plisqin is a step in the right direction.

@section{Installation}
@(itemlist
  #:style 'ordered
  @item{Intall Racket. If you've never used Racket before, follow
 @hyperlink["https://docs.racket-lang.org/quick/index.html"]{these instructions}
 to install Racket and learn the basics}
  @item{From your shell, run @racketplainfont{raco pkg install plisqin}}
  @item{Using @racketplainfont{#lang racket}, verify that the following code works:})
@(racketblock
  (require plisqin
           (prefix-in aw: plisqin-examples/adventure-works)
           plisqin-examples/adventure-works/schema))
@(repl-query
  (aw:show-table
   (from p Product
         (limit 3)
         (select (ProductName p))
         (select (CategoryName p))
         (select (TotalSales p))
         (order-by 'desc (TotalSales p)))))

Click on "Show SQL" and you might be surprised!
The generated SQL is much larger than the Plisqin query.
This may seem like "too much magic" right now, but once you learn how it works
it is actually pretty formulaic.

@section{Motivation}
During my software development career, many of my tasks could be described
as "fetch data from an SQL database and display that data."
Some tasks have very little display logic, such as a report that simply
shows an SQL result set verbatim.
Other tasks have more complicated display logic, such as a web page that shows
a musical artist, their albums, and the track listings for each album.
But regardless of how complex the display logic might be, I found that my favorite
strategy for these tasks is usually
@(itemlist
  #:style 'ordered
  @item{Imagine the ideal result set(s) for the task.}
  @item{Use SQL to produce those result sets.}
  @item{Use something else to display the results.})

It is step 2 that Plisqin aims to improve.
SQL and all the SQL alternatives I have tried are lacking in some way.
I always end up duplicating fragments of queries and joins all over the place.
Using Plisqin allows me to reduce duplication to what I suspect is the theoretical minimum.

@section{Project Scope}
Plisqin is now stable but incomplete.

Plisqin's ideal scope includes all "application-level SQL", basically equivalent
to the CRUD operations (create, read, update, delete).
All "DBA-level SQL", such as creating tables, indexes, logins, etc... is
explicitly a non-goal.

Right now Plisqin only supports queries.
Create, update, and delete operations are not provided yet.
If you have an urgent need, please
@hyperlink["https://github.com/default-kramer/plisqin"]{open an issue}.

@section{Variants}
Plisqin has two @deftech{variants}.
The @deftech{unsafe} variant does very little argument validation and may
allow SQL injection if you are not careful.
The @deftech{strict} variant does much more argument validation and protects
against SQL injection.
By convention, everything that is unsafe is @(racket require)d with the prefix
@(racket %%), for example @(racket %%join-on) or @(racket %%+).
You can mix and match variants at will, as the following example demonstrates:
@(repl-query
  (aw:show-table
   (from cat ProductCategory
         (select (Name cat))
         (%%select "substr("(Name cat)", 2) as SubstringDemo"))))

I suggest that you use the strict variant most of the time, and resort to the
unsafe variant only when the strict variant fails you.
But using only the unsafe variant is also a valid approach, especially in
situations where SQL injection is impossible such as static SQL generation.

@section{Usage Patterns}
When and how does Plisqin generate SQL?
What am I supposed to do with the SQL after it is generated?
Plisqin is flexible, and can be used:
@(itemlist
  @item{As a static SQL generator.
 Simply use Plisqin's @(racket to-sql) and Racket's file IO to generate SQL files,
 then do whatever you want with those SQL files.
 Any application that consumes the SQL isn't even aware that Plisqin exists.}
  @item{As a "Racket-aware" static SQL generator.
 Use @(racket compile-statements) to generate the SQL and hide it inside a
 normal Racket procedure. This procedure cooperates with the
 @link["https://docs.racket-lang.org/db/index.html"]{db module}
 so that your Racket application can fetch data at runtime.}
  @item{As a dynamic SQL generator.
 In this situation, you construct queries and call @(racket to-sql) at runtime.
 I have not tested this usage pattern and don't recommend it,
 but sometimes it is unavoidable.
 Be very careful -- even using the @tech{strict} variant to avoid SQL injection
 does not guarantee that you are safe.
 For example, an attacker might be able to construct a computationally expensive
 query that renders your database unresponsive.})

@section{What Should I Read Next?}
You might want to peek at @(secref "modules-and-prefixes") to get the lay of
the land and see some other prefix conventions that this documentation uses.
Then continue reading at @(secref "using-define-schema").
