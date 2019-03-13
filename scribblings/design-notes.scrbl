#lang scribble/manual
@(require (for-label plisqin
                     "racket.rkt"
                     plisqin/examples/video-rental-schema))
@(require scribble/eval
          plisqin
          rackunit
          "helpers.rkt"
          "racket.rkt")

@title{Design Notes}

@section[#:tag "sources-need-uids"]{Why do sources need UIDs?}
For some reason I keep thinking "can I just have all UIDs be 0 by default and use something
like object equality?" And the answer is still NO. Consider this:

@(racketblock+eval
  #:eval my-eval
  (define (duplicates-of x)
    (RS from t "Title"
        (where t".PrimaryTitle = "x".PrimaryTitle")
        (where t".TitleID < "x".TitleID")))
  (from t "Title"
        (where (exists (duplicates-of t))))
  )

If all the sources are @(racket '(Source "Title" "t" 0)) then @(racket t) and @(racket x) will
refer to the same thing. This is clearly wrong.

@section{Language Configuration}
It would be nice if #lang plisqin was customizable.
Couldn't the reader recognize something like
@(racketblock
  (lang-options #:string-literals->sql #t
                #:dotted-application #t
                #:etc ...))
as the first datum after the #lang line?

Ideas
@itemlist[
 @item{Enable/disable converting all string literals to @(racket raw-sql).
  Maybe use @(racket #"byte-string-literals") as an escape, which would get converted to strings via @(racket bytes->string/utf8).}
 @item{Enable/disable dot handling "a.b" -> "(b a)"}
 @item{Enable/disable any of the rewrite rules.}
 ]

@section{Cardinality Tracking}
This is something I've always wanted from SQL.
@(racketblock
  (def/append! (Foo x)
    (join f Foo
          (join-on f.FooId = x.FooId)
          (assert-cardinality x))))
In the above example @(racket (assert-cardinality x)) is a new clause that says
"I (the programmer) think the cardinality of this join is x, give an error if you can't prove me right."
Plisqin would have to know that @(racket FooId) is unique in order to prove this.
There would also be something like @(racket trust-cardinality) which would say
"I (the programmer) am telling you the cardinality of this join is x, just take my word for it."

But... it might be difficult to make it smart enough to solve actual problems.