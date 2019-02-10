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
    (from t "Title"
          (where t".PrimaryTitle = "x".PrimaryTitle")
          (where t".TitleID < "x".TitleID")))
  (from t "Title"
        (where (exists (duplicates-of t))))
  )

If all the sources are @(racket '(Source "Title" "t" 0)) then @(racket t) and @(racket x) will
refer to the same thing. This is clearly wrong.

@section{On Parameters}
It's very convenient to be able to put raw SQL right into a clause, like this:
@(racketblock
  (from x "X"
        (where x".foo = 'bar'")))

But this comes with the risk of SQL injection.
Here is what I am thinking.
@(itemlist
  #:style 'ordered
  @item{Create a new data structure @(racket trusted-string).}
  @item{Tighten the contract (sql-token? probably) to replace @(racket string?) with @(racket trusted-string?).}
  @item{Create a Racket parameter that says what to do if a @(racket string?) gets passed in.
 Either error or automatically convert to an SQL parameter.
 (Oh yeah, I'll need to add parameters too.)}
  @item{Create a rewrite rule so that a syntax object @(racket #':) immediately left of
 a syntax object @(racket #'"string literal") gets rewritten to @(racket (trust "string literal")).}
  @item{Should we do the same for numbers? Probably...}
  )

Then you can do
@(racketblock
  (from x "X"
        {where x :".foo = 'bar'"}))

Which is almost as convenient.

If you don't have a string literal, you would have to do @(racket (trust the-string)).

I think that if @(racket (syntax->datum stx)) returns a @(racket string?)
then you know that stx is a string literal.