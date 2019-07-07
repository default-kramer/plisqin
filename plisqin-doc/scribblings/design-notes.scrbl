#lang scribble/manual
@(require (for-label plisqin-lib
                     "racket.rkt"
                     plisqin-lib/examples/video-rental-schema))
@(require scribble/eval
          plisqin-lib
          rackunit
          "helpers.rkt"
          "racket.rkt")

@title{Design Notes}

@section{String Literals}
Can #lang plisqin do better with string literals?
Consider
@(racketblock
  {where x.Foo like {val "bar%"}})

Using @(racket val) can get tedious.
Plus, if you get in the habit of using val you open yourself
to SQL injection if you accidentally use it on untrusted data.
What options do we have?

Maybe some convenient syntax like
@(racketblock
  {where x.Foo like :"bar%"})

It would be nice if it also worked for numeric literals.
Maybe customize the reader so that @(racketfont "#\"blah\"") reads as
@(racket (#%string-literal "blah")) and @(racketfont "#123") reads as
@(racket (#%numeric-literal 123)). This would override the default behavior of reading
Byte Strings, Vectors, and Graph Structure, but you are unlikely to need those in #lang plisqin.

The next question would then be the interpretation of @(racket (#%string-literal "blah")).
Is it raw SQL or a value?
And what if you want to put a value inside a larger piece of raw SQL?
(You can't put raw SQL inside a value, right?)

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

@section{The Reader - Lessons Learned}
This has tripped me up twice now, so let me write down my thoughts while they are fresh.
It seems that Scribble works using the output of @(racket read-syntax).
If you want things to get labeled properly, you need to make sure the syntax you produce
has an obvious correspondence to the original source code.
(It seems that @(racket srcloc) and @(racket prop) are important to this correspondence.)

Specifically, one thing I tried was having Racket's built-in @(racket read-syntax) return
and then wrapping dotted identifiers in something like @(racket (#%DOTS foo.bar foo bar))
thinking "this will be perfect, the label phase can see all 3 pieces of syntax I might want
to use. Then I can decide later which one I actually want." This approach doesn't work.
I'm guessing it is because Scribble doesn't know which one(s) I will end up using.

Also, customizing the readtable for @(racket #\.) wasn't the best approach either.
It requires a terminating macro which breaks other things, most notably @(racket 3.14) no longer
reads as a number. Also, it wasn't obvious how to know whether the most recent paren-shape was
braces or not. This meant that @(racket a.b) would read as @(racket a .b) everywhere, and I didn't
really want that outside of braces.

So now to handle dotted identifiers I just let Racket read everything as normal.
Then, before returning from @(racket read-syntax) I find dotted identifiers inside braces and split them
apart, for example @(racket {a.b}) becomes @(racket {a |.| b}) (but with correct srclocs).
This makes it easy for Scribble to figure out the correspondence.
Then later in module begin, I can rewrite @(racket {a |.| b}) to something like
@(racket {(#%do-dot a b)}).
(Scribble can probably handle this kind of rewrite, but best to keep things simple.)
