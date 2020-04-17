#lang scribble/manual

@(require (for-label plisqin "racket.rkt")
          "helpers.rkt")

@title{plisqin-lib}
TODO explain that most of the good stuff is in strict, loose, and unsafe.

@(defmodule plisqin-lib)
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

@defproc[(nullability [x any/c]) (or/c nullability? #f)]{
 If @(racket x) is a token, returns its @tech{nullability}.
 Otherwise returns false.
 @(repl
   (eval:check (nullability (%%sql "foo"))
               maybe)
   (eval:check (nullability "not a token")
               #f))
}
