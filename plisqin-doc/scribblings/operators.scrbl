#lang scribble/manual
@(require (for-label plisqin-lib
                     plisqin-lib/operators
                     (except-in "racket.rkt" define
                                and or not)))

@title{Operators}
@(defmodule plisqin-lib/operators)

TODO get some examples going here.

TODO I think @(racket (require plisqin)) should provide the operators
with a prefix, and all of the documentation should follow that convention
so that for example @(racket :and) would link here.

@defproc[(and [a sql-token?] [b sql-token?]) sql-token?]{
 SQL "and" operator.
}

@defproc[(or [a sql-token?] [b sql-token?]) sql-token?]{
 SQL "or" operator.
}

@defproc[(not [x sql-token?]) sql-token?]{
 SQL "not" operator.
}

@(define-syntax-rule (def-comparisons [cmp-id ...] content ...)
   (deftogether
     ((defproc (cmp-id [a sql-token?] [b sql-token?]) bool?)
      ...)
     content ...))

@def-comparisons[[= <> like not-like is is-not in not-in < <= > >=]]{
 SQL comparison operators.

 TODO explain the fallback-to-Racket behavior, or get rid of it.
}

@(define-syntax-rule (def-math [math-id ...] content ...)
   (deftogether
     ((defproc (math-id [arg sql-token?] ...+) sql-token?)
      ...)
     content ...))

@def-math[[+ - * /]]{
 SQL math operators.

 TODO explain the fallback-to-Racket behavior, or get rid of it.

 TODO also explain intervals and date math.
}

@defproc[(|| [arg0 sql-token?] [arg1 sql-token?] [more sql-token?] ...) sql-token?]{
 SQL string concatenation.
}

@defproc[(?? [arg0 sql-token?] [more sql-token] ...) sql-token]{
 SQL coalesce.
}
