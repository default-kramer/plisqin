#lang racket

(module+ test
  (require "./check-sql.rkt"
           rackunit
           (only-in "../_null.rkt" nullability yes no maybe
                    /void /any /minval /maxval)
           (only-in "fragment.rkt" >>)
           (submod "frags.rkt" strict)
           (prefix-in |.| (submod "frags.rkt" strict operators))
           (prefix-in % (submod "frags.rkt" loose))
           (prefix-in % (submod "frags.rkt" loose operators))
           (prefix-in %% (submod "frags.rkt" unsafe))
           (prefix-in %% (submod "frags.rkt" unsafe operators)))

  ; TODO need to implement this
  (define (?? token fallback)
    (>> token #:fallback fallback))

  ; check bits and bools
  (check-sql (select (%%bit "foo"))
             ; no conversion
             #:all "foo")
  (check-sql (select (%%= 22 33))
             ; only MS requires Bool->Bit conversion
             #:ms "cast(case when (22 = 33) then 1 else 0 end as bit)"
             #:all "(22 = 33)")
  (check-sql (%%where (%%bit "bar"))
             ; all dialects require Bit->Bool conversion
             #:all "bar = 1")
  (check-sql (%%where (%%= 42 42))
             ; no conversion
             #:all "(42 = 42)")

  ; order by
  (check-sql (%%order-by 'desc "foo" ".bar")
             #:all "foo.bar desc")
  (check-sql (%%order-by 'asc "foo" ".bar")
             #:all "foo.bar asc")
  (check-sql (%%order-by "a" "b" "c")
             #:all "abc")

  ; coalesce
  (check-sql (%%coalesce "foo" "bar")
             #:all "coalesce(foo, bar)")
  (check-sql (%%coalesce "foo" "bar" "baz")
             #:all "coalesce(foo, bar, baz)")
  ; Nullability 1:
  ; If any arg is `no` then the result is `no`
  (check-equal? (nullability (%%coalesce (>> (%%sql "foo") #:null yes)
                                         (>> (%%sql "bar") #:null no)))
                no)
  (check-equal? (nullability (%%coalesce (>> (%%sql "foo") #:null no)
                                         (>> (%%sql "bar") #:null yes)))
                no)
  ; Nullability 2:
  ; Else If any arg is `maybe` then the result is `maybe`
  (check-equal? (nullability (%%coalesce (>> (%%sql "foo") #:null yes)
                                         (>> (%%sql "bar") #:null maybe)))
                maybe)
  (check-equal? (nullability (%%coalesce (>> (%%sql "foo") #:null yes)
                                         ; implicitly maybe:
                                         "bar"))
                maybe)
  ; Nullability 3:
  ; Else (all args are `yes`) then the result is `yes`
  (check-equal? (nullability (%%coalesce (>> (%%sql "foo") #:null yes)
                                         (>> (%%sql "bar") #:null yes)))
                yes)


  ; Test comparisons with fallbacks
  (define-syntax-rule (check-cmp cmp /lhs /rhs sql)
    (check-sql (cmp (?? (%%sql "lhs") /lhs)
                    (?? (%%sql "rhs") /rhs))
               #:all sql))
  (define-syntax-rule (check<= /lhs /rhs sql)
    (check-cmp %%<= /lhs /rhs sql))

  (check<= /void /void   "(lhs is not null and rhs is not null and (lhs <= rhs))")
  (check<= /void /minval "(lhs is not null and rhs is not null and (lhs <= rhs))")
  (check<= /void /maxval "(lhs is not null and (rhs is null or (lhs <= rhs)))")
  (check<= /void /any    "(lhs is not null and (rhs is null or (lhs <= rhs)))")
  (check<= /void #f      "(lhs is not null and (lhs <= rhs))")

  (check<= /minval /void   "(rhs is not null and (lhs is null or (lhs <= rhs)))")
  (check<= /minval /minval "(lhs is null or (rhs is not null and (lhs <= rhs)))")
  (check<= /minval /maxval "(lhs is null or rhs is null or (lhs <= rhs))")
  (check<= /minval /any    "(lhs is null or rhs is null or (lhs <= rhs))")
  (check<= /minval #f      "(lhs is null or (lhs <= rhs))")

  (check<= /maxval /void   "(lhs is not null and rhs is not null and (lhs <= rhs))")
  (check<= /maxval /minval "(lhs is not null and rhs is not null and (lhs <= rhs))")
  (check<= /maxval /maxval "(rhs is null or (lhs is not null and (lhs <= rhs)))")
  (check<= /maxval /any    "(rhs is null or (lhs is not null and (lhs <= rhs)))")
  (check<= /maxval #f      "(lhs is not null and (lhs <= rhs))")

  (check<= /any /void   "(rhs is not null and (lhs is null or (lhs <= rhs)))")
  (check<= /any /minval "(lhs is null or (rhs is not null and (lhs <= rhs)))")
  (check<= /any /maxval "(lhs is null or rhs is null or (lhs <= rhs))")
  (check<= /any /any    "(lhs is null or rhs is null or (lhs <= rhs))")
  (check<= /any #f      "(lhs is null or (lhs <= rhs))")

  (check<= #f /void   "(rhs is not null and (lhs <= rhs))")
  (check<= #f /minval "(rhs is not null and (lhs <= rhs))")
  (check<= #f /maxval "(rhs is null or (lhs <= rhs))")
  (check<= #f /any    "(rhs is null or (lhs <= rhs))")
  (check<= #f #f      "(lhs <= rhs)")

  ; TODO maybe optimize this to "(lhs is not distinct from rhs)" for Postgres:
  (check-cmp %%= /minval /minval "((lhs is null and rhs is null) or (lhs = rhs))")
  ; ... and think about "is distinct from" also.
  ; Also think about how "is [not] distinct from" only works for = and <>.
  ; Can any of the other operators produce this (#f #f #t) probe result?

  (check-cmp %%= /void /any "(lhs is not null and (rhs is null or (lhs = rhs)))")
  (check-cmp %%= /any /void "(rhs is not null and (lhs is null or (lhs = rhs)))")
  (check-cmp %%= /void #f   "(lhs is not null and (lhs = rhs))")
  (check-cmp %%= #f /void   "(rhs is not null and (lhs = rhs))")
  )
