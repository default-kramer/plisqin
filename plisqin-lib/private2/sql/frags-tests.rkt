#lang racket

(module+ test
  (require "./check-sql.rkt"
           "../_types.rkt"
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

  ; round
  (check-sql (%%round "foo" 3)
             #:all "round(foo, 3)")
  (check-sql (%%round "foo")
             ; "round(foo)" is not valid MSSQL.
             ; So always include the zero:
             #:all "round(foo, 0)")
  (check-equal? (nullability (%%round (>> (%%sql "foo") #:null no)
                                      ; Make sure this zero doesn't introduce nullability
                                      0))
                no)

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
    (check-sql (cmp (or (and /lhs
                             (?? (%%sql "lhs") /lhs))
                        (%%sql "lhs"))
                    (or (and /rhs
                             (?? (%%sql "rhs") /rhs))
                        (%%sql "rhs")))
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


  ; == Datetime Math ==
  (define-syntax-rule (make-Datetime anything ...)
    (>> (%%scalar anything ...) #:cast Datetime #:null no))
  (check-equal? (nullability (hours 3))
                no)
  (check-sql (date+ (make-Datetime "now")
                    (hours 3)
                    (days 1))
             #:ms "dateadd(day, 1, dateadd(hour, 3, now))"
             #:pg "(now + interval '3 hour' + interval '1 day')"
             #:lite "datetime(now, '+3 hour', '+1 day')")
  (check-sql (date- (make-Datetime "now")
                    (hours 3)
                    (days 1))
             #:ms "dateadd(day, -1, dateadd(hour, -3, now))"
             #:pg "(now - interval '3 hour' - interval '1 day')"
             #:lite "datetime(now, '-3 hour', '-1 day')")
  (check-sql (date- (make-Datetime "now")
                    ; watch out for double negatives
                    (hours -3)
                    (days 1))
             #:ms "dateadd(day, -1, dateadd(hour, 3, now))"
             #:pg "(now - interval '-3 hour' - interval '1 day')"
             #:lite "datetime(now, '+3 hour', '-1 day')")
  ; Here's a tricky thing: date addition is not commutative.
  (check-sql (date+ (make-Datetime "Feb28")
                    (months 1)
                    (days 2))
             #:ms "dateadd(day, 2, dateadd(month, 1, Feb28))"
             #:pg "(Feb28 + interval '1 month' + interval '2 day')"
             #:lite "datetime(Feb28, '+1 month', '+2 day')")
  (check-sql (date+ (make-Datetime "Feb28")
                    (days 2)
                    (months 1))
             #:ms "dateadd(month, 1, dateadd(day, 2, Feb28))"
             #:pg "(Feb28 + interval '2 day' + interval '1 month')"
             #:lite "datetime(Feb28, '+2 day', '+1 month')")
  ; Intervals can be based on a column
  (check-sql (date- (make-Datetime "now")
                    (months (>> (%%sql "foo") #:cast Number #:null no))
                    (days (>> (%%sql "bar") #:cast Number #:null no)))
             #:ms "dateadd(day, -(bar), dateadd(month, -(foo), now))"
             #:pg "(now - ((foo) * interval '1 month') - ((bar) * interval '1 day'))"
             #:lite "datetime(now, -(foo) || ' month', -(bar) || ' day')")
  ; Repeat previous test with date+ instead of date-
  (check-sql (date+ (make-Datetime "now")
                    (months (>> (%%sql "foo") #:cast Number #:null no))
                    (days (>> (%%sql "bar") #:cast Number #:null no)))
             #:ms "dateadd(day, +(bar), dateadd(month, +(foo), now))"
             #:pg "(now + ((foo) * interval '1 month') + ((bar) * interval '1 day'))"
             #:lite "datetime(now, +(foo) || ' month', +(bar) || ' day')")
  )
