#lang racket

(module+ test
  (require "./check-sql.rkt"
           rackunit
           (only-in "../_null.rkt" nullability yes no maybe)
           (only-in "fragment.rkt" >>)
           (submod "frags.rkt" strict)
           (prefix-in |.| (submod "frags.rkt" strict operators))
           (prefix-in % (submod "frags.rkt" loose))
           (prefix-in % (submod "frags.rkt" loose operators))
           (prefix-in %% (submod "frags.rkt" unsafe))
           (prefix-in %% (submod "frags.rkt" unsafe operators)))

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
  )
