#lang racket

(module+ test
  (require "./check-sql.rkt"
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
  )
