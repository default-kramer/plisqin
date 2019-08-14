#lang reader "reader.rkt"
(module+ test
  (require rackunit)

  ; Pipes are not necessary when not in {braces}
  (define |a.b| "hi")
  (check-equal? a.b "hi")

  ; Make sure rest args work
  (define (test-rest-args a . rest)
    rest)
  (check-equal? (test-rest-args 1 2 3 4)
                '(2 3 4))
  (check-equal? (test-rest-args 1)
                '())

  ; Using |.| should bypass the "rest arg rewrite" rule
  (define (test-not-rest a |.| c)
    (list a |.| c))
  (check-equal? (test-not-rest 4 5 6)
                '(4 5 6))

  ; Make sure ellipses work
  (define-syntax-rule (ellipsis-test args ...)
    (list args ...))
  (check-equal? (ellipsis-test 9 8 7)
                '(9 8 7))

  (check-true {equal? 10.add1 11})
  {check-equal? 3.add1.add1 5}

  (check-equal? {42} 42)

  (check-equal? {(list 1 2 3).length.add1} 4)
  {check-equal? '(1 2 3).length.add1 4}

  ; These correctly error. How to test?
  ;(define a.b 3)
  ;{identity .procedure-arity}
  ;{(identity identity) .procedure-arity}
  ;{.add1 10}
  ;{.add1}

  (check-equal? {begin {begin {begin 10.add1}}} 11)
  (check-equal? (begin (begin {begin 10.add1})) 11)

  (check-true {41.add1{.* 2}{.equal? 84}})
  (check-true {41 .add1 {.* 2} {.equal? 84}})

  (check-true {if 41.add1{.* 2}{.equal? 84} #t #f})
  (check-true {if 41 .add1 {.* 2} {.equal? 84} #t #f})

  {check-true 41.add1 * 2 = 84}
  (check-true {if 41.add1 * 2 = 84 #t #f})

  ; OK, this is an error but the message is bad.
  ; (To be fair, if you didn't put a space before the dot it would be way more obvious that it's wrong)
  ;{10 .add1 * 9 .println}

  {check-equal? {10.add1 * 9}.~a "99"}

  {define {foo x} x + 10 * 10}
  (check-equal? {foo 4} 104)

  ; We can read decimal numbers
  {check-true {number? 10.5}}
  {check-true 10.5 = 10.5}
  {check-equal? {10 + .5} 10.5}
  {check-equal? 10 + 0.5 10.5}

  ; checks the fragment against the various dialects
  (define (check frag
                 #:pg [pg-expected #f]
                 #:ms [ms-expected #f]
                 #:lite [lite-expected #f]
                 #:all [all-expected #f])
    (set! pg-expected (or pg-expected all-expected))
    (set! ms-expected (or ms-expected all-expected))
    (set! lite-expected (or lite-expected all-expected))
    (define (check expected)
      (check-equal? (string-normalize-spaces (to-sql frag))
                    (string-normalize-spaces expected)))
    (when pg-expected
      (parameterize ([current-dialect (postgres)])
        (check pg-expected)))
    (when ms-expected
      (parameterize ([current-dialect (mssql)])
        (check ms-expected)))
    (when lite-expected
      (parameterize ([current-dialect (sqlite)])
        (check lite-expected))))

  (check
   {db-now + 3.hours - 1.day}
   #:pg "(current_timestamp + interval '3 hour' + interval '-1 day')"
   #:ms "dateadd(day, -1, dateadd(hour, 3, getdate()))"
   #:lite "datetime(datetime('now'), '+3 hour', '-1 day')")

  ; Here's a tricky thing: date addition is not commutative.
  ; Pretend db-now is Feb 1 to see.
  (check
   {db-now + 1.month + 29.days}
   #:pg "(current_timestamp + interval '1 month' + interval '29 day')"
   #:ms "dateadd(day, 29, dateadd(month, 1, getdate()))"
   #:lite "datetime(datetime('now'), '+1 month', '+29 day')")
  (check
   {db-now + 29.days + 1.month}
   #:pg "(current_timestamp + interval '29 day' + interval '1 month')"
   #:ms "dateadd(month, 1, dateadd(day, 29, getdate()))"
   #:lite "datetime(datetime('now'), '+29 day', '+1 month')")

  ; Intervals can be based on a column
  (check
   {db-now + (RS scalar "foo").months - (RS scalar "bar").days}
   #:pg "(current_timestamp + (foo * interval '1 month') + ((-bar) * interval '1 day'))"
   #:ms "dateadd(day, (-bar), dateadd(month, foo, getdate()))"
   ; SQLite - we can't have the '+' here because it won't work if the dynamic part is negative:
   #:lite "datetime(datetime('now'), foo || ' month', (-bar) || ' day')")


  ; These operators are binary with regard to SQL:
  (check
   {{RS scalar "foo"} + {RS scalar "bar"}}
   #:all "(foo + bar)")
  (check
   {{RS scalar "foo"} - {RS scalar "bar"}}
   #:all "(foo - bar)")
  (check
   {{RS scalar "foo"} * {RS scalar "bar"}}
   #:all "(foo * bar)")
  (check
   {{RS scalar "foo"} / {RS scalar "bar"}}
   #:all "(foo / bar)")
  (check
   {{RS scalar "foo"} = {val: "bar"}}
   #:all "(foo = 'bar')")
  (check
   {{val: "foo"} <> {RS scalar "bar"}}
   #:all "('foo' <> bar)")
  (check
   {{RS scalar "foo"} like {val: "bar"}}
   #:all "(foo like 'bar')")
  (check
   {{val: "foo"} not-like {RS scalar "bar"}}
   #:all "('foo' not like bar)")
  (check
   {{RS scalar "foo"} < {val: "bar"}}
   #:all "(foo < 'bar')")
  (check
   {{val: "foo"} <= {RS scalar "bar"}}
   #:all "('foo' <= bar)")
  (check
   {{RS scalar "foo"} > {val: "bar"}}
   #:all "(foo > 'bar')")
  (check
   {{val: "foo"} >= {RS scalar "bar"}}
   #:all "('foo' >= bar)")
  ; But if there is no SQL involved, it falls back to the built-in Racket
  ; version which allows more than 2 arguments.
  ; Add some tests to make sure I haven't broken it somehow.
  (check-equal? (+ 1 2 3) 6)
  (check-equal? (- 10 1 2) 7)
  (check-equal? (* 2 3 4) 24)
  (check-equal? (/ 1000 10 2) 50)
  (check-true (= 1 1 1))
  (check-false (= 1 1 2))
  (check-true (< 1 2 3))
  (check-false (< 1 2 2))
  (check-true (<= 3 4 4))
  (check-false (<= 3 4 3))
  (check-true (> 9 8 7))
  (check-false (> 9 8 8))
  (check-true (>= 9 9 8))
  (check-false (>= 9 9 10))


  ; Test combinations of limit, offset, and distinct.
  ; SQL Server with no offset uses "top" instead of "limit"
  (check
   (from x "X"
         (limit 5))
   #:all "select x.* from X x limit 5"
   #:ms "select top 5 x.* from X x")
  ; If offset and limit are both set, then SQL Server can't use "top"
  (check
   (from x "X"
         (limit 5)
         (offset 9))
   #:all "select x.* from X x limit 5 offset 9"
   #:ms "select x.* from X x offset 9 rows fetch next 5 rows only")
  ; Repeat the tests with distinct added
  (check
   (from x "X"
         (distinct #t)
         (limit 5))
   #:all "select distinct x.* from X x limit 5"
   #:ms "select distinct top 5 x.* from X x")
  (check
   (from x "X"
         (distinct #t)
         (limit 5)
         (offset 9))
   #:all "select distinct x.* from X x limit 5 offset 9"
   #:ms "select distinct x.* from X x offset 9 rows fetch next 5 rows only")


  ; Order-by
  (check
   {RS order-by 'desc "foo" ".bar"}
   #:all "foo.bar desc")
  (check
   {RS order-by desc "foo" ".bar"}
   #:all "foo.bar desc")
  (check
   {RS order-by 'asc "foo" ".bar"}
   #:all "foo.bar asc")
  (check
   {RS order-by asc "foo" ".bar"}
   #:all "foo.bar asc")
  (check
   {RS order-by "a" "b" "c"}
   #:all "abc")


  ; Make sure group-by doesn't somehow revert to racket's version
  (check
   {RS group-by "foo"}
   #:all "foo")

  ; String concatenation
  (check
   {{val: "a"} || {val: "b"}}
   #:ms "('a' + 'b')"
   #:all "('a' || 'b')")
  (check
   {(RS scalar "a") || (RS scalar "b") || {val: "c"}}
   #:ms "((a + b) + 'c')"
   #:all "((a || b) || 'c')")

  ; Case expression
  (check
   {case-when {RS scalar "foo"}
              {when 10 then 11}
              {when 12 then 13}
              {else 99}}
   #:all "case foo when 10 then 11 when 12 then 13 else 99 end")
  (check
   {case-when {when {RS bool "a"} then 1}
              {when {RS bool "b"} then 2}}
   #:all "case when a then 1 when b then 2 end")
  ; Repeat tests, but rely on {case args ...} -> {case-when args ...}
  (check
   {case {RS scalar "foo"}
     {when 10 then 11}
     {when 12 then 13}
     {else 99}}
   #:all "case foo when 10 then 11 when 12 then 13 else 99 end")
  (check
   {case {when {RS bool "a"} then 1}
     {when {RS bool "b"} then 2}}
   #:all "case when a then 1 when b then 2 end")
  ; A more Racket-like syntax when not using braces:
  (check
   (case-when #:of (RS scalar "foo")
              [10 11]
              [12 13]
              #:else 99)
   #:all "case foo when 10 then 11 when 12 then 13 else 99 end")
  (check
   (case-when [(RS bool "a") 1]
              [(RS bool "b") 2])
   #:all "case when a then 1 when b then 2 end")
  ; Racket's built-in case is still available
  (check-equal? (case (+ 7 5)
                  [(1 2 3) 'small]
                  [(10 11 12) 'big])
                'big)

  ; select as
  (check {RS from x "X"
             {select x".foo" as "FOO"}
             {select x".bar" as "BAR"}}
         #:all "select x.foo as FOO , x.bar as BAR from X x")
  (check {RS from x "X"
             {select x".foo" #:as "FOO"}}
         #:all "select x.foo as FOO from X x")

  ; and, or, not
  (check {RS where not "foo"}
         #:all "not foo")
  (check {RS where "1=1" and not "2=2" or not "3=3"}
         #:all "((1=1 and not 2=2) or not 3=3)")
  (check {RS where "1=1" and {not "2=2"} or {not "3=3"}}
         #:all "((1=1 and not 2=2) or not 3=3)")
  (check {RS where {and "1=1" {not "2=2"}} or {not "3=3"}}
         #:all "((1=1 and not 2=2) or not 3=3)")
  (check {RS where {or {and "1=1" {not "2=2"}} {not "3=3"}}}
         #:all "((1=1 and not 2=2) or not 3=3)")
  (check {RS where not "a=a" and "b=b"}
         #:all "(not a=a and b=b)")
  ; Make sure the Racket versions still work when not braced
  (check-equal? {identity (and 'foo 'bar)}
                'bar)
  (check-equal? {identity (or #f 'foo)}
                'foo)
  (check-equal? {identity (not 'foo)}
                #f)
  ; Regression:
  (check-equal?
   ; (Is this testing an implementation detail?
   '{x.Foo is null or x.Bar = 1}
   '{{or {is x.Foo null}
         {= x.Bar 1}}})

  ; String literal escaping:
  (check {val: "Tasita D'mour"}
         #:all "'Tasita D''mour'")

  ; {select bool-expr} conversion:
  (check {select {RS bool "1 = null"}}
         ; Note that null is preserved:
         #:ms "cast(case when 1 = null then 1 when not 1 = null then 0 end as bit)"
         #:all "1 = null")

  ; coalesce
  (check {RS scalar "foo" + "bar" ?? "baz"}
         #:all "coalesce((foo + bar), baz)")

  ; in
  (check {RS {scalar "foo"} in {scalar "bar"}}
         #:all "foo in bar")
  (check {RS {scalar "foo"} in {list: 1 2 3}}
         #:all "foo in (1, 2, 3)")
  (check {RS {scalar "foo"} not-in {list: "a" "b" "c"}}
         #:all "not foo in ('a', 'b', 'c')")
  (check {RS {scalar "foo"} not-in {from x "X"
                                         {select x".baz"}}}
         #:all "not foo in (select x.baz from X x)")

  ; End test submodule
  )