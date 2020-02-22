#lang racket

(require rackunit)
(check-equal? #f "TODO rewire these tests when ready")

(define-syntax-rule (check stuff ...)
  (void))

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

; select as
(check {RS from x "X"
           {select x".foo" as "FOO"}
           {select x".bar" as "BAR"}}
       #:all "select x.foo as FOO , x.bar as BAR from X x")
(check {RS from x "X"
           {select x".foo" #:as "FOO"}}
       #:all "select x.foo as FOO from X x")

; String literal escaping:
(check {val: "Tasita D'mour"}
       #:all "'Tasita D''mour'")

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
