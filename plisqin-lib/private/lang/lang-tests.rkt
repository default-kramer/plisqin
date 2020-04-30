#lang racket

(require rackunit)
(check-equal? #f "TODO rewire these tests when ready")

(define-syntax-rule (check stuff ...)
  (void))

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
