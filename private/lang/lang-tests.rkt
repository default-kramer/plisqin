#lang reader "reader.rkt"
(require rackunit)

; Use pipes to avoid the custom read of "."
(define |a.b| "hi")
(check-equal? |a.b| "hi")

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

(check-true {41.add1(.* 2)(.equal? 84)})
(check-true {41 .add1 (.* 2) (.equal? 84)})

(check-true {if 41.add1(.* 2)(.equal? 84) #t #f})
(check-true {if 41 .add1 (.* 2) (.equal? 84) #t #f})

{check-true 41.add1 * 2 = 84}
(check-true {if 41.add1 * 2 = 84 #t #f})

; OK, this is an error but the message is bad.
; (To be fair, if you didn't put a space before the dot it would be way more obvious that it's wrong)
;{10 .add1 * 9 .println}

{check-equal? {10.add1 * 9}.~a "99"}

{define {foo x} x + 10 * 10}
(check-equal? {foo 4} 104)

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


; These operators are binary with regard to SQL:
(check
 {{scalar "foo"} + {scalar "bar"}}
 #:all "(foo + bar)")
(check
 {{scalar "foo"} - {scalar "bar"}}
 #:all "(foo - bar)")
(check
 {{scalar "foo"} * {scalar "bar"}}
 #:all "(foo * bar)")
(check
 {{scalar "foo"} / {scalar "bar"}}
 #:all "(foo / bar)")
(check
 {{scalar "foo"} = "bar"}
 #:all "(foo = 'bar')")
(check
 {"foo" <> {scalar "bar"}}
 #:all "('foo' <> bar)")
(check
 {{scalar "foo"} like "bar"}
 #:all "(foo like 'bar')")
(check
 {"foo" not-like {scalar "bar"}}
 #:all "('foo' not like bar)")
(check
 {{scalar "foo"} < "bar"}
 #:all "(foo < 'bar')")
(check
 {"foo" <= {scalar "bar"}}
 #:all "('foo' <= bar)")
(check
 {{scalar "foo"} > "bar"}
 #:all "(foo > 'bar')")
(check
 {"foo" >= {scalar "bar"}}
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
 {order-by 'desc "foo" ".bar"}
 #:all "foo.bar desc")
(check
 {order-by 'asc "foo" ".bar"}
 #:all "foo.bar asc")
(check
 {order-by "a" "b" "c"}
 #:all "abc")


; Make sure group-by doesn't somehow revert to racket's version
(check
 {group-by "foo"}
 #:all "foo")