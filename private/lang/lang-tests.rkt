#lang reader "reader.rkt"
(require rackunit)

; Use pipes to avoid the custom read of "."
(define |a.b| "hi")
(check-equal? |a.b| "hi")

(check-true {equal? 10.add1 11})

(check-equal? {42} 42)

(check-equal? {(list 1 2 3).length.add1} 4)
{check-equal? '(1 2 3).length.add1 4}

; These correctly error. How to test?
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
               #:lite [lite-expected #f])
  (when pg-expected
    (parameterize ([current-dialect (postgres)])
      (check-equal? (to-sql frag) pg-expected)))
  (when ms-expected
    (parameterize ([current-dialect (mssql)])
      (check-equal? (to-sql frag) ms-expected)))
  (when lite-expected
    (parameterize ([current-dialect (sqlite)])
      (check-equal? (to-sql frag) lite-expected))))

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