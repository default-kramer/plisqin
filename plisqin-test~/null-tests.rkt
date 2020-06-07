#lang racket

(require plisqin
         rackunit)

(define-syntax (check-nullability stx)
  (syntax-case stx ()
    [(_ [expected expr])
     #`(let* ([e expr]
              [actual (nullability e)])
         #,(syntax/loc (cadr (syntax->list stx))
             (check-equal? actual expected)))]))

(define-syntax-rule (check-nullabilities form ...)
  (begin
    (check-nullability form)
    ...))

; The type Number? is the most useful when working with the strict variant
(define :yes (>> (%%scalar "yes") #:null yes #:cast Number?))
(define :no (>> (%%scalar "no") #:null no #:cast Number?))
(define :maybe (>> (%%scalar "maybe") #:null maybe #:cast Number?))

(define :yesDT (>> :yes #:cast Datetime?))
(define :noDT (>> :no #:cast Datetime?))
(define :maybeDT (>> :maybe #:cast Datetime?))


(check-nullabilities
 ; Look through the documentation of plisqin-lib/{strict-or-unsafe} for anything
 ; that has an interesting nullchecking behavior and test it here.
 ; Try to follow the order of the documentation.

 ; == val ==
 [no (%%val 42)]
 [no (%%val "hello")]
 [no (val 42)]
 [no (val "world")]

 ; == ?? and coalesce ==
 [no    (%%?? :yes :no)]
 [maybe (%%?? :yes :maybe)]
 [yes   (%%?? :yes :yes)]
 [no    (%%coalesce :yes :no)]
 [maybe (%%coalesce :yes :maybe)]
 [yes   (%%coalesce :yes :yes)]
 [no    (?? :yes :no)]
 [maybe (?? :yes :maybe)]
 [yes   (?? :yes :yes)]
 [no    (coalesce :yes :no)]
 [maybe (coalesce :yes :maybe)]
 [yes   (coalesce :yes :yes)]

 ; == order-by ==
 [no    (%%order-by 'asc :no)]
 [no    (%%order-by 'desc :no)]
 [maybe (%%order-by 'asc :maybe)]
 [maybe (%%order-by 'desc :maybe)]
 [yes   (%%order-by 'asc :yes)]
 [yes   (%%order-by 'desc :yes)]
 [no    (order-by 'asc :no)]
 [no    (order-by 'desc :no)]
 [maybe (order-by 'asc :maybe)]
 [maybe (order-by 'desc :maybe)]
 [yes   (order-by 'asc :yes)]
 [yes   (order-by 'desc :yes)]

 ; == count ==
 [no    (%%count 'distinct :no)]
 [maybe (%%count 'distinct :maybe)]
 [yes   (%%count 'distinct :yes)]
 [no    (count 'distinct :no)]
 [maybe (count 'distinct :maybe)]
 [yes   (count 'distinct :yes)]

 ; == exists ==
 ; always produces a non-null boolean
 [no (%%exists "foo")]
 [no (%%exists (from x 'X))]
 [no (%%exists (subquery (from x 'X)))]
 [no (exists (from x 'X))]
 [no (exists (subquery (from x 'X)))]

 ; == subquery ==
 ; This is tricky... a subquery used in a scalar position should always be maybe,
 ; because we have know way of knowing what, for example, the subquery
 ;   (select Bar from Foo where FooId = -1)
 ; will produce. It could produce a non-null value, dbnull, or no value at all,
 ; which is equivalent to dbnull in this case.
 ; So the best we can do is say "maybe".
 ;
 ; If a subquery is used in a non-scalar position, for example,
 ;   (from x subquery ....)
 ; we shouldn't care about it's nullability.
 ; What about (%%where "foo.bar in "subquery)
 ; I guess if we had an `in` operator it would have to handle subqueries,
 ; but we don't yet, so this seems fine.
 [maybe (%%subquery :no)]
 [maybe (%%subquery :yes)]
 [maybe (%%subquery (from x 'X))]
 [maybe (subquery (from x 'X))]

 ; == round ==
 [no    (%%round :no)]
 [no    (%%round :no 2)]
 [maybe (%%round :maybe)]
 [maybe (%%round :maybe 2)]
 [yes   (%%round :yes)]
 [yes   (%%round :yes 2)]
 [no    (round :no)]
 [no    (round :no 2)]
 [maybe (round :maybe)]
 [maybe (round :maybe 2)]
 [yes   (round :yes)]
 [yes   (round :yes 2)]

 ; == intervals ==
 [no (%%years 1)]
 {no (%%months 1)}
 [no (%%days 1)]
 [no (%%hours 1)]
 [no (%%minutes 1)]
 [no (%%seconds 1)]
 [no (%%years :no)]
 [no (%%months :no)]
 [no (%%days :no)]
 [no (%%hours :no)]
 [no (%%minutes :no)]
 [no (%%seconds :no)]
 [maybe (%%years :maybe)]
 [maybe (%%months :maybe)]
 [maybe (%%days :maybe)]
 [maybe (%%hours :maybe)]
 [maybe (%%minutes :maybe)]
 [maybe (%%seconds :maybe)]
 [yes (%%years :yes)]
 [yes (%%months :yes)]
 [yes (%%days :yes)]
 [yes (%%hours :yes)]
 [yes (%%minutes :yes)]
 [yes (%%seconds :yes)]
 [no (years 1)]
 {no (months 1)}
 [no (days 1)]
 [no (hours 1)]
 [no (minutes 1)]
 [no (seconds 1)]
 [no (years :no)]
 [no (months :no)]
 [no (days :no)]
 [no (hours :no)]
 [no (minutes :no)]
 [no (seconds :no)]
 [maybe (years :maybe)]
 [maybe (months :maybe)]
 [maybe (days :maybe)]
 [maybe (hours :maybe)]
 [maybe (minutes :maybe)]
 [maybe (seconds :maybe)]
 [yes (years :yes)]
 [yes (months :yes)]
 [yes (days :yes)]
 [yes (hours :yes)]
 [yes (minutes :yes)]
 [yes (seconds :yes)]

 ; == date+ and date- ==
 [no    (%%date+ :noDT (years :no))]
 [maybe (%%date+ :maybeDT (years :no))]
 [maybe (%%date+ :noDT (years :maybe))]
 [yes   (%%date+ :yesDT (years :no))]
 [yes   (%%date+ :noDT (years :yes))]
 [no    (date+ :noDT (years :no))]
 [maybe (date+ :maybeDT (years :no))]
 [maybe (date+ :noDT (years :maybe))]
 [yes   (date+ :yesDT (years :no))]
 [yes   (date+ :noDT (years :yes))]
 [no    (%%date- :noDT (years :no))]
 [maybe (%%date- :maybeDT (years :no))]
 [maybe (%%date- :noDT (years :maybe))]
 [yes   (%%date- :yesDT (years :no))]
 [yes   (%%date- :noDT (years :yes))]
 [no    (date- :noDT (years :no))]
 [maybe (date- :maybeDT (years :no))]
 [maybe (date- :noDT (years :maybe))]
 [yes   (date- :yesDT (years :no))]
 [yes   (date- :noDT (years :yes))]

 ; TODO test the rest of plisqin-lib/{variant}/operators

 ; == is and is-not ==
 ; always produce a non-null boolean
 [no (%%is 'null 'null)]
 [no (%%is 'null :yes)]
 [no (%%is :yes 'null)]
 [no (%%is :yes :yes)]
 [no (%%is-not 'null 'null)]
 [no (%%is-not 'null :yes)]
 [no (%%is-not :yes 'null)]
 [no (%%is-not :yes :yes)]
 [no (.is 'null 'null)]
 [no (.is 'null :yes)]
 [no (.is :yes 'null)]
 [no (.is :yes :yes)]
 [no (.is-not 'null 'null)]
 [no (.is-not 'null :yes)]
 [no (.is-not :yes 'null)]
 [no (.is-not :yes :yes)]
 )
