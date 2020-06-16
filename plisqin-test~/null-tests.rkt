#lang racket

(require plisqin
         rackunit
         (for-syntax syntax/parse))

(define-syntax (check-nullability stx)
  (define (get-loc)
    (cadr (syntax->list stx)))
  (syntax-parse stx
    [(_ [#:err expr])
     (syntax/loc (get-loc)
       (check-exn exn:fail:contract?
                  (lambda () expr)))]
    [(_ [expected expr])
     (quasisyntax/loc (get-loc)
       (let* ([e expr]
              [actual (nullability e)])
         #,(syntax/loc (get-loc)
             (check-equal? actual expected))))]))

(define-syntax-rule (check-nullabilities form ...)
  (begin
    (check-nullability form)
    ...))

(define-syntax-rule (define-null-ctors [id n] ...)
  (begin
    (define-syntax (id stx)
      (syntax-parse stx
        [(_ type)
         #'(>> (%%scalar 'id) #:null n #:cast type)]
        [x:id
         ; Number? is the most useful type when working with the strict variant
         #'(x Number?)]))
    ...))

(define-null-ctors [:yes yes] [:no no] [:maybe maybe])


(check-nullabilities
 ; For the strict variant only, check that all incoming booleans must be non-null

 ; == where, having, join-on ==
 [#:err (where (:maybe Bool?))]
 [#:err (having (:maybe Bool?))]
 [#:err (join-on (:maybe Bool?))]
 [no (where (:no Bool?))]
 [no (having (:no Bool?))]
 [no (join-on (:no Bool?))]

 ; == and, or, not ==
 [#:err (.and (:maybe Bool?) (:no Bool?))]
 [#:err (.and (:no Bool?) (:maybe Bool?))]
 [#:err (.or (:maybe Bool?) (:no Bool?))]
 [#:err (.or (:no Bool?) (:maybe Bool?))]
 [#:err (.not (:maybe Bool?))]
 [no (.and (:no Bool?) (:no Bool?))]
 [no (.or (:no Bool?) (:no Bool?))]
 [no (.not (:no Bool?))]
 )

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
 [no    (%%date+ (:no Datetime?) (years :no))]
 [maybe (%%date+ (:maybe Datetime?) (years :no))]
 [maybe (%%date+ (:no Datetime?) (years :maybe))]
 [yes   (%%date+ (:yes Datetime?) (years :no))]
 [yes   (%%date+ (:no Datetime?) (years :yes))]
 [no    (date+ (:no Datetime?) (years :no))]
 [maybe (date+ (:maybe Datetime?) (years :no))]
 [maybe (date+ (:no Datetime?) (years :maybe))]
 [yes   (date+ (:yes Datetime?) (years :no))]
 [yes   (date+ (:no Datetime?) (years :yes))]
 [no    (%%date- (:no Datetime?) (years :no))]
 [maybe (%%date- (:maybe Datetime?) (years :no))]
 [maybe (%%date- (:no Datetime?) (years :maybe))]
 [yes   (%%date- (:yes Datetime?) (years :no))]
 [yes   (%%date- (:no Datetime?) (years :yes))]
 [no    (date- (:no Datetime?) (years :no))]
 [maybe (date- (:maybe Datetime?) (years :no))]
 [maybe (date- (:no Datetime?) (years :maybe))]
 [yes   (date- (:yes Datetime?) (years :no))]
 [yes   (date- (:no Datetime?) (years :yes))]

 ; The standard comparison operators will be tested separately, but
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

; Only the strict comparisons have interesting null check behavior:
; 1) They raise an error when the resulting boolean would be null.
; 2) They accept fallbacks.
(define-syntax-rule (test-comparisons cmp-id ...)
  (check-nullabilities
   [no (cmp-id :no :no)]
   ...
   [#:err (cmp-id :no :maybe)]
   ...
   [#:err (cmp-id :maybe :no)]
   ...
   [no (cmp-id :no (?? :maybe /void))]
   ...
   [no (cmp-id (?? :maybe /void) :no)]
   ...
   ))

(test-comparisons .= .<> .< .<= .> .>= .like .not-like)
