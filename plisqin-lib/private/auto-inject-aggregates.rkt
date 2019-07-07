#lang racket
(provide inject-aggregates)
(require "core.rkt" "util.rkt" "aggregate.rkt")
(module+ test
  (require rackunit "macros.rkt" "deduplicate.rkt"))
(define-syntax-rule (test forms ...)
  (module+ test
    (let ([a 0])
      forms
      ...)))

; The idea here is to transform an aggregate into an injection.
; For example, assuming that (Rentals-of/g item) is a grouped join, then this:
(void '(aggregate "sum("
                  (scalar (Rentals-of/g item)".PRICE")
                  ")"))
; Gets rewritten to:
(void '(inject [r (Rentals-of/g item)]
               (scalar "sum("
                       (scalar r".PRICE")
                       ")")))
; First, recall that a grouped join is a simply a join that contains a group-by clause:
(module+ test
  (define (make-grouped-join x)
    (RS join gj "GJ"
        (group-by (scalar gj".foo"))
        (join-on (scalar gj".foo")" = "x)))
  (check-true (grouped-join? (make-grouped-join 9))))
; The aggregate constructor does not allow more than one unique grouped join into an aggregate:
(test
 (check-exn
  exn:fail:plisqin:invalid-aggregate?
  (lambda ()
    (aggregate (make-grouped-join 1) (make-grouped-join 2)))))
; But duplication of the same grouped join is allowed as you would expect:
(test
 (check-not-exn
  (lambda ()
    (aggregate (make-grouped-join 1) (make-grouped-join 1)))))
; The single grouped join is called the "target" of the aggregate:
(test
 (check-equal?
  (find-target (aggregate (make-grouped-join 1) (make-grouped-join 1)))
  (make-grouped-join 1)))
; But an aggregate is not required to have a target:
(test
 (check-equal?
  (find-target (RS aggregate "count(*)"))
  #f))
; So to summarize, every aggregate has exactly 0 or 1 target.
; If it does not have a target, it simply gets changed to a scalar.
; If it does have a target, we use that as the target of the injection.
(define/contract (convert agg)
  (-> aggregate? (or/c scalar? injection?))
  (define target (find-target agg))
  ; Whether or not we inject it, we always change it to a scalar
  (set! agg (change-kind 'Scalar agg))
  (define (to-injection)
    (define placeholder (make-injection-placeholder target))
    (injection target
               placeholder
               (replace agg
                        target
                        placeholder)))
  (if target
      (to-injection)
      agg))
(test
 (define actual (RS convert (aggregate
                             "sum("(make-grouped-join 4)".foo)")))
 (define expected (RS inject [x (make-grouped-join 4)]
                      "sum("x".foo)"))
 (check-equal? (normalize actual) (normalize expected)))
; So now the algorithm becomes pretty simple.
; But there is a slight caveat regarding nesting.
; Consider this expression:
(void '(sum (sum (scalar (Rentals-of/g (Checkouts-of/g employee))".PRICE"))))
; It is a sum of sums. It's not the most intuitive way to get this number.
; I would probably favor writing it this way instead:
(void '(sum (scalar (Rentals-of/g employee)".PRICE")))
; But either way should work correctly. So let's consider the more complicated
; "sum of sums" example:
(void '(sum (sum (scalar (Rentals-of/g (Checkouts-of/g employee))".PRICE"))))
; The inner sum has a target of (Rentals-of/g (Checkouts-of/g employee)).
; So what is the target of the outer sum?
; When looking for a target, it finds an aggregate (the inner sum).
; It does not explore the aggregate except for the join-on clauses of its target.
; There it will find (Checkouts-of/g employee), which is the outer sum's target.
; Notice how this has a nice "balancing" property, where enclosing aggregates
; match with grouped joins that occur in other grouped joins' join-on clauses:
(test
 (check-equal?
  ; inner aggregate:
  (find-target (aggregate (RS make-grouped-join (make-grouped-join "emp"))))
  (make-grouped-join (RS make-grouped-join "emp")))
 (check-equal?
  ; outer aggregate:
  (find-target (aggregate (aggregate (make-grouped-join (RS make-grouped-join "emp")))))
  (RS make-grouped-join "emp")))
; So if we are working inwards from the root, as soon as we encounter an aggregate
; we convert it. But then we also need to check inside it for more aggregates.
(define (inject-aggregates root)
  ;(-> any/c any/c)
  (define (enter node)
    (cond
      [(aggregate? node)
       (return (convert node))]
      [else (return node)]))
  (walk root #:on-enter enter #:return 'root))
; And that's it! Too easy, right?


(test
 (define-syntax-rule (actual joiner)
   (RS from a "A"
       (joiner x (join x "X"
                       (group-by x".AID")
                       (join-on x".AID = "a".AID")))
       (define (y x) (join y "Y"
                           (group-by y".XID")
                           (join-on y".XID = "x".XID")))
       (select (aggregate "sum("
                          (aggregate "sum("
                                     (scalar (y x)".FOO")
                                     ")")
                          ")"))))
 (define-syntax-rule (expected joiner)
   (RS from a "A"
       (joiner x (join x "X"
                       (group-by x".AID")
                       (join-on x".AID = "a".AID")))
       (define (y x) (join y "Y"
                           (group-by y".XID")
                           (join-on y".XID = "x".XID")))
       (select (inject [x x]
                       "sum("
                       (inject [y (y x)]
                               "sum("
                               (scalar y".FOO")
                               ")")
                       ")"))))
 (check-equal?
  (normalize (inject-aggregates (actual define)))
  (normalize (expected define)))
 (check-equal?
  (normalize (inject-aggregates (actual join)))
  (normalize (expected join))))
