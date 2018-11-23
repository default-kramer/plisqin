#lang racket
(provide auto-inject)

(require "core.rkt" "util.rkt")
   
(module+ test
  (require rackunit)
  (require "macros.rkt"))

; For writing long comments that can include S-expressions
(define-syntax-rule (C FORMS ...)
  (void))

; Consider this manual injection:
(C (racketblock
    (join j "J"
          (group-by (scalar j".SOMETHING"))
          (join-on (inject j (scalar j".SOMETHING"))" = 42"))))

; This is such a common pattern, it would be nice to handle it automatically.
; I think we can - it seems that in a join-on clause we always want to inject:
; 1) The largest possible scalar
; 2) That encloses a self-reference to the current grouped-join
; 3) That does not contain aggregates or injections, ignoring subqueries
;     (except maybe subqueries' join-on clauses?)
; 4) That is resolved relative to the current grouped-join
;
; What does it mean for a token to be "resolved relative to" a given query?
; Perhaps it is easier to defined "unresolved" instead.
; Given a query Q, a source S is "unresolved relative to Q" if it was created
; outside of Q. For example, consider:
(C (racketblock
    (define (Rentals-of/g customer)
      (join r "Rental"
            (group-by (CustomerId r))
            (join-on (CustomerId r)" = "(CustomerId customer))))))

; In this example, the scalar @(CustomerId r) is resolved relative to the Rental join,
; because the source "r" was created inside the Rental join. If there are any inline joins
; hiding in this expression, they are probably resolved too! We can see that the only "foreign source"
; is @customer and that does not go into @(CustomerId r) - so any other sources in the expansion
; of @(CustomerId r) must have been created within the Rental join.

; However, the scalar @(CustomerId customer) is obviously not resolved relative to the Rental join.
; We can see that @customer must have been created outside the Rental join.


(struct source-info (source resolved?) #:transparent)
   
(define/contract (get-sources token queries)
  (-> any/c (listof (or/c query? join?)) (listof source-info?))
  ; accum is the list of source-infos being built up
  (define (visit node accum)
    (cond
      ; Manually recurse into subquery, with this node added to the list of queries in scope
      [(or (query? node)
           (join? node))
       (let ([children (get-sources node
                                    (cons node queries))])
         (return (no-recurse node) (append children accum)))]
      [(source? node)
       (let* ([resolved? (and (member node (map get-src queries))
                              #t)]
              [si (source-info node resolved?)])
         (return node (cons si accum)))]
      [else (return node)]))
  (walk token #:on-enter visit #:accum (list) #:return 'accum
        #:skip-root (or (join? token)
                        (query? token))))

(define/contract (all-resolved? infos)
  (-> (listof source-info?) boolean?)
  (andmap source-info-resolved? infos))

(define (is-resolved? token queries)
  (all-resolved? (get-sources token queries)))

(module+ test
  (begin
    (define foreign (source "f" "F"))
    (define q (from x "X"
                    (select x)))
    (check-true (is-resolved? q (list q)))

    (set! q (from x "X"
                  (select foreign)))
    (check-false (is-resolved? q (list q)))))

; This looks for an aggregate or injection within the given token.
; Subqueries are not searched, but subquery join-on clauses are.
(define (contains-aggregate-or-injection token)
  (define (visit node accum)
    ; accum will be #t while good, #f as soon as bad
    (cond
      [(or (not accum)
           (aggregate? node)
           (injection? node))
       (return (no-recurse node) #f)]
      [(query? node) (return (no-recurse node))]
      [else (return node)]))
  (car (walk token #:on-enter visit #:accum #f)))

(define/contract (can-inject token join)
  (-> any/c join? boolean?)
  (and #t
       (scalar? token)
       (let ([sources (get-sources token (list join))])
         (and (all-resolved? sources)
              ; must contain self-reference:
              (member (get-src join) (map source-info-source sources))))
       (not (contains-aggregate-or-injection token))
       #t))

(define/contract (walk-candidates join accum callback)
  (-> join? any/c procedure? any/c)
  (define (visit node accum)
    (cond
      [(query? node) (return (no-recurse node))]
      [(can-inject node join) (callback node accum)]
      [else (return node)]))
  (walk join #:on-enter visit #:accum accum))
   
; Find all auto-injectible scalars
(define/contract (find-candidates join)
  (-> join? (listof scalar?))
  (car (walk-candidates join (list)
                        (λ(node accum)
                          (return (no-recurse node)
                                  (cons node accum))))))

(test
 (define j (join j "J"
                 (group-by (scalar j".ONE"))
                 (join-on (scalar j".ONE")" = "(scalar "42"))
                 (join-on (scalar j".TWO")" = "(scalar "2"))))
 ; (scalar "2") is resolved, but does not contain a self-reference so it should not be included
 (define cands (find-candidates j))
 (check-equal? (length cands) 2))

; Just use the existing source as the placeholder.
; This relies on the fact that we know the target and the placeholder
; are both the source of the join. Seems solid, but might become shaky in the future?
(define/contract (convert-to-injection scalar join)
  (-> scalar? join? injection?)
  ; This would work, but it wouldn't be equal? to the way the inject macro does it:
  (void '(injection (get-src join) (get-src join) scalar))
  ; So let's do it this way instead:
  (let ([placeholder (make-injection-placeholder join)])
    (injection (get-src join)
               placeholder
               (replace scalar (get-src join) placeholder))))

(test
 (define J (source "j" "J"))
 (define j (join j J
                 (group-by (scalar j".ONE"))
                 (join-on (scalar j".ONE"))))
 (define cands (find-candidates j))
 (check-equal? (length cands) 1)
 (define orig (car cands))
 (convert-to-injection orig j)
 (void))

(test
 (define (A-of/s x)
   (join a "A"
         (join-on a".AID = "x".AID")))
 (define J (source "j" "J"))
 (define j (join j J
                 (group-by (scalar j".ONE"))
                 (join-on (scalar (A-of/s j)".ONE")" = 1")))
 (define cands (find-candidates j))
 (check-equal? (length cands) 1)
 (check-equal?
  (normalize (first cands))
  (normalize (scalar (A-of/s J)".ONE"))))

(define/contract (auto-inject2 join)
  (-> join? join?)
  (cdr (walk-candidates join #f
                        (λ(node accum)
                          (return (no-recurse (convert-to-injection node join)))))))

(define/contract (auto-inject x)
  (-> query? query?)
  (define (visit node)
    (cond
      [(grouped-join? node) (return (auto-inject2 node))]
      [else (return node)]))
  (walk x #:on-exit visit))

(test
 (define actual
   (from x "X"
         (join b "B"
               (group-by (scalar b".XID"))
               (join-on (scalar b".XID")" = "(scalar x".XID")))))
 (define expected
   (from x "X"
         (join b "B"
               (group-by (scalar b".XID"))
               (join-on (inject b b".XID")" = "(scalar x".XID")))))
 (check-equal?
  (normalize (auto-inject actual))
  (normalize expected)))

(test
 ; In this example, we group-by and join-on (foo b) which contains an inline join.
 ; That inline join belongs inside the grouped join, and should get injected into it.
 (define actual
   (from x "X"
         (join b "B"
               (define (A-of/s b) (join a "A"
                                        (join-on a".BID = "b".BID")))
               (define (foo b)
                 (scalar (A-of/s b)".FOO"))
               (group-by (foo b))
               (join-on (scalar (foo b))" = "x".FOO"))))
 (define expected
   (from x "X"
         (join b "B"
               (define (A-of/s b) (join a "A"
                                        (join-on a".BID = "b".BID")))
               (define (foo b)
                 (scalar (A-of/s b)".FOO"))
               (group-by (foo b))
               (join-on (inject b (foo b))" = "x".FOO"))))
 (check-equal?
  (normalize (auto-inject actual))
  (normalize expected)))