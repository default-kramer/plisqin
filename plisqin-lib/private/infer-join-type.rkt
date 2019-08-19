#lang racket

(provide infer-join-type)

(require "core.rkt" "util.rkt")

; If a join has an explicitly set join-type, we use that.
; Otherwise we look at the join-on clauses.
; If we find a left join there, we infer 'left-join.
; Otherwise we infer 'inner-join.
; This represents the fact that we may have an inner join from
; ProductSubcategory to ProductCategory, but a left join from Product
; to ProductSubcategory (meaning: not every product is categorized).
; If we say (ProductCategory subcategory) we infer an inner join.
; If we say (ProductCategory (ProductSubcategory product)) we infer a left join,
; assuming that (ProductSubcategory product) was explicitly marked 'left-join.
(def/c (infer-join-type j)
  (-> join? join-type?)
  (let ([jt (s:join-type j)])
    (if (not (equal? jt 'infer-join-type))
        jt
        (infer-from-clauses j))))

(def/c (infer-from-clauses j)
  (-> join? join-type?)
  (define (visit node accum)
    ; accum will be #f until we see a left join (or left apply),
    ; then it will become #t and we are done
    (cond
      [accum
       (return (no-recurse node))]
      [(query? node)
       (return (no-recurse node))]
      [(join? node)
       (let* ([jt (infer-join-type node)]
              [is-left? (member jt '(left-join left-apply))])
         (return (no-recurse node)
                 is-left?))]
      [else (return node)]))
  (define is-left?
    (walk (s:join-clauses j) #:on-enter visit #:accum #f #:return 'accum))
  (if is-left?
      'left-join
      'inner-join))
