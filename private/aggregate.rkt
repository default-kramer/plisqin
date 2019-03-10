#lang racket
(require "util.rkt" "core.rkt" "stack-tracker.rkt")
(provide aggregate find-target)

; An aggregate will have at most 1 target.
; We will enforce this in a constructor procedure later.
(define/contract (find-target agg)
  (-> aggregate? (or/c attached-join? join? #f))
  (match (find-targets (s:fragment-tokens agg))
    [(list x) x]
    [(list) #f]
    [else (error "assert fail: somehow an aggregate was constructed with more than 1 target")]))

; Tests and better explanation of this are in "auto-inject-aggregates.rkt"
(define/contract (find-targets root)
  (-> any/c (listof (or/c attached-join? join?)))
  (define (enter node accum)
    (cond
      ; Don't explore subqueries
      [(query? node)
       (return (no-recurse node))]
      ; An aggregate has at most 1 target.
      ; If it does, we look in its join-on clauses
      [(aggregate? node)
       (let* ([target (find-target node)]
              [join (if (attached-join? target)
                        (attached-join-join target)
                        target)]
              [join-clauses (if (join? join)
                                (s:join-clauses join)
                                '())])
         (return (no-recurse node)
                 (append (find-targets join-clauses) accum)))]
      [(or (grouped-join? node)
           (and (attached-join? node)
                (grouped-join? (attached-join-join node))))
       (return (no-recurse node) (cons node accum))]
      [else (return node)]))
  (remove-duplicates
   (walk root #:on-enter enter #:accum '() #:return 'accum)))

(define/contract (make-msg targets counter)
  (-> (listof token?) integer? string?)
  (match targets
    [(list) ""]
    [(list token rest ...)
     (define stack (callstack-get token))
     (define msg
       (if (empty? stack)
           (format "~e" token)
           (string-join (map (λ(stx) (format "  ~a" stx)) stack)
                        "\n")))
     (string-append
      (format "\nTarget ~a was:\n~a" counter msg)
      (make-msg rest (add1 counter)))]))

(define/contract (aggregate . tokens)
  (fragment-contract aggregate?)
  (let* ([targets (find-targets tokens)]
         [num-targets (length targets)])
    (if (num-targets . > . 1)
        (raise (exn:fail:plisqin:invalid-aggregate
                (format "Aggregate expression has ~a targets. At most one target is allowed.~a"
                        num-targets
                        (make-msg (reverse targets) 1))
                (current-continuation-marks)))
        (apply make-aggregate tokens))))