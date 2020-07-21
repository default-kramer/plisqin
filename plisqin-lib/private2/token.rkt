#lang racket

(provide token%
         (contract-out [>> contract:>>]))

(require "_null.rkt"
         "_types.rkt"
         (for-syntax syntax/parse)
         (only-in (submod "_core.rkt" fragment-helper)
                  sql-token<%> define-token-aspect-stuff))

(define never-quote<%>
  (interface* () ([prop:custom-print-quotable 'never])))

(define token%
  (class* object% (sql-token<%> typed<%> nulltrack<%> equal<%> never-quote<%>)
    (init-field [type Token?]       ; type?
                [as-name #f]        ; perhaps (or/c #f symbol? string? Token?)
                [nullability maybe] ; nullability?
                [fallback #f]       ; (or/c #f fallback?)
                )
    (super-new)

    (abstract change)
    ; #f means "do not change"; this means that once `as-name` or `fallback`
    ; becomes non-false, you can't use `change` to revert them.
    ; But so far I haven't needed to do that, so wait for a real need before
    ; worrying about that.
    #;(change #:cast [type type]
              #:as [as-name as-name]
              #:null [nullability nullability]
              #:fallback [fallback fallback])

    ; typed<%>
    (define/public (get-type) type)
    (define/public (assign-type t)
      (change #:cast t))

    ; nulltrack<%>
    (define/public (get-nullability) nullability)
    (define/public (get-fallback) fallback)

    ; sql-token<%>
    (define-token-aspect-stuff)
    (abstract token-kind
              token-content
              sql-token-reduce)

    ; equal<%>
    (abstract equal-content)
    (define/public (equal-to? other recur)
      (recur (equal-content) (send other equal-content)))
    (define/public (equal-hash-code-of hasher)
      (hasher (equal-content)))
    (define/public (equal-secondary-hash-code-of hasher)
      (hasher (equal-content)))
    ))

; Unfortunately not every Token? is a changeable-token? right now.
; (Specifically, queries and joins are not.)
(define (changeable-token? x)
  (is-a? x token%))

; The contract of >>
(define contract:>>
  (->* (changeable-token?)
       (#:cast type? #:as (or/c symbol? string?)
        #:null nullability? #:fallback fallback?)
       changeable-token?))

(define (>> token
            #:cast [type #f]
            #:as [as-name #f]
            #:null [nullability #f]
            #:fallback [fallback #f])
  (send token change
        #:cast type
        #:as as-name
        #:null nullability
        #:fallback fallback))
