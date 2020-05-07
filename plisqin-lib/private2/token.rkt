#lang racket

(provide token% >>)

(require "_null.rkt"
         "_types.rkt"
         (for-syntax syntax/parse)
         (only-in (submod "_core.rkt" fragment-helper)
                  sql-token<%> define-token-aspect-stuff))

(define never-quote<%>
  (interface* () ([prop:custom-print-quotable 'never])))

(define token%
  (class* object% (sql-token<%> typed<%> nulltrack<%> equal<%> never-quote<%>)
    (init-field [type Token?]        ; (or/c #f type?) TODO - should probably not allow #f, use Token? instead
                [as-name #f]        ; perhaps (or/c #f symbol? string? Token?)
                [nullability maybe] ; nullability?
                [fallback #f]       ; (or/c #f fallback?)
                )
    (super-new)

    (abstract change)
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

(define-syntax (>> stx)
  (syntax-parse stx
    [(_ token
        (~alt (~optional (~seq #:cast Type))
              (~optional (~seq #:as as-name))
              (~optional (~seq #:null nullability))
              (~optional (~seq #:fallback fallback))) ...)
     #:declare Type (expr/c #'type?)
     #:declare as-name (expr/c #'(or/c symbol? string?))
     #:declare nullability (expr/c #'nullability?)
     #:declare fallback (expr/c #'fallback?)
     (syntax/loc stx
       (send token change
             (~? (~@ #:cast Type.c))
             (~? (~@ #:as as-name.c))
             (~? (~@ #:null nullability.c))
             (~? (~@ #:fallback fallback.c))))]))
