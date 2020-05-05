#lang racket

(provide from join instance? instanceof)

(module+ define-schema-helper
  (provide prop:instance))

(require (prefix-in m: (submod "_core.rkt" from-helper))
         "_core.rkt"
         "_types.rkt"
         (for-syntax syntax/parse))

(define-values (prop:instance custom-instance? --ignored--)
  (make-struct-type-property 'prop:instance))

(define (instance? x)
  (or (tuple? x)
      (join? x)
      (custom-instance? x)))

(define (instanceof t)
  (lambda (x)
    (and (instance? x)
         (equal? (get-queryable t)
                 (get-queryable x)))))

(define-for-syntax to-not-allowed
  (string-append
   "The `#:to` option is not allowed here."
   " This join is automatically linked to the enclosing query or join."))

(define-syntax (handle-statement stx-orig)
  (define-values (clause-contract stx)
    (syntax-case stx-orig ()
      [(_ #:from a)
       (values #'(or/c QueryClause (listof QueryClause)) #'a)]
      [(_ #:join a)
       (values #'(or/c JoinClause (listof JoinClause)) #'a)]))
  (syntax-parse stx
    #:literals (join define)
    [(join a b #:to c ...)
     (raise-syntax-error #f to-not-allowed stx)]
    [(join a b statement ...)
     ; Translate our `join` into Morsel's `attach`. This should only be used
     ; where `stx` is in "clause position"; otherwise Morsel will error.
     (syntax/loc stx
       (m:attach a b
                 (handle-statement #:join statement)
                 ...))]
    [(define stuff ...)
     ; Morsel recognizes `define` so just pass it through.
     stx]
    [clause-expr
     #:declare clause-expr (expr/c clause-contract)
     (syntax/loc stx
       (let ([c clause-expr.c])
         c))]))

(define-syntax-rule (wrap expr)
  (parameterize ([m:flatten-lists? #t])
    expr))

(define-syntax (from stx)
  (syntax-parse stx
    [(_ id:id queryable:expr statement ...)
     ; TODO need a contract on queryable
     (syntax/loc stx
       (wrap (m:from id queryable
                     (handle-statement #:from statement)
                     ...)))]))

(define-syntax (join stx)
  (syntax-parse stx
    [(_ id:id queryable:expr #:to raw-link statement ...)
     ; TODO need a contract on queryable
     #:declare raw-link (expr/c #'instance?)
     (syntax/loc stx
       (let ([link raw-link.c])
         (wrap (m:join id queryable #:to link
                       (handle-statement #:join statement)
                       ...))))]))
