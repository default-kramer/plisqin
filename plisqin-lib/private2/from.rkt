#lang racket

(provide from join instance? instanceof)

(module+ define-schema-helper
  (provide prop:instance prop:trusted-queryable))

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
  (let* ([queryable (get-queryable t)]
         [ctc (coerce-flat-contract 'instanceof queryable)])
    (flat-named-contract
     ; Can't pass in 'Foo directly, it will print as (instanceof Foo) and we
     ; want (instanceof 'Foo). That's why we needed to use coerce-flat-contract
     (build-compound-type-name 'instanceof ctc)
     (lambda (x)
       (and (instance? x)
            (equal? queryable
                    (get-queryable x)))))))

; This prop is just a marker interface that allows the tables from
; define-schema to pass the `from` and `join` contracts.
(define-values (prop:trusted-queryable trusted-queryable? --ignored2--)
  (make-struct-type-property 'prop:trusted-queryable))

(define (from-queryable? x)
  (or (symbol? x)
      (trusted-queryable? x)
      (Subquery? x)
      (query? x)))

(define (join-queryable? x)
  (or (symbol? x)
      (trusted-queryable? x)
      (Subquery? x)
      (join? x)))

(define-for-syntax to-not-allowed
  (string-append
   "The `#:to` option is not allowed here."
   " This join is automatically linked to the enclosing query or join."))

(define-syntax (handle-statement stx-orig)
  (define-values (clause-contract stx macro-name)
    (syntax-case stx-orig ()
      [(_ #:from a)
       (values #'(or/c QueryClause? (listof QueryClause?)) #'a 'from)]
      [(_ #:join a)
       (values #'(or/c JoinClause? (listof JoinClause?)) #'a 'join)]))
  (syntax-parse stx
    #:literals (join define)
    [(join a b #:to c ...)
     (raise-syntax-error #f to-not-allowed stx)]
    [(join id queryable statement ...)
     #:declare queryable (expr/c #'join-queryable?)
     ; Translate our `join` into Morsel's `attach`. This should only be used
     ; where `stx` is in "clause position"; otherwise Morsel will error.
     (syntax/loc stx
       (m:attach id (preformat queryable.c)
                 (handle-statement #:join statement)
                 ...))]
    [(define stuff ...)
     ; Morsel recognizes `define` so just pass it through.
     stx]
    [clause-expr
     #:declare clause-expr (expr/c clause-contract #:macro macro-name)
     (syntax/loc stx
       (let ([c clause-expr.c])
         c))]))

(define-syntax-rule (wrap expr)
  (parameterize ([m:flatten-lists? #t])
    expr))

(define (preformat queryable)
  ; no preformatting needed anymore, but it's better to enforce the
  ; queryable's contract sooner rather than later
  queryable)

(define-syntax (from stx)
  (syntax-parse stx
    [(_ id:id queryable statement ...)
     #:declare queryable (expr/c #'from-queryable?)
     (syntax/loc stx
       (wrap (m:from id (preformat queryable.c)
                     (handle-statement #:from statement)
                     ...)))]))

(define-syntax (join stx)
  (syntax-parse stx
    [(_ id:id queryable #:to raw-link statement ...)
     #:declare queryable (expr/c #'join-queryable?)
     #:declare raw-link (expr/c #'instance?)
     (syntax/loc stx
       (let ([link raw-link.c])
         (wrap (m:join id (preformat queryable.c)
                       #:to link
                       (handle-statement #:join statement)
                       ...))))]))
