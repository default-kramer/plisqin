#lang racket

(provide
 (contract-out
  [limit (-> (or/c #f nonnegative-integer?) Limit?)]
  [offset (-> (or/c #f nonnegative-integer?) Offset?)]
  [distinct (-> any/c Distinct?)]
  [join-type (-> (or/c #f 'inner 'left) JoinType?)]
  ))

(require (prefix-in m: (submod "../_core.rkt" special-clause-helper))
         "../token.rkt"
         "../_types.rkt"
         racket/struct
         )

(define clause%
  (class* token% (m:clause<%> printable<%>)
    (inherit-field type as-name nullability fallback)
    (init-field prop val name)
    (super-new)

    (define/override (change #:cast [type type]
                             #:as [as-name as-name]
                             #:null [nullability nullability]
                             #:fallback [fallback fallback])
      (new clause%
           [prop prop]
           [val val]
           [name name]
           [type type]
           [as-name as-name]
           [nullability nullability]
           [fallback fallback]))

    ; clause<%>
    (define/public (apply content)
      (m:content-set content prop val))

    ; sql-token<%>
    (define/override (token-kind) 'special-clause)
    (define/override (token-content) (list))
    (define/override (sql-token-reduce) (list))

    ; equal<%>
    (define/override (equal-content)
      (list prop val name))

    ; printable<%>
    (define/public (custom-print port mode)
      (clause-printer this port mode))
    (define/public (custom-write port)
      (clause-printer this port #t))
    (define/public (custom-display port)
      (clause-printer this port #f))))

(define clause-name (class-field-accessor clause% name))
(define clause-val (class-field-accessor clause% val))

(define clause-printer
  (make-constructor-style-printer
   clause-name
   (λ (me) (list (clause-val me)))))

(define-syntax-rule (define-clauses [id :type :prop] ...)
  (begin
    (define (id x)
      (new clause%
           [prop :prop]
           [type :type]
           [val x]
           [name 'id]))
    ...))

(define-clauses
  ; we use the m::properties from Morsel so that it can read what we write
  [limit Limit? m::limit]
  [offset Offset? m::offset]
  [distinct Distinct? m::distinct]
  [join-type JoinType? m::join-type])
