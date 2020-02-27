#lang racket

(provide fragment% fragment? >>
         fragment-as-name fragment-nullability fragment-fallback)

(require morsel-lib
         morsel-lib/sql
         racket/struct
         "../_null.rkt"
         "../_types.rkt")

(define never-quote<%>
  (interface* () ([prop:custom-print-quotable 'never])))

(define fragment%
  (class* object% (sql-token<%> equal<%> printable<%> typed<%> nulltrack<%> never-quote<%>)
    (init-field kind
                id
                content
                reducer
                ; type: (or/c #f type?)
                type
                ; as-name: perhaps (or/c #f symbol? string? Token)
                as-name
                ; nullability: nullability?
                [nullability maybe]
                ; fallback: (or/c #f fallback?)
                [fallback #f])
    (super-new)

    (define/public (change #:cast [type type]
                           #:as [as-name as-name]
                           #:null [nullability nullability]
                           #:fallback [fallback fallback])
      (new fragment%
           [kind kind]
           [id id]
           [content content]
           [reducer reducer]
           [type type]
           [as-name as-name]
           [nullability nullability]
           [fallback fallback]))

    ; typed<%>
    (define/public (get-type) type)
    (define/public (assign-type t)
      (change #:cast t))

    ; nulltrack<%>
    (define/public (get-nullability) nullability)
    (define/public (get-fallback) fallback)

    ; token<%>
    (define/public (token-kind) kind)
    (define/public (token-content) content)
    (define-token-aspect-stuff)
    (define/public (sql-token-reduce) (reducer content))

    ; equal<%>
    (define/public (equal-content)
      (list kind content type as-name))
    (define/public (equal-to? other recur)
      (recur (equal-content) (send other equal-content)))
    (define/public (equal-hash-code-of hasher)
      (hasher (equal-content)))
    (define/public (equal-secondary-hash-code-of hasher)
      (hasher (equal-content)))

    ; printable<%>
    (define/public (custom-print port mode)
      (fragment-printer this port mode))
    (define/public (custom-write port)
      (fragment-printer this port #t))
    (define/public (custom-display port)
      (fragment-printer this port #f))))

(define fragment? (is-a?/c fragment%))
(define fragment-id (class-field-accessor fragment% id))
(define fragment-content (class-field-accessor fragment% content))
(define fragment-as-name (class-field-accessor fragment% as-name))
(define fragment-nullability (class-field-accessor fragment% nullability))
(define fragment-fallback (class-field-accessor fragment% fallback))

(define fragment-printer
  (make-constructor-style-printer fragment-id fragment-content))

(define-syntax-rule (>> frag stuff ...)
  (send frag change stuff ...))
