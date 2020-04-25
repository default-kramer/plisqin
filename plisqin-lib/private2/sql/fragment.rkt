#lang racket

(provide fragment% fragment? >>
         fragment-as-name fragment-nullability fragment-fallback)

(require "../token.rkt"
         racket/struct)

(define fragment%
  (class* token% (printable<%>)
    (inherit-field type as-name nullability fallback)
    (init-field kind
                id
                content
                reducer)
    (super-new)

    (define/override (change #:cast [type type]
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

    ; sql-token<%>
    (define/override (token-kind) kind)
    (define/override (token-content) content)
    (define/override (sql-token-reduce) (reducer content))

    ; equal<%>
    (define/override (equal-content)
      (list kind content type as-name))

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
