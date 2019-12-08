#lang racket

(provide fragment%)

(require morsel-lib
         morsel-lib/sql
         racket/struct
         "../_types.rkt")

(define fragment%
  (class* object% (sql-token<%> equal<%> printable<%> typed<%>)
    (init-field kind id content reduction)
    (super-new)

    ; typed<%>
    (define-typed-stuff)

    ; token<%>
    (define/public (token-kind) kind)
    (define/public (token-content) content)
    (define-token-aspect-stuff)
    (define/public (sql-token-reduce) reduction)

    ; equal<%>
    (define/public (equal-content)
      (list (token-kind) (token-content)))
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

(define fragment-id (class-field-accessor fragment% id))
(define fragment-content (class-field-accessor fragment% content))

(define fragment-printer
  (make-constructor-style-printer fragment-id fragment-content))
