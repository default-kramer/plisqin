#lang racket
(require (prefix-in racket: racket))

; This module is required by #lang plisqin by default.

(require plisqin)
(provide (all-from-out plisqin))

(require plisqin/private/operators)
(provide (all-from-out plisqin/private/operators))

; TODO still not sure if overriding Racket's "second" is wise...
(require (submod plisqin/private/core time-units))
(provide (all-from-out (submod plisqin/private/core time-units)))

(require plisqin/private/dialect)
(provide (all-from-out plisqin/private/dialect))

(require plisqin/private/show-table)
(provide (all-from-out plisqin/private/show-table))

; Special version of "case" only for #lang plisqin.
; Curly braces {case args ...} means {case-when args ...}
; Otherwise it refers to case from racket/base.
(define-syntax (case stx)
  (define shape
    (syntax-property stx 'paren-shape))
  (syntax-case stx ()
    [(_ args ...)
     (if (equal? shape #\{)
         #'{case-when args ...}
         #'(racket:case args ...))]))
(provide case)
