#lang racket
(require "util.rkt" "model.rkt")
(provide (all-defined-out))

(define-syntax-rule (def-contract NAME BODY)
  (begin
    (define NAME (flat-named-contract 'NAME BODY))
    (def-doc NAME (racketblock BODY))))

(def-contract join-type?
  (or/c 'InnerJoin
        'LeftJoin
        'CrossApply
        'OuterApply))

(def-contract sql-token?
  (or/c fragment?
        query?
        source?
        join?
        binding?
        injection?
        string?
        number?))

; It's convenient for the API to allow unflattened lists of tokens.
; This contract will accept an unflattened list of tokens and automatically flatten it.
(define token-list? (flattenof sql-token?))

(def-contract fragment-kind?
  (or/c 'Select
        'Where
        'JoinOn
        'GroupBy
        'OrderBy
        'Having
        'Scalar
        'Aggregate
        'Bool
        'Subquery
        'Sql))

; A value that can be added to a query.
(def-contract statement?
  (or/c fragment?
        join-type?))

; In the from and join macros, we allow lists of statements.
(def-contract statement-expr?
  (or/c statement? (listof statement?)))