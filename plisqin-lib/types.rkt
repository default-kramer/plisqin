#lang racket/base

(require "./private2/_types.rkt")

(provide
 type? type-supertypes
 ; base types
 Token Scalar
 ; booleans
 Boolish Bit Bool
 ; non-scalar values
 Interval
 ; scalar values
 Datetime Number String
 ; other expressions
 Query Join Subquery
 )
