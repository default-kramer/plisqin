#lang racket

(require "./lib/core.rkt")

(provide Token Scalar
         Bool Datetime Interval Number String
         Query Join Subquery)

(define-types
  [Token]
  [Scalar Token]

  [Bool Token]
  [Datetime Scalar]
  [Interval Token]
  [Number Scalar]
  [String Scalar]

  [Query Token]
  [Join Token]
  [Subquery Token])
