#lang racket

(require "./lib/core.rkt")
(require (only-in "../_core.rkt" query? join?))

(provide
 ; base types
 Token Scalar
 ; booleans
 Boolish Bit Bool
 ; scalar values
 Datetime Number String
 ; other expressions
 Subquery
 ; For now, make Query and Join predicates.
 ; Maybe there will be a reason to make them types later.
 (rename-out [query? Query]
             [join? Join]))

; On booleans
; ===
; We have
; * Bit
;   - a value in (0,1,null)
;   - all dialects requires Bit->Bool conversion
;        eg (where x:Bit) -> (where (<> 0 x))
; * Bool
;   - a value in (true,false,null)
;   - MSSQL (at least) requires Bool->Bit conversion
;        eg (select x:Bool) -> (select (case-when [x 1] [else 0]))
;
; For now, fragment-kind of 'scalar means injectable.
; TODO it would probably be better to make it a method like (send x injectable?)
; Every Bit should have kind 'scalar.
; If a DB supports true boolean columns, those should have type Bool and kind 'scalar.
; Every other Bool should have kind 'bool, because we don't want to try to
; inject things like (= foo bar) in some dialects but not others.

(define-types
  ; base types
  [Token]
  [Scalar Token]

  ; booleans
  [Boolish Scalar]
  [Bit Boolish]
  [Bool Boolish]

  ; scalar values
  [Datetime Scalar]
  [Number Scalar]
  [String Scalar]

  ; other expressions
  [Subquery Token])
