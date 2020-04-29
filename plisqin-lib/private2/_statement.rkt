#lang racket

(provide define-statement compile-statements)
; Be careful not to provide too much here.
; In particular, parameters should be as opaque as possible.
; We don't want someone trying to branch based on a parameter because the
; branch condition will only be tested once during compile-statements, not
; each time the query is executed with a bound parameter value.
;
; The general problem of "what kinds of branching should be allowed" is a problem
; for the future, possibly for a #lang plisqin.


(require "./statement/define-statement.rkt"
         "./statement/compile-statements.rkt")
