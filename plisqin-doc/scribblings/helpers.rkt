#lang racket

(provide my-eval reset-eval! reset-uid make-eval
         show-sql check-sql check-sql2 rb
         to-table)

(require scribble/manual
         scribble/eval
         plisqin-lib
         rackunit
         "to-table.rkt")
(require (for-syntax syntax/strip-context
                     plisqin-lib/private/util))

(define (make-eval)
  (let ([eval (make-base-eval)])
    (interaction-eval #:eval eval
                      (require racket/match)
                      (require (only-in racket string-join))
                      (require plisqin))
    eval))

(define my-eval (make-eval))

(define (reset-eval!)
  (set! my-eval (make-eval)))
(reset-eval!)

(define (reset-uid)
  (my-eval '(reset-uid-for-testing!)))

(define (show-sql str)
  (verbatim str))

; use a macro here so that DrRacket takes you right to the failure
(define-syntax-rule (check-sql eval q str)
  (let ([eval-sql
         (with-handlers ([exn? (λ(ex) "")])
           (eval '(to-sql q)))])
    (check-equal?
     (string-normalize-spaces eval-sql)
     (string-normalize-spaces str))))

(define (check-sql2 actual expected)
  (check-equal?
   (string-normalize-spaces actual)
   (string-normalize-spaces expected)))

; Renders a (racketblock ...) for the given key
(define-syntax (rb stx)
  (syntax-case stx ()
    [(rb ITEM)
     #`(racketblock #,(replace-context stx (get-captured-syntax #'ITEM)))]))