#lang racket

(provide null-dispatcher/unsafe
         null-dispatcher/strict)

(require "weave.rkt"
         "frags.helpers.rkt"
         "../_null.rkt")

(define-syntax (~def-nulltable stx)
  (syntax-case stx ()
    [(_ dispatcher-id
        [proc-id body]
        ...)
     (with-syntax ([ooo (quote-syntax ...)])
       (quasisyntax/loc stx
         (def-dispatcher dispatcher-id
           [(proc-id arg ooo)
            (let ([checker body])
              (checker (list arg ooo) (current-continuation-marks) 'proc-id))]
           ...
           [(proc-id . arglist)
            (let ([checker body])
              (checker arglist (current-continuation-marks) 'proc-id))]
           ...)))]
    [else (error "~def-nulltable can't parse:" stx)]))

(define-syntax (def-nulltable stx)
  (syntax-case stx ()
    [(_ dispatcher-id scribble-id
        [(id ...) body]
        ...)
     (quasisyntax/loc stx
       (begin
         (~def-nulltable dispatcher-id #,@(matchup #'([(id ...) body]
                                                      ...)))
         (define (scribble-id sym)
           (case sym
             [(id ...) #'body]
             ...
             [else #f]))))]))

(def-nulltable null-dispatcher/unsafe scribble-nulltable/unsafe
  [(select where group-by having join-on
           scalar aggregate subquery sql round
           ; TODO do the other aggregates (besides count) also work with 'distinct
           avg min max sum
           and or not
           + - * /)
   (nullchecker #:permit-null)]
  [(exists)
   never-null]
  [(coalesce)
   coalesce-nullchecker]
  [(= <> < <= > >=
      like not-like)
   (nullchecker
    #:accept /void /minval /maxval /any
    #:permit-null)]
  [(is is-not)
   ; We guarantee to produce a non-null Bool? for all inputs
   never-null]
  [(count)
   (nullchecker
    #:permit-null
    ; TODO #:over
    #;(match arglist
        [(list 'distinct rest ...) rest]
        [else arglist]))]
  [(order-by)
   (nullchecker
    #:permit-null
    ; TODO #:over
    #;(match arglist
        [(list 'asc rest ...) rest]
        [(list 'desc rest ...) rest]
        [else arglist]))]
  ; Even in unsafe, intervals should never be null.
  [(years months days hours minutes seconds)
   (nullchecker #:deny-null)]
  [(date+ date-)
   (nullchecker #:permit-null)]
  )

(def-nulltable null-dispatcher/strict scribble-nulltable/strict
  [(select group-by
           subquery round
           ; TODO these definitely need something
           avg min max sum
           + - * /)
   (nullchecker
    #:permit-null)]
  [(exists)
   never-null]
  [(coalesce)
   coalesce-nullchecker]
  [(where having join-on
          and or not)
   ; All arguments are Boolish?. In this strict variant, we enforce that
   ; every Boolish? is not null
   (nullchecker
    #:deny-null)]
  [(= <> < <= > >=
      like not-like)
   (nullchecker
    #:accept /void /minval /maxval /any
    #:deny-null)]
  [(is is-not)
   ; We guarantee to produce a non-null Bool? for all inputs
   never-null]
  [(count)
   (nullchecker
    #:permit-null
    ; TODO #:over
    #;(match arglist
        [(list 'distinct rest ...) rest]
        [else arglist]))]
  [(order-by)
   (nullchecker
    #:permit-null
    ; TODO #:over
    #;(match arglist
        [(list 'asc rest ...) rest]
        [(list 'desc rest ...) rest]
        [else arglist]))]
  ; Intervals should never be null.
  [(years months days hours minutes seconds)
   (nullchecker #:deny-null)]
  ; We know the intervals will be non-null, but if the Datetime? is null that's
  ; fine, we accept and propogate it as normal.
  [(date+ date-)
   (nullchecker #:permit-null)]
  )

; A nullchecker ignores the input and always returns "not null"
(define (never-null . ignored-args) no)

(define (coalesce-nullchecker arglist . more-stuff)
  (define (check lst accum)
    (match lst
      [(list a rest ...)
       (let ([n (or (nullability a)
                    maybe)])
         (cond
           [(equal? n no)
            no]
           [(equal? n maybe)
            (check rest maybe)]
           [else
            (check rest accum)]))]
      [(list) accum]))
  (check arglist yes))
