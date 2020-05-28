#lang racket

; There are 2 submodules: unsafe and strict.
; Each one weaves the procs together and provides all the ids.

(require "frag-bodies.rkt"
         "frag-types.rkt"
         "frag-nullchecks.rkt"
         "weave.rkt")

(define-syntax-rule (export-all type-dispatcher null-dispatcher id ...)
  (begin
    (define/weave retval-dispatcher type-dispatcher null-dispatcher
      (id . tokens))
    ...
    (provide id ...)))

; Export with types via submodules
(define-syntax-rule (do-export mod-id type-dispatcher null-dispatcher)
  (module+ mod-id
    (export-all type-dispatcher null-dispatcher
                select where group-by having order-by join-on
                subquery
                count avg min max sum exists round coalesce
                date+ date- years months days hours minutes seconds
                )
    (module+ operators
      (export-all type-dispatcher null-dispatcher
                  and or not
                  = <> < <= > >=
                  like not-like
                  is is-not
                  + - * /))))

; these are in unsafe only
(module+ unsafe
  (export-all type-dispatcher/unsafe null-dispatcher/unsafe
              scalar aggregate sql))

(do-export unsafe type-dispatcher/unsafe null-dispatcher/unsafe)
(do-export strict type-dispatcher/strict null-dispatcher/strict)

(module+ test
  (require rackunit
           (prefix-in r: racket)
           "fragment.rkt"
           "../_null.rkt"
           (submod ".." unsafe)
           (submod ".." unsafe operators))

  ; Check that each id is bound to a fragment constructor.
  (define-syntax-rule (check-frags frag-id ...)
    (begin
      ; Almost everything accepts two arguments. The comparisons typically
      ; require exactly two, so that is how many we will use
      (let* ([result (frag-id '(1 "2") '(three four))]
             [expected (format "(~a '(1 \"2\") '(three four))" 'frag-id)]
             [actual (format "~v" result)])
        (check-pred fragment? result)
        (check-equal? expected actual))
      ...

      ; Test that (frag-id "foo") is `maybe` nullable.
      ; Also test annotation and inference.
      ; Skip some that have special handling.
      (when (r:not (member 'frag-id '(exists coalesce is is-not)))
        (let* ([default (frag-id "foo")]
               [annotated (>> default #:null no)]
               [inferred (frag-id annotated)])
          (check-equal? maybe (nullability default))
          (check-equal? no (nullability annotated))
          (check-equal? no (nullability inferred))))
      ...
      ))

  (check-frags select where group-by having order-by join-on
               scalar aggregate subquery sql
               count avg min max sum exists round coalesce
               ; The date math functions won't pass any tests because they are
               ; too strict in all variants (by design):
               #;[date+ date- years months days hours minutes seconds]
               )
  (check-frags and or not
               = <> < <= > >=
               like not-like
               is is-not
               + - * /))
