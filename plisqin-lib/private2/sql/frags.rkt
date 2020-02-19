#lang racket

; There are 3 submodules: unsafe, loose, and strict.
; Each one weaves the procs together and provides all the ids.

(require "frag-bodies.rkt"
         "frag-types.rkt"
         "weave.rkt")

(def-dispatcher null-dispatcher #:constant (void))

(define-syntax-rule (export-all type-dispatcher id ...)
  (begin
    (define/weave retval-dispatcher type-dispatcher null-dispatcher
      (id . tokens))
    ...
    (provide id ...)))

; Export with types via submodules
(define-syntax-rule (do-export mod-id type-dispatcher)
  (module+ mod-id
    (export-all type-dispatcher
                select where group-by having order-by join-on
                scalar bit aggregate subquery sql
                count avg min max sum exists)
    (module+ operators
      (export-all type-dispatcher
                  and or not
                  = <> < <= > >=
                  like not-like
                  is is-not
                  in not-in
                  + - * /))))

(do-export unsafe type-dispatcher/unsafe)
(do-export loose type-dispatcher/loose)
(do-export strict type-dispatcher/strict)

(module+ test
  (require rackunit
           "fragment.rkt"
           (submod ".." unsafe)
           (submod ".." unsafe operators))

  ; Check that each id is bound to a fragment constructor.
  (define-syntax-rule (check-frags frag-id ...)
    (begin
      ; The unsafe fragment constructor should accept any arguments
      (let* ([result (frag-id 1 "2" '(three four))]
             [expected (format "(~a 1 \"2\" '(three four))" 'frag-id)]
             [actual (format "~v" result)])
        (check-pred fragment? result)
        (check-equal? expected actual))
      ...))

  (check-frags select where group-by having order-by join-on
               scalar bit aggregate subquery sql
               count avg min max sum exists)
  (check-frags and or not
               = <> < <= > >=
               like not-like
               is is-not
               in not-in
               + - * /))
