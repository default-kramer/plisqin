#lang racket

(provide %% def-token def-content-provider
         type-lookup
         unsafe-table loose-table strict-table
         (for-syntax unsafe-ctx loose-ctx strict-ctx)
         (for-syntax relabel2))

(require (only-in plisqin-lib/private2/_types
                  unsafe-table loose-table strict-table)
         scribble/manual)

(define type-lookup (make-parameter "type-lookup-not-set"))

(define-syntax (def-token stx)
  (syntax-case stx ()
    [(_ id stuff ...)
     #'(defthing #:kind "procedure" id #,(or ((type-lookup) (syntax-e #'id))
                                             #'INTERNAL-ERROR)
         stuff ...)]))

(define-syntax-rule (def-ctx ctx-id blah label-mod-id ...)
  (module blah racket/base
    (require (for-label label-mod-id ...))
    (define ctx-id #'here)
    (provide ctx-id)))

(def-ctx unsafe-ctx ab1 plisqin-lib/unsafe/main plisqin-lib/unsafe/operators)
(require (for-syntax 'ab1))

(def-ctx loose-ctx ab2 plisqin-lib/loose/main plisqin-lib/loose/operators)
(require (for-syntax 'ab2))

(def-ctx strict-ctx ab3 plisqin-lib/strict/main plisqin-lib/strict/operators)
(require (for-syntax 'ab3))

; Defines a function that takes an identifier? and returns a scribble
; val to be used as the content of the `def-token` for the given identifier.
; The return value will be enclosed in `quasisyntax`.
; You can use `nested` as a root element if needed.
; You can use the empty string to indicate "no content".
(define-syntax-rule (def-content-provider (provider-id token-id)
                      [match-clause body]
                      ...)
  (define-for-syntax (provider-id token-id)
    (case (syntax-e token-id)
      [match-clause (with-syntax ([token-id token-id])
                      (quasisyntax body))]
      ...)))


(define-for-syntax the-ctx (make-parameter #f))

; Recursively change the context of certain ids so that they appear to have been
; required for-label in the given context
(define-for-syntax (relabel2 stx ctx)
  (parameterize ([the-ctx ctx])
    (relabel stx)))

(define-for-syntax (relabel stx)
  (let ([result (syntax-e stx)])
    (cond
      [(list? result)
       (datum->syntax stx (map relabel result) stx stx)]
      [(pair? result)
       (datum->syntax stx (cons (relabel (car result))
                                (relabel (cdr result)))
                      stx stx)]
      [(member (syntax-e stx)
               '(select where group-by having order-by join-on
                        scalar bool aggregate subquery sql
                        count avg min max sum exists
                        and or not
                        = <> < <= > >=
                        like not-like is is-not in not-in
                        + - * /))
       (let ([ctx (or (the-ctx)
                      (error "CTX not set"))])
         (datum->syntax ctx (syntax-e stx) stx ctx))]
      [else stx])))

(define-syntax (%% stx)
  (syntax-case stx ()
    [(_ a)
     (relabel2 #'a unsafe-ctx)]))