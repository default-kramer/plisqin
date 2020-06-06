#lang at-exp racket

(provide docgen/unsafe
         docgen/strict)

(require scribble/manual
         (for-label (prefix-in strict: plisqin-lib/strict)
                    (prefix-in strict: plisqin-lib/strict/operators))
         "tokens-helpers.rkt"
         "tokens-strict.scrbl")

; Defines a "document generator" which produces the documentation for the Token Constructors.
; This macro is parameterized to work with the 3 variants: unsafe, loose, and strict.
(define-syntax (define-docgen stx)
  (syntax-case stx ()
    [(_ docgen-id
        get-content      ; a function created by def-content-provider
        my-ctx           ; a syntax object from a scope with the desired `for-label`
        the-type-table   ; the lookup function to get type signatures
        mod-id           ; plisqin-lib/{variant}
        operators-mod-id ; plisqin-lib/{variant}/operators
        )
     (with-syntax ([ooo (quote-syntax ...)])
       #'(begin
           (define-syntax (docgen-id stx)
             (syntax-case stx ()
               [(_)
                ; Pipe all these ids through GO2 to change their context
                #'(GO2
                   ;clauses
                   [select where group-by having order-by join-on]
                   ; aggregates
                   [avg min max sum count]
                   ; misc
                   [exists subquery coalesce round]
                   ; date math
                   [date+ date- years months days hours minutes seconds]
                   ; operators
                   [and or not
                    = <> < <= > >=
                    like not-like
                    is is-not
                    + - * /])]))

           (define-for-syntax (fixup stx)
             (relabel2 stx my-ctx))

           ; Pipe everything into GO3 after changing the context of the ids.
           (define-syntax (GO2 stx)
             (syntax-case stx (quote)
               [(_ [id ooo] ooo)
                #`(GO3 #,(fixup #'[[id ooo] ooo]))]))

           ; At this point, each id should have the correct context.
           ; Now we can generate some scribble stuff.
           (define-syntax-rule (GO3 [[clause-id ooo]
                                     [aggregate-id ooo]
                                     [misc-id ooo]
                                     [datemath-id ooo]
                                     [operator-id ooo]])
             (begin
               ; We set the type-lookup parameter to whichever type table was given.
               ; This is used by `def-token` to extract the type signature.
               ; I worried about Scribble being lazy, but this seems to work.
               (type-lookup the-type-table)
               ; This `defmodule` has been moved to the parent doc:
               ;(defmodule mod-id)
               (:section "Clauses" clause-id ooo)
               (:section "Aggregates" aggregate-id ooo)
               (:section "Misc" misc-id ooo)
               (:section "Date Math" datemath-id ooo)
               (:section "Operators")
               (defmodule operators-mod-id)
               (def operator-id)
               ooo))

           ; Our "sections" are actually subsections
           (define-syntax-rule (:section name id ooo)
             (begin
               ; Make a consistent tag that will not clash with other variants
               (subsection #:tag (format "~a/~a" 'mod-id name) name)
               (def id)
               ooo))

           ; Calls `def-token` with the content returned by `get-content`
           (define-syntax (def stx)
             (syntax-case stx ()
               [(_ id)
                (let ([content (get-content #'id)])
                  (when (not (syntax? content))
                    (error "plisqin error erjk2"))
                  #`(def-token id #,content))]))
           ))]))

(define-docgen docgen/unsafe
  get-unsafe-content unsafe-ctx unsafe-table
  plisqin-lib/unsafe plisqin-lib/unsafe/operators)

(define-docgen docgen/strict
  get-strict-content strict-ctx strict-table
  plisqin-lib/strict plisqin-lib/strict/operators)

@(define-syntax (strict-link stx)
   (syntax-case stx ()
     [(_ id)
      (let* ([str (format "~a" (syntax-e #'id))]
             [datum (string->symbol (format "strict:~a" str))])
        (with-syntax ([strict-id datum]
                      [str str])
          #'(racketlink strict-id (racketplainfont str))))]))

(def-content-provider (get-unsafe-content id)
  [else @nested{
    @tech{Unsafe} variant of @(strict-link id).}])
