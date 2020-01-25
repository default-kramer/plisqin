#lang at-exp racket

(provide docgen/unsafe
         docgen/loose
         docgen/strict)

(require scribble/manual
         "tokens-helpers.rkt")

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
                   ; primitives
                   [scalar bool aggregate subquery sql]
                   ; aggregates
                   [count avg min max sum]
                   ; misc
                   [exists]
                   ; operators
                   [and or not
                    = <> < <= > >=
                    like not-like
                    is is-not in not-in
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
                                     [primitive-id ooo]
                                     [aggregate-id ooo]
                                     [misc-id ooo]
                                     [operator-id ooo]])
             (begin
               ; We set the type-lookup parameter to whichever type table was given.
               ; This is used by `def-token` to extract the type signature.
               ; I worried about Scribble being lazy, but this seems to work.
               (type-lookup the-type-table)
               (defmodule mod-id)
               (:section "Clauses" clause-id ooo)
               (:section "Primitives" primitive-id ooo)
               (:section "Aggregates" aggregate-id ooo)
               (:section "Misc" misc-id ooo)    
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
  example-get-content unsafe-ctx unsafe-table
  plisqin-lib/unsafe plisqin-lib/unsafe/operators)

(define-docgen docgen/loose
  default-content loose-ctx loose-table
  plisqin-lib/loose plisqin-lib/loose/operators)

(define-docgen docgen/strict
  default-content strict-ctx strict-table
  plisqin-lib/strict plisqin-lib/strict/operators)

(def-content-provider (default-content id)
  [else
   @nested{Stricter version of @(%% (racket id))}])

; This eventually belongs in another file, when I get around to writing the documentation.
(def-content-provider (example-get-content id)
  [(select where)
   @nested{This is just a demonstration of how I could write custom content
    in a different file and hook it into the skeleton.

    This is the documentation for @(racket id).}]
  [else ""])
