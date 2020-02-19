#lang racket

(provide retval-dispatcher)
; The retval dispatcher provides the main bodies of the fragments.
; We will actually support two versions, one like `(select arg ...)` and another
; like `(select . args)`. This allows the weaver to choose the arity.
; In either case the list of arguments is passed directly to the fragment as its content.
; The fragment id is always the name of the proc as as symbol, for example 'select.
;
; We have to pay attention to the fragment kind, which Morsel relies on.
; We can also define an optional reduction which transforms the original argument
; list during SQL rendering.

(require "weave.rkt"
         "./fragment.rkt"
         "./frags.helpers.rkt"
         racket/stxparam)

; In the body of `(select . x)` we will bind self to (quote select).
; A few fragments will use self as their fragment kind.
(define-syntax-parameter self #f)

; In the body of `(select . x)` we will bind arglist to x.
; In the body of `(select x ...)` we will bind arglist to (list x ...)
(define-syntax-parameter arglist #f)

; `make2` is a purpose-built fragment constructor, more concise than `make`.
; We know that
;   the fragment id is always `self` and
;   the fragment content is always `arglist`
; so the caller only needs to provide the fragment kind and an (optional) reduction.
(define-syntax-rule (make2 fragment-kind stuff ...)
  (make self fragment-kind arglist stuff ...))


; This version wants [id body] in order to create the dispatcher.
(define-syntax (define-bodies2 stx)
  (syntax-case stx ()
    [(_ dispatcher-id [id body] ...)
     (with-syntax ([ooo (quote-syntax ...)])
       (quasisyntax/loc stx
         (def-dispatcher dispatcher-id
           ; The order matters here:
           ; (id a ...) must come before (id . rest) because
           ; the latter matches the former, but not vice-versa.
           [(id a ooo)
            (syntax-parameterize ([self (TODO #'(quote id))]
                                  [arglist (TODO #'(list a ooo))])
              body)]
           ...
           [(id . args)
            (syntax-parameterize ([self (TODO #'(quote id))]
                                  [arglist (TODO #'args)])
              body)]
           ...
           )))]))

; This is the version of define-bodies that we use.
; It transforms [(a b c) body] to mean [a body] [b body] [c body].
(define-syntax (define-bodies stx)
  (syntax-case stx ()
    [(_ dispatcher-id a ...)
     (quasisyntax/loc stx
       (define-bodies2 dispatcher-id
         #,@(matchup #'(a ...))))]
    [else (error "TODO bfakjl4523")]))


(define-bodies retval-dispatcher
  ; == Clauses ==
  [(select)
   (make2 self
          #:reduce
          (let ([as-name (match tokens
                           [(list a)
                            #:when (fragment? a)
                            (fragment-as-name a)]
                           [else #f])]
                [tokens (->Bit tokens)])
            (if as-name
                ; TODO should leave as-name unchanged, but Morsel doesn't render symbols for now
                (list tokens " as " (~a as-name))
                tokens)))]
  [(group-by)
   (make2 self
          #:reduce (->Bit tokens))]
  [(order-by)
   (make2 self
          #:reduce-proc reduce-order-by)]
  [(where join-on having)
   (make2 self
          #:reduce (->Bool tokens))]

  ; == Fragments ==
  [(scalar aggregate subquery sql)
   (make2 self)]
  [(bit)
   (make2 'scalar)]

  ; == SQL Functions ==
  [(count avg max min sum)
   (make2 'aggregate
          #:reduce (list (~a self) (parens tokens)))]
  [(exists)
   (make2 'bool
          #:reduce (list "exists " (parens tokens)))]
  [(round)
   (make2 'scalar
          #:reduce (error "TODO need dialect here"))]
  ; TODO add coalesce
  ; TODO still need `case-when` and `case` but they are probably macros, not procs

  ; == "Operators" ==
  [(and or)
   (make2 'bool
          #:reduce (parens (interpose (format " ~a " self) tokens)))]
  [(not)
   (make2 'bool
          #:reduce (list "not " (parens tokens)))]
  [(= <> < <= > >=
      like not-like is is-not in not-in)
   (make2 'bool
          #:reduce (let ([sqlname (translate-op self)])
                     (parens (insert-ands sqlname tokens))))]
  [(+ - * /)
   (make2 'scalar
          #:reduce (parens (interpose (format " ~a " self) tokens)))]
  )

(define (reduce-order-by tokens)
  (match tokens
    [(list dir rest ...)
     #:when (member dir '(asc desc))
     (list (->Bit rest) (format " ~a" dir))]
    [else (->Bit tokens)]))

(define (translate-op sym)
  (case sym
    [(not-like) "not like"]
    [(is-not) "is not"]
    [(not-in) "not in"]
    [else (~a sym)]))
