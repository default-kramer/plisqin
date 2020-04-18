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
         (only-in "../_core.rkt" join? tuple?)
         (only-in "../_null.rkt" fallback-symbol)
         (only-in "truth.rkt" probe)
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
  [(scalar aggregate sql)
   (make2 self)]
  [(subquery)
   (make2 self #:reduce (parens tokens))]
  [(bit)
   (make2 'scalar)]

  ; == SQL Functions ==
  [(count)
   (make2 'aggregate
          #:reduce-proc reduce-count)]
  [(avg max min sum)
   (make2 'aggregate
          #:reduce (list (~a self) (parens tokens)))]
  [(exists)
   (make2 'bool
          #:reduce (list "exists " (parens tokens)))]
  [(round)
   (make2 'scalar #:reduce (reduce-round tokens))]
  [(coalesce)
   (make2 'scalar
          #:reduce (list "coalesce" (parens (interpose ", " tokens))))]
  ; TODO still need `case-when` and `case` but they are probably macros, not procs

  ; == "Operators" ==
  [(and or)
   (make2 'bool
          #:reduce (parens (interpose (format " ~a " self) tokens)))]
  [(not)
   (make2 'bool
          #:reduce (list "not " (parens tokens)))]
  [(= <> < <= > >=)
   (make2 'bool
          #:reduce (reduce-comparison self tokens))]
  [(like not-like is is-not in not-in)
   (make2 'bool
          #:reduce (reduce-operation self tokens))]
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

(define (reduce-count tokens)
  (match tokens
    [(list a)
     #:when
     (or (join? a)
         (tuple? a))
     "count(*)"]
    [(list 'distinct stuff ...)
     (list "count" (parens (list "distinct " stuff)))]
    [else
     (list "count" (parens tokens))]))

(define (reduce-round tokens)
  (match tokens
    [(list a)
     (list "round("a", 0)")]
    [else
     (list "round" (parens (interpose ", " tokens)))]))

(define (translate-op sym)
  (case sym
    [(not-like) "not like"]
    [(is-not) "is not"]
    [(not-in) "not in"]
    [else (~a sym)]))

(define (reduce-operation op tokens)
  (let ([sqlname (translate-op op)])
    (parens (insert-ands sqlname tokens))))

(define (reduce-comparison op tokens)
  ; See truth.rkt for a more complete description of a "probe result".
  ; Basically, there are 3 booleans representing: lhs, rhs, and lhs+rhs.
  ; This gives us 2^3 = 8 possibilities, but 1 of them is never used by our truth table.
  ; We need to translate the other 7 into SQL.
  (let ([base-reduction (reduce-operation op tokens)])
    (let*-values ([(lhs rhs /lhs /rhs probe-result)
                   (match tokens
                     [(list lhs rhs)
                      (let ([/lhs (fallback-symbol lhs)]
                            [/rhs (fallback-symbol rhs)])
                        (values lhs rhs /lhs /rhs
                                (probe op /lhs /rhs)))]
                     [else (values #f #f #f #f #f)])])
      (cond
        [(and /lhs /rhs)
         (reduce-probe2 probe-result lhs rhs base-reduction)]
        [/lhs
         (reduce-probe1 probe-result lhs base-reduction)]
        [/rhs
         (match probe-result
           ; Swap the first two elements, because reduce-probe1 assumes that the
           ; lhs was the one with the fallback
           [(list a b c)
            (reduce-probe1 (list b a c)
                           rhs base-reduction)])]
        [else
         base-reduction]))))

(define (reduce-probe2 probe-result lhs rhs base-reduction)
  ; Reduce a probe result when both the lhs and rhs had fallbacks.
  (case probe-result
    ; == Truths: 3 ==
    [((#t #t #t))
     (parens (list lhs" is null or "rhs" is null or "base-reduction))]
    ; == Truths: 2 ==
    [((#t #f #t))
     (parens (list lhs" is null or "(parens (list rhs" is not null and "base-reduction))))]
    [((#f #t #t))
     (parens (list rhs" is null or "(parens (list lhs" is not null and "base-reduction))))]
    [((#t #t #f))
     (error "This case is impossible to hit with the current truth tables")]
    ; == Truths: 1 ==
    [((#t #f #f))
     (parens (list rhs" is not null and "(parens (list lhs" is null or "base-reduction))))]
    [((#f #t #f))
     (parens (list lhs" is not null and "(parens (list rhs" is null or "base-reduction))))]
    [((#f #f #t))
     ; TODO this will leak an unknown value when only one operand is null.
     ; Note also that this is the case where (= /minval /minval) can be rendered as
     ; "lhs is not distinct from rhs" on Postgres.
     (parens (list (parens (list lhs" is null and "rhs" is null"))" or "base-reduction))]
    ; == Truths: 0 ==
    [((#f #f #f))
     (parens (list lhs" is not null and "rhs" is not null and "base-reduction))]
    ; ==============
    [else
     (error "unhandled probe result:" probe-result)]))

(define (reduce-probe1 probe-result lhs base-reduction)
  ; Reduce a probe result when only the lhs had a fallback.
  ; (If only the rhs had a fallback, the caller can swap the first two elements
  ;  of the probe-result and this function will be none the wiser.)
  (case probe-result
    [((#f #f #t)
      (#f #t #t)
      (#f #t #f)
      (#t #f #f)
      (#t #t #f))
     ; These cases should all be impossible.
     ; The rhs didn't have a fallback so it cannot change the result.
     (error "unexpected probe result" probe-result)]
    [((#f #f #f))
     (parens (list lhs" is not null and "base-reduction))]
    [((#t #t #t)
      (#t #f #t))
     (parens (list lhs" is null or "base-reduction))]
    [else
     (error "unhandled probe result" probe-result)]))
