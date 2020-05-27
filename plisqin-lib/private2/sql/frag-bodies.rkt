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
         "interval.rkt"
         "../_dialect.rkt"
         (only-in "../_core.rkt" join? tuple?)
         (only-in "../_null.rkt" fallback-symbol nullability no)
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
            (syntax-parameterize ([self (bind:: #'(quote id))]
                                  [arglist (bind:: #'(list a ooo))])
              body)]
           ...
           [(id . args)
            (syntax-parameterize ([self (bind:: #'(quote id))]
                                  [arglist (bind:: #'args)])
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
    [else (error "assert fail: define-bodies")]))


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
                (list tokens " as " as-name)
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
  [(like not-like)
   (make2 'bool
          #:reduce (reduce-operation self tokens))]
  [(is is-not)
   (make2 'bool
          #:reduce (reduce-is/is-not self tokens))]
  [(+ - * /)
   (make2 'scalar
          #:reduce (parens (interpose (format " ~a " self) tokens)))]

  ; == Date Math ==
  ; PostgreSQL has intervals which are Scalars, but MS does not.
  ; Let's standardize on MS-compatible date math, meaning that our intervals
  ; are not Scalars. In fact, they are not even Tokens. This is because date+
  ; and date- are the only functions that know how to do anything sensible
  ; with intervals.
  [(date+ date-)
   (make2 'scalar
          #:reduce (reduce-dateadd self arglist))]
  [(years months days hours minutes seconds)
   (match arglist
     [(list a)
      (make-interval self a)]
     [else
      ; The type checkers should prevent this case
      (error "internal error - interval constructor needs exactly one arg")])]
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
    [(= <> < <= > >=)
     (~a sym)]
    [(like) "like"]
    [(not-like) "not like"]
    [else (error "unknown op:" sym)]))

(define (reduce-operation op tokens)
  (let ([sqlname (translate-op op)])
    (parens (insert-ands sqlname tokens))))

(define (reduce-is/is-not op tokens)
  (define (get-fallback-sym token)
    (if (equal? no (nullability token))
        #f
        '/minval))
  (define-values
    (join-str constant-str)
    (case op
      [(is)
       (values " is " "1=1")]
      [(is-not)
       (values " is not " "1<>1")]
      [else (error "expected 'is or 'is-not but got" op)]))
  (match tokens
    ; Note: SQL Server (at least) is finicky in that "null is foo" is not
    ; allowed, so we generate "foo is null" instead.
    ; It also doesn't allow "foo is bar" so if neither argument is null we
    ; generate an equivalent "=" or "<>" comparison, with fallbacks if needed.
    [(list 'null 'null)
     ; For maximum portability, generate "1=1" or "1<>1"
     (parens constant-str)]
    [(list a 'null)
     (parens (list a join-str "null"))]
    [(list 'null a)
     (parens (list a join-str "null"))]
    ; If neither token is dbnull, we do an = comparison in which null
    ; equals itself and nothing else. In other words, if we have
    #;(like a b)
    ; then it is equivalent to
    #;(= (?? a /minval) (?? b /minval))
    ; assuming neither a nor b is 'null.
    [(list a b)
     (let ([reduction
            (reduce-comparison2 '= a b
                                (get-fallback-sym a)
                                (get-fallback-sym b))])
       (if (equal? op 'is-not)
           (parens (list "not " reduction))
           reduction))]
    [else
     (error "is/is-not expected exactly two arguments")]))

(define (reduce-comparison op tokens)
  (let*-values ([(lhs rhs /lhs /rhs)
                 (match tokens
                   [(list lhs rhs)
                    (let ([/lhs (fallback-symbol lhs)]
                          [/rhs (fallback-symbol rhs)])
                      (values lhs rhs /lhs /rhs))]
                   [else
                    (error "comparison expected exactly two arguments" op)])])
    (reduce-comparison2 op lhs rhs /lhs /rhs)))

(define (reduce-comparison2 op lhs rhs /lhs /rhs)
  ; op: symbol?   (eg '<=)
  ; lhs: token
  ; rhs: token
  ; /lhs: (or/c #f symbol?)   (eg '/minval)
  ; /rhs: (or/c #f symbol?)   (eg '/minval)
  (define base-reduction (reduce-operation op (list lhs rhs)))
  ; See truth.rkt for a more complete description of a "probe result".
  ; Basically, there are 3 booleans representing: lhs, rhs, and lhs+rhs.
  ; This gives us 2^3 = 8 possibilities, but 1 of them is never used by our truth table.
  ; We need to translate the other 7 into SQL.
  (define probe-result (probe op /lhs /rhs))
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
     base-reduction]))

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



(define (reduce-dateadd self arglist)
  (define negate? (equal? self 'date-))
  (define dialect (current-dialect))
  (define dt (car arglist))
  (define intervals (cdr arglist))
  (cond
    [(mssql? dialect)
     (dateadd-mssql dt intervals negate?)]
    [(postgres? dialect)
     (dateadd-postgres dt intervals negate?)]
    [(sqlite? dialect)
     (dateadd-sqlite dt intervals negate?)]
    [else
     (error "Cannot perform date math with dialect:" dialect)]))

(define (dateadd-mssql dt intervals negate?)
  (define (reduce-qty qty)
    (cond
      [(number? qty)
       (if negate?
           (- 0 qty)
           qty)]
      [else
       (list (if negate? "-" "+") (parens qty))]))
  (match intervals
    [(list) dt]
    [(list iv rest ...)
     (dateadd-mssql
      (list "dateadd("
            (~a (interval-unit iv))
            ", "
            (reduce-qty (interval-qty iv))
            ", "
            dt
            ")")
      rest negate?)]
    [else
     (error "dateadd-mssql expected a list")]))

(define (dateadd-postgres dt intervals negate?)
  (define (reduce num unit)
    (list "interval '"num" "unit"'"))
  (define (reduce-interval iv)
    (let ([qty (interval-qty iv)]
          [unit (~a (interval-unit iv))])
      (if (number? qty)
          (reduce qty unit)
          (parens (list (parens qty)
                        " * "
                        (reduce 1 unit))))))
  (define joiner (if negate? " - " " + "))
  (parens (interpose joiner (cons dt (map reduce-interval intervals)))))

(define (dateadd-sqlite dt intervals negate?)
  (define (reduce-interval iv)
    (let ([sign (if negate? "-" "+")]
          [qty (interval-qty iv)]
          [unit (~a (interval-unit iv))])
      (cond
        [(and negate? (number? qty) (negative? qty))
         ; avoid double negatives
         (format "'+~a ~a'" (- 0 qty) unit)]
        [(number? qty)
         (format "'~a~a ~a'" sign qty unit)]
        [else
         (list sign (parens qty)" || ' "unit"'")])))
  (list "datetime(" (interpose ", " (cons dt (map reduce-interval intervals)))
        ")"))
