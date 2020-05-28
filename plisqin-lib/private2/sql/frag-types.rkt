#lang racket

; Capture type signatures twice.
; The dispatchers are used to weave type checking into the fragment constructors.
; The tables are used by Scribble to generate documentation
(provide type-dispatcher/unsafe type-dispatcher/strict
         unsafe-table strict-table)

(require "weave.rkt"
         "../_types.rkt"
         "frags.helpers.rkt"
         (only-in "../_core.rkt" query? join?)
         (only-in "../from.rkt" instance?)
         "interval.rkt"
         )

(define-syntax (~def-typetable stx)
  (syntax-case stx ()
    [(_ dispatcher-id
        [proc-id (token-constructor
                  type-spec
                  ...)]
        ...)
     (with-syntax ([ooo (quote-syntax ...)])
       (quasisyntax/loc stx
         (begin
           (def-dispatcher dispatcher-id
             [(proc-id arg ooo)
              (build-typechecker 'proc-id (list arg ooo) type-spec ...)]
             ...
             [(proc-id . arglist)
              (build-typechecker 'proc-id arglist type-spec ...)]
             ...))))]
    [else (error "assert fail ~def-typetable")]))

(define-syntax (def-typetable stx)
  (syntax-case stx ()
    [(_ dispatcher-id scribble-id
        [(id ...) body]
        ...)
     (quasisyntax/loc stx
       (begin
         (~def-typetable dispatcher-id #,@(matchup #'([(id ...) body]
                                                      ...)))
         ; Stash the syntax objects to be used by Scribble:
         (define (scribble-id sym)
           (case sym
             [(id ...) #'body]
             ...
             [else #f]))))]
    [else (error "assert fail def-typetable")]))

(define content? any/c) ; TODO

(def-typetable type-dispatcher/unsafe unsafe-table
  [(select)
   (token-constructor
    [any/c ...+ -> Select?])]
  [(where)
   (token-constructor
    [any/c ...+ -> Where?])]
  [(group-by)
   (token-constructor
    [any/c ...+ -> GroupBy?])]
  [(having)
   (token-constructor
    [any/c ...+ -> Having?])]
  [(order-by)
   (token-constructor
    [any/c ...+ -> OrderBy?])]
  [(join-on)
   (token-constructor
    [any/c ...+ -> JoinOn?])]
  [(sql)
   (token-constructor
    [any/c ...+ -> Token?])]
  [(scalar aggregate min max
           + - * /)
   (token-constructor
    [any/c ...+ -> Scalar?])]
  [(coalesce)
   (token-constructor
    [any/c any/c ...+ -> Scalar?])]
  [(exists and or not
           = <> < <= > >=
           like not-like)
   (token-constructor
    [any/c ...+ -> Bool?])]
  [(is is-not)
   (token-constructor
    [any/c any/c -> Bool?])]
  [(round)
   (token-constructor
    [any/c ...+ -> Number?])]
  [(subquery)
   (token-constructor
    [any/c ...+ -> Subquery?])]
  [(count avg sum)
   (token-constructor
    [any/c ...+ -> Number?])]
  [(years months days hours minutes seconds)
   (token-constructor
    [any/c -> interval?])]
  [(date+ date-)
   (token-constructor
    [any/c interval? ...+ -> Datetime?])]
  )

(def-typetable type-dispatcher/strict strict-table
  [(select)
   (token-constructor
    [Scalar? -> Select?])]
  [(group-by)
   (token-constructor
    [Scalar? -> GroupBy?])]
  [(order-by)
   (token-constructor
    [(or/c 'asc 'desc) Scalar? -> OrderBy?]
    [Scalar? -> OrderBy?])]
  [(where )
   (token-constructor
    [Boolish? -> Where?])]
  [(join-on)
   (token-constructor
    [Boolish? -> JoinOn?])]
  [(having)
   (token-constructor
    [Boolish? -> Having?])]
  [(subquery)
   (token-constructor
    [query? -> Subquery?])]
  [(count)
   ; We could allow (or/c 'distinct 'all) and ignore 'all during reduction.
   ; But no need to do that yet.
   (token-constructor
    [instance? -> Number?]
    [Scalar? -> Number?]
    [(or/c 'distinct) Scalar? -> Number?])]
  [(coalesce)
   (token-constructor
    [String? String? ...+ -> String?]
    [Datetime? Datetime? ...+ -> Datetime?]
    [Number? Number? ...+ -> Number?])]
  [(avg sum)
   (token-constructor
    [Number? -> Number?])]
  [(min max)
   (token-constructor
    [String? -> String?]
    [Datetime? -> Datetime?]
    [Number? -> Number?])]
  [(exists)
   (token-constructor
    [(or/c query? Subquery?) -> Bool?])]
  [(round)
   (token-constructor
    [Number? -> Number?]
    [Number? number? -> Number?])]
  [(and or)
   (token-constructor
    [Boolish? ...+ -> Bool?])]
  [(not)
   (token-constructor
    [Boolish? -> Bool?])]
  [(= <> < <= > >=
      like not-like) ; confirmed that like works on numbers and datetimes in SQL Server
   (token-constructor
    [Number? Number? -> Bool?]
    [String? String? -> Bool?]
    [Datetime? Datetime? -> Bool?])]
  [(is is-not)
   (token-constructor
    [(or/c 'null Number?) (or/c 'null Number?) -> Bool?]
    [(or/c 'null String?) (or/c 'null String?) -> Bool?]
    [(or/c 'null Datetime?) (or/c 'null Datetime?) -> Bool?])]
  [(+ - * /)
   (token-constructor
    [Number? ...+ -> Number?])]
  [(years months days hours minutes seconds)
   (token-constructor
    [Number? -> interval?]
    [number? -> interval?])]
  [(date+ date-)
   (token-constructor
    [Datetime? interval? ...+ -> Datetime?])]
  )
