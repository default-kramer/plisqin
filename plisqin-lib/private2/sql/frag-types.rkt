#lang racket

; Capture type signatures twice.
; The dispatchers are used to weave type checking into the fragment constructors.
; The tables are used by Scribble to generate documentation
(provide type-dispatcher/unsafe type-dispatcher/loose type-dispatcher/strict
         unsafe-table loose-table strict-table)

(require "weave.rkt"
         "../_types.rkt"
         (only-in "../_core.rkt" query? join?)
         "interval.rkt"
         "frags.helpers.rkt"
         (for-label plisqin-lib/types))

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
    [else (error "assert fail brq9j053")]))

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
    [else (error "TODO bvfqjkl2")]))

(define content? any/c) ; TODO

(def-typetable type-dispatcher/unsafe unsafe-table
  [(select)
   (token-constructor
    [any/c ...+ -> Select])]
  [(where)
   (token-constructor
    [any/c ...+ -> Where])]
  [(group-by)
   (token-constructor
    [any/c ...+ -> GroupBy])]
  [(having)
   (token-constructor
    [any/c ...+ -> Having])]
  [(order-by)
   (token-constructor
    [any/c ...+ -> OrderBy])]
  [(join-on)
   (token-constructor
    [any/c ...+ -> JoinOn])]
  [(sql)
   (token-constructor
    [any/c ...+ -> Token])]
  [(scalar aggregate min max
           + - * /)
   (token-constructor
    [any/c ...+ -> Scalar])]
  [(bit)
   (token-constructor
    [any/c ...+ -> Bit])]
  [(coalesce)
   (token-constructor
    [any/c any/c ...+ -> Scalar])]
  [(exists and or not
           = <> < <= > >=
           like not-like
           is is-not
           in not-in)
   (token-constructor
    [any/c ...+ -> Bool])]
  [(round)
   (token-constructor
    [any/c ...+ -> Number])]
  [(subquery)
   (token-constructor
    [any/c ...+ -> Subquery])]
  [(count avg sum)
   (token-constructor
    [any/c ...+ -> Number])]
  [(years months days hours minutes seconds)
   (token-constructor
    [any/c -> interval?])]
  [(date+ date-)
   (token-constructor
    [any/c interval? ...+ -> Datetime])]
  )

(def-typetable type-dispatcher/loose loose-table
  [(select)
   (token-constructor
    [content? ...+ -> Select])]
  [(where)
   (token-constructor
    [content? ...+ -> Where])]
  [(group-by)
   (token-constructor
    [content? ...+ -> GroupBy])]
  [(having)
   (token-constructor
    [content? ...+ -> Having])]
  [(order-by)
   (token-constructor
    [content? ...+ -> OrderBy])]
  [(join-on)
   (token-constructor
    [content? ...+ -> JoinOn])]
  [(sql)
   (token-constructor
    [content? ...+ -> Token])]
  [(scalar aggregate min max
           + - * /)
   (token-constructor
    [content? ...+ -> Scalar])]
  [(bit)
   (token-constructor
    [content? ...+ -> Bit])]
  [(coalesce)
   (token-constructor
    [content? content? ...+ -> Scalar])]
  [(exists and or not
           = <> < <= > >=
           like not-like
           is is-not
           in not-in)
   (token-constructor
    [content? ...+ -> Bool])]
  [(round)
   (token-constructor
    [content? -> Number]
    [content? number? -> Number])]
  [(subquery)
   (token-constructor
    [content? ...+ -> Subquery])]
  [(count avg sum)
   (token-constructor
    [content? ...+ -> Number])]
  [(years months days hours minutes seconds)
   (token-constructor
    [content? -> interval?])]
  [(date+ date-)
   (token-constructor
    [content? interval? ...+ -> Datetime])]
  )

(def-typetable type-dispatcher/strict strict-table
  [(select)
   (token-constructor
    [Scalar -> Select])]
  [(group-by)
   (token-constructor
    [Scalar -> GroupBy])]
  [(order-by)
   (token-constructor
    [(or/c 'asc 'desc) Scalar -> OrderBy]
    [Scalar -> OrderBy])]
  [(where )
   (token-constructor
    [Boolish -> Where])]
  [(join-on)
   (token-constructor
    [Boolish -> JoinOn])]
  [(having)
   (token-constructor
    [Boolish -> Having])]
  [(scalar aggregate)
   (token-constructor
    [content? ...+ -> Scalar])]
  [(bit)
   (token-constructor
    [content? ...+ -> Bit])]
  [(sql)
   (token-constructor
    [content? ...+ -> Token])]
  [(subquery)
   (token-constructor
    [content? ...+ -> Subquery])]
  [(count)
   (token-constructor
    [Scalar -> Number])]
  [(coalesce)
   (token-constructor
    [String String ...+ -> String]
    [Datetime Datetime ...+ -> Datetime]
    [Number Number ...+ -> Number])]
  [(avg sum)
   (token-constructor
    [Number -> Number])]
  [(min max)
   (token-constructor
    [String -> String]
    [Datetime -> Datetime]
    [Number -> Number])]
  [(exists)
   (token-constructor
    [(or/c query? Subquery) -> Bool])]
  [(round)
   (token-constructor
    [Number -> Number]
    [Number number? -> Number])]
  [(and or)
   (token-constructor
    [Boolish ...+ -> Bool])]
  [(not)
   (token-constructor
    [Boolish -> Bool])]
  [(= <> < <= > >=
      like not-like) ; confirmed that like works on numbers and datetimes in SQL Server
   (token-constructor
    [Number Number -> Bool]
    [String String -> Bool]
    [Datetime Datetime -> Bool])]
  [(is is-not in not-in)
   (token-constructor
    [content? ...+ -> Bool])] ; TODO figure out the real type
  [(+ - * /)
   (token-constructor
    [Number ...+ -> Number])]
  [(years months days hours minutes seconds)
   (token-constructor
    [Number -> interval?]
    [number? -> interval?])]
  [(date+ date-)
   (token-constructor
    [Datetime interval? ...+ -> Datetime])]
  )
