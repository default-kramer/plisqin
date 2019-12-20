#lang racket

; Capture type signatures in two tables.
; The first (eg `unsafe-table`) is for use by Scribble.
; The second (eg `:unsafe-table`) is used to create the wrapper functions.
(provide unsafe-table (for-syntax :unsafe-table)
         loose-table  (for-syntax :loose-table)
         strict-table (for-syntax :strict-table))

(require "./lib/type-table.rkt"
         "types.rkt"
         (for-label (except-in racket/contract ->)
                    "types.rkt"))

(def-type-table unsafe-table :unsafe-table
  [(select where group-by having order-by join-on sql)
   (token-constructor
    [any/c ...+ -> Token])]
  [(scalar aggregate min max
           + - * /)
   (token-constructor
    [any/c ...+ -> Scalar])]
  [(bool exists
         and or not
         = <> < <= > >=
         like not-like
         is is-not
         in not-in)
   (token-constructor
    [any/c ...+ -> Bool])]
  [(subquery)
   (token-constructor
    [any/c ...+ -> Subquery])]
  [(count avg sum)
   (token-constructor
    [any/c ...+ -> Number])])

(define content? any/c) ; TODO

(def-type-table loose-table :loose-table
  [(select where group-by having order-by join-on sql)
   (token-constructor
    [content? ...+ -> Token])]
  [(scalar aggregate min max
           + - * /)
   (token-constructor
    [content? ...+ -> Scalar])]
  [(bool exists
         and or not
         = <> < <= > >=
         like not-like
         is is-not
         in not-in)
   (token-constructor
    [content? ...+ -> Bool])]
  [(subquery)
   (token-constructor
    [content? ...+ -> Subquery])]
  [(count avg sum)
   (token-constructor
    [content? ...+ -> Number])])

(def-type-table strict-table :strict-table
  [(select group-by)
   (token-constructor
    [Scalar -> Token]
    [Bool -> Token])]
  [(order-by)
   (token-constructor
    [(or/c 'asc 'desc) (or/c Scalar Bool) -> Token]
    [(or/c Scalar Bool) -> Token])]
  [(where join-on having)
   (token-constructor
    [Bool -> Token])]
  [(scalar aggregate)
   (token-constructor
    [content? ...+ -> Scalar])]
  [(bool)
   (token-constructor
    [content? ...+ -> Bool])]
  [(sql)
   (token-constructor
    [content? ...+ -> Token])]
  [(subquery)
   (token-constructor
    [content? ...+ -> Subquery])]
  [(count)
   (token-constructor
    [Scalar -> Number])]
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
    [(or/c Query Subquery) -> Bool])]
  [(and or)
   (token-constructor
    [Bool ...+ -> Bool])]
  [(not)
   (token-constructor
    [Bool -> Bool])]
  [(= <> < <= > >=
      like not-like) ; confirmed that like works on numbers and datetimes in SQL Server
   (token-constructor
    [Number Number -> Bool]
    [String String -> Bool]
    [Datetime Datetime -> Bool])]
  [(is is-not in not-in)
   (token-constructor
    [content? ...+ -> Token])] ; TODO figure out the real type
  [(+ - * /)
   (token-constructor
    [Number ...+ -> Number])])
