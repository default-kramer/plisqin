#lang racket

; Only submodules are allowed to `provide` anything.
; Put raw definitions in this module, and use `module+` to provide wrappers.


; Move as much code to frag-helpers.rkt as possible.
; We are about to redefine things like `and` and `+` so we want to avoid writing
; much code in this namespace.
(require "frags.helpers.rkt")



; == Clauses ==
(define-syntax-rule (def-clauses id ...)
  (begin
    (def id #:kind 'id #:reduce tokens)
    ...))

(def-clauses
  ; `select` should do bool->scalar conversion, but when?
  ; Either during construction or during reduction (to sql)... not sure which is better.
  ; Also, maybe `group-by` and `order-by` should do the same?
  select group-by order-by
  where join-on having)



; == Fragments ==
; we can reuse def-clauses for now
(def-clauses scalar aggregate bool subquery sql)



; == SQL Functions ==
; TODO still need
#;(void '(case-when
          case))

(define-syntax-rule (def-aggs id ...)
  (begin
    (def id #:kind 'aggregate
      #:reduce (list (~a 'id) (parens tokens)))
    ...))

(def-aggs count avg max min sum)

(def exists #:kind 'bool
  #:reduce (list "exists" (parens tokens)))

#;(def round #:kind 'scalar
    #:reduce (error "TODO need dialect here"))



; == "Operators" ==
; TODO still need
#;(void '(|| ??))
; But they should probably be renamed

(define-syntax-rule (def-and/or id ...)
  (begin
    (def id #:kind 'bool
      #:reduce (parens (interpose (format " ~a " 'id) tokens)))
    ...))

(def-and/or and or)

(def not #:kind 'bool
  #:reduce (list "not " (parens tokens)))

(define-syntax-rule (def-comparisons [id sqlname] ...)
  (begin
    (def id #:kind 'bool
      #:reduce (parens (insert-ands sqlname tokens)))
    ...))

(def-comparisons
  [=  "="]
  [<> "<>"]
  [<  "<"]
  [<= "<="]
  [>  ">"]
  [>= ">="]
  [like "like"]
  [not-like "not like"]
  [is "is"]
  [is-not "is-not"]
  [in "in"]
  [not-in "not in"])

(define-syntax-rule (def-math id ...)
  (begin
    (def id #:kind 'scalar
      #:reduce (parens (interpose (format " ~a " 'id) tokens)))
    ...))

(def-math + - * /)



; Begin providing
(module+ untyped
  (provide select group-by order-by
           where join-on having))

; The `type` macro should create a wrapper function that checks the argument
; types and assign!s the return type.
; It can also store the type for Scribble to extract.
; So be careful not to use a macro that will ruin typesetting.
; Here `f` is a hypothetical construct that probably exists only for documentation.
; See "guard.rkt" for a partial implementation.
#;(module+ loose
    (type select   (f [content? ...+ -> Token]))
    (type group-by (f [content? ...+ -> Token]))
    (type order-by (f [content? ...+ -> Token]))
    (type where    (f [content? ...+ -> Token]))
    (type join-on  (f [content? ...+ -> Token]))
    (type having   (f [content? ...+ -> Token])))

#;(module+ strict
    (type select   (f [Scalar -> Token]
                      [Bool -> Token]))
    (type group-by (f [Scalar -> Token]
                      [Bool -> Token]))
    (type order-by (f [Scalar -> Token]
                      [Bool -> Token]))

    (type where   (f [Bool -> Token]))
    (type join-on (f [Bool -> Token]))
    (type having  (f [Bool -> Token])))

(module+ untyped
  (provide scalar aggregate bool sql subquery))

(module+ untyped
  (provide count avg max min sum exists))

; Here is where typing gets slightly interesting. For example:
#;(module+ strict
    (type min (f [Number -> Number]
                 [String -> String]
                 [Datetime -> Datetime]))
    (type avg (f [Number -> Number])))

(module+ untyped
  (module+ operators
    (provide and or not
             = <> < <= > >=
             like not-like
             is is-not
             in not-in
             + - * /)))
