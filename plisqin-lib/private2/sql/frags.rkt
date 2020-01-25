#lang racket

; Only submodules are allowed to `provide` anything.
; Put raw definitions in this module, and use `module+` to provide wrappers.


; Move as much code to frag-helpers.rkt as possible.
; We are about to redefine things like `and` and `+` so we want to avoid writing
; much code in this namespace.
(require "frags.helpers.rkt"
         "fragment.rkt"
         (for-syntax "../_types.rkt"))

; == Clauses ==
(def select #:kind 'select
  #:reduce (let ([as-name (match tokens
                            [(list a)
                             #:when (fragment? a)
                             (fragment-as-name a)]
                            [else #f])]
                 [tokens (->Bit tokens)])
             (if as-name
                 (list tokens " as " (~a as-name))
                 tokens)))

(def group-by #:kind 'group-by
  #:reduce (->Bit tokens))

(def order-by #:kind 'order-by
  #:reduce (match tokens
             [(list dir rest ...)
              #:when (member dir '(asc desc))
              (list (->Bit rest) (format " ~a" dir))]
             [else (->Bit tokens)]))

(define-syntax-rule (def-where-style id ...)
  (begin
    (def id #:kind 'id
      #:reduce (->Bool tokens))
    ...))
(def-where-style where join-on having)

; == Fragments ==
(define-syntax-rule (def-scalar-style id ...)
  (begin
    (def id #:kind 'id #:reduce tokens)
    ...))

(def-scalar-style scalar aggregate subquery sql)

(def bit #:kind 'scalar #:reduce tokens)



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
  #:reduce (list "exists " (parens tokens)))

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


; Export with types via submodules
(define-syntax-rule (do-export mod-id keyword)
  (module+ mod-id
    (export keyword
            select where group-by having order-by join-on
            scalar bit aggregate subquery sql
            count avg min max sum exists)
    (module+ operators
      (export keyword
              and or not
              = <> < <= > >=
              like not-like
              is is-not
              in not-in
              + - * /))))

(do-export unsafe #:unsafe)
(do-export loose #:loose)
(do-export strict #:strict)

(module+ test
  (require rackunit
           (submod ".." unsafe)
           (submod ".." unsafe operators))

  ; Check that each id is bound to a fragment constructor.
  (define-syntax-rule (check-frags frag-id ...)
    (begin
      ; The unsafe fragment constructor should accept any arguments
      (let* ([result (frag-id 1 "2" '(three four))]
             [expected (format "(~a 1 \"2\" '(three four))" 'frag-id)]
             [actual (format "~v" result)])
        (check-equal? expected actual))
      ...))

  (check-frags select where group-by having order-by join-on
               scalar bit aggregate subquery sql
               count avg min max sum exists)
  (check-frags and or not
               = <> < <= > >=
               like not-like
               is is-not
               in not-in
               + - * /))
