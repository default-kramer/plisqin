#lang racket

(module all racket
  (provide (all-defined-out))
  (require racket/struct "util.rkt")

  (define-syntax-rule (def-contract NAME BODY)
    (begin
      (define NAME (flat-named-contract 'NAME BODY))
      (def-doc NAME (racketblock BODY))))

  (define (my-custom-write x port mode)
    ; https://docs.racket-lang.org/reference/Printer_Extension.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._gen~3acustom-write%29%29
    (define (to-list x)
      (match x
        [(dateadd _ date interval)
         `(dateadd ,(to-list date) ,interval)]
        [(fragment _ kind tokens)
         (list kind (to-list tokens))]
        [(source _ alias table uid)
         `(source ,(to-list alias) ,(to-list table) ,(to-list uid))]
        [(query _ source clauses joins)
         `(query ,(to-list source) ,(to-list clauses) ,(to-list joins))]
        [(join _ type query clauses)
         `(join ,(to-list type) ,(to-list query) ,(to-list clauses))]
        [(binding _ join)
         `(binding ,(to-list join))]
        [(injection _ target placeholder fragment)
         `(injection ,(to-list target) ,(to-list placeholder) ,(to-list fragment))]
        [x #:when (list? x)
           (map to-list x)]
        [else x]))
    (match mode
      [#t (write (to-list x) port)]
      [#f (display (to-list x) port)]
      [0 (print (to-list x) port)]
      [1 (print (to-list x) port)]))

  (struct metadata (key hidden? value) #:transparent)
  (define (metadata-visible? x)
    (not (metadata-hidden? x)))

  (struct token (metadata) #:transparent) ; (listof metadata?)

  (def-contract sql-token?
    (or/c token?
          string?
          number?
          'db-now))

  ; It's convenient for the API to allow unflattened lists of tokens.
  ; This contract will accept an unflattened list of tokens and automatically flatten it.
  (define token-list? (flattenof sql-token?))

  (define-syntax-rule (def-token name name? ([fields contract-exprs] ...))
    (struct name token (fields ...) #:transparent
      #:methods gen:custom-write
      [(define write-proc my-custom-write)]
      #:methods gen:equal+hash
      [(define (to-list x)
         ;(-> name? list?)
         ; Return a list containing the stuff we care about for equality.
         (match x
           [(name metadata fields ...)
            ; Put the visible metadata into a set so that the order doesn't matter
            (list (apply set (filter metadata-visible? metadata))
                  fields ...)]
           [else (error (format "wanted ~a, got ~a" 'name x))]))
       (define (equal-proc a b equal-recur)
         (and (name? a)
              (name? b)
              (equal-recur (to-list a) (to-list b))))
       (define (hash-proc x proc)
         (proc (to-list x)))
       (define hash2-proc hash-proc)]
      #:guard (build-guard-proc [base #f]
                                [fields contract-exprs]
                                ...)))

  (def-token fragment fragment? ([kind fragment-kind?]
                                 [tokens token-list?]))

  (define (empty-metadata) '())

  ; Contract for a fragment constructor
  (define (fragment-contract return-contract)
    (->* () () #:rest token-list? return-contract))

  (define-syntax-rule (define-frag SYMBOL CTOR TESTER)
    (begin
      (define (TESTER x)
        (and (fragment? x)
             (equal? SYMBOL (fragment-kind x))))

      (define/contract (CTOR . tokens)
        (fragment-contract TESTER)
        (fragment (empty-metadata) SYMBOL tokens))))

  (define-frag 'Select select select?)
  (define-frag 'Where where where?)
  (define-frag 'JoinOn join-on join-on?)
  (define-frag 'GroupBy group-by group-by?)
  (define-frag 'OrderBy order-by order-by?)
  (define-frag 'Having having having?)
  (define-frag 'Scalar scalar scalar?)
  (define-frag 'Aggregate make-aggregate aggregate?)
  (define-frag 'Bool bool bool?)
  (define-frag 'Subquery subquery subquery?)
  (define-frag 'Sql sql sql?)
  ; Silence allows you to put a token into an expression but not render it to SQL.
  ; Designed so that (count x) can return (sql "count(*)" (silence x))
  (define-frag 'Silence silence silence?)

  (def-contract fragment-kind?
    (or/c 'Select
          'Where
          'JoinOn
          'GroupBy
          'OrderBy
          'Having
          'Scalar
          'Aggregate
          'Bool
          'Subquery
          'Sql
          'Silence))

  (def-contract query-clause?
    (or/c select?
          where?
          group-by?
          order-by?
          having?))

  (def-contract clause?
    (or/c query-clause?
          join-on?))

  (def-token source source? ([alias string?]
                             [table (or/c string? subquery?)]
                             [uid #f]))

  (def-token query query? ([source source?]
                           [clauses (listof query-clause?)]
                           [joins (listof join?)]))

  (def-token join join? ([type join-type?]
                         [query query?]
                         [clauses (listof join-on?)]))

  (def-token binding binding? ([join join?]))

  ; TODO I think target should actually be grouped-join?
  ; Oh wait, target can also be a source? for scalar injection.
  (def-token injection injection? ([target (or/c join? binding? source?)]
                                   [placeholder source?]
                                   [fragment fragment?]))

  (def-contract join-type?
    (or/c 'InnerJoin
          'LeftJoin
          'CrossApply
          'OuterApply))

  ; A value that can be added to a query.
  (def-contract statement?
    (or/c clause?
          join-type?))

  ; In the from and join macros, we allow lists of statements.
  (def-contract statement-expr?
    (or/c statement? (listof statement?)))

  ; Use prop:procedure so that the time units can be procedures or values.
  ; For example, if "day" is a time-unit instance,
  ; we can do (interval #f 1 day) or (day 1)
  (struct time-unit (symbol) #:transparent
    #:property prop:procedure (λ(me num) (apply-time-unit me num)))

  ; Intervals are not tokens because we can't render them to SQL
  (struct interval (added-to qty unit) #:transparent
    #:guard (build-guard-proc [added-to (or/c interval? #f)]
                              [qty real?]
                              [unit time-unit?]))

  (define/contract (apply-time-unit me num)
    (-> time-unit? real? interval?)
    (interval #f num me))

  (def-token dateadd dateadd? ([date sql-token?]
                               [interval interval?])))

; Provide everything except the constructor
(require 'all)
(define-syntax-rule (provide-struct struct-id ...)
  (begin
    (provide (except-out (struct-out struct-id) struct-id))
    ...))
(provide-struct source fragment query join binding injection token)