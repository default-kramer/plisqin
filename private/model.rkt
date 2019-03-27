#lang racket

(module all racket
  (provide (all-defined-out))
  (require racket/struct "util.rkt")

  (define-syntax-rule (def-contract NAME BODY)
    (begin
      (define NAME (flat-named-contract 'NAME BODY))
      (def-doc NAME (racketblock BODY))))

  (define already-writing (make-parameter #f))

  (define (my-custom-write x port mode)
    ; https://docs.racket-lang.org/reference/Printer_Extension.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._gen~3acustom-write%29%29
    (define (to-list x)
      (define root? (not (already-writing)))
      (parameterize ([already-writing #t])
        (match x
          [(dateadd _ date interval)
           `(dateadd ,(to-list date) ,interval)]
          [(fragment _ kind tokens)
           (list kind (to-list tokens))]
          [(source _ alias table uid)
           `(source ,(to-list alias) ,(to-list table) ,(to-list uid))]
          [(query _ source clauses joins options)
           `(query ,(to-list source) ,(to-list clauses) ,(to-list joins) ,options)]
          [(join _ type query clauses)
           `(join ,(to-list type) ,(to-list query) ,(to-list clauses))]
          [(binding _ join)
           `(binding ,(to-list join))]
          [(injection _ target placeholder fragment)
           `(injection ,(to-list target) ,(to-list placeholder) ,(to-list fragment))]
          [(cases _ of else contents)
           `(cases ,(to-list of) ,(to-list else) ,contents)]
          [(param _ name value)
           `(param ,name ,value)]
          [(value _ content)
           `(val ,content)]
          [(raw-sql _ content)
           ; At root level, indicate this is raw sql.
           ; But if we're inside another token, it doesn't need to be restated all the time.
           (if root?
               `(raw-sql ,content)
               content)]
          [x #:when (list? x)
             (map to-list x)]
          [else x])))
    (match mode
      [#t (write (to-list x) port)]
      [#f (display (to-list x) port)]
      [0 (print (to-list x) port)]
      [1 (print (to-list x) port)]))

  (struct metadata (key hidden? value) #:transparent)
  (define (metadata-visible? x)
    (not (metadata-hidden? x)))

  (struct token (metadata) #:transparent) ; (listof metadata?)

  ; The "in" operator relies on the fact that (sql-token? x) implies (not (list? x))
  ; So don't change that fact.
  (def-contract sql-token?
    (or/c token?
          number?
          'db-now
          'concat))

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

  (def-token value value? ([content (or/c string? number?)]))
  (def-token raw-sql raw-sql? ([content string?]))
  (def-token param param? ([name (or/c string? identifier? #f)]
                           [value any/c]))

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

  (define-frag 'Select select-raw select?)
  (define-frag 'Where where where?)
  (define-frag 'JoinOn join-on join-on?)
  (define-frag 'GroupBy group-by group-by?)
  (define-frag 'OrderBy order-by-raw order-by?)
  (define-frag 'Having having having?)
  (define-frag 'Scalar scalar-raw scalar?)
  (define-frag 'Aggregate make-aggregate aggregate?)
  (define-frag 'Bool bool-raw bool?)
  (define-frag 'Subquery subquery subquery?)
  (define-frag 'Sql sql-raw sql?)
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

  (struct limit (num) #:transparent)
  (struct offset (num) #:transparent)
  (struct distinct (flag) #:transparent)

  (def-contract query-clause?
    (or/c select?
          where?
          group-by?
          order-by?
          having?
          limit?
          offset?
          distinct?))

  (def-contract clause?
    (or/c query-clause?
          join-on?))

  (def-token source source? ([alias string?]
                             [table (or/c string? subquery?)]
                             [uid #f]))

  (def-token query query? ([source source?]
                           [clauses (listof query-clause?)]
                           [joins (listof join?)]
                           [options (and/c hash? immutable?)]))

  (def-token join join? ([type join-type?]
                         [query query?]
                         [clauses (listof join-on?)]))

  (def-token binding binding? ([join join?]))

  ; TODO I think target should actually be grouped-join?
  ; Oh wait, target can also be a source? for scalar injection.
  (def-token injection injection? ([target (or/c join? binding? source?)]
                                   [placeholder source?]
                                   [fragment fragment?]))

  ; The SQL case statement comes in two forms; this structure will model both.
  ; 1) CASE input WHEN value THEN result [WHEN ...] [ELSE result] END
  ; 2) CASE WHEN condition THEN result [WHEN ...] [ELSE result] END
  ; The second form will use #f for the input.
  ; Both forms allow #f to indicate the absence of "else".
  (def-token cases cases? ([of (or/c #f sql-token?)]
                           [else (or/c #f sql-token?)]
                           ; the pair is (cons when-token then-token)
                           [contents (listof (cons/c sql-token? sql-token?))]))

  ; SQL Server doesn't allow a boolean expression directly in a select list.
  ; We can support it though, by holding it here then wrapping it in a "case" during rendering.
  (def-token selected-bool selected-bool? ([content bool?]))

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
                               [interval interval?]))

  ; Special nothing for use by "appendable"
  (define nothing (gensym "nothing"))
  (define (nothing? x)
    (eq? nothing x))

  ; An "appendable" means a cond-like proc that can be appended to.
  ; The proc always takes 1 argument - a list "arglist"
  ; TODO it might be nice to maintain a list of syntax objects for debugging purposes,
  ; so that you can "see" what the proc is
  (struct appendable ([proc #:mutable])
    #:property prop:procedure
    (λ(me . arglist)
      (let ([result ((appendable-proc me) arglist)])
        (if (nothing? result)
            (error (format "Could not apply ~v\nNo matching case for args: ~v" me arglist))
            result))))

  (struct table appendable (name default-alias) #:transparent)
  (struct proc appendable (name) #:transparent))

; Provide everything except the constructor
(require 'all)
(define-syntax-rule (provide-struct struct-id ...)
  (begin
    (provide (except-out (struct-out struct-id) struct-id))
    ...))
(provide-struct source fragment query join binding injection token value raw-sql param)