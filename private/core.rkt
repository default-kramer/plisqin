#lang racket
(module+ test
  (require rackunit))
(require (submod "model.rkt" all) "util.rkt")

; Provide all struct stuff with a prefix.
; Also, provide all struct members (but not the constructor) without a prefix.
(define-syntax-rule (provide-struct struct-id ...)
  (begin
    (provide (prefix-out s: (struct-out struct-id)))
    ...))
(provide-struct source fragment query join binding injection interval time-unit dateadd)
(provide source? fragment? query? join? binding? injection? interval? time-unit? dateadd?)
(provide (rename-out [make-source source]
                     [make-binding binding]
                     [make-injection injection]
                     [make-dateadd dateadd]
                     [make-interval interval]
                     [make-limit limit]
                     [make-offset offset]
                     [make-distinct distinct]))

(provide add-statement add-join join? is-simple-join? grouped-join? query-fragments
         make-query make-join join-type? sql-token? change-kind
         token-list? fragment-kind? convert-to-subquery
         get-src get-alias reset-uid-for-testing! next-uid normalize
         exists query-scope replace make-injection-placeholder
         statement? statement-expr? queryable?
         token? metadata-get metadata-set fragment-contract
         (struct-out exn:fail:plisqin)
         (struct-out exn:fail:plisqin:invalid-aggregate)
         interval-plus interval-minus interval-negate
         db-now
         query-limit query-offset query-distinct?
         ; fragments
         select select?
         where where?
         join-on join-on?
         group-by group-by?
         order-by order-by?
         having having?
         scalar scalar?
         make-aggregate aggregate?
         bool bool?
         subquery subquery?
         sql sql?
         silence silence?)

(module+ test
  (check-true (fragment? (where (make-source "x" "X")".FIELD > 100"))))

(define limit-val? (or/c #f (and/c integer? positive?)))
(define offset-val? limit-val?)

(define/contract (make-limit x)
  (-> limit-val? limit?)
  (limit x))
(define/contract (make-offset x)
  (-> offset-val? offset?)
  (offset x))
(define/contract (make-distinct x)
  (-> any/c distinct?)
  (distinct (and x #t)))

; What can be used to start a query
(def-contract queryable?
  (or/c source?
        procedure?
        string?
        query?
        join?
        subquery?))

(define/contract (change-kind kind frag)
  (-> fragment-kind? fragment? fragment?)
  (struct-copy fragment frag
               [kind kind]))

; a runtime scope to deterministically generate UIDs
(struct scope (is-global? uid-counter next-proc) #:mutable)
(define current-scope (make-parameter (scope #t -1 sub1)))

; If the current scope is the global one, create a private one.
; Otherwise just return the current scope.
(define/contract (next-scope)
  (-> scope?)
  (if (scope-is-global? (current-scope))
      (scope #f 1 add1)
      (current-scope)))

(define/contract (finalize x)
  (-> (or/c query? join?) (or/c query? join?))
  ; TODO could add a "root" or "sealed" flag here
  x)

(define-syntax-rule (query-scope query-expr)
  (let ([result
         (parameterize ([current-scope (next-scope)])
           query-expr)])
    (finalize result)))

(define (next-uid)
  (let* ([scope (current-scope)]
         [uid (scope-uid-counter scope)]
         [next-proc (scope-next-proc scope)])
    (set-scope-uid-counter! scope (next-proc uid))
    uid))

(define (reset-uid-for-testing!)
  (void))

(define/contract (interval-negate iv)
  (-> interval? interval?)
  (define child-iv (interval-added-to iv))
  (struct-copy interval iv
               [qty (- (interval-qty iv))]
               [added-to (if child-iv
                             (interval-negate child-iv)
                             #f)]))

(define/contract (interval-plus iv1 iv2)
  (-> interval? interval? interval?)
  (if (not (interval-added-to iv1))
      (struct-copy interval iv1
                   [added-to iv2])
      (struct-copy interval iv1
                   [added-to (interval-plus
                              (interval-added-to iv1)
                              iv2)])))

(define/contract (interval-minus iv1 iv2)
  (-> interval? interval? interval?)
  (interval-plus iv1 (interval-negate iv2)))

(define/contract (make-dateadd date-token interval)
  (-> token? interval? dateadd?)
  (if (dateadd? date-token)
      (struct-copy dateadd date-token
                   [interval (interval-plus (dateadd-interval date-token)
                                            interval)])
      (dateadd (empty-metadata) date-token interval)))

(define/contract (make-interval qty unit [added-to #f])
  (->* [number? time-unit?] [(or/c interval? #f)] interval?)
  (interval added-to qty unit))

(define/contract (make-source alias table #:uid [uid #f])
  (->* (string? (or/c string? subquery?))
       (#:uid any/c)
       source?)
  (source (empty-metadata) alias table (or uid (next-uid))))

(module+ test
  (check-equal?
   (struct-copy source (make-source "x" "X" #:uid 0)
                [table "Table"])
   (make-source "x" "Table" #:uid 0)))

(define/contract (add-single-statement x frag)
  (-> (or/c query? join?)
      statement?
      (or/c query? join?))
  (cond
    [(join-type? frag)
     (if (join? x)
         (struct-copy join x
                      [type frag])
         (error "join type cannot be applied to a query: " frag))]
    [(join-on? frag)
     (cond [(join? x) (struct-copy join x
                                   [clauses (append (join-clauses x) (list frag))])]
           [else (error "TODO got join-on in query")])]
    [else
     (cond
       [(join? x) (struct-copy join x
                               [query (add-single-statement (join-query x) frag)])]
       ; else x must be a query
       [(fragment? frag)
        (struct-copy query x
                     [clauses (append (query-clauses x) (list frag))])]
       [(limit? frag)
        (set-option x 'limit (limit-num frag))]
       [(offset? frag)
        (set-option x 'offset (offset-num frag))]
       [(distinct? frag)
        (set-option x 'distinct (distinct-flag frag))]
       )]))

;; set-option
;; If val is #f the hash entry will be cleared.
;; This matters for equality, for example
#; (from x "X")
;; should be equal to
#; (from x "X"
         (distinct #t)
         (distinct #f))
(define/contract (set-option q key val)
  (-> query? any/c any/c query?)
  (struct-copy query q
               [options (if val
                            (hash-set (query-options q) key val)
                            (hash-remove (query-options q) key))]))

(define/contract (get-option q key)
  (-> query? any/c any/c)
  (hash-ref (query-options q) key #f))

(define/contract (query-limit q)
  (-> query? limit-val?)
  (get-option q 'limit))
(define/contract (query-offset q)
  (-> query? offset-val?)
  (get-option q 'offset))
(define/contract (query-distinct? q)
  (-> query? any/c)
  (get-option q 'distinct))

(define/contract (add-statement q frag)
  (-> (or/c query? join?) statement-expr? (or/c query? join?))
  (match frag
    [(list) q]
    [(list f rest ...) (add-statement (add-single-statement q f) rest)]
    [f (add-single-statement q f)]))

(define/contract (make-query src)
  (-> source? query?)
  (query (empty-metadata) src '() '() (make-immutable-hash)))

(define/contract (make-join type x)
  (-> join-type? (or/c source? query? join?) join?)
  (cond [(source? x) (make-join type (make-query x))]
        [(query? x) (join (empty-metadata) type x '())]
        [(join? x) (struct-copy join x
                                [type type])]))

(define/contract (make-binding join)
  (-> join? binding?)
  (binding (empty-metadata) join))

(define/contract (make-injection target placeholder fragment)
  (-> sql-token? source? fragment? injection?)
  (injection (empty-metadata) target placeholder fragment))

(define/contract (add-join q j)
  (-> (or/c query? join?) join? (or/c query? join?))
  (cond [(query? q)
         (struct-copy query q
                      [joins (append (query-joins q) (list j))])]
        [(join? q)
         (struct-copy join q
                      [query (add-join (join-query q) j)])]))

(define/contract (convert-to-subquery x)
  (-> (or/c query? join? binding?) query?)
  (define/contract (convert j)
    (-> join? query?)
    (let* ([q (join-query j)]
           [join-ons (join-clauses j)]
           ; change type from 'JoinOn to 'Where
           [new-wheres (map (λ(clause) (struct-copy fragment clause
                                                    [kind 'Where]))
                            join-ons)])
      (struct-copy query q
                   [clauses (append new-wheres (query-fragments q))])))
  (cond
    [(query? x) x]
    [(join? x) (convert x)]
    [(binding? x) (convert (binding-join x))]))

(define/contract (exists . tokens)
  (->* () () #:rest token-list? bool?)
  ; TODO maybe (subquery x) should have convert-to-subquery built in?
  ; Then this proc could just return (bool "exists "(subquery tokens))
  ; Yeah! And (subquery (subquery blah ...)) should get compressed to (subquery blah ...)
  ; which would normalize the parentheses in the rendered SQL.
  (match tokens
    [(list x)
     #:when (or (query? x)
                (join? x)
                (binding? x))
     (bool "exists ("(convert-to-subquery x)")")]
    [else
     (bool "exists (" tokens ")")]))

;(: filter-frags (-> (Listof Fragment) (Listof FragmentKind) (Listof Fragment)))
(define (filter-frags frags kinds)
  (filter (λ(f) (member (fragment-kind f) kinds)) frags))

;(: query-fragments (-> Query FragmentKind * (Listof Fragment)))
(define (query-fragments q . kinds)
  (let ([all-frags (query-clauses q)])
    (if (empty? kinds)
        all-frags
        (filter-frags all-frags kinds))))

;(: is-simple-join? (-> Query Boolean))
(define (is-simple-join? j)
  (empty? (query-fragments (join-query j))))

(define (grouped-join? x)
  (and (join? x)
       (not (is-simple-join? x))))

(define/contract (get-src x)
  (-> any/c (or/c source? #f))
  (cond [(source? x) x]
        [(query? x) (query-source x)]
        [(join? x) (get-src (join-query x))]
        [(binding? x) (get-src (binding-join x))]
        [(injection? x) (get-src (injection-target x))]
        [else #f]))

(define/contract (get-alias x)
  (-> any/c (or/c string? #f))
  (if (get-src x)
      (source-alias (get-src x))
      #f))

; The "normal form" of a Join is a mechanism to test for equivalence.
; Because we need UIDs, a standard "equals?" test will never work on two different Joins.
; So instead, we test (equals? (normalize join1) (normalize join2))
; The normalize function reassigns all UIDs starting with 1 and forces all aliases to "normal"
; (because the alias also shouldn't hinder equivalence).
; hash (Immutable-HashTable Source NormalSource)
(struct normalizer (hash) #:transparent)

; Given a Source, add it to the normalizer if it doesn't already exist.
; Return the updated normalizer and the normal version of the source.
(define/contract (normalizer-add-source norm src)
  (-> normalizer? source? return?)
  (let ([hash (normalizer-hash norm)])
    (cond [(hash-has-key? hash src)
           (return (hash-ref hash src) norm)]
          [else
           (let* ([normal-src (struct-copy source src
                                           [alias "normal"]
                                           [uid (add1 (length (hash-keys hash)))])]
                  [new-hash (hash-set hash src normal-src)])
             (return normal-src (normalizer new-hash)))])))

; (-> any any)
(define (normalize root)
  (define (visit node norm)
    (-> any/c normalizer? return?)
    (cond [(source? node)
           (normalizer-add-source norm node)]
          [else (return node)]))
  (cdr (walk root #:on-enter visit #:accum (normalizer (make-immutable-hash)))))

(define (replace root x y)
  (define (visit node)
    (if (equal? node x)
        (return y)
        (return node)))
  (walk root #:on-exit visit))

(define/contract (make-injection-placeholder x)
  (-> get-src source?)
  (make-source "__placeholder__" (source-table (get-src x))))

; TODO this sucks, surely there is a better way
(define-syntax-rule (token-copy x forms ...)
  (cond
    [(source? x)
     (struct-copy source x forms ...)]
    [(fragment? x)
     (struct-copy fragment x forms ...)]
    [(query? x)
     (struct-copy query x forms ...)]
    [(join? x)
     (struct-copy join x forms ...)]
    [(binding? x)
     (struct-copy binding x forms ...)]
    [(injection? x)
     (struct-copy injection x forms ...)]))

; Metadata API.
; We can attach metadata to tokens and decide whether it should affect equality or not.
; Warning - these need to use the token? contract instead of sql-token? because we can
; only attach metadata to tokens.
(define/contract (metadata-set key t value #:hidden? [hidden? #t])
  (->* (any/c token? any/c) (#:hidden? any/c) token?)
  (let* ([metas (token-metadata t)]
         [metas (filter (λ(meta) (not (equal? key (metadata-key meta)))) metas)]
         [new-meta (metadata key hidden? value)]
         [metas (cons new-meta metas)])
    (token-copy t
                [metadata #:parent token metas])))

(define/contract (metadata-get key t [else-val #f])
  (->* (any/c token?) (any/c) any/c)
  (let* ([metas (token-metadata t)]
         [metas (filter (λ(meta) (equal? key (metadata-key meta))) metas)])
    (if (empty? metas)
        else-val
        (metadata-value (car metas)))))

(module+ test
  (define-syntax-rule (go forms ...)
    ((λ() forms ...)))
  (go
   (define w (where "3=4"))
   (define w2 (metadata-set 'foo w 'bar))
   (check-equal? w w2)
   (check-equal?
    (equal-hash-code w)
    (equal-hash-code w2))
   (check-equal? 'bar (metadata-get 'foo w2))
   (check-equal? "NONE" (metadata-get 'foo w "NONE"))))


; We previously defined "make-aggregate" because we want "aggregate" to add some error checking.
; Specifically, an aggregate expression must have at most 1 target.
; A target is a grouped-join into which the aggregate could be injected.
(struct exn:fail:plisqin exn:fail () #:transparent)
(struct exn:fail:plisqin:invalid-aggregate exn:fail:plisqin () #:transparent)

(define db-now (scalar 'db-now))

; Define the time units. Plural will be a synonym for singular.
(module time-units racket
  (require (submod "model.rkt" all))
  (define-syntax-rule (def-time-units [singular plural] ...)
    (begin
      (provide singular ...)
      (provide plural ...)
      (define singular (time-unit 'singular))
      ...
      (define plural singular)
      ...
      (define time-unit-symbol?
        (or/c 'singular ...))))

  (def-time-units
    [year years]
    [month months]
    [week weeks]
    [day days]
    [hour hours]
    [minute minutes]
    [second seconds]
    [millisecond milliseconds]
    [microsecond microseconds]))
(require 'time-units)
; TODO Warning! "second" conflicts with Racket's built-in "second".
; Error is very surprising if you do (second list) and get a contract failure
; from time-unit. What would be best here?
(provide (prefix-out : (all-from-out 'time-units)))